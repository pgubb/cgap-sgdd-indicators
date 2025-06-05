# R/modules/filterPanel.R - Module for filter panel

# UI function
filterPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    textInput(ns("search"), "Search", 
              placeholder = "Search by indicator name, sector, use-case..."),
    
    accordion(
      open = c("Mandates"),
      accordion_panel(
        "Mandates", 
        icon = icon("scroll"),
        div(
          class = "modern-checkbox-group",
          checkboxGroupInput(ns("mandates"), "", choices = NULL)
        )
      ),
      accordion_panel(
        "Use cases", 
        icon = icon("sliders"),
        div(
          class = "modern-checkbox-group",
          checkboxGroupInput(ns("use_cases"), "", choices = NULL)
        )
      ),
      accordion_panel(
        "Sectors", 
        icon = icon("sliders"),
        div(
          class = "modern-checkbox-group sector-checkboxes",
          checkboxGroupInput(ns("sectors"), "", choices = NULL)
        )
      )
    ),
    
    div(
      style = "display: flex; gap: 10px; margin-top: 10px;",
      actionButton(ns("reset"), "Reset", 
                   icon = icon("undo"), 
                   class = "btn btn-sm btn-primary")
    )
  )
}

# Server function
filterPanelServer <- function(id, indicators_data) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize filters
    observe({
      updateCheckboxGroupInput(
        session, "mandates",
        choices = sort(unique(indicators_data$main_mandate)),
        selected = unique(indicators_data$main_mandate)
      )
      
      sectors <- sort(unique(indicators_data$main_sector))
      updateCheckboxGroupInput(
        session, "sectors",
        choices = sectors,
        selected = sectors
      )
      
      # Add custom CSS for sector colors
      sector_styles <- paste(
        sapply(sectors, function(sector) {
          color <- SECTOR_COLORS[[sector]]
          if (!is.null(color)) {
            paste0(
              "#", session$ns("sectors"), " input[value='", sector, "'] + span::before { ",
              "background-color: ", color, " !important; ",
              "border-color: ", color, " !important; }",
              "#", session$ns("sectors"), " input[value='", sector, "']:checked + span::before { ",
              "background-color: ", color, " !important; ",
              "border-color: ", color, " !important; }"
            )
          }
        }),
        collapse = "\n"
      )
      
      insertUI(
        selector = "head",
        ui = tags$style(HTML(sector_styles)),
        immediate = TRUE
      )
      
      updateCheckboxGroupInput(
        session, "use_cases",
        choices = names(USE_CASES),
        selected = names(USE_CASES)
      )
    })
    
    # Reset filters
    observeEvent(input$reset, {
      updateCheckboxGroupInput(
        session, "mandates",
        selected = unique(indicators_data$main_mandate)
      )
      
      updateCheckboxGroupInput(
        session, "sectors",
        selected = unique(indicators_data$main_sector)
      )
      
      updateCheckboxGroupInput(
        session, "use_cases",
        selected = names(USE_CASES)
      )
      
      updateTextInput(session, "search", value = "")
    })
    
    # Return reactive with filtered data
    filtered_indicators <- reactive({
      # Check if any filters are selected
      if (length(input$mandates) == 0 || 
          length(input$sectors) == 0 || 
          length(input$use_cases) == 0) {
        return(data.frame())  # Return empty dataframe if no filters selected
      }
      
      pattern <- paste(input$use_cases, collapse = "|")
      
      filtered <- indicators_data %>%
        filter(
          main_mandate %in% input$mandates,
          main_sector %in% input$sectors
        ) %>%
        filter(
          !is.na(use_cases) & 
            str_detect(use_cases, regex(pattern, ignore_case = TRUE))
        )
      
      # Apply search filter if not empty
      if (!is.null(input$search) && input$search != "") {
        search_pattern <- regex(input$search, ignore_case = TRUE)
        filtered <- filtered %>%
          filter(
            str_detect(indicator_name, search_pattern) |
              str_detect(coalesce(indicator_description, ""), search_pattern) | 
              str_detect(coalesce(gender_questions, ""), search_pattern) | 
              str_detect(coalesce(main_sector, ""), search_pattern) |
              str_detect(coalesce(secondary_mandates, ""), search_pattern) |
              str_detect(coalesce(main_mandate, ""), search_pattern) |
              str_detect(coalesce(main_objectives, ""), search_pattern)
          )
      }
      
      # Sort by mandate count, then by other fields
      filtered %>% 
        group_by(main_mandate) %>% 
        add_count() %>% 
        ungroup() %>% 
        arrange(desc(n), main_objectives, main_sector, indicator_name)
    })
    
    # Return the reactive
    return(list(
      filtered_indicators = filtered_indicators
    ))
  })
}