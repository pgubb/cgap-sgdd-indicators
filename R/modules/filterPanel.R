# R/modules/filterPanel.R - Module for filter panel with priority switch

# UI function
filterPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    textInput(ns("search"), "Search", 
              placeholder = "Search by indicator name, sector, use-case..."),
    
    # Priority filter switch (new addition)
      div(
        style = "display: flex; align-items: center; gap: 2px;",
        input_switch(
          ns("priority_only"), 
          label = "Featured in CGAP technical guide",
          value = FALSE
        ), 
        #tags$i(class = "fas fa-star", style = "color: gold; font-size: 16px;"),
      ),
    
    accordion(
      open = c("mandates"),
      
      # Mandates filter with toggle and CSS tooltip
      accordion_panel(
        value = "mandates",
        title = div(
          style = "display: inline-flex; align-items: center; gap: 8px;",
          "Mandates",
          div(
            class = "my-tooltip",
            tags$i(class = "fas fa-info-circle", style = "color: #87CEFA; font-size: 12px;"),
            div(
              class = "my-tooltiptext",
              "Filter indicators by their primary regulatory mandate. Toggle below to include secondary mandates in the search."
            )
          )
        ), 
        icon = icon("scroll"),
        div(
          class = "modern-checkbox-group",
          checkboxGroupInput(ns("mandates"), "", choices = NULL)
        ),
        div(
          style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
          bslib::input_switch(
            ns("include_secondary_mandates"), 
            label = "Include secondary mandates",
            value = FALSE
          )
        )
      ),
      
      # Objectives filter with toggle and CSS tooltip
      accordion_panel(
        value = "objectives",
        title = div(
          style = "display: inline-flex; align-items: center; gap: 8px;",
          "Objectives",
          div(
            class = "my-tooltip",
            tags$i(class = "fas fa-info-circle", style = "color: #87CEFA; font-size: 12px;"),
            div(
              class = "my-tooltiptext",
              "Filter by specific regulatory objectives within each mandate (e.g., Access, Usage, Consumer protection). Toggle below to include secondary objectives."
            )
          )
        ), 
        icon = icon("bullseye"),
        div(
          class = "modern-checkbox-group",
          checkboxGroupInput(ns("objectives"), "", choices = NULL),
        ), 
        div(
          style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
          input_switch(
            ns("include_secondary_objectives"), 
            label = "Include secondary objectives",
            value = FALSE
          )
        )
      ),
      
      # Sectors filter with toggle and CSS tooltip
      accordion_panel(
        value = "sectors",
        title = div(
          style = "display: inline-flex; align-items: center; gap: 8px;",
          "Sectors",
          div(
            class = "my-tooltip",
            tags$i(class = "fas fa-info-circle", style = "color: #87CEFA; font-size: 12px;"),
            div(
              class = "my-tooltiptext",
              "Filter by financial sector (Payments, Credit, Insurance, etc.). Toggle below to include indicators that apply to multiple sectors."
            )
          )
        ), 
        icon = icon("chart-pie"),
        div(
          class = "modern-checkbox-group sector-checkboxes",
          checkboxGroupInput(ns("sectors"), "", choices = NULL)
        ), 
        div(
          style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
          input_switch(
            ns("include_secondary_sectors"), 
            label = "Include secondary sectors",
            value = FALSE
          )
        )
      ),
      
      # Use cases filter with CSS tooltip (no toggle)
      accordion_panel(
        value = "use_cases",
        title = div(
          style = "display: inline-flex; align-items: center; gap: 8px;",
          "Use cases",
          div(
            class = "my-tooltip",
            tags$i(class = "fas fa-info-circle", style = "color: #87CEFA; font-size: 12px;"),
            div(
              class = "my-tooltiptext",
              "Filter by practical applications showing how these indicators can be used to address specific regulatory challenges and policy goals."
            )
          )
        ), 
        icon = icon("sliders"),
        div(
          class = "modern-checkbox-group",
          checkboxGroupInput(ns("use_cases"), "", choices = NULL)
        )
      )
      
    ),
    
    div(
      style = "d-flex justify-content-center",
      actionButton(ns("reset"), "Reset filters", 
                   icon = icon("undo"), 
                   class = "btn btn-sm btn-primary")
    )
  )
}

# Server function
filterPanelServer <- function(id, indicators_data) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize filters with no selections (but will show all indicators)
    observe({
      # Mandates
      updateCheckboxGroupInput(
        session, "mandates",
        choices = levels(indicators_data$main_mandate_umbrella),
        selected = character(0)  # Start with none selected
      )
      
      # Objectives - extract unique values from both columns
      all_objectives <- c(
        # Main objectives
        unlist(strsplit(indicators_data$main_objectives[!is.na(indicators_data$main_objectives)], ",")),
        # Secondary objectives
        unlist(strsplit(indicators_data$secondary_objectives[!is.na(indicators_data$secondary_objectives)], ","))
      )
      
      # Clean and get unique objectives
      unique_objectives <- unique(trimws(all_objectives))
      unique_objectives <- unique_objectives[unique_objectives != ""]
      unique_objectives <- sort(unique_objectives)
      
      updateCheckboxGroupInput(
        session, "objectives",
        choices = unique_objectives,
        selected = character(0)  # Start with none selected
      )
      
      # Use cases
      updateCheckboxGroupInput(
        session, "use_cases",
        choices = names(USE_CASES),
        selected = character(0)  # Start with none selected
      )
      
      # Sectors
      sectors <- sort(unique(indicators_data$main_sector))
      updateCheckboxGroupInput(
        session, "sectors",
        choices = sectors,
        selected = character(0)  # Start with none selected
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
    })
    
    updateInputSwitch <- function(session, inputId, value = NULL, label = NULL) {
      if (!is.null(value)) {
        session$sendInputMessage(inputId, list(value = value))
      }
      if (!is.null(label)) {
        session$sendInputMessage(inputId, list(label = label))
      }
    }
    
    # Reset filters (clear all selections)
    observeEvent(input$reset, {
      updateCheckboxGroupInput(session, "mandates", selected = character(0))
      updateCheckboxGroupInput(session, "objectives", selected = character(0))
      updateCheckboxGroupInput(session, "sectors", selected = character(0))
      updateCheckboxGroupInput(session, "use_cases", selected = character(0))
      updateTextInput(session, "search", value = "")
      
      # Reset toggles
      updateInputSwitch(session, "include_secondary_mandates", value = FALSE)
      updateInputSwitch(session, "include_secondary_objectives", value = FALSE)
      updateInputSwitch(session, "include_secondary_sectors", value = FALSE)
      
      # Reset priority filter
      updateInputSwitch(session, "priority_only", value = FALSE)
    })
    
    # Return reactive with filtered data
    filtered_indicators <- reactive({
      # If no filters are selected in a category, include all items for that category
      selected_mandates <- if (length(input$mandates) == 0) {
        unique(indicators_data$main_mandate_umbrella)
      } else {
        input$mandates
      }
      
      selected_objectives <- if (length(input$objectives) == 0) {
        # Get all unique objectives
        all_obj <- c(
          unlist(strsplit(indicators_data$main_objectives[!is.na(indicators_data$main_objectives)], ",")),
          unlist(strsplit(indicators_data$secondary_objectives[!is.na(indicators_data$secondary_objectives)], ","))
        )
        unique(trimws(all_obj[all_obj != ""]))
      } else {
        input$objectives
      }
      
      selected_sectors <- if (length(input$sectors) == 0) {
        unique(indicators_data$main_sector)
      } else {
        input$sectors
      }
      
      selected_use_cases <- if (length(input$use_cases) == 0) {
        names(USE_CASES)
      } else {
        input$use_cases
      }
      
      # Apply filters
      filtered <- indicators_data %>%
        filter({
          # Mandate filter with toggle
          if (input$include_secondary_mandates) {
            main_mandate %in% selected_mandates | 
              (!is.na(secondary_mandates_umbrella) & 
                 sapply(secondary_mandates_umbrella, function(x) {
                   any(trimws(unlist(strsplit(x, ";"))) %in% selected_mandates)
                 }))
          } else {
            main_mandate_umbrella %in% selected_mandates
          }
        }) %>%
        filter({
          # Objectives filter with toggle
          if (input$include_secondary_objectives) {
            # Check both main and secondary objectives
            sapply(1:n(), function(i) {
              main_obj <- main_objectives[i]
              sec_obj <- secondary_objectives[i]
              
              # Check main objectives
              main_match <- if (!is.na(main_obj)) {
                any(trimws(unlist(strsplit(main_obj, ","))) %in% selected_objectives)
              } else {
                FALSE
              }
              
              # Check secondary objectives
              sec_match <- if (!is.na(sec_obj)) {
                any(trimws(unlist(strsplit(sec_obj, ","))) %in% selected_objectives)
              } else {
                FALSE
              }
              
              main_match | sec_match
            })
          } else {
            # Only check main objectives
            sapply(main_objectives, function(obj) {
              if (!is.na(obj)) {
                any(trimws(unlist(strsplit(obj, ","))) %in% selected_objectives)
              } else {
                FALSE
              }
            })
          }
        }) %>%
        filter({
          # Sector filter with toggle
          if (input$include_secondary_sectors) {
            main_sector %in% selected_sectors | 
              (!is.na(secondary_sectors) & 
                 sapply(secondary_sectors, function(x) {
                   any(trimws(unlist(strsplit(x, ";"))) %in% selected_sectors)
                 }))
          } else {
            main_sector %in% selected_sectors
          }
        })
      
      # Use cases filter
      pattern <- paste(selected_use_cases, collapse = "|")
      filtered <- filtered %>%
        filter(
          !is.na(use_cases) & 
            str_detect(use_cases, regex(pattern, ignore_case = TRUE))
        )
      
      # Priority filter (new addition)
      if (input$priority_only) {
        filtered <- filtered %>%
          filter(high_priority == "High priority")
      }
      
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
              str_detect(coalesce(main_objectives, ""), search_pattern) | 
              str_detect(coalesce(secondary_objectives, ""), search_pattern) 
          )
      }
      
      # Sort by mandate count, then by other fields
      filtered %>% 
        group_by(main_mandate_umbrella) %>% 
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