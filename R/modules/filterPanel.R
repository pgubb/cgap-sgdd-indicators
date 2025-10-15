# R/modules/filterPanel.R - Module for filter panel with hierarchical objectives

# UI function
filterPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    textInput(ns("search"), "Search", 
              placeholder = "Search by indicator name, sector, use-case..."),
    
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
      
      # Objectives filter with hierarchical structure
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
        # Hierarchical objectives display
        uiOutput(ns("objectives_ui")),
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
    
      # Presets filter with toggle and CSS tooltip
      accordion_panel(
        value = "presets",
        title = div(
          style = "display: inline-flex; align-items: center; gap: 8px;",
          "Presets",
          div(
            class = "my-tooltip",
            tags$i(class = "fas fa-info-circle", style = "color: #87CEFA; font-size: 12px;"),
            div(
              class = "my-tooltiptext",
              "Toggle filters by presets that align with cross-cutting themes"
            )
          )
        ), 
        icon = icon("sliders"),
        # Basic preset
        div(
          style = "display: flex; align-items: center; gap: 2px;",
          input_switch(
            ns("presets_foundation"), 
            label = "Foundational indicators",
            value = FALSE
          )
        ),
      ),
      
      # Reset button
      div(
        style = "padding: 20px 0 10px 0; border-top: 1px solid #e9ecef; margin-top: 15px;",
        actionButton(
          ns("reset"), 
          "Reset filters", 
          icon = icon("rotate-left", class = "fas"),  # Modern icon
          class = "btn btn-sm",
          style = paste0(
            "background: white; ",
            "border: 2px solid #667eea; ",
            "color: #667eea; ",
            "font-weight: 500; ",
            "padding: 8px 20px; ",
            "border-radius: 20px; ",
            "transition: all 0.3s ease; ",
            "width: 100%; ",
            "margin-top: 10px;"
          ),
          onmouseover = "this.style.background='#667eea'; this.style.color='white';",
          onmouseout = "this.style.background='white'; this.style.color='#667eea';"
        )
      )
    
  )
  )
}

# Server function
filterPanelServer <- function(id, indicators_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive to store selected objectives
    selected_objectives <- reactiveVal(character(0))
    
    # Initialize filters with no selections
    observe({
      # Mandates
      updateCheckboxGroupInput(
        session, "mandates",
        choices = levels(indicators_data$main_mandate),
        selected = character(0)
      )
      
      # Sectors
      sectors <- sort(unique(indicators_data$main_sector))
      updateCheckboxGroupInput(
        session, "sectors",
        choices = sectors,
        selected = character(0)
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
    
    # Render hierarchical objectives UI
    output$objectives_ui <- renderUI({
      ns <- session$ns
      current_selected <- selected_objectives()
      
      # Create hierarchical structure from MND_OBJ_2
      objective_groups <- lapply(names(MND_OBJ_2), function(mandate_name) {
        objectives <- MND_OBJ_2[[mandate_name]]
        
        # Create checkboxes for each objective under this mandate
        checkbox_items <- lapply(objectives, function(obj) {
          input_id <- paste0("obj_", gsub("[^a-zA-Z0-9]", "_", obj))
          
          div(
            class = "objective-item",
            style = "margin-left: 15px; margin-bottom: 5px;",
            tags$input(
              type = "checkbox",
              id = ns(input_id),
              value = obj,
              checked = if (obj %in% current_selected) "checked" else NULL,
              onclick = sprintf(
                "Shiny.setInputValue('%s', {objective: '%s', checked: this.checked}, {priority: 'event'})",
                ns("objective_clicked"),
                obj
              )
            ),
            tags$label(
              `for` = ns(input_id),
              style = "cursor: pointer; user-select: none; margin-left: 5px; font-size: 13px;",
              obj
            )
          )
        })
        
        # Create the mandate header with its objectives
        div(
          class = "objective-group",
          style = "margin-bottom: 15px;",
          div(
            class = "objective-header",
            style = "font-weight: 600; color: #495057; margin-bottom: 8px; font-size: 13px; padding: 5px 0; border-bottom: 1px solid #e9ecef;",
            mandate_name
          ),
          div(
            class = "modern-checkbox-group",
            checkbox_items
          )
        )
      })
      
      tagList(objective_groups)
    })
    
    # Handle objective checkbox clicks
    observeEvent(input$objective_clicked, {
      req(input$objective_clicked)
      
      obj <- input$objective_clicked$objective
      is_checked <- input$objective_clicked$checked
      
      current <- selected_objectives()
      
      if (is_checked) {
        # Add objective if not already selected
        if (!(obj %in% current)) {
          selected_objectives(c(current, obj))
        }
      } else {
        # Remove objective
        selected_objectives(setdiff(current, obj))
      }
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
      selected_objectives(character(0))  # Reset objectives
      updateCheckboxGroupInput(session, "sectors", selected = character(0))
      updateTextInput(session, "search", value = "")
      
      # Reset toggles
      updateInputSwitch(session, "include_secondary_mandates", value = FALSE)
      updateInputSwitch(session, "include_secondary_objectives", value = FALSE)
      updateInputSwitch(session, "include_secondary_sectors", value = FALSE)
      
      # Reset presets filters
      updateInputSwitch(session, "presets_foundation", value = FALSE)
    })
    
    # Return reactive with filtered data
    filtered_indicators <- reactive({
      # If no filters are selected in a category, include all items for that category
      selected_mandates <- if (length(input$mandates) == 0) {
        unique(indicators_data$main_mandate)
      } else {
        input$mandates
      }
      
      # Get selected objectives from reactive value
      objectives_filter <- if (length(selected_objectives()) == 0) {
        # Get all unique objectives
        all_obj <- c(
          unlist(strsplit(indicators_data$main_objectives[!is.na(indicators_data$main_objectives)], ",")),
          unlist(strsplit(indicators_data$secondary_objectives[!is.na(indicators_data$secondary_objectives)], ","))
        )
        unique(trimws(all_obj[all_obj != ""]))
      } else {
        selected_objectives()
      }
      
      selected_sectors <- if (length(input$sectors) == 0) {
        unique(indicators_data$main_sector)
      } else {
        input$sectors
      }
      
      # Apply filters
      filtered <- indicators_data %>%
        filter({
          # Mandate filter with toggle
          if (input$include_secondary_mandates) {
            main_mandate %in% selected_mandates | 
              (!is.na(secondary_mandates) & 
                 sapply(secondary_mandates, function(x) {
                   any(trimws(unlist(strsplit(x, ";"))) %in% selected_mandates)
                 }))
          } else {
            main_mandate %in% selected_mandates
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
                any(trimws(unlist(strsplit(main_obj, ","))) %in% objectives_filter)
              } else {
                FALSE
              }
              
              # Check secondary objectives
              sec_match <- if (!is.na(sec_obj)) {
                any(trimws(unlist(strsplit(sec_obj, ","))) %in% objectives_filter)
              } else {
                FALSE
              }
              
              main_match | sec_match
            })
          } else {
            # Only check main objectives
            sapply(main_objectives, function(obj) {
              if (!is.na(obj)) {
                any(trimws(unlist(strsplit(obj, ","))) %in% objectives_filter)
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
      
      # Presets filter
      if (input$presets_foundation) {
        filtered <- filtered %>%
          filter(preset_foundation == 1)
      }
      
      # Apply search filter if not empty
      if (!is.null(input$search) && input$search != "") {
        search_pattern <- regex(input$search, ignore_case = TRUE)
        filtered <- filtered %>%
          filter(
            str_detect(indicator_name, search_pattern) |
              str_detect(coalesce(indicator_description, ""), search_pattern) | 
              str_detect(coalesce(main_sector, ""), search_pattern) |
              str_detect(coalesce(main_mandate, ""), search_pattern) |
              str_detect(coalesce(main_objectives, ""), search_pattern) | 
              str_detect(coalesce(secondary_objectives, ""), search_pattern) | 
              str_detect(coalesce(secondary_mandates, ""), search_pattern) 
          )
      }
      
      # Sort by mandate count, then by other fields
      filtered %>% 
        group_by(main_mandate) %>% 
        add_count() %>% 
        ungroup() %>% 
        arrange(desc(n), main_objectives, indicator_order)
    })
    
    # Return the reactive
    return(list(
      filtered_indicators = filtered_indicators,
      selected_objectives = selected_objectives  # Add this line
    ))
    
  })
}