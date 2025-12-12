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
            value = TRUE
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
            value = TRUE
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
              "Filter by financial sector (Payments, Credit, Insurance, etc.)"
            )
          )
        ), 
        icon = icon("chart-pie"),
        div(
          class = "modern-checkbox-group sector-checkboxes",
          checkboxGroupInput(ns("sectors"), "", choices = NULL)
        )
      ),
      
      # Links with other initiatives filter
      accordion_panel(
        value = "initiatives",
        title = div(
          style = "display: inline-flex; align-items: center; gap: 8px;",
          "Links with other initiatives",
          div(
            class = "my-tooltip",
            tags$i(class = "fas fa-info-circle", style = "color: #87CEFA; font-size: 12px;"),
            div(
              class = "my-tooltiptext",
              "Filter indicators that have correspondence with those used by other global organizations and initiatives."
            )
          )
        ), 
        icon = icon("link"),
        div(
          class = "modern-checkbox-group",
          checkboxGroupInput(
            ns("initiatives"), 
            "", 
            choices = c(
              "GPFI" = "GPFI",
              "IMF-FAS" = "IMF",
              "AFI" = "AFI",
              "WE Finance Code" = "WEF", 
              "CGAP FeMA Meter" = "FEMAMETER"
            ),
            selected = character(0)
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
        
        # Foundational indicators preset
        div(
          style = "display: flex; align-items: center; gap: 2px;",
          input_switch(
            ns("presets_foundation"), 
            label = "Foundational indicators",
            value = FALSE
          )
        ),
        
        # Basic preset
        div(
          style = "display: flex; align-items: center; gap: 2px;",
          input_switch(
            ns("presets_digital"), 
            label = "Digital finance ecosystem",
            value = FALSE
          )
        ),
        
      ),
      
      # Reset button
      div(
        style = "padding: 20px 0 10px 0; margin-top: 5px;",
        actionButton(
          ns("reset"), 
          "Reset filters", 
          icon = icon("rotate-left", class = "fas"),  # Modern icon
          class = "btn btn-sm"
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
      
      unique_sectors <- indicators_data %>% filter(!is.na(main_sector)) %>% 
        pull(main_sector) %>% 
        str_split(",") %>% 
        unlist() %>% 
        str_trim() %>% 
        unique() %>% 
        sort()
      
      # Sectors
      sectors <- unique_sectors
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
      updateCheckboxGroupInput(session, "initiatives", selected = character(0))  # Reset initiatives
      updateTextInput(session, "search", value = "")
      
      # Reset toggles
      updateInputSwitch(session, "include_secondary_mandates", value = FALSE)
      updateInputSwitch(session, "include_secondary_objectives", value = FALSE)
      
      # Reset presets filters
      updateInputSwitch(session, "presets_foundation", value = FALSE)
      updateInputSwitch(session, "presets_digital", value = FALSE)
      
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
        })
      
      # FIXED: Objectives filter - now properly returns a logical vector for all rows
      filtered <- filtered %>%
        filter({
          n_rows <- n()
          
          # If no rows, return empty logical (this won't cause issues as there's nothing to filter)
          if (n_rows == 0) {
            logical(0)
          } else if (input$include_secondary_objectives) {
            # Check both main and secondary objectives
            vapply(seq_len(n_rows), function(i) {
              main_obj <- main_objectives[i]
              sec_obj <- secondary_objectives[i]
              
              # Check main objectives
              main_match <- if (!is.na(main_obj) && main_obj != "") {
                any(trimws(unlist(strsplit(main_obj, ","))) %in% objectives_filter)
              } else {
                FALSE
              }
              
              # Check secondary objectives
              sec_match <- if (!is.na(sec_obj) && sec_obj != "") {
                any(trimws(unlist(strsplit(sec_obj, ","))) %in% objectives_filter)
              } else {
                FALSE
              }
              
              main_match || sec_match
            }, logical(1))
          } else {
            # Only check main objectives
            vapply(seq_len(n_rows), function(i) {
              obj <- main_objectives[i]
              if (!is.na(obj) && obj != "") {
                any(trimws(unlist(strsplit(obj, ","))) %in% objectives_filter)
              } else {
                FALSE
              }
            }, logical(1))
          }
        })
      
      # Sectors filter
      filtered <- filtered %>%
        filter({
          # Only check main sectors
          vapply(main_sector, function(sec) {
            if (!is.na(sec) && sec != "") {
              any(trimws(unlist(strsplit(sec, ","))) %in% selected_sectors)
            } else {
              FALSE
            }
          }, logical(1))
        })
      
      # Initiatives filter (GPFI, IMF, AFI, WEF)
      # Only apply if at least one initiative is selected
      if (length(input$initiatives) > 0) {
        selected_initiatives <- input$initiatives
        
        # Build a logical vector indicating which rows have at least one selected initiative
        has_initiative <- rowSums(
          sapply(selected_initiatives, function(col) {
            vals <- filtered[[col]]
            !is.na(vals) & vals != ""
          }),
          na.rm = TRUE
        ) > 0
        
        filtered <- filtered[has_initiative, ]
      }
      
      # Presets filter
      if (input$presets_foundation) {
        filtered <- filtered %>%
          filter(preset_foundation == 1)
      }
      
      # Presets filter
      if (input$presets_digital) {
        filtered <- filtered %>%
          filter(preset_digital == 1)
      }
      
      # Apply search filter if not empty
      if (!is.null(input$search) && nchar(trimws(input$search)) > 0) {
        
        # Convert search input to lowercase and trim whitespace
        search_input <- tolower(trimws(input$search))
        
        # Split into words
        search_words <- unlist(strsplit(search_input, "\\s+"))
        search_words <- search_words[nchar(search_words) > 0]
        
        # Helper function: safely get column value as lowercase character
        safe_lower <- function(x) {
          tolower(as.character(ifelse(is.na(x), "", x)))
        }
        
        # Create a combined searchable text field for each row (do this once, outside the loop)
        # This is more efficient than checking multiple columns repeatedly
        filtered <- filtered %>%
          mutate(
            .search_text = paste(
              safe_lower(indicator_name),
              safe_lower(indicator_description),
              safe_lower(main_sector),
              safe_lower(main_mandate),
              safe_lower(main_objectives),
              safe_lower(secondary_objectives),
              safe_lower(secondary_mandates),
              # Only include these columns if they exist
              if ("GPFI" %in% names(.)) safe_lower(GPFI) else "",
              if ("IMF" %in% names(.)) safe_lower(IMF) else "",
              if ("AFI" %in% names(.)) safe_lower(AFI) else "",
              if ("WEF" %in% names(.)) safe_lower(WEF) else "",
              sep = " "
            )
          )
        
        # For each search word, check if EITHER the original word OR its stemmed version matches
        # This ensures "early" matches "early" in the text, and "cards" matches "card"
        for (word in search_words) {
          # Get stemmed version (if SnowballC available)
          if (requireNamespace("SnowballC", quietly = TRUE)) {
            stemmed_word <- SnowballC::wordStem(word, language = "english")
          } else {
            stemmed_word <- word
          }
          
          # Match if EITHER original word OR stemmed word is found
          # This handles cases like "early" (where stem "earli" won't match "early" in text)
          # and "cards" (where stem "card" will match "card" in text)
          filtered <- filtered %>%
            filter(
              grepl(word, .search_text, fixed = TRUE) | 
                grepl(stemmed_word, .search_text, fixed = TRUE)
            )
        }
        
        # Remove the temporary search column
        filtered <- filtered %>% select(-.search_text)
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