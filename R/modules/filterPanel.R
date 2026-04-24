# R/modules/filterPanel.R - Module for filter panel with hierarchical objectives

# UI function
filterPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    textInput(ns("search"), "Search", 
              placeholder = "Search by keyword or phrase"),
    
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
          style = "margin-top: 10px; padding: 6px 10px; background-color: #f8f9fa; border-radius: 5px;",
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
          style = "margin-top: 10px; padding: 6px 10px; background-color: #f8f9fa; border-radius: 5px;",
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
          "Services",
          div(
            class = "my-tooltip",
            tags$i(class = "fas fa-info-circle", style = "color: #87CEFA; font-size: 12px;"),
            div(
              class = "my-tooltiptext",
              "Filter by financial service (Payments, Credit, Insurance, etc.)"
            )
          )
        ), 
        icon = icon("chart-pie"),
        div(
          class = "modern-checkbox-group sector-checkboxes",
          checkboxGroupInput(ns("sectors"), "", choices = NULL)
        ),
        # NEW: Toggle for multi-sector indicators
        div(
          style = "margin-top: 10px; padding: 6px 10px; background-color: #f8f9fa; border-radius: 5px;",
          input_switch(
            ns("include_multi_sector"), 
            label = "Include cross-cutting indicators",
            value = TRUE
          )
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
              "A2ii FeMa-Meter" = "FEMAMETER"
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
        
        # Presets with inline memo link
        div(
          style = "display: flex; align-items: center; gap: 2px;",
          input_switch(
            ns("presets_digital"),
            label = tagList(
              "Digital finance ecosystem",
              tags$a(
                class = "preset-memo-link",
                href = "#",
                title = "Read about this preset",
                onclick = "openPresetMemo('preset_digital'); return false;",
                icon("file-lines", class = "fas")
              )
            ),
            value = FALSE
          )
        ),
        div(
          style = "display: flex; align-items: center; gap: 2px;",
          input_switch(
            ns("presets_msme"),
            label = tagList(
              "MSME focus",
              tags$a(
                class = "preset-memo-link",
                href = "#",
                title = "Read about this preset",
                onclick = "openPresetMemo('preset_msme'); return false;",
                icon("file-lines", class = "fas")
              )
            ),
            value = FALSE
          )
        ),
        div(
          style = "display: flex; align-items: center; gap: 2px;",
          input_switch(
            ns("presets_finhealth"),
            label = tagList(
              "Financial health",
              tags$a(
                class = "preset-memo-link",
                href = "#",
                title = "Read about this preset",
                onclick = "openPresetMemo('preset_finhealth'); return false;",
                icon("file-lines", class = "fas")
              )
            ),
            value = FALSE
          )
        ),

      ),
      
      # Reset button
      div(
        style = "padding: 12px 0 4px 0;",
        actionButton(
          ns("reset"),
          "Reset filters",
          icon = icon("rotate-left", class = "fas"),
          class = "btn btn-sm reset-filters-btn"
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

    # --- Pre-compute lookup tables (once at module init) ---

    # Secondary mandates: indicator_id → character vector of secondary mandates
    .secondary_mandates_lookup <- {
      ids <- indicators_data$indicator_id
      secs <- indicators_data$secondary_mandates
      lkp <- setNames(vector("list", length(ids)), ids)
      for (i in seq_along(ids)) {
        if (!is.na(secs[i]) && secs[i] != "") {
          lkp[[as.character(ids[i])]] <- trimws(unlist(strsplit(secs[i], ";")))
        }
      }
      lkp
    }

    # Objectives: indicator_id → list(main = c(...), secondary = c(...))
    .objectives_lookup <- {
      ids <- indicators_data$indicator_id
      main_obj <- indicators_data$main_objectives
      sec_obj <- indicators_data$secondary_objectives
      lkp <- setNames(vector("list", length(ids)), ids)
      for (i in seq_along(ids)) {
        m <- if (!is.na(main_obj[i]) && main_obj[i] != "") trimws(unlist(strsplit(main_obj[i], ","))) else character()
        s <- if (!is.na(sec_obj[i]) && sec_obj[i] != "") trimws(unlist(strsplit(sec_obj[i], ","))) else character()
        lkp[[as.character(ids[i])]] <- list(main = m, secondary = s)
      }
      lkp
    }

    # All unique objectives (for "no filter selected" case)
    .all_objectives <- unique(c(
      unlist(lapply(.objectives_lookup, function(x) x$main)),
      unlist(lapply(.objectives_lookup, function(x) x$secondary))
    ))

    # Sectors: indicator_id → character vector of sectors
    .sectors_lookup <- {
      ids <- indicators_data$indicator_id
      secs <- indicators_data$main_sector
      lkp <- setNames(vector("list", length(ids)), ids)
      for (i in seq_along(ids)) {
        if (!is.na(secs[i]) && secs[i] != "") {
          lkp[[as.character(ids[i])]] <- trimws(unlist(strsplit(secs[i], ",")))
        } else {
          lkp[[as.character(ids[i])]] <- character()
        }
      }
      lkp
    }

    # Pre-compute search text (once, lowercase concatenation of all searchable fields)
    .search_text <- {
      safe_lower <- function(x) tolower(as.character(ifelse(is.na(x), "", x)))
      paste(
        safe_lower(indicators_data$indicator_name),
        safe_lower(indicators_data$indicator_description),
        safe_lower(indicators_data$main_sector),
        safe_lower(indicators_data$main_mandate),
        safe_lower(indicators_data$main_objectives),
        safe_lower(indicators_data$secondary_objectives),
        safe_lower(indicators_data$secondary_mandates),
        if ("GPFI" %in% names(indicators_data)) safe_lower(indicators_data$GPFI) else "",
        if ("IMF" %in% names(indicators_data)) safe_lower(indicators_data$IMF) else "",
        if ("AFI" %in% names(indicators_data)) safe_lower(indicators_data$AFI) else "",
        if ("WEF" %in% names(indicators_data)) safe_lower(indicators_data$WEF) else "",
        sep = " "
      )
    }
    # Store search text as a column on the data
    indicators_data$.search_text <- .search_text

    # --- Initialize filters with no selections ---
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
    
    # Reset filters (clear all selections and set toggles to ON)
    observeEvent(input$reset, {
      updateCheckboxGroupInput(session, "mandates", selected = character(0))
      selected_objectives(character(0))  # Reset objectives
      updateCheckboxGroupInput(session, "sectors", selected = character(0))
      updateCheckboxGroupInput(session, "initiatives", selected = character(0))  # Reset initiatives
      updateTextInput(session, "search", value = "")
      
      # Reset toggles to ON (TRUE) - these are the "include" toggles
      updateInputSwitch(session, "include_secondary_mandates", value = TRUE)
      updateInputSwitch(session, "include_secondary_objectives", value = TRUE)
      updateInputSwitch(session, "include_multi_sector", value = TRUE)
      
      # Reset presets filters to OFF (FALSE)
      updateInputSwitch(session, "presets_digital", value = FALSE)
      updateInputSwitch(session, "presets_msme", value = FALSE)
      updateInputSwitch(session, "presets_finhealth", value = FALSE)

    })
    
    # Return reactive with filtered data (uses pre-computed lookups for performance)
    filtered_indicators <- reactive({
      selected_mandates <- if (length(input$mandates) == 0) {
        unique(indicators_data$main_mandate)
      } else {
        input$mandates
      }

      objectives_filter <- if (length(selected_objectives()) == 0) {
        .all_objectives
      } else {
        selected_objectives()
      }

      selected_sectors <- if (length(input$sectors) == 0) {
        unique(indicators_data$main_sector)
      } else {
        input$sectors
      }

      # Mandate filter using pre-computed lookup
      mandate_match <- if (input$include_secondary_mandates) {
        vapply(as.character(indicators_data$indicator_id), function(id) {
          indicators_data$main_mandate[indicators_data$indicator_id == as.integer(id)] %in% selected_mandates ||
            any(.secondary_mandates_lookup[[id]] %in% selected_mandates)
        }, logical(1))
      } else {
        indicators_data$main_mandate %in% selected_mandates
      }

      # Objectives filter using pre-computed lookup
      obj_match <- if (input$include_secondary_objectives) {
        vapply(as.character(indicators_data$indicator_id), function(id) {
          obj <- .objectives_lookup[[id]]
          any(obj$main %in% objectives_filter) || any(obj$secondary %in% objectives_filter)
        }, logical(1))
      } else {
        vapply(as.character(indicators_data$indicator_id), function(id) {
          any(.objectives_lookup[[id]]$main %in% objectives_filter)
        }, logical(1))
      }

      # Sectors filter using pre-computed lookup
      sector_match <- vapply(as.character(indicators_data$indicator_id), function(id) {
        secs <- .sectors_lookup[[id]]
        if (length(secs) == 0) return(FALSE)
        if (!input$include_multi_sector && length(secs) > 1) return(FALSE)
        any(secs %in% selected_sectors)
      }, logical(1))

      # Combine all boolean masks
      filtered <- indicators_data[mandate_match & obj_match & sector_match, ]

      # Initiatives filter
      if (length(input$initiatives) > 0) {
        has_initiative <- rowSums(
          sapply(input$initiatives, function(col) {
            vals <- filtered[[col]]
            !is.na(vals) & vals != ""
          }),
          na.rm = TRUE
        ) > 0
        filtered <- filtered[has_initiative, ]
      }

      # Presets filter (intersection when multiple active)
      if (input$presets_digital) {
        filtered <- filtered[!is.na(filtered$preset_digital) & filtered$preset_digital == 1, ]
      }
      if (input$presets_msme) {
        filtered <- filtered[!is.na(filtered$preset_msme) & filtered$preset_msme == 1, ]
      }
      if (input$presets_finhealth) {
        filtered <- filtered[!is.na(filtered$preset_finhealth) & filtered$preset_finhealth == 1, ]
      }

      # Search filter using pre-computed .search_text column
      if (!is.null(input$search) && nchar(trimws(input$search)) > 0) {
        search_words <- unlist(strsplit(tolower(trimws(input$search)), "\\s+"))
        search_words <- search_words[nchar(search_words) > 0]

        search_mask <- rep(TRUE, nrow(filtered))
        for (word in search_words) {
          stemmed_word <- SnowballC::wordStem(word, language = "english")
          search_mask <- search_mask & (
            grepl(word, filtered$.search_text, fixed = TRUE) |
            grepl(stemmed_word, filtered$.search_text, fixed = TRUE)
          )
        }
        filtered <- filtered[search_mask, ]
      }

      # Sort by mandate count, then by other fields
      filtered %>%
        select(-any_of(".search_text")) %>%
        group_by(main_mandate) %>%
        add_count() %>%
        ungroup() %>%
        arrange(desc(n), main_objectives, indicator_name)
    })
    
    # Return the reactive
    return(list(
      filtered_indicators = filtered_indicators,
      selected_objectives = selected_objectives  # Add this line
    ))
    
  })
}