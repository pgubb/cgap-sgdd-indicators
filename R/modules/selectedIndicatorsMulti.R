# R/modules/selectedIndicatorsMulti.R - Enhanced version with multiple sets support

# UI function
selectedIndicatorsMultiUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Set manager UI at the top
    setManagerUI(ns("set_manager")),
    
    # Header section with count and actions
    div(
      class = "selected-indicators-header",
      style = paste0(
        "background: linear-gradient(135deg, #6F5B9D 0%, #402C60 100%); ",
        "color: white; ",
        "padding: 24px; ",
        "border-radius: 16px; ",
        "margin-bottom: 32px; ",
        "box-shadow: 0 8px 32px rgba(102, 126, 234, 0.3);"
      ),
      
      div(
        style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 16px;",
        
        # Left side - title and count
        div(
          uiOutput(ns("count_badge"))
        ),
        
        # Right side - action buttons
        div(
          class = "d-flex gap-2",
          uiOutput(ns("action_buttons"))
        )
      )
    ),
    
    # Grid of selected indicators
    uiOutput(ns("indicators_grid"))
  )
}

# Server function  
selectedIndicatorsMultiServer <- function(id, indicators_data, sector_colors) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize set manager
    set_manager <- setManagerServer("set_manager")
    
    # Reactive to track expanded cards
    expanded_cards <- reactiveVal(character(0))
    
    # Get selected indicators for active set
    selected_indicators <- reactive({
      indicator_ids <- set_manager$get_active_indicators()
      
      if (length(indicator_ids) == 0) {
        return(data.frame())
      }
      
      indicators_data %>%
        filter(indicator_id %in% indicator_ids) %>%
        select(indicator_id, indicator_name, indicator_description, indicator_long_description, 
               main_mandate, main_objectives, main_sector, 
               preset_foundation, preset_digital, gender_questions)
    })
    
    # Count badge with active set name
    output$count_badge <- renderUI({
      selected <- selected_indicators()
      active_set <- set_manager$active_set()
      
      if (is.null(selected) || nrow(selected) == 0) {
        div(
          h3(
            icon("clipboard-list", class = "fas", style = "margin-right: 8px;"),
            active_set,
            style = "margin: 0; font-size: 24px; font-weight: 700;"
          ),
          p(
            "Start building your indicator set by clicking 'Add' on indicators in the Browse page",
            style = "margin: 8px 0 0 0; font-size: 14px; opacity: 0.9;"
          )
        )
      } else {
        div(
          h3(
            icon("clipboard-check", class = "fas", style = "margin-right: 8px;"),
            active_set,
            style = "margin: 0; font-size: 24px; font-weight: 700;"
          ),
          p(
            paste("You have selected", nrow(selected), "indicator(s) in this set"),
            style = "margin: 8px 0 0 0; font-size: 14px; opacity: 0.9;"
          )
        )
      }
    })
    
    # Action buttons
    output$action_buttons <- renderUI({
      selected <- selected_indicators()
      
      if (!is.null(selected) && nrow(selected) > 0) {
        tagList(
          downloadButton(ns("download_csv"), 
                         "CSV", 
                         icon = icon("download"),
                         class = "btn btn-light btn-sm",
                         style = "font-weight: 500;"),
          actionButton(ns("open_pdf"), 
                       "Print Report", 
                       icon = icon("file-pdf"),
                       class = "btn btn-light btn-sm",
                       style = "font-weight: 500;")
        )
      }
    })
    
    # Handle card expansion toggle
    observeEvent(input$toggle_expand, {
      indicator_id <- input$toggle_expand
      current_expanded <- expanded_cards()
      
      if (indicator_id %in% current_expanded) {
        expanded_cards(setdiff(current_expanded, indicator_id))
      } else {
        expanded_cards(c(current_expanded, indicator_id))
      }
    })
    
    # Selected indicators grid (simplified version of original)
    output$indicators_grid <- renderUI({
      selected <- selected_indicators()
      
      if (is.null(selected) || nrow(selected) == 0) {
        return(
          div(
            style = paste0(
              "text-align: center; ",
              "padding: 60px 20px; ",
              "background: #f8f9fa; ",
              "border-radius: 12px; ",
              "border: 2px dashed #dee2e6;"
            ),
            icon("inbox", class = "fas", style = "font-size: 48px; color: #adb5bd; margin-bottom: 16px;"),
            h4("No indicators in this set yet", style = "color: #6c757d; margin-bottom: 8px;"),
            p("Browse indicators and click 'Add' to build your set", 
              style = "color: #adb5bd; margin: 0;")
          )
        )
      }
      
      current_expanded <- expanded_cards()
      
      # Create grid of indicator cards
      div(
        style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(380px, 1fr)); gap: 24px;",
        
        lapply(1:nrow(selected), function(i) {
          ind <- selected[i, ]
          
          # Safely get sector color with fallback
          sector_color <- sector_colors[[ind$main_sector]]
          if (is.null(sector_color) || !is.character(sector_color) || sector_color == "") {
            sector_color <- "#EAE9E6"
          }
          
          is_expanded <- ind$indicator_id %in% current_expanded
          
          div(
            class = "selected-indicator-card",
            style = paste0(
              "background: white; ",
              "border: 1px solid #e9ecef; ",
              "border-radius: 12px; ",
              "overflow: hidden; ",
              "transition: all 0.3s ease; ",
              "box-shadow: 0 2px 8px rgba(0,0,0,0.08); ",
              "display: flex; ",
              "flex-direction: column; ",
              "height: 100%;"
            ),
            
            # Color accent bar
            div(
              style = paste0(
                "height: 6px; ",
                "background: linear-gradient(90deg, ", sector_color, " 0%, ", 
                adjustcolor(sector_color, alpha.f = 0.6), " 100%);"
              )
            ),
            
            # Card content
            div(
              style = "padding: 20px; flex: 1; display: flex; flex-direction: column;",
              
              h4(
                ind$indicator_name,
                style = paste0(
                  "font-size: 16px; ",
                  "font-weight: 600; ",
                  "color: #1a1a1a; ",
                  "margin: 0 0 12px 0; ",
                  "line-height: 1.4;"
                )
              ),
              
              # Badges
              div(
                style = "display: flex; flex-wrap: wrap; gap: 6px; margin-bottom: 12px;",
                
                span(
                  ind$main_mandate,
                  style = paste0(
                    "background-color: #e3f2fd; ",
                    "color: #1565c0; ",
                    "padding: 3px 8px; ",
                    "border-radius: 12px; ",
                    "font-size: 11px; ",
                    "font-weight: 500; ",
                    "border: 1px solid #bbdefb;"
                  )
                ),
                
                span(
                  ind$main_sector,
                  style = paste0(
                    "background-color: ", sector_color, "; ",
                    "color: #333; ",
                    "padding: 3px 8px; ",
                    "border-radius: 12px; ",
                    "font-size: 11px; ",
                    "font-weight: 500; ",
                    "border: 1px solid ", adjustcolor(sector_color, red.f = 0.8, green.f = 0.8, blue.f = 0.8), ";"
                  )
                )
              ),
              
              # Description
              p(
                substr(ind$indicator_description, 1, 150),
                if(nchar(ind$indicator_description) > 150) "...",
                style = paste0(
                  "font-size: 13px; ",
                  "color: #5f6368; ",
                  "line-height: 1.5; ",
                  "margin: 0 0 16px 0; ",
                  "flex: 1;"
                )
              ),
              
              # Comments section
              div(
                style = "margin-top: auto;",
                textAreaInput(
                  inputId = ns(paste0("comment_", ind$indicator_id)),
                  label = div(
                    style = "display: flex; align-items: center; gap: 6px; margin-bottom: 8px;",
                    icon("comment-dots", class = "fas", style = "font-size: 12px; color: #6c757d;"),
                    span("Notes", style = "font-size: 13px; font-weight: 500; color: #495057;")
                  ),
                  placeholder = "Add notes...",
                  width = "100%",
                  rows = 2,
                  resize = "vertical"
                )
              )
            )
          )
        })
      )
    })
    
    # Open PDF in new tab
    observeEvent(input$open_pdf, {
      selected <- selected_indicators()
      
      if (!is.null(selected) && nrow(selected) > 0) {
        # Get comments from UI
        comments <- sapply(selected$indicator_id, function(id) {
          input[[paste0("comment_", id)]] %||% ""
        })
        
        # Create HTML content for the report
        report_html <- create_pdf_report(selected, comments, sector_colors)
        
        # Send custom message to open in new tab
        session$sendCustomMessage("openPdfInNewTab", list(html = report_html))
      }
    })
    
    # CSV download handler with set name in filename
    output$download_csv <- downloadHandler(
      filename = function() {
        set_name <- gsub("[^A-Za-z0-9_-]", "_", set_manager$active_set())
        paste0(set_name, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        selected <- selected_indicators()
        
        if (!is.null(selected) && nrow(selected) > 0) {
          # Update comments from UI
          selected$comment <- vapply(selected$indicator_id, function(id) {
            input[[paste0("comment_", id)]] %||% ""
          }, character(1))
          
          write.csv(selected, file, row.names = FALSE)
        } else {
          write.csv(data.frame(), file, row.names = FALSE)
        }
      }
    )
    
    # Return set_manager for app to use
    return(set_manager)
  })
}