# Updated selectedIndicators module with direct PDF download

# UI function (replace the existing one)
selectedIndicatorsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      style = "width: 100%; min-height: 40px;",
      
      # Left side - count badge
      div(
        class = "d-flex align-items-center",
        uiOutput(ns("count_badge"))
      ),
      
      # Right side - buttons (rendered together)
      div(
        class = "d-flex gap-2",
        uiOutput(ns("action_buttons"))
      )
    ),
    
    uiOutput(ns("indicators"))
  )
}

# Updated server function with direct PDF download
selectedIndicatorsServer <- function(id, selected_indicators, sector_colors) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Count badge
    output$count_badge <- renderUI({
      selected <- selected_indicators()
      
      if (is.null(selected) || nrow(selected) == 0) {
        span(
          icon("times-circle", class = "text-danger", lib = "font-awesome"),
          "No indicators selected yet.",
          style = "font-size: 16px;"
        )
      } else {
        span(
          icon("check-circle", class = "text-success", lib = "font-awesome"),
          paste("You have selected", nrow(selected), "indicator(s)"),
          style = "font-size: 16px; font-weight: bold;"
        )
      }
    })
    
    # Combined action buttons with PDF in new tab
    output$action_buttons <- renderUI({
      selected <- selected_indicators()
      
      if (!is.null(selected) && nrow(selected) > 0) {
        tagList(
          downloadButton(ns("download_csv"), 
                         "Download CSV", 
                         class = "btn btn-success btn-sm"),
          actionButton(ns("open_pdf"), 
                       "Open PDF Report", 
                       icon = icon("external-link-alt"),
                       class = "btn btn-info btn-sm")
        )
      } else {
        # Return NULL when no indicators
        NULL
      }
    })
    
    # Selected indicators display
    output$indicators <- renderUI({
      selected <- selected_indicators()
      
      if (is.null(selected) || nrow(selected) == 0) {
        return(h4(""))
      }
      
      indicator_cards <- lapply(1:nrow(selected), function(i) {
        ind <- selected[i, ]
        sector_color <- sector_colors[[ind$main_sector]]
        
        accordion_panel(
          value = ind$indicator_name,
          title = div(
            span(
              span(ind$indicator_name, 
                   style = "background-color: white; display: inline-block; padding:2px; border-radius: 4px; color:black; font-weight: bold; font-size: 14px"), 
              span(ind$main_objectives, 
                   style = "background-color: #E5E7E6; display: inline-block; padding:2px; border-radius: 4px; color:black; font-weight: normal; font-size: 12px;"), 
              span(ind$main_sector, 
                   style = paste0("background-color: ", sector_color, "; display: inline-block; padding:2px; border-radius: 4px; color:black; font-weight: normal; font-size: 12px")), 
              if (!is.na(ind$high_priority) && ind$high_priority == "High priority") {
                tags$i(class = "fas fa-star", 
                       style = "color: gold; margin-left: 8px; font-size: 14px;")
              }
            )
          ),
          div(
            class = "card-body",
            style = "font-size: 13px; padding-top:10px;",
            div(
              style = "display: flex; justify-content: space-around; align-items: flex-start; gap: 30px;",
              div(
                style = "flex-grow: 1; padding: 10px;",
                p(icon("scroll", lib = "font-awesome"), 
                  strong("Primary mandate:"), ind$main_mandate), 
                p(icon("person", lib = "font-awesome"), 
                  icon("person-dress", lib = "font-awesome"), 
                  strong("Example questions to get started on analysis of indicator by gender:"), 
                  ind$gender_questions)
              ),
              div(
                style = "flex-grow: 1; padding: 10px;",
                textAreaInput(
                  inputId = ns(paste0("comment_", ind$indicator_id)),
                  label = "Add your observations/comments", 
                  width = '600px'
                )
              )
            )
          )
        )
      })
      
      accordion(!!!indicator_cards, open = TRUE)
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
    
    # Keep the direct download option as well (optional)
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("RGDD_indicators_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        selected <- selected_indicators()
        
        if (!is.null(selected) && nrow(selected) > 0) {
          # Get comments from UI
          comments <- sapply(selected$indicator_id, function(id) {
            input[[paste0("comment_", id)]] %||% ""
          })
          
          # Create HTML content for the report
          report_html <- create_pdf_report(selected, comments, sector_colors)
          
          # Write to file
          writeLines(report_html, file, useBytes = TRUE)
        } else {
          # Write empty report
          writeLines("<html><body><h1>No indicators selected</h1></body></html>", file)
        }
      },
      contentType = "text/html"
    )
    
    # CSV download handler (renamed to avoid conflicts)
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("selected_indicators_", Sys.Date(), ".csv")
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
          # Write empty file
          write.csv(data.frame(), file, row.names = FALSE)
        }
      }
    )
  })
}

# Updated create_pdf_report function in R/modules/selectedIndicators.R

# Helper function to convert Shiny tags to HTML string
shiny_tags_to_html <- function(tags_object) {
  if (is.null(tags_object)) {
    return("")
  }
  
  # Use as.character to convert Shiny tags to HTML
  return(as.character(tags_object))
}

# Updated create_pdf_report function
create_pdf_report <- function(indicators, comments, sector_colors) {
  paste0('
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>RGDD Selected Indicators Report</title>
    <style>
        @page {
            size: A4;
            margin: 2cm;
        }
        body {
            font-family: Arial, sans-serif;
            line-height: 1.6;
            color: #333;
        }
        .header {
            text-align: center;
            margin-bottom: 30px;
            padding-bottom: 20px;
            border-bottom: 2px solid #007bff;
        }
        .header h1 {
            color: #007bff;
            margin-bottom: 10px;
        }
        .header .date {
            color: #666;
            font-style: italic;
        }
        .summary {
            background-color: #f8f9fa;
            padding: 15px;
            border-radius: 5px;
            margin-bottom: 30px;
        }
        .indicator {
            page-break-inside: avoid;
            margin-bottom: 30px;
            border: 1px solid #ddd;
            padding: 20px;
            border-radius: 5px;
        }
        .indicator h3 {
            color: #333;
            border-bottom: 1px solid #ddd;
            padding-bottom: 10px;
            margin-bottom: 15px;
        }
        .indicator-header {
            display: flex;
            align-items: center;
            gap: 10px;
            margin-bottom: 15px;
        }
        .sector-badge {
            display: inline-block;
            padding: 4px 8px;
            border-radius: 4px;
            font-size: 12px;
            font-weight: normal;
        }
        .objective-badge {
            display: inline-block;
            padding: 4px 8px;
            border-radius: 4px;
            font-size: 12px;
            background-color: #E5E7E6;
        }
        .priority-star {
            color: gold;
            font-size: 16px;
        }
        .field {
            margin-bottom: 12px;
        }
        .field-label {
            font-weight: bold;
            color: #555;
        }
        .comment-box {
            background-color: #f0f0f0;
            padding: 10px;
            border-radius: 3px;
            margin-top: 10px;
            font-style: italic;
        }
        .footer {
            text-align: center;
            margin-top: 50px;
            padding-top: 20px;
            border-top: 1px solid #ddd;
            color: #666;
            font-size: 12px;
        }
        
        /* Styles for the rendered tables */
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 10px 0;
            font-size: 11pt;
        }
        td {
            border: 1px solid #ccc;
            padding: 6px;
            vertical-align: top;
        }
        td:first-child {
            background-color: #f2f2f2;
            font-weight: bold;
            width: 30%;
        }
        
        /* Tooltip styling for print */
        .my-tooltip {
            position: relative;
            display: inline;
        }
        .my-tooltip .my-tooltiptext {
            display: none; /* Hide tooltips in print */
        }
        .my-tooltip .fa-info-circle {
            display: none; /* Hide info icons in print */
        }
        
        /* Link button styling for print */
        a {
            color: #007bff;
            text-decoration: none;
            border: 1px solid #007bff;
            padding: 2px 6px;
            border-radius: 3px;
            font-size: 10pt;
        }
        
        pre {
            white-space: pre-wrap;
            word-break: break-word;
            background-color: #f8f9fa;
            padding: 8px;
            border-radius: 4px;
            margin: 0;
            font-size: 10pt;
        }
        
        ul {
            margin: 0;
            padding-left: 20px;
        }
        
        @media print {
            body {
                font-size: 10pt;
            }
            .indicator {
                break-inside: avoid;
            }
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>RGDD Selected Indicators Report</h1>
        <div class="date">Generated on: ', Sys.Date(), '</div>
    </div>
    
    <div class="summary">
        <h2>Summary</h2>
        <p><strong>Total indicators selected:</strong> ', nrow(indicators), '</p>
        <p><strong>Sectors covered:</strong> ', paste(unique(indicators$main_sector), collapse = ", "), '</p>
        <p><strong>Mandates covered:</strong> ', paste(unique(indicators$main_mandate), collapse = ", "), '</p>
    </div>
    
    <div class="indicators-list">
        <h2>Selected Indicators</h2>',
         paste0(lapply(1:nrow(indicators), function(i) {
           ind <- indicators[i, ]
           comment <- comments[i]
           sector_color <- sector_colors[[ind$main_sector]] %||% "#cccccc"
           
           # Generate the simplified table with only essential details
           detail_table_html <- shiny_tags_to_html(
             render_disagg_table_vertical(
               ind, 
               columns = c("indicator_description", "indicator_long_description", 
                           "gender_questions", "unit_of_analysis", "measurement_type")
             )
           )
           
           paste0('
        <div class="indicator">
            <h3>', htmlEscape(ind$indicator_name), '</h3>
            <div class="indicator-header">
                <span class="objective-badge">', htmlEscape(ind$main_objectives), '</span>
                <span class="sector-badge" style="background-color: ', sector_color, ';">', 
                  htmlEscape(ind$main_sector), '</span>',
                  if (ind$high_priority == "High priority") '<span class="priority-star">★ Priority</span>' else '',
                  '</div>
            
            ', detail_table_html, '
            
            ', if (comment != "") paste0('
            <div class="comment-box">
                <span class="field-label">Comments/Observations:</span><br>
                ', htmlEscape(comment), '
            </div>') else '', '
        </div>')
         }), collapse = "\n"),
         '
    </div>
    
    <div class="footer">
        <p>This report was generated by the RGDD Explorer by CGAP</p>
        <p>For more information, visit www.cgap.org</p>
    </div>
</body>
</html>')
}