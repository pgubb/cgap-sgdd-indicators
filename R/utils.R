# R/utils.R - Utility functions

# Helper operators and functions
`%not_in%` <- function(x, table) {
  !(x %in% table)
}



# Text processing
slugify <- function(text) {
  text %>%
    tolower() %>%
    gsub("[^a-z0-9]+", "-", .) %>%
    gsub("-$", "", .) %>%
    gsub("^-", "", .)
}

# Use the built-in htmltools version (handles more edge cases)
htmlEscape <- htmltools::htmlEscape


# Generate CSS for sector colors
generate_sector_styles <- function(sector_colors) {
  paste(
    sapply(names(sector_colors), function(sector) {
      paste0(
        ".selectize-control.multi .selectize-input .item[data-value=\"", 
        sector, "\"] {
                    background-color: ", sector_colors[[sector]], ";
                }"
      )
    }),
    collapse = "\n"
  )
}

# Create indicator key legend

indicator_key <- function(sector_colors = SECTOR_COLORS) {
  # Generate gradient dynamically from sector colors
  colors <- unlist(sector_colors)
  n <- length(colors)
  stops <- unlist(lapply(seq_along(colors), function(i) {
    start <- round((i - 1) / n * 100, 2)
    end <- round(i / n * 100, 2)
    c(paste0(colors[i], " ", start, "%"), paste0(colors[i], " ", end, "%"))
  }))
  gradient_bg <- paste0("linear-gradient(90deg, ", paste(stops, collapse = ", "), ")")
  
  div(
    style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 12px; padding: 12px 0; margin-bottom: 16px;",
    
    # Left side - indicator key examples
    div(
      style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap;",
      
      # Indicator name example
      span(
        "Indicator name", 
        style = paste0(
          "background-color: white; ",
          "display: inline-block; ",
          "padding: 4px 10px; ",
          "border-radius: 4px; ",
          "color: black; ",
          "font-weight: bold; ",
          "font-size: 14px; ",
          "border: 1px solid #e9ecef;"
        )
      ),
      
      # Main objective example
      span(
        "Main objective(s)", 
        style = paste0(
          "background-color: #e3f2fd; ",
          "color: #1565c0; ",
          "display: inline-block; ",
          "padding: 4px 10px; ",
          "border-radius: 4px; ",
          "font-size: 12px; ",
          "font-weight: 500; ",
          "border: 1px solid #bbdefb;"
        )
      ),

      # Main sector example with multicolor gradient
      span(
        "Main service",
        style = paste0(
          "background: ", gradient_bg, "; ",
          "color: #333; ",
          "display: inline-block; ",
          "padding: 4px 10px; ",
          "border-radius: 4px; ",
          "font-size: 12px; ",
          "font-weight: 600; ",
          "border: 1px solid rgba(0,0,0,0.15); ",
          "text-shadow: 0 0 3px rgba(255,255,255,0.9), 0 0 5px rgba(255,255,255,0.7);"
        )
      )
    ),
    
    # Right side - download link
    tags$a(
      href = "LENS_INDICATORS_DB.xlsx",
      download = "LENS_INDICATORS_DB.xlsx",
      style = paste0(
        "display: inline-flex; ",
        "align-items: center; ",
        "gap: 6px; ",
        "color: #1A5A80; ",
        "text-decoration: none; ",
        "font-size: 13px; ",
        "font-weight: 600; ",
        "padding: 6px 0; ",
        "border: none; ",
        "background: transparent; ",
        "transition: opacity 0.2s ease;"
      ),
      class = "catalog-download-link",
      icon("download", class = "fas", style = "font-size: 12px;"),
      "Download full catalog"
    )
  )
}

# Create mandate links
create_mandate_links <- function(indicators_data) {
  # Handle empty or NULL data
  if (is.null(indicators_data) || nrow(indicators_data) == 0) {
    return(p("No indicators to display", style = "color: #666; font-style: italic;"))
  }
  
  # Get unique mandates from the filtered data
  mandates <- unique(indicators_data$main_mandate)
  
  # Remove NA values if any
  mandates <- mandates[!is.na(mandates)]
  
  # Getting number of indicators per mandate 
  N_ind_bymandate <- indicators_data %>% 
    filter(!is.na(main_mandate)) %>%  # Filter out NA mandates
    group_by(main_mandate) %>% 
    summarise(n = n(), .groups = "drop")
  
  # Create named vector for lookup
  ref <- setNames(as.character(N_ind_bymandate$n), as.character(N_ind_bymandate$main_mandate))
  
  tagList(
    lapply(mandates, function(mandate) {
      # Convert mandate to character to ensure proper matching
      mandate_char <- as.character(mandate)
      
      # Use str_to_title to match the header generation
      title_case_mandate <- str_to_title(mandate_char)
      
      # Get count with fallback
      count <- ref[mandate_char]
      if (is.na(count) || is.null(count)) {
        count <- "0"
      }
      
      tags$a(
        href = paste0("#mandate_", slugify(title_case_mandate)),
        class = "mandate-link",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span(mandate_char, style = "flex-grow: 1;"),
          span(
            count,
            class = "badge",
            style = "background-color: #6c757d; color: white; padding: 2px 8px; border-radius: 12px; font-size: 12px;"
          )
        )
      )
    })
  )
}

# Generalized table renderer
render_disagg_table_generalized <- function(indicator_row, columns, 
                                            delimiter = ";", 
                                            descriptions = COLUMN_DESCRIPTIONS,
                                            mapping = BREAKDOWNS) {
  
  render_item <- function(item) {
    item <- trimws(item)
    if (item == "") return("")
    
    tooltip <- mapping[[item]]
    if (!is.null(tooltip)) {
      shiny::HTML(
        paste0(
          '<div class="my-tooltip" style="white-space: normal; display: inline-block;">',
          htmlEscape(item),
          ' <i class="fas fa-info-circle" style="color:#87CEFA; margin-left:5px;"></i>',
          '<div class="my-tooltiptext">',
          paste(vapply(tooltip, htmlEscape, character(1)), collapse = "<br>"),
          '</div></div>'
        )
      )
    } else {
      htmlEscape(item)
    }
  }
  
  value_lists <- lapply(columns, function(col) {
    val <- indicator_row[[col]]
    if (is.na(val) || is.null(val) || val == "") return(character())
    unlist(strsplit(val, delimiter))
  })
  
  max_len <- max(sapply(value_lists, length))
  padded_lists <- lapply(value_lists, function(vals) c(vals, rep("", max_len - length(vals))))
  
  tags$table(
    style = "width: 100%; border-collapse: collapse; margin-top: 10px;",
    tags$thead(
      tags$tr(
        lapply(columns, function(col) {
          tags$th(
            descriptions[[col]],
            style = "border: 1px solid #ccc; padding: 4px; background-color: #f2f2f2;"
          )
        })
      )
    ),
    tags$tbody(
      lapply(seq_len(max_len), function(i) {
        tags$tr(
          lapply(padded_lists, function(vals) {
            tags$td(render_item(vals[[i]]), style = "border: 1px solid #ccc; padding: 4px;")
          })
        )
      })
    )
  )
}




render_disagg_table_vertical <- function(indicator_row, columns, 
                                         delimiter = "!-", 
                                         descriptions = COLUMN_DESCRIPTIONS,
                                         mapping = BREAKDOWNS,
                                         pre_columns = character()) {
  
  # Define target strings that should be bolded
  bold_targets <- c(
    "Definitions and concepts:",
    "Data requirements:",
    "Limitations and considerations:",
    "Derivable indicators:"
  )
  
  # Helper function to check if a value has actual content
  has_content <- function(val) {
    if (is.na(val) || is.null(val) || val == "") {
      return(FALSE)
    }
    
    # If there's a delimiter, check if there are any non-empty items after splitting
    if (!is.null(delimiter) && delimiter != "") {
      items <- unlist(strsplit(val, delimiter))
      items <- trimws(items)
      items <- items[items != ""]
      return(length(items) > 0)
    }
    
    return(TRUE)
  }
  
  # Helper function to render items with tooltips and bold formatting
  render_item <- function(item, use_pre = FALSE) {
    item <- trimws(item)
    if (item == "") return("")
    
    # Store original item for processing
    original_item <- item
    
    # Check if this item contains any of the bold targets
    contains_bold_target <- any(sapply(bold_targets, function(target) {
      grepl(target, item, fixed = TRUE)
    }))
    
    # If it contains a bold target, apply bold formatting AFTER escaping the content
    if (contains_bold_target) {
      # First escape the entire content
      escaped_item <- htmlEscape(item)
      
      # Then apply bold formatting to the escaped targets
      formatted_item <- escaped_item
      for (target in bold_targets) {
        if (grepl(target, original_item, fixed = TRUE)) {
          # Escape the target for searching in the escaped text
          escaped_target <- htmlEscape(target)
          # Replace with bold version
          formatted_item <- gsub(
            escaped_target, 
            paste0("<strong>", escaped_target, "</strong>"), 
            formatted_item, 
            fixed = TRUE
          )
        }
      }
      item <- formatted_item
    } else {
      # For non-bold content, escape normally
      item <- htmlEscape(item)
    }
    
    # Check for URLs in the original (unescaped) text
    url_pattern <- "(?i)\\bhttps?://[\\w\\-._~:/?#\\[\\]@!$&'()*+,;=%]+"
    
    if (grepl(url_pattern, original_item, perl = TRUE)) {
      url_matches <- str_extract_all(original_item, url_pattern)[[1]]
      
      # Create modified text with all URLs replaced
      # Start with the current item (which may already have bold formatting)
      modified_text <- item
      
      for (url in url_matches) {
        clean_url <- str_replace(url, "[\\.,;!?]+$", "")
        
        link_button <- paste0(
          '<a href="', clean_url, '" target="_blank" ',
          'style="display: inline-block; padding: 3px 10px; ',
          'border: 1px solid #333; border-radius: 15px; ',
          'text-decoration: none; color: #333; font-size: 12px; ',
          'background-color: white; margin: 0 4px; ',
          'transition: all 0.2s ease;" ',
          'onmouseover="this.style.backgroundColor=\'#007bff\'; this.style.borderColor=\'#007bff\'; this.style.color=\'white\';" ',
          'onmouseout="this.style.backgroundColor=\'white\'; this.style.borderColor=\'#333\'; this.style.color=\'#333\';">',
          'Link <i class="fa-solid fa-link" style="margin-left: 4px; font-size: 10px;"></i>',
          '</a>'
        )
        
        # Replace the escaped URL in the modified text
        escaped_url <- htmlEscape(url)
        modified_text <- sub(escaped_url, link_button, modified_text, fixed = TRUE)
      }
      
      if (use_pre) {
        return(HTML(paste0('<pre style="display: inline; margin: 0; background: none; padding: 0;">', modified_text, '</pre>')))
      } else {
        return(HTML(modified_text))
      }
    }
    
    # Check for tooltips using the original item
    tooltip <- mapping[[original_item]]
    if (!is.null(tooltip)) {
      tooltip_html <- paste0(
        '<div class="my-tooltip" style="white-space: normal; display: inline-block;">',
        item,  # Use the already processed item (escaped and potentially bolded)
        ' <i class="fas fa-info-circle" style="color:#87CEFA; margin-left:5px;"></i>',
        '<div class="my-tooltiptext">',
        paste(vapply(tooltip, htmlEscape, character(1)), collapse = "<br>"),
        '</div></div>'
      )
      
      if (use_pre) {
        return(HTML(paste0(
          '<pre style="margin: 0; white-space: pre-wrap; word-break: break-word; background-color: #f8f9fa; padding: 8px; border-radius: 4px;">',
          tooltip_html,
          '</pre>'
        )))
      } else {
        return(HTML(tooltip_html))
      }
    }
    
    # Default case
    if (use_pre) {
      return(HTML(paste0(
        '<pre style="margin: 0; white-space: pre-wrap; word-break: break-word; background-color: #f8f9fa; padding: 8px; border-radius: 4px;">',
        item,
        '</pre>'
      )))
    } else {
      # Return as HTML since we've already handled escaping properly
      return(HTML(item))
    }
  }
  
  # Filter columns to only include those with content
  columns_with_content <- columns[sapply(columns, function(col) {
    val <- indicator_row[[col]]
    has_content(val)
  })]
  
  # If no columns have content, return empty
  if (length(columns_with_content) == 0) {
    return(NULL)
  }
  
  # Create rows only for columns with content
  rows <- lapply(columns_with_content, function(col) {
    val <- indicator_row[[col]]
    use_pre <- col %in% pre_columns
    
    # We already know this column has content from the filter above
    # Split by delimiter if provided
    items <- unlist(strsplit(val, delimiter))
    items <- trimws(items)
    items <- items[items != ""]
    
    if (length(items) == 1) {
      content <- render_item(items[1], use_pre)
    } else {
      # Multiple items
      if (use_pre) {
        # Multiple pre blocks
        content <- tags$div(
          lapply(seq_along(items), function(i) {
            tags$div(
              style = if (i < length(items)) "margin-bottom: 8px;" else "",
              render_item(items[i], use_pre)
            )
          })
        )
      } else {
        # List format for regular items
        content <- tags$ul(
          style = "margin: 0; padding-left: 20px;",
          lapply(items, function(item) {
            tags$li(render_item(item, use_pre))
          })
        )
      }
    }
    
    tags$tr(
      tags$td(
        descriptions[[col]] %||% col,
        style = "border: 1px solid #ccc; padding: 8px; background-color: #f2f2f2; font-weight: bold; width: 30%; vertical-align: top;"
      ),
      tags$td(
        content,
        style = "border: 1px solid #ccc; padding: 8px; vertical-align: top;"
      )
    )
  })
  
  # Return the table only if there are rows to display
  if (length(rows) > 0) {
    tags$table(
      style = "width: 100%; border-collapse: collapse; margin-top: 10px;",
      tags$tbody(rows)
    )
  } else {
    return(NULL)
  }
}



# Enhanced navigation helper with active filters display and Add All/Remove All buttons

enhanced_navigation_helper <- function(filtered_indicators, total_indicators, active_filters = NULL,
                                       active_set_name = "My Indicators") {
  n <- nrow(filtered_indicators)
  N <- nrow(total_indicators)
  
  # Calculate statistics
  mandates_count <- length(unique(filtered_indicators$main_mandate))
  sectors_count <- filtered_indicators$main_sector %>%
    strsplit(",\\s*") %>%
    unlist() %>%
    trimws() %>%
    unique() %>%
    length()
  objectives_count <- length(unique(filtered_indicators$main_objectives))
  
  # Check if any filters are active
  has_active_filters <- !is.null(active_filters) && (
    length(active_filters$mandates) > 0 ||
      length(active_filters$objectives) > 0 ||
      length(active_filters$sectors) > 0 ||
      length(active_filters$use_cases) > 0 ||
      !is.null(active_filters$search) && active_filters$search != "" ||
      isTRUE(active_filters$presets_digital) ||
      isTRUE(active_filters$presets_msme) ||
      isTRUE(active_filters$presets_finhealth)
  )
  
  div(
    class = "navigation-header nav-header-banner",

    # Main title row with Add All / Remove All buttons
    div(
      style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 16px; gap: 16px; flex-wrap: wrap;",

      # Left side - title and description
      div(
        style = "flex: 1; min-width: 300px;",
        h2("Regulatory indicators with a sociodemographic lens",
           style = "margin: 0; font-size: 22px; font-weight: 700;"),
        p(paste("Explore", N, "curated indicators designed to support evidence-based financial regulation & policy"),
          style = "margin: 6px 0 0 0; font-size: 14px; opacity: 0.9; font-weight: 300;")
      ),

      # Right side - Active set name + Add All / Remove All buttons
      div(
        style = "display: flex; flex-direction: column; align-items: flex-end; gap: 8px; flex-shrink: 0;",

        div(
          class = "active-set-label",
          icon("layer-group", class = "fas", style = "font-size: 11px; opacity: 0.8;"),
          span("Adding to:", style = "opacity: 0.8;"),
          span(active_set_name, style = "font-weight: 600;")
        ),

        div(
          style = "display: flex; gap: 8px; align-items: center;",
          actionButton("add_all_filtered",
            label = tagList(icon("plus-circle", class = "fas", style = "margin-right: 6px; font-size: 11px;"),
                            paste("Add all", n)),
            class = "btn btn-sm"),
          actionButton("remove_all_filtered",
            label = tagList(icon("minus-circle", class = "fas", style = "margin-right: 6px; font-size: 11px;"),
                            "Remove all"),
            class = "btn btn-sm")
        )
      )
    ),

    # Active filters section
    if (has_active_filters) {
      div(
        class = "active-filters-box",
        style = "margin-bottom: 16px;",
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 10px;",
          icon("filter", class = "fas", style = "font-size: 13px;"),
          span("Active Filters:", style = "font-weight: 600; font-size: 13px;")
        ),
        div(
          style = "display: flex; flex-wrap: wrap; gap: 6px;",
          if (!is.null(active_filters$search) && active_filters$search != "") {
            span(class = "filter-pill",
                 icon("search", class = "fas", style = "font-size: 10px;"),
                 paste0('"', active_filters$search, '"'))
          },
          if (isTRUE(active_filters$presets_digital)) {
            span(class = "filter-pill-preset",
                 icon("mobile-screen", class = "fas", style = "font-size: 10px;"),
                 "Digital")
          },
          if (isTRUE(active_filters$presets_msme)) {
            span(class = "filter-pill-preset",
                 icon("building", class = "fas", style = "font-size: 10px;"),
                 "MSME")
          },
          if (isTRUE(active_filters$presets_finhealth)) {
            span(class = "filter-pill-preset",
                 icon("heart-pulse", class = "fas", style = "font-size: 10px;"),
                 "Financial Health")
          },
          if (length(active_filters$mandates) > 0) {
            lapply(active_filters$mandates, function(m)
              span(class = "filter-pill", icon("scroll", class = "fas", style = "font-size: 10px;"), m))
          },
          if (length(active_filters$objectives) > 0) {
            lapply(active_filters$objectives, function(o)
              span(class = "filter-pill", icon("bullseye", class = "fas", style = "font-size: 10px;"), o))
          },
          if (length(active_filters$sectors) > 0) {
            lapply(active_filters$sectors, function(s)
              span(class = "filter-pill", icon("chart-pie", class = "fas", style = "font-size: 10px;"), s))
          }
        )
      )
    },

    # Compact statistics cards
    div(
      class = "stat-card-grid",

      div(
        class = "stat-card stat-card-highlight",
        div(icon("chart-simple", class = "fas", style = "font-size: 18px; color: #ffd700; margin-bottom: 4px;")),
        div(n, style = "font-size: 26px; font-weight: 800; margin-bottom: 2px;"),
        div(paste("of", N, "indicators"), class = "stat-card-label")
      ),

      div(
        class = "stat-card stat-card-default",
        div(icon("scroll", class = "fas stat-card-icon")),
        div(mandates_count, class = "stat-card-value"),
        div("Mandates", class = "stat-card-label")
      ),

      div(
        class = "stat-card stat-card-default",
        div(icon("bullseye", class = "fas stat-card-icon")),
        div(objectives_count, class = "stat-card-value"),
        div("Objectives", class = "stat-card-label")
      ),

      div(
        class = "stat-card stat-card-default",
        div(icon("chart-pie", class = "fas stat-card-icon")),
        div(sectors_count, class = "stat-card-value"),
        div("Services", class = "stat-card-label")
      )

    )
  )
}

# Enhanced mandate section headers
enhanced_mandate_header <- function(mandate, count) {
  # Mandate descriptions for context
  descriptions <- list(
    "FINANCIAL INCLUSION" = "Indicators measuring access, usage, and quality of financial services",
    "CONSUMER PROTECTION" = "Indicators for fair treatment, complaints handling, and consumer safety",
    "STABILITY, SAFETY AND SOUNDNESS" = "Risk management and stability indicators for financial institutions",
    "MARKET DEVELOPMENT" = "Capital markets and competition indicators",
    "SUSTAINABILITY" = "Climate, environmental, and gender equality indicators"
  )
  
  div(
    class = "mandate-section-header",

    div(
      style = "display: flex; justify-content: space-between; align-items: baseline;",
      h3(mandate,
         style = "margin: 0; font-size: 18px; font-weight: 700; color: #1a1a1a; text-transform: capitalize;"),
      span(paste(count, "indicators"),
           style = "color: #6c757d; font-size: 13px; font-weight: 500; white-space: nowrap;")
    ),

    # Description as subtitle
    if (!is.null(descriptions[[toupper(mandate)]])) {
      p(descriptions[[toupper(mandate)]],
        style = "margin: 6px 0 0 0; color: #9ca3af; font-size: 13px; line-height: 1.4;")
    }
  )
}

# Enhanced create_pdf_report function for R/utils.R
# Creates a modern, printer-friendly HTML report matching app styling

# Format text for PDF reports - removes delimiters and bolds specific headers
format_text_for_pdf <- function(text) {
  if (is.na(text) || is.null(text) || text == "") {
    return("")
  }
  
  # Define target strings that should be bolded
  bold_targets <- c(
    "Definitions and concepts:",
    "Data requirements:",
    "Limitations and considerations:",
    "Derivable indicators:"
  )
  
  # Remove the delimiter
  text <- gsub("!-", "", text, fixed = TRUE)
  
  # Escape HTML characters
  text <- htmlEscape(text)
  
  # Apply bold formatting to target strings
  for (target in bold_targets) {
    escaped_target <- htmlEscape(target)
    text <- gsub(
      escaped_target,
      paste0("<strong>", escaped_target, "</strong>"),
      text,
      fixed = TRUE
    )
  }
  
  # Convert line breaks to <br> tags for HTML display
  text <- gsub("\n", "<br>", text, fixed = TRUE)
  
  return(text)
}

# About modal content — extracted from app.R for maintainability
about_modal_content <- function() {
  modalDialog(
    title = NULL,
    size = "xl",
    easyClose = TRUE,
    footer = modalButton("Close"),

    div(
      # Header section
      div(
        style = paste0(
          "background: linear-gradient(135deg, #6F5B9D 0%, #402C60 100%); ",
          "color: white; ",
          "padding: 30px; ",
          "margin: -15px -15px 30px -15px; ",
          "border-radius: 6px 6px 0 0;"
        ),
        h2(
          icon("book", class = "fas", style = "margin-right: 10px;"),
          "About CGAP LENS",
          style = "margin: 0; font-size: 28px; font-weight: 700;"
        ),
        p(
          "Regulatory Indicators with a Sociodemographic Lens",
          style = "margin: 10px 0 0 0; font-size: 16px; opacity: 0.9;"
        )
      ),

      # Content sections
      div(
        style = "padding: 0 20px;",
        p("CGAP's LENS is a curated catalog of indicators for use with regulatory adminisitrative data, helping financial sector authorities understand who uses retail financial services, under what conditions, and with what outcomes. Rather than pre-built metrics, LENS is built around high-level indicators that analysts combine with breakdowns — such as customer type, gender, or provider type — to achieve the disaggregated analysis needed for more inclusive, evidence-based financial policies and supervision. Both indicators and breakdowns should be adapted to specific country contexts. For a full explanation, consult the User Guide.", 
          style = "line-height: 1.6; color: #555; margin-bottom: 15px;"),
        # User Guide Download
        div(
          class = "about-section",
          style = paste0(
            "margin-bottom: 30px; ",
            "background: linear-gradient(135deg, #1A5A80 0%, #2980b9 100%); ",
            "border-radius: 8px; ",
            "padding: 24px; ",
            "color: white; ",
            "position: relative; ",
            "overflow: hidden;"
          ),
          div(
            style = paste0(
              "position: absolute; ",
              "top: -30px; right: -30px; ",
              "width: 120px; height: 120px; ",
              "background: rgba(255,255,255,0.1); ",
              "border-radius: 50%;"
            )
          ),
          div(
            style = "position: relative; z-index: 1;",
            div(
              style = "display: flex; align-items: flex-start; gap: 20px; flex-wrap: wrap;",
              div(
                style = paste0(
                  "background: rgba(255,255,255,0.2); ",
                  "border-radius: 12px; padding: 16px; ",
                  "display: flex; align-items: center; justify-content: center;"
                ),
                icon("file-pdf", class = "fas", style = "font-size: 32px; color: white;")
              ),
              div(
                style = "flex: 1; min-width: 250px;",
                h4("LENS User Guide",
                   style = "margin: 0 0 8px 0; font-size: 20px; font-weight: 600;"),
                p("New to LENS? Download our comprehensive user guide to learn how to navigate the indicator catalog, create custom indicator sets, and generate reports for your analysis.",
                  style = "margin: 0 0 16px 0; font-size: 14px; opacity: 0.9; line-height: 1.5;"),
                tags$a(
                  href = "CGAP LENS User Guide.pdf",
                  download = "CGAP LENS User Guide.pdf",
                  style = paste0(
                    "display: inline-flex; align-items: center; gap: 8px; ",
                    "background: white; color: #1A5A80; text-decoration: none; ",
                    "font-size: 14px; font-weight: 600; padding: 10px 20px; ",
                    "border-radius: 6px; transition: background-color 0.2s ease;"
                  ),
                  class = "user-guide-download-btn",
                  icon("download", class = "fas", style = "font-size: 14px;"),
                  "Download User Guide (PDF)"
                )
              )
            )
          )
        ),

        # Context & Additional Resources
        div(
          class = "about-section",
          style = "margin-bottom: 30px;",
          h3("Context & Additional Resources",
             style = "color: #333; margin-bottom: 15px; font-size: 20px;"),
          p("LENS is one component of CGAP's project to support the mainstreaming of regulatory gender-disaggregated data (RGDD) in the financial sector and should be consulted jointly with 'Using Disaggregated Data to Improve Policy, Regulation and Supervision: A Technical Guide for Financial Sector Authorities' and accompanying pieces",
            style = "line-height: 1.6; color: #555; margin-bottom: 15px;"),
          p("This Guidance package describes RGDD use cases, provides recommendations for Financial Sector Authorities on how to leverage regulatory reporting regimes to maximize the use of gender-disaggregated data for multiple FSA mandates and to support the goals of other actors (including funders and financial firms).",
            style = "line-height: 1.6; color: #555; margin-bottom: 15px;"),
          div(
            style = "display: flex; gap: 10px; flex-wrap: wrap;",
            span(class = "btn btn-outline-secondary btn-sm",
                 style = "opacity: 0.6; pointer-events: none;",
                 icon("book", class = "fas", style = "margin-right: 6px;"),
                 "Technical Guide (Coming 2026/Q3)")
          )
        ),

        # Tutorial video
        tags$div(
          class = "video-link-container",
          style = "max-width: 680px; margin: 20px auto;",
          tags$a(
            href = "https://www.loom.com/share/30cd180dc6bb4c31b3ff78a0c85effe3",
            target = "_blank",
            style = "text-decoration: none; color: inherit;",
            tags$div(
              style = paste0(
                "display: flex; align-items: center; gap: 16px; ",
                "padding: 20px 24px; ",
                "background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%); ",
                "border-radius: 8px; ",
                "transition: background-color 0.2s ease; ",
                "cursor: pointer;"
              ),
              onmouseenter = "this.style.background='linear-gradient(135deg, #252542 0%, #1e2d4a 100%)';",
              onmouseleave = "this.style.background='linear-gradient(135deg, #1a1a2e 0%, #16213e 100%)';",
              tags$div(
                style = paste0(
                  "flex-shrink: 0; width: 52px; height: 52px; ",
                  "background: #6F5B9D; border-radius: 50%; ",
                  "display: flex; align-items: center; justify-content: center;"
                ),
                tags$div(style = paste0(
                  "width: 0; height: 0; ",
                  "border-top: 10px solid transparent; ",
                  "border-bottom: 10px solid transparent; ",
                  "border-left: 16px solid #ffffff; ",
                  "margin-left: 3px;"
                ))
              ),
              tags$div(
                tags$div(style = "font-size: 1.05em; font-weight: 600; color: #ffffff; margin-bottom: 4px;",
                         "Watch the LENS Video Tutorial"),
                tags$div(style = "font-size: 0.85em; color: #a0aec0;",
                         "Learn how to explore indicators, build custom sets, and generate reports")
              )
            )
          )
        ),

        # Objectives & Approach
        div(
          class = "about-section",
          style = "margin-bottom: 30px;",
          h3("Objectives & Approach",
             style = "color: #333; margin-bottom: 15px; font-size: 20px;"),
          p("LENS provides a way of interacting with a curated catalog of indicators compiled and developed by CGAP that are relevant for measuring and describing different aspects of the financial system and for supporting common goals of regulatory decision-making, including those relating to financial inclusion, consumer protection, stability and soundness, market development and sustainability. The selection of indicators is informed both by their general relevance to these different mandates and goals of financial sector authorities as well as their potential relevance for understanding the role of key sociodemographic traits, such as gender, within those dimensions. The indicators in LENS, including their definitions, assigned mandates/objectives and proposed breakdowns, do not reflect a global consensus but rather constitute an attempt at providing working definitions, classification options and analytical questions that each user should adapt to their specific country context and institutional goals.",
            style = "line-height: 1.6; color: #555;")
        ),

        # Sources
        div(
          class = "about-section",
          style = "margin-bottom: 30px;",
          h3("References and sources",
             style = "color: #333; margin-bottom: 15px; font-size: 20px;"),
          p("To build LENS, the CGAP team examined a variety of sources, including work from prior CGAP projects and existing indicators from international organizations focused on financial inclusion and regulation.",
            style = "line-height: 1.6; color: #555; margin-bottom: 15px;"),
          div(
            style = "display: flex; gap: 10px; flex-wrap: wrap;",
            tags$a(href = "https://data.imf.org/en/datasets/IMF.STA:FAS", target = "_blank",
                   class = "btn btn-outline-secondary btn-sm", "IMF FAS"),
            tags$a(href = "https://www.gpfi.org", target = "_blank",
                   class = "btn btn-outline-secondary btn-sm", "GPFI"),
            tags$a(href = "https://www.afi-global.org", target = "_blank",
                   class = "btn btn-outline-secondary btn-sm", "AFI"),
            tags$a(href = "https://www.we-fi.org/we-finance-code/#home", target = "_blank",
                   class = "btn btn-outline-secondary btn-sm", "WE Finance Code"),
            tags$a(href = "https://www.cgap.org/topics/collections/fema-meter", target = "_blank",
                   class = "btn btn-outline-secondary btn-sm", "A2ii FeMa-Meter")
          )
        ),

        # About CGAP
        div(
          class = "about-section",
          style = paste0(
            "background: #f8f9fa; ",
            "padding: 20px; border-radius: 8px; margin-bottom: 20px; ",
            "border: 1px solid rgba(0,0,0,0.08);"
          ),
          h3("About CGAP",
             style = "color: #333; margin-bottom: 15px; font-size: 20px;"),
          p("CGAP is a global partnership of more than 40 leading development organizations that works to advance the lives of people living in poverty, especially women, through financial inclusion. CGAP works at the frontier of inclusive finance to test solutions, spark innovation, generate evidence, and share insights. Our knowledge enables public and private stakeholders to scale solutions that make financial ecosystems meet the needs of poor, vulnerable, and underserved people and of micro and small enterprises (MSEs), including through advancing women's economic empowerment.",
            style = "line-height: 1.6; color: #555; margin-bottom: 15px;"),
          div(
            style = "display: flex; gap: 15px; align-items: center; flex-wrap: wrap;",
            tags$a(href = "https://www.cgap.org", target = "_blank",
                   class = "btn btn-primary btn-sm",
                   icon("globe", class = "fas", style = "margin-right: 6px;"),
                   "Visit CGAP.org")
          )
        )
      )
    )
  )
}

create_pdf_report <- function(indicators, comments, sector_colors, active_set_name = "Selected Indicators") {
  paste0('
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>', htmlEscape(active_set_name), ' - CGAP LENS</title>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css">
    <style>
        /* Base Styles */
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            color: #333;
            background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
            padding: 20px;
        }
        
        .container {
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            box-shadow: 0 10px 40px rgba(0,0,0,0.1);
            border-radius: 16px;
            overflow: hidden;
        }
        
        /* Header Section */
        .report-header {
            background: linear-gradient(135deg, #6F5B9D 0%, #402C60 100%);
            color: white;
            padding: 40px;
            text-align: center;
            position: relative;
            overflow: hidden;
        }
        
        .report-header::before {
            content: "";
            position: absolute;
            top: -50%;
            right: -10%;
            width: 300px;
            height: 300px;
            background: radial-gradient(circle, rgba(255,255,255,0.1) 0%, transparent 70%);
            border-radius: 50%;
        }
        
        .report-header h1 {
            font-size: 32px;
            font-weight: 700;
            margin-bottom: 8px;
            position: relative;
            z-index: 1;
        }
        
        .report-subtitle {
            font-size: 16px;
            opacity: 0.9;
            margin-bottom: 20px;
            position: relative;
            z-index: 1;
        }
        
        .report-meta {
            display: flex;
            justify-content: center;
            gap: 30px;
            margin-top: 20px;
            position: relative;
            z-index: 1;
            flex-wrap: wrap;
        }
        
        .meta-item {
            background: rgba(255, 255, 255, 0.2);
            padding: 12px 20px;
            border-radius: 8px;
            backdrop-filter: blur(10px);
            border: 1px solid rgba(255, 255, 255, 0.3);
        }
        
        .meta-label {
            font-size: 11px;
            opacity: 0.8;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            margin-bottom: 4px;
        }
        
        .meta-value {
            font-size: 20px;
            font-weight: 700;
        }
        
        /* Summary Section */
        .summary-section {
            padding: 30px 40px;
            background: #f8f9fa;
            border-bottom: 1px solid #e9ecef;
        }
        
        .summary-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin-top: 20px;
        }
        
        .summary-card {
            background: white;
            padding: 20px;
            border-radius: 12px;
            border: 1px solid #e9ecef;
            text-align: center;
        }
        
        .summary-card i {
            font-size: 28px;
            color: #1A5A80;
            margin-bottom: 12px;
        }
        
        .summary-card-value {
            font-size: 24px;
            font-weight: 700;
            color: #1a1a1a;
            margin-bottom: 4px;
        }
        
        .summary-card-label {
            font-size: 13px;
            color: #6c757d;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }
        
        /* Indicators Section */
        .indicators-section {
            padding: 40px;
        }
        
        .section-title {
            font-size: 24px;
            font-weight: 700;
            color: #1a1a1a;
            margin-bottom: 30px;
            padding-bottom: 15px;
            border-bottom: 2px solid #e9ecef;
        }
        
        .indicator-card {
            background: white;
            border: 1px solid #e9ecef;
            border-radius: 12px;
            margin-bottom: 30px;
            overflow: hidden;
            page-break-inside: avoid;
            box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        }
        
        .indicator-accent {
            height: 6px;
            background: linear-gradient(90deg, var(--sector-color) 0%, var(--sector-color-light) 100%);
        }
        
        .indicator-content {
            padding: 24px;
        }
        
        .indicator-header {
            margin-bottom: 16px;
        }
        
        .indicator-title {
            font-size: 18px;
            font-weight: 600;
            color: #1a1a1a;
            margin-bottom: 12px;
            line-height: 1.4;
        }
        
        .badge-group {
            display: flex;
            flex-wrap: wrap;
            gap: 8px;
            margin-bottom: 16px;
        }
        
        .badge {
            display: inline-flex;
            align-items: center;
            gap: 4px;
            padding: 4px 10px;
            border-radius: 12px;
            font-size: 11px;
            font-weight: 500;
        }
        
        .badge-mandate {
            background-color: #e3f2fd;
            color: #1565c0;
            border: 1px solid #bbdefb;
        }
        
        .badge-objective {
            background-color: #f3e5f5;
            color: #7b1fa2;
            border: 1px solid #e1bee7;
        }
        
        .badge-sector {
            background-color: var(--sector-color);
            color: #333;
            border: 1px solid rgba(0,0,0,0.1);
        }
        
        .badge-priority {
            background: linear-gradient(135deg, #ffd700 0%, #ffed4e 100%);
            color: #856404;
            font-weight: 600;
        }
        
        .indicator-description {
            font-size: 14px;
            color: #5f6368;
            line-height: 1.6;
            margin-bottom: 20px;
            padding: 16px;
            background: #f8f9fa;
            border-radius: 8px;
            border-left: 4px solid #667eea;
        }
        
        .details-section {
            margin-top: 20px;
        }
        
        .detail-item {
            margin-bottom: 16px;
            padding-bottom: 16px;
            border-bottom: 1px solid #f1f3f4;
        }
        
        .detail-item:last-child {
            border-bottom: none;
        }
        
        .detail-label {
            font-size: 12px;
            font-weight: 600;
            color: #495057;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            margin-bottom: 6px;
        }
        
        .detail-value {
            font-size: 13px;
            color: #6c757d;
            line-height: 1.5;
        }
        
        .comment-section {
            margin-top: 20px;
            padding: 16px;
            background: linear-gradient(135deg, #fff7e6 0%, #fffbf0 100%);
            border-radius: 8px;
            border: 1px solid #ffe4b3;
        }
        
        .comment-header {
            display: flex;
            align-items: center;
            gap: 8px;
            margin-bottom: 8px;
        }
        
        .comment-header i {
            color: #ff9800;
        }
        
        .comment-label {
            font-size: 13px;
            font-weight: 600;
            color: #e65100;
        }
        
        .comment-text {
            font-size: 13px;
            color: #5d4037;
            line-height: 1.6;
            font-style: italic;
        }
        
        /* Footer */
        .report-footer {
            background: #f8f9fa;
            padding: 30px 40px;
            text-align: center;
            border-top: 1px solid #e9ecef;
        }
        
        .footer-logo {
            font-size: 14px;
            color: #6c757d;
            margin-bottom: 8px;
        }
        
        .footer-text {
            font-size: 12px;
            color: #adb5bd;
        }
        
        /* Print Styles */
        @media print {
            body {
                background: white;
                padding: 0;
            }
            
            .container {
                box-shadow: none;
                border-radius: 0;
            }
            
            .report-header {
                background: #667eea;
                -webkit-print-color-adjust: exact;
                print-color-adjust: exact;
            }
            
            .indicator-card {
                page-break-inside: avoid;
                margin-bottom: 20px;
            }
            
            .badge, .comment-section {
                -webkit-print-color-adjust: exact;
                print-color-adjust: exact;
            }
        }
        
        @page {
            size: A4;
            margin: 1.5cm;
        }
    </style>
</head>
<body>
    <div class="container">
        <!-- Header -->
        <div class="report-header">
            <h1><i class="fas fa-clipboard-check"></i>', htmlEscape(paste("", active_set_name)), '</h1>
            <div class="report-subtitle"> Regulatory Indicators with a Sociodemographic LENS </div>
            <div class="report-meta">
                <div class="meta-item">
                    <div class="meta-label">Generated</div>
                    <div class="meta-value">', format(Sys.Date(), "%B %d, %Y"), '</div>
                </div>
                <div class="meta-item">
                    <div class="meta-label">Total Indicators</div>
                    <div class="meta-value">', nrow(indicators), '</div>
                </div>
            </div>
        </div>
        
        <!-- Summary Section -->
        <div class="summary-section">
            <h2 class="section-title">Summary Overview</h2>
            <div class="summary-grid">
                <div class="summary-card">
                    <i class="fas fa-scroll"></i>
                    <div class="summary-card-value">', length(unique(indicators$main_mandate)), '</div>
                    <div class="summary-card-label">Mandates</div>
                </div>
                <div class="summary-card">
                    <i class="fas fa-chart-pie"></i>
                    <div class="summary-card-value">', length(unique(indicators$main_sector)), '</div>
                    <div class="summary-card-label">Services</div>
                </div>
                <div class="summary-card">
                    <i class="fas fa-bullseye"></i>
                    <div class="summary-card-value">', length(unique(indicators$main_objectives)), '</div>
                    <div class="summary-card-label">Objectives</div>
                </div>
            </div>
        </div>
        
        <!-- Indicators Section -->
        <div class="indicators-section">
            <h2 class="section-title">Selected Indicators</h2>
            ',
         paste0(lapply(1:nrow(indicators), function(i) {
           ind <- indicators[i, ]
           comment <- comments[i]
           
           # Safely get sector color with fallback
           sector_color <- sector_colors[[ind$main_sector]]
           if (is.null(sector_color) || !is.character(sector_color) || sector_color == "") {
             sector_color <- "#EAE9E6"
           }
           sector_color_light <- adjustcolor(sector_color, alpha.f = 0.6)
           
           paste0('
            <div class="indicator-card" style="--sector-color: ', sector_color, '; --sector-color-light: ', sector_color_light, ';">
                <div class="indicator-accent"></div>
                <div class="indicator-content">
                    <div class="indicator-header">
                        <h3 class="indicator-title">', htmlEscape(ind$indicator_name), '</h3>
                        <div class="badge-group">
                            <span class="badge badge-mandate">
                                <i class="fas fa-scroll"></i>
                                ', htmlEscape(ind$main_mandate), '
                            </span>
                            <span class="badge badge-objective">
                                <i class="fas fa-bullseye"></i>
                                ', htmlEscape(ind$main_objectives), '
                            </span>
                            <span class="badge badge-sector">
                                <i class="fas fa-chart-pie"></i>
                                ', htmlEscape(ind$main_sector), '
                            </span>',
                  if (!is.null(ind$preset_digital) && !is.na(ind$preset_digital) && ind$preset_digital == 1) {
                    '<span class="badge badge-priority"><i class="fas fa-mobile-screen"></i> Digital finance ecosystem </span>'
                  } else {
                    ''
                  },
                  if (!is.null(ind$preset_msme) && !is.na(ind$preset_msme) && ind$preset_msme == 1) {
                    '<span class="badge badge-priority"><i class="fas fa-building"></i> MSME focus </span>'
                  } else {
                    ''
                  },
                  if (!is.null(ind$preset_finhealth) && !is.na(ind$preset_finhealth) && ind$preset_finhealth == 1) {
                    '<span class="badge badge-priority"><i class="fas fa-heart-pulse"></i> Financial health </span>'
                  } else {
                    ''
                  }, '
                        </div>
                    </div>
                    
                    <div class="indicator-description">
                        ', htmlEscape(ind$indicator_description), '
                    </div>
                    
                    <div class="details-section">',
                  if (!is.null(ind$indicator_long_description) && !is.na(ind$indicator_long_description) && ind$indicator_long_description != "") {
                    formatted_description <- format_text_for_pdf(ind$indicator_long_description)
                    
                    paste0('
      <div class="detail-item">
          <div class="detail-label">Detailed Description</div>
          <div class="detail-value">', formatted_description, '</div>
      </div>')
                  } else {
                    ''
                  }, 
                  
                  if (!is.null(ind$gender_questions) && !is.na(ind$gender_questions) && ind$gender_questions != "") {
                    paste0('
                        <div class="detail-item">
                            <div class="detail-label"><i class="fas fa-question-circle"></i> Gender Analysis Questions</div>
                            <div class="detail-value">', substr(htmlEscape(ind$gender_questions), 1, 1000),
                           if(nchar(ind$gender_questions) > 1000) '...' else '', '</div>
                        </div>')
                  } else {
                    ''
                  },
                  
                  if (!is.null(ind$secondary_mandate_objective) && !is.na(ind$secondary_mandate_objective) && ind$secondary_mandate_objective != "") {
                    paste0('
      <div class="detail-item">
          <div class="detail-label"><i class="fas fa-scroll"></i> Secondary mandates (objectives)</div>
          <div class="detail-value">', htmlEscape(ind$secondary_mandate_objective), '</div>
      </div>')
                  } else {
                    ''
                  },
                  
                  # Suggested Breakdowns with categories
                  if (!is.null(ind$disaggregation_vars) && !is.na(ind$disaggregation_vars) && ind$disaggregation_vars != "") {
                    breakdown_items <- trimws(unlist(strsplit(ind$disaggregation_vars, ";")))
                    breakdown_items <- breakdown_items[breakdown_items != ""]
                    
                    breakdown_cards <- paste0(lapply(breakdown_items, function(item) {
                      categories <- BREAKDOWNS[[item]]
                      if (!is.null(categories)) {
                        cats_html <- paste0(
                          '<span style="font-size: 11px; color: #6c757d; line-height: 1.4;">',
                          paste(vapply(categories, htmlEscape, character(1)), collapse = " &middot; "),
                          '</span>'
                        )
                        paste0(
                          '<div style="padding: 8px 12px; background: #f8f9fa; border-radius: 6px; border-left: 3px solid #667eea;">',
                          '<div style="font-weight: 600; font-size: 13px; color: #333; margin-bottom: 4px;">', htmlEscape(item), '</div>',
                          cats_html,
                          '</div>'
                        )
                      } else {
                        paste0(
                          '<div style="padding: 8px 12px; background: #f8f9fa; border-radius: 6px; border-left: 3px solid #667eea;">',
                          '<div style="font-weight: 600; font-size: 13px; color: #333;">', htmlEscape(item), '</div>',
                          '</div>'
                        )
                      }
                    }), collapse = "\n")
                    
                    breakdown_html <- paste0(
                      '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 8px;">',
                      breakdown_cards,
                      '</div>'
                    )
                    
                    paste0('
      <div class="detail-item">
          <div class="detail-label"><i class="fas fa-layer-group"></i> Suggested Breakdowns</div>
          <div class="detail-value">', breakdown_html, '</div>
      </div>')
                  } else {
                    ''
                  },'
                  
                  
                    </div>',
                  
                  if (comment != "") paste0('
                    <div class="comment-section">
                        <div class="comment-header">
                            <i class="fas fa-comment-dots"></i>
                            <span class="comment-label">Notes & Observations</span>
                        </div>
                        <div class="comment-text">', htmlEscape(comment), '</div>
                    </div>') else '', '
                </div>
            </div>')
         }), collapse = "\n"), '
        </div>
        
        <!-- Footer -->
        <div class="report-footer">
            <div class="footer-logo">
                <strong>CGAP LENS</strong> - Regulatory Indicators with a Sociodemographic Lens
            </div>
            <div class="footer-text">
                This report was generated by the CGAP LENS platform<br>
                For more information, visit <a href="https://www.cgap.org" style="color: #1A5A80;">www.cgap.org</a>
            </div>
        </div>
    </div>
</body>
</html>')
}
