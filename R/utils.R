# R/utils.R - Utility functions

# Helper operators and functions
`%not_in%` <- function(x, table) {
  !(x %in% table)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

conditional <- function(condition, success) {
  if (condition) success else TRUE
}

# Text processing
slugify <- function(text) {
  text %>%
    tolower() %>%
    gsub("[^a-z0-9]+", "-", .) %>%
    gsub("-$", "", .) %>%
    gsub("^-", "", .)
}

htmlEscape <- function(text) {
  text <- as.character(text)
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub("\"", "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&#039;", text, fixed = TRUE)
  text
}


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

indicator_key <- function() {
  # Simpler, direct gradient creation using all sector colors
  gradient_bg <- "linear-gradient(90deg, #FFB718 0%, #FFB718 14.28%, #ED7700 14.28%, #ED7700 28.56%, #17A627 28.56%, #17A627 42.84%, #38A5D6 42.84%, #38A5D6 57.12%, #C64689 57.12%, #C64689 71.4%, #EAE9E6 71.4%, #EAE9E6 85.68%, #0080B2 85.68%, #0080B2 100%)"
  
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
          "border-radius: 16px; ",
          "font-size: 12px; ",
          "font-weight: 500; ",
          "border: 1px solid #bbdefb;"
        )
      ),
      
      # Main sector example with multicolor gradient
      span(
        "Main sector", 
        style = paste0(
          "background: ", gradient_bg, "; ",
          "color: #333; ",
          "display: inline-block; ",
          "padding: 4px 10px; ",
          "border-radius: 16px; ",
          "font-size: 12px; ",
          "font-weight: 600; ",
          "border: 1px solid rgba(0,0,0,0.15); ",
          "text-shadow: 0 0 3px rgba(255,255,255,0.9), 0 0 5px rgba(255,255,255,0.7);"
        )
      )
    ),
    
    # Right side - download link
    tags$a(
      href = "LENS_INDICATORS_DB_v11062025.xlsx",
      download = "LENS_INDICATORS_DB_v11062025.xlsx",
      style = paste0(
        "display: flex; ",
        "align-items: center; ",
        "gap: 6px; ",
        "color: #6c757d; ",
        "text-decoration: none; ",
        "font-size: 13px; ",
        "transition: all 0.2s ease; ",
        "padding: 4px 8px; ",
        "border-radius: 4px;"
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

# Render disaggregation table with tooltips
render_disagg_table <- function(essential, nonessential, descriptions = BREAKDOWNS) {
  
  render_cell <- function(item) {
    if (is.na(item) || item == "") return("")
    
    matched <- descriptions[[item]]
    
    if (!is.null(matched)) {
      shiny::HTML(
        paste0(
          '<div class="my-tooltip" style="white-space: normal; display: inline-block;">',
          htmlEscape(item),
          ' <i class="fas fa-info-circle" style="color:#87CEFA; margin-left:5px;"></i>',
          '<div class="my-tooltiptext">',
          paste(vapply(matched, htmlEscape, character(1)), collapse = "<br>"),
          '</div>',
          '</div>'
        )
      )
    } else {
      htmlEscape(item)
    }
  }
  
  essentials <- if (!is.na(essential)) unlist(strsplit(essential, ";\\s*")) else character()
  nonessentials <- if (!is.na(nonessential)) unlist(strsplit(nonessential, ";\\s*")) else character()
  
  max_len <- max(length(essentials), length(nonessentials))
  essentials <- c(essentials, rep("", max_len - length(essentials)))
  nonessentials <- c(nonessentials, rep("", max_len - length(nonessentials)))
  
  tags$div(
    tags$table(
      style = "width: 100%; border-collapse: collapse; margin-top: 10px;",
      tags$thead(
        tags$tr(
          tags$th("High priority", style = "border: 1px solid #ccc; padding: 4px; background-color: #f2f2f2;"),
          tags$th("Low priority", style = "border: 1px solid #ccc; padding: 4px; background-color: #f2f2f2;")
        )
      ),
      tags$tbody(
        lapply(seq_along(essentials), function(i) {
          tags$tr(
            tags$td(render_cell(essentials[i]), style = "border: 1px solid #ccc; padding: 4px;"),
            tags$td(render_cell(nonessentials[i]), style = "border: 1px solid #ccc; padding: 4px;")
          )
        })
      )
    )
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


render_cols_as_table <- function(indicator_row, columns, descriptions = COLUMN_DESCRIPTIONS) {
  valid_columns <- columns[columns %in% names(indicator_row) & columns %in% names(descriptions)]
  if (length(valid_columns) == 0) return(NULL)
  
  tags$div(
    class = "table-responsive",
    tags$table(
      class = "table table-bordered table-sm",
      style = "width: 100%; margin-top: 10px;",
      tags$thead(
        tags$tr(
          lapply(valid_columns, function(col) {
            tags$th(
              descriptions[[col]] %||% col,
              style = "background-color: #f9f9f9;"
            )
          })
        )
      ),
      tags$tbody(
        tags$tr(
          lapply(valid_columns, function(col) {
            tags$td(
              tags$pre(style = "margin: 0; white-space: pre-wrap; word-break: break-word;", htmlEscape(indicator_row[[col]]))
            )
          })
        )
      )
    )
  )
}




# Alternative version that groups multiple values in a single cell without bullets
render_disagg_table_vertical_grouped <- function(indicator_row, columns, 
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
  
  # Create rows for each column
  rows <- lapply(columns, function(col) {
    val <- indicator_row[[col]]
    
    # Handle NA, NULL, or empty values
    if (is.na(val) || is.null(val) || val == "") {
      content <- ""
    } else {
      # Split by delimiter if provided
      items <- unlist(strsplit(val, delimiter))
      items <- trimws(items)
      items <- items[items != ""]
      
      if (length(items) == 0) {
        content <- ""
      } else {
        # Render all items with line breaks between them
        content <- tags$div(
          lapply(seq_along(items), function(i) {
            tagList(
              render_item(items[i]),
              if (i < length(items)) tags$br() else NULL
            )
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
  
  # Return the table
  tags$table(
    style = "width: 100%; border-collapse: collapse; margin-top: 10px;",
    tags$tbody(rows)
  )
}

# Fixed render_disagg_table_vertical function in R/utils.R
# Updated render_disagg_table_vertical function in R/utils.R
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
                                       selected_count = 0, filtered_ids = character(),
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
      isTRUE(active_filters$presets_foundation) ||
      isTRUE(active_filters$presets_digital)
  )
  
  div(
    class = "navigation-header",
    style = paste0(
      "background: linear-gradient(135deg, #6F5B9D 0%, #402C60 100%); ",
      "color: white; ",
      "padding: 24px; ",
      "border-radius: 16px; ",
      "margin-bottom: 32px; ",
      "box-shadow: 0 8px 32px rgba(102, 126, 234, 0.3);"
    ),
    
    # Main title and count row with Add All / Remove All buttons
    div(
      style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 20px; gap: 16px; flex-wrap: wrap;",
      
      # Left side - title and description
      div(
        style = "flex: 1; min-width: 300px;",
        h2(
          "Regulatory indicators with a sociodemographic lens",
          style = "margin: 0; font-size: 28px; font-weight: 700; text-shadow: 0 2px 4px rgba(0,0,0,0.1);"
        ),
        p(
          paste("Explore", N, "curated indicators designed to support evidence-based financial regulation & policy"),
          style = "margin: 8px 0 0 0; font-size: 16px; opacity: 0.9; font-weight: 300;"
        )
      ),
      
      # Right side - Active set name + Add All / Remove All buttons
      div(
        style = "display: flex; flex-direction: column; align-items: flex-end; gap: 8px; flex-shrink: 0;",
        
        # Active set name label
        div(
          style = paste0(
            "display: flex; ",
            "align-items: center; ",
            "gap: 6px; ",
            "padding: 6px 12px; ",
            "background: rgba(255, 255, 255, 0.15); ",
            "border-radius: 16px; ",
            "font-size: 13px; ",
            "backdrop-filter: blur(10px);"
          ),
          icon("layer-group", class = "fas", style = "font-size: 11px; opacity: 0.8;"),
          span("Adding to:", style = "opacity: 0.8;"),
          span(
            active_set_name,
            style = "font-weight: 600;"
          )
        ),
        
        # Buttons row
        div(
          style = "display: flex; gap: 8px; align-items: center;",
          
          # Add All button
          actionButton(
            "add_all_filtered",
            label = tagList(
              icon("plus-circle", class = "fas", style = "margin-right: 6px; font-size: 11px;"),
              paste("Add all", n)
            ),
            class = "btn btn-sm",
            style = paste0(
              "background: rgba(255, 255, 255, 0.95); ",
              "color: #198754; ",
              "border: none; ",
              "font-weight: 600; ",
              "font-size: 13px; ",
              "padding: 6px 12px; ",
              "border-radius: 16px; ",
              "transition: all 0.2s ease; ",
              "box-shadow: 0 2px 8px rgba(0,0,0,0.15); ",
              "line-height: 1.4;"
            )
          ),
          
          # Remove All button
          actionButton(
            "remove_all_filtered",
            label = tagList(
              icon("minus-circle", class = "fas", style = "margin-right: 6px; font-size: 11px;"),
              "Remove all"
            ),
            class = "btn btn-sm",
            style = paste0(
              "background: transparent; ",
              "color: white; ",
              "border: 1px solid rgba(255,255,255,0.6); ",
              "font-weight: 500; ",
              "font-size: 13px; ",
              "padding: 6px 12px; ",
              "border-radius: 16px; ",
              "transition: all 0.2s ease; ",
              "line-height: 1.4;"
            )
          )
        )
      )
    ),
    
    # Active filters section (only show if filters are active)
    if (has_active_filters) {
      div(
        style = paste0(
          "background: rgba(255, 255, 255, 0.15); ",
          "backdrop-filter: blur(10px); ",
          "padding: 16px; ",
          "border-radius: 12px; ",
          "border: 1px solid rgba(255, 255, 255, 0.2); ",
          "margin-bottom: 20px;"
        ),
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 12px;",
          icon("filter", class = "fas", style = "font-size: 16px;"),
          span("Active Filters:", style = "font-weight: 600; font-size: 14px;")
        ),
        div(
          style = "display: flex; flex-wrap: wrap; gap: 8px;",
          
          # Search filter
          if (!is.null(active_filters$search) && active_filters$search != "") {
            span(
              style = paste0(
                "background: rgba(255, 255, 255, 0.9); ",
                "color: #667eea; ",
                "padding: 4px 12px; ",
                "border-radius: 16px; ",
                "font-size: 12px; ",
                "font-weight: 500; ",
                "display: flex; ",
                "align-items: center; ",
                "gap: 6px;"
              ),
              icon("search", class = "fas", style = "font-size: 10px;"),
              paste0('"', active_filters$search, '"')
            )
          },
          
          # Presets foundation filter
          if (!is.null(active_filters$presets_foundation) && active_filters$presets_foundation == TRUE) {
            span(
              style = paste0(
                "background: linear-gradient(135deg, #ffd700 0%, #ffed4e 100%); ",
                "color: #856404; ",
                "padding: 4px 12px; ",
                "border-radius: 16px; ",
                "font-size: 12px; ",
                "font-weight: 600; ",
                "display: flex; ",
                "align-items: center; ",
                "gap: 6px;"
              ),
              icon("building-columns", class = "fas", style = "font-size: 10px;"),
              "Foundational"
            )
          },
          
          # Presets digital filter
          if (!is.null(active_filters$presets_digital) && active_filters$presets_digital == TRUE) {
            span(
              style = paste0(
                "background: linear-gradient(135deg, #ffd700 0%, #ffed4e 100%); ",
                "color: #856404; ",
                "padding: 4px 12px; ",
                "border-radius: 16px; ",
                "font-size: 12px; ",
                "font-weight: 600; ",
                "display: flex; ",
                "align-items: center; ",
                "gap: 6px;"
              ),
              icon("mobile-screen", class = "fas", style = "font-size: 10px;"),
              "Digital"
            )
          },
          
          # Mandate filters
          if (length(active_filters$mandates) > 0) {
            lapply(active_filters$mandates, function(mandate) {
              span(
                style = paste0(
                  "background: rgba(255, 255, 255, 0.9); ",
                  "color: #667eea; ",
                  "padding: 4px 12px; ",
                  "border-radius: 16px; ",
                  "font-size: 12px; ",
                  "font-weight: 500; ",
                  "display: flex; ",
                  "align-items: center; ",
                  "gap: 6px;"
                ),
                icon("scroll", class = "fas", style = "font-size: 10px;"),
                mandate
              )
            })
          },
          
          # Objective filters
          if (length(active_filters$objectives) > 0) {
            lapply(active_filters$objectives, function(objective) {
              span(
                style = paste0(
                  "background: rgba(255, 255, 255, 0.9); ",
                  "color: #667eea; ",
                  "padding: 4px 12px; ",
                  "border-radius: 16px; ",
                  "font-size: 12px; ",
                  "font-weight: 500; ",
                  "display: flex; ",
                  "align-items: center; ",
                  "gap: 6px;"
                ),
                icon("bullseye", class = "fas", style = "font-size: 10px;"),
                objective
              )
            })
          },
          
          # Sector filters
          if (length(active_filters$sectors) > 0) {
            lapply(active_filters$sectors, function(sector) {
              span(
                style = paste0(
                  "background: rgba(255, 255, 255, 0.9); ",
                  "color: #667eea; ",
                  "padding: 4px 12px; ",
                  "border-radius: 16px; ",
                  "font-size: 12px; ",
                  "font-weight: 500; ",
                  "display: flex; ",
                  "align-items: center; ",
                  "gap: 6px;"
                ),
                icon("chart-pie", class = "fas", style = "font-size: 10px;"),
                sector
              )
            })
          }
        )
      )
    },
    
    # Statistics cards
    div(
      style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(130px, 1fr)); gap: 16px;",
      
      # N Indicators cards
      div(
        class = "stat-card",
        style = paste0(
          "background: rgba(255, 215, 0, 0.2); ",
          "backdrop-filter: blur(10px); ",
          "padding: 16px; ",
          "border-radius: 12px; ",
          "border: 1px solid rgba(255, 215, 0, 0.3); ",
          "text-align: center; ",
          "transition: all 0.3s ease;"
        ),
        div(
          icon("chart-simple", class = "fas", style = "font-size: 24px; margin-bottom: 8px; color: #ffd700;")
        ),
        div(
          n, 
          style = "font-size: 32px; font-weight: 800; margin-bottom: 4px;"
        ),
        div(
          paste("of", N, "indicators"),
          style = "font-size: 14px; opacity: 0.8; font-weight: 400;"
        )
      ),
      
      # Mandates card
      div(
        class = "stat-card",
        style = paste0(
          "background: rgba(255, 255, 255, 0.15); ",
          "backdrop-filter: blur(10px); ",
          "padding: 16px; ",
          "border-radius: 12px; ",
          "border: 1px solid rgba(255, 255, 255, 0.2); ",
          "text-align: center; ",
          "transition: all 0.3s ease;"
        ),
        div(
          icon("scroll", class = "fas", style = "font-size: 24px; margin-bottom: 8px; opacity: 0.8;")
        ),
        div(
          mandates_count,
          style = "font-size: 24px; font-weight: 700; margin-bottom: 4px;"
        ),
        div(
          "Mandates",
          style = "font-size: 13px; opacity: 0.8; font-weight: 400;"
        )
      ),
      
      # Objectives
      div(
        class = "stat-card",
        style = paste0(
          "background: rgba(255, 255, 255, 0.15); ",
          "backdrop-filter: blur(10px); ",
          "padding: 16px; ",
          "border-radius: 12px; ",
          "border: 1px solid rgba(255, 255, 255, 0.2); ",
          "text-align: center; ",
          "transition: all 0.3s ease;"
        ),
        div(
          icon("bullseye", class = "fas", style = "font-size: 24px; margin-bottom: 8px; opacity: 0.8;")
        ),
        div(
          objectives_count,
          style = "font-size: 24px; font-weight: 700; margin-bottom: 4px;"
        ),
        div(
          "Objectives",
          style = "font-size: 13px; opacity: 0.8; font-weight: 400;"
        )
      ), 
      
      # Sectors card
      div(
        class = "stat-card",
        style = paste0(
          "background: rgba(255, 255, 255, 0.15); ",
          "backdrop-filter: blur(10px); ",
          "padding: 16px; ",
          "border-radius: 12px; ",
          "border: 1px solid rgba(255, 255, 255, 0.2); ",
          "text-align: center; ",
          "transition: all 0.3s ease;"
        ),
        div(
          icon("chart-pie", class = "fas", style = "font-size: 24px; margin-bottom: 8px; opacity: 0.8;")
        ),
        div(
          sectors_count,
          style = "font-size: 24px; font-weight: 700; margin-bottom: 4px;"
        ),
        div(
          "Sectors",
          style = "font-size: 13px; opacity: 0.8; font-weight: 400;"
        )
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
    "PRUDENTIAL SUPERVISION" = "Risk management and stability indicators for financial institutions",
    "MARKET DEVELOPMENT" = "Capital markets and competition indicators",
    "SUSTAINABILITY" = "Climate, environmental, and gender equality indicators",
    "STATISTICS & RESEARCH" = "Data collection and research indicators",
    "CENTRAL BANKING" = "Currency management and central bank operations"
  )
  
  div(
    class = "mandate-section-header",
    style = paste0(
      "background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%); ",
      "border: 1px solid #e9ecef; ",
      "border-radius: 12px; ",
      "padding: 24px; ",
      "margin: 32px 0 24px 0; ",
      "box-shadow: 0 2px 12px rgba(0,0,0,0.04); ",
      "position: relative; ",
      "overflow: hidden;"
    ),
    
    # Background pattern
    div(
      style = paste0(
        "position: absolute; ",
        "top: -50%; ",
        "right: -10%; ",
        "width: 200px; ",
        "height: 200px; ",
        "background: radial-gradient(circle, rgba(0,123,255,0.05) 0%, transparent 70%); ",
        "border-radius: 50%;"
      )
    ),
    
    div(
      style = "position: relative; z-index: 1;",
      
      # Header row
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
        
        h3(
          mandate,
          style = paste0(
            "margin: 0; ",
            "font-size: 24px; ",
            "font-weight: 700; ",
            "color: #1a1a1a; ",
            "text-transform: capitalize;"
          )
        ),
        
        span(
          paste(count, "indicators"),
          style = paste0(
            "background: #007bff; ",
            "color: white; ",
            "padding: 6px 16px; ",
            "border-radius: 20px; ",
            "font-size: 14px; ",
            "font-weight: 600; ",
            "box-shadow: 0 2px 8px rgba(0,123,255,0.3);"
          )
        )
      ),
      
      # Description
      if (!is.null(descriptions[[toupper(mandate)]])) {
        p(
          descriptions[[toupper(mandate)]],
          style = paste0(
            "margin: 0; ",
            "color: #6c757d; ",
            "font-size: 15px; ",
            "line-height: 1.5;"
          )
        )
      }
    )
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
            color: #667eea;
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
                    <div class="summary-card-label">Sectors</div>
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
                  if (!is.null(ind$preset_foundation) && !is.na(ind$preset_foundation) && ind$preset_foundation == 1) {
                    '<span class="badge badge-priority"><i class="fas fa-building-columns"></i> Foundational</span>'
                  } else if (!is.null(ind$preset_digital) && !is.na(ind$preset_digital) && ind$preset_digital == 1) {
                    '<span class="badge badge-priority"><i class="fas fa-mobile-screen"></i> Digital finance ecosystem </span>'
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
                For more information, visit <a href="https://www.cgap.org" style="color: #667eea;">www.cgap.org</a>
            </div>
        </div>
    </div>
</body>
</html>')
}
