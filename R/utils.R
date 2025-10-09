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
  # Create gradient from all sector colors
  sector_colors <- c("#FFD700", "#FFA07A", "#98FB98", "#87CEFA", "#DDA0DD", "#FFC0CB", "#B0C4DE")
  
  # Calculate gradient stops
  num_colors <- length(sector_colors)
  gradient_stops <- paste(
    sapply(seq_along(sector_colors), function(i) {
      start_pct <- (i - 1) * (100 / num_colors)
      end_pct <- i * (100 / num_colors)
      paste0(sector_colors[i], " ", start_pct, "%, ", sector_colors[i], " ", end_pct, "%")
    }),
    collapse = ", "
  )
  
  gradient_bg <- paste0("linear-gradient(90deg, ", gradient_stops, ")")
  
  div(
    style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap; padding: 12px 0; margin-bottom: 16px;",
    
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
        "font-weight: 500; ",
        "border: 1px solid rgba(0,0,0,0.1); ",
        "font-weight: 600; ",
        "text-shadow: 0 0 2px rgba(255,255,255,0.8);"
      )
    ),
    
    # Priority indicator explanation
    div(
      style = "display: flex; align-items: center; gap: 6px;",
      div(
        style = paste0(
          "background: linear-gradient(135deg, #ffd700 0%, #ffed4e 100%); ",
          "color: #856404; ",
          "padding: 4px 8px; ",
          "border-radius: 12px; ",
          "font-size: 11px; ",
          "font-weight: 600; ",
          "display: flex; ",
          "align-items: center; ",
          "gap: 4px; ",
          "box-shadow: 0 2px 4px rgba(255, 215, 0, 0.3);"
        ),
        tags$i(class = "fas fa-star", style = "font-size: 10px;"),
        "Featured"
      ),
      span(
        "= Featured in CGAP technical guide",
        style = "font-size: 12px; color: #495057;"
      )
    )
  )
}
# Updated indicator_key function for R/utils.R
# Matches the CSS styling from indicatorCardModern


indicator_key <- function() {
  # Simpler, direct gradient creation using all sector colors
  gradient_bg <- "linear-gradient(90deg, #FFD700 0%, #FFD700 14.28%, #FFA07A 14.28%, #FFA07A 28.56%, #98FB98 28.56%, #98FB98 42.84%, #87CEFA 42.84%, #87CEFA 57.12%, #DDA0DD 57.12%, #DDA0DD 71.4%, #FFC0CB 71.4%, #FFC0CB 85.68%, #B0C4DE 85.68%, #B0C4DE 100%)"
  
  div(
    style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap; padding: 12px 0; margin-bottom: 16px;",
    
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
    ),
    
    # Priority indicator explanation
    div(
      style = "display: flex; align-items: center; gap: 6px;",
      div(
        style = paste0(
          "background: linear-gradient(135deg, #ffd700 0%, #ffed4e 100%); ",
          "color: #856404; ",
          "padding: 4px 8px; ",
          "border-radius: 12px; ",
          "font-size: 11px; ",
          "font-weight: 600; ",
          "display: flex; ",
          "align-items: center; ",
          "gap: 4px; ",
          "box-shadow: 0 2px 4px rgba(255, 215, 0, 0.3);"
        ),
        tags$i(class = "fas fa-star", style = "font-size: 10px;"),
        "Featured"
      ),
      span(
        "= Featured in CGAP technical guide",
        style = "font-size: 12px; color: #495057;"
      )
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
  mandates <- unique(indicators_data$main_mandate_umbrella)
  
  # Remove NA values if any
  mandates <- mandates[!is.na(mandates)]
  
  # Getting number of indicators per mandate 
  N_ind_bymandate <- indicators_data %>% 
    filter(!is.na(main_mandate_umbrella)) %>%  # Filter out NA mandates
    group_by(main_mandate_umbrella) %>% 
    summarise(n = n(), .groups = "drop")
  
  # Create named vector for lookup
  ref <- setNames(as.character(N_ind_bymandate$n), as.character(N_ind_bymandate$main_mandate_umbrella))
  
  # Debug: print the reference to console (remove in production)
  # print("Mandate counts:")
  # print(ref)
  
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



# Enhanced navigation helper with active filters display for R/utils.R

enhanced_navigation_helper <- function(filtered_indicators, total_indicators, active_filters = NULL) {
  n <- nrow(filtered_indicators)
  N <- nrow(total_indicators)
  
  # Calculate statistics
  mandates_count <- length(unique(filtered_indicators$main_mandate_umbrella))
  sectors_count <- length(unique(filtered_indicators$main_sector))
  priority_count <- sum(filtered_indicators$high_priority == "High priority", na.rm = TRUE)
  
  # Check if any filters are active
  has_active_filters <- !is.null(active_filters) && (
    length(active_filters$mandates) > 0 ||
      length(active_filters$objectives) > 0 ||
      length(active_filters$sectors) > 0 ||
      length(active_filters$use_cases) > 0 ||
      !is.null(active_filters$search) && active_filters$search != "" ||
      active_filters$priority_only == TRUE
  )
  
  div(
    class = "navigation-header",
    style = paste0(
      "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); ",
      "color: white; ",
      "padding: 24px; ",
      "border-radius: 16px; ",
      "margin-bottom: 32px; ",
      "box-shadow: 0 8px 32px rgba(102, 126, 234, 0.3);"
    ),
    
    # Main title and count
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
      
      div(
        h2(
          "Regulatory indicators with a sociodemographic lens",
          style = "margin: 0; font-size: 28px; font-weight: 700; text-shadow: 0 2px 4px rgba(0,0,0,0.1);"
        ),
        p(
          paste("Explore", N, "curated indicators designed to support evidence-based financial regulation & policy"),
          style = "margin: 8px 0 0 0; font-size: 16px; opacity: 0.9; font-weight: 300;"
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
          
          # Priority filter
          if (!is.null(active_filters$priority_only) && active_filters$priority_only == TRUE) {
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
              icon("star", class = "fas", style = "font-size: 10px;"),
              "Featured only"
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
          },
          
          # Use case filters
          if (length(active_filters$use_cases) > 0) {
            lapply(active_filters$use_cases, function(use_case) {
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
                icon("sliders", class = "fas", style = "font-size: 10px;"),
                use_case
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
          "background: rgba(255, 255, 255, 0.2); ",
          "backdrop-filter: blur(10px); ",
          "padding: 12px 20px; ",
          "border-radius: 12px; ",
          "border: 1px solid rgba(255, 255, 255, 0.3);",
          "text-align: center; ",
          "transition: all 0.3s ease;"
        ),
        div(
          icon("chart-simple", class = "fas", style = "font-size: 24px; margin-bottom: 8px; opacity: 0.8;")
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
      ),
      
      # Priority indicators card
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
          icon("star", class = "fas", style = "font-size: 24px; margin-bottom: 8px; color: #ffd700;")
        ),
        div(
          priority_count,
          style = "font-size: 24px; font-weight: 700; margin-bottom: 4px;"
        ),
        div(
          "Featured indicators",
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
