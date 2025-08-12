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
  span(
    span("Indicator name", 
         style = "background-color: white; display: inline-block; padding:2px 5px; border-radius: 4px; color:black; font-weight: bold; font-size: 12px; margin-right: 5px; border: 1px solid #ccc;"), 
    span("Main objective(s)", 
         style = "background-color: #E5E7E6; display: inline-block; align-items: center; padding:2px; border-radius: 4px; color:black; font-weight: normal; font-size: 12px;"), 
    span("Main sector", 
         style = "background-color: #FFD700; display: inline-block; align-items: center; padding:2px; border-radius: 4px; color:black; font-weight: normal; font-size: 12px"), 
    span(tags$i(class = "fas fa-star", style = "color: gold; margin-left: 8px; font-size: 14px;"), 
         " = Priority indicator", 
         style = "font-size: 12px")
  )
}

# Create mandate links
create_mandate_links <- function(indicators_data) {
  # Handle empty or NULL data
  if (is.null(indicators_data) || nrow(indicators_data) == 0) {
    return(p("No indicators to display", style = "color: #666; font-style: italic;"))
  }
  
  mandates <- unique(indicators_data$main_mandate)
  
  # Getting number of indicators per mandate 
  N_ind_bymandate <- indicators_data %>% 
    group_by(main_mandate) %>% 
    count()
  ref <- setNames(as.character(N_ind_bymandate$n), N_ind_bymandate$main_mandate)
  
  tagList(
    lapply(mandates, function(mandate) {
      tags$a(
        href = paste0("#mandate_", slugify(mandate)),
        class = "mandate-link",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span(mandate, style = "flex-grow: 1;"),
          span(
            ref[mandate],
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
