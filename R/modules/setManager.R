# R/modules/setManager.R - Module for managing multiple indicator sets

# UI function
setManagerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # 1. Add this CSS to remove the bottom margin from the input inside our specific wrapper
    tags$style(HTML("
    .set-selector-control .form-group {
      margin-bottom: 0 !important;
    }
  ")),
    
    # Compact set selector and controls
    div(
      style = paste0(
        "background: white; ",
        "border: 1px solid #e9ecef; ",
        "border-radius: 8px; ",
        "padding: 12px 16px; ",
        "margin-bottom: 16px; ",
        "box-shadow: 0 2px 4px rgba(0,0,0,0.05);"
      ),
      
      # 2. Keep 'align-items: center' here
      div(
        style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap;",
        
        # Label
        span(
          icon("layer-group", class = "fas", style = "margin-right: 4px;"),
          "Active Set:",
          style = "font-weight: 600; color: #495057; font-size: 14px; white-space: nowrap;"
        ),
        
        # Set selector dropdown
        div(
          # 3. Add the custom class here to target the input inside
          class = "set-selector-control", 
          style = "flex: 1; min-width: 200px;",
          selectInput(
            ns("active_set"),
            label = NULL,
            choices = NULL,
            width = "100%"
          )
        ),
        
        # Action buttons
        div(
          style = "display: flex; gap: 8px;",
          
          actionButton(
            ns("create_set"),
            label = NULL,
            icon = icon("plus", class = "fas"),
            class = "btn btn-sm btn-success",
            title = "Create new set",
            style = "padding: 6px 12px;"
          ),
          
          actionButton(
            ns("rename_set"),
            label = NULL,
            icon = icon("pen", class = "fas"),
            class = "btn btn-sm btn-outline-secondary",
            title = "Rename current set",
            style = "padding: 6px 12px;"
          ),
          
          actionButton(
            ns("delete_set"),
            label = NULL,
            icon = icon("trash", class = "fas"),
            class = "btn btn-sm btn-outline-danger",
            title = "Delete current set",
            style = "padding: 6px 12px;"
          )
        )
      )
    )
  )
  
}

# Server function
setManagerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize with one default set
    indicator_sets <- reactiveVal(list(
      "My Indicators" = character()
    ))
    
    active_set_name <- reactiveVal("My Indicators")
    
    # Update dropdown choices whenever sets change
    observe({
      sets <- indicator_sets()
      current_active <- active_set_name()
      
      updateSelectInput(
        session,
        "active_set",
        choices = names(sets),
        selected = current_active
      )
    })
    
    # Handle set switching
    observeEvent(input$active_set, {
      req(input$active_set)
      if (input$active_set %in% names(indicator_sets())) {
        active_set_name(input$active_set)
      }
    })
    
    # Create new set
    observeEvent(input$create_set, {
      showModal(modalDialog(
        # 1. Custom CSS
        tags$style(HTML("
    .modal-header {
      border-bottom: none !important;
      padding-top: 24px;
      /* Ensure the header itself doesn't force left alignment */
      display: flex;
      justify-content: center;
      position: relative;
    }
    .modal-footer {
      border-top: none !important;
      display: flex;
      justify-content: center;
      gap: 12px;
      padding-bottom: 24px;
    }
    .modal-content {
      border-radius: 16px !important;
      box-shadow: 0 10px 25px rgba(0,0,0,0.15) !important;
      border: none !important;
    }
    .modern-input label {
      display: none; 
    }
    .modern-input input {
      text-align: center;
      font-size: 16px;
      height: 48px;
      border-radius: 8px;
      border: 1px solid #dee2e6;
      background-color: #f8f9fa;
    }
    .modern-input input:focus {
      background-color: #fff;
      box-shadow: 0 0 0 4px rgba(0,123,255, 0.1);
      border-color: #80bdff;
    }
    /* Fix for the close button to stay in the corner while title centers */
    .modal-header .close {
      position: absolute;
      right: 16px;
      top: 16px;
      z-index: 10;
    }
    /* Ensure the wrapper h4 spans full width to allow centering */
    .modal-title {
      width: 100%;
    }
  ")),
        
        # 2. Updated Title Wrapper with Explicit Flex Centering
        title = div(
          style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
          
          div(
            style = "font-size: 40px; color: #4dadf7; margin-bottom: 8px;",
            icon("layer-group", class = "fas")
          ),
          span("Create New Indicator Set", style = "font-weight: 700; font-size: 20px; text-align: center;")
        ),
        
        size = "s",
        easyClose = TRUE,
        fade = TRUE,
        
        div(
          style = "padding: 0 24px;", 
          p("Give your new set a descriptive name.", style = "text-align: center; color: #6c757d; margin-bottom: 20px;"),
          
          div(
            class = "modern-input",
            textInput(
              ns("new_set_name"),
              label = NULL,
              placeholder = "e.g. Credit usage set",
              value = "",
              width = "100%"
            )
          )
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_create"), 
            "Create Set", 
            class = "btn-primary",
            style = "padding: 8px 24px; font-weight: 600;"
          )
        )
      ))
    })
    
    # Confirm create
    observeEvent(input$confirm_create, {
      req(input$new_set_name)
      
      new_name <- trimws(input$new_set_name)
      
      # Validation
      if (new_name == "") {
        showNotification("Please enter a name for the set.", type = "warning")
        return()
      }
      
      if (new_name %in% names(indicator_sets())) {
        showNotification("A set with this name already exists.", type = "warning")
        return()
      }
      
      # Create new set
      sets <- indicator_sets()
      sets[[new_name]] <- character()
      indicator_sets(sets)
      
      # Switch to new set
      active_set_name(new_name)
      
      removeModal()
      showNotification(paste("Created set:", new_name), type = "message")
    })
    
    # Rename set
    observeEvent(input$rename_set, {
      current_name <- active_set_name()
      
      showModal(modalDialog(
        # 1. Same Custom CSS for consistency
        tags$style(HTML("
    .modal-header {
      border-bottom: none !important;
      padding-top: 24px;
      display: flex;
      justify-content: center;
      position: relative;
    }
    .modal-footer {
      border-top: none !important;
      display: flex;
      justify-content: center;
      gap: 12px;
      padding-bottom: 24px;
    }
    .modal-content {
      border-radius: 16px !important;
      box-shadow: 0 10px 25px rgba(0,0,0,0.15) !important;
      border: none !important;
    }
    .modern-input label {
      display: none; 
    }
    .modern-input input {
      text-align: center;
      font-size: 16px;
      height: 48px;
      border-radius: 8px;
      border: 1px solid #dee2e6;
      background-color: #f8f9fa;
    }
    .modern-input input:focus {
      background-color: #fff;
      box-shadow: 0 0 0 4px rgba(0,123,255, 0.1);
      border-color: #80bdff;
    }
    .modal-header .close {
      position: absolute;
      right: 16px;
      top: 16px;
      z-index: 10;
    }
    .modal-title {
      width: 100%;
    }
  ")),
        
        # 2. Title with Flex Centering and Pen Icon
        title = div(
          style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
          
          div(
            style = "font-size: 40px; color: #4dadf7; margin-bottom: 8px;",
            icon("pen", class = "fas")
          ),
          span("Rename Indicator Set", style = "font-weight: 700; font-size: 20px; text-align: center;")
        ),
        
        size = "s", # Kept small for consistency
        easyClose = TRUE,
        fade = TRUE,
        
        div(
          style = "padding: 0 24px;", 
          
          # 3. Styled 'Current Name' display
          p(
            span("Current name:", style = "color: #6c757d; margin-right: 5px;"),
            strong(current_name, style = "color: #495057;"),
            style = "text-align: center; margin-bottom: 20px; font-size: 14px;"
          ),
          
          # 4. Input Field
          div(
            class = "modern-input",
            textInput(
              ns("rename_set_name"),
              label = NULL,
              placeholder = "Enter new name",
              value = current_name, # Pre-fill the input with the old name
              width = "100%"
            )
          )
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_rename"), 
            "Rename", 
            class = "btn-primary",
            style = "padding: 8px 24px; font-weight: 600;"
          )
        )
      ))
      
    })
    
    # Confirm rename
    observeEvent(input$confirm_rename, {
      req(input$rename_set_name)
      
      old_name <- active_set_name()
      new_name <- trimws(input$rename_set_name)
      
      # Validation
      if (new_name == "") {
        showNotification("Please enter a name for the set.", type = "warning")
        return()
      }
      
      if (new_name %in% names(indicator_sets()) && new_name != old_name) {
        showNotification("A set with this name already exists.", type = "warning")
        return()
      }
      
      if (new_name == old_name) {
        removeModal()
        return()
      }
      
      # Rename set
      sets <- indicator_sets()
      names(sets)[names(sets) == old_name] <- new_name
      indicator_sets(sets)
      active_set_name(new_name)
      
      removeModal()
      showNotification(paste("Renamed to:", new_name), type = "message")
    })
    
    # Delete set
    observeEvent(input$delete_set, {
      current_name <- active_set_name()
      sets <- indicator_sets()
      
      # Can't delete if only one set
      if (length(sets) == 1) {
        showNotification("Cannot delete the only remaining set.", type = "warning")
        return()
      }
      
      showModal(modalDialog(
        # 1. Consistent Custom CSS
        tags$style(HTML("
    .modal-header {
      border-bottom: none !important;
      padding-top: 24px;
      display: flex;
      justify-content: center;
      position: relative;
    }
    .modal-footer {
      border-top: none !important;
      display: flex;
      justify-content: center;
      gap: 12px;
      padding-bottom: 24px;
    }
    .modal-content {
      border-radius: 16px !important;
      box-shadow: 0 10px 25px rgba(0,0,0,0.15) !important;
      border: none !important;
    }
    .modal-header .close {
      position: absolute;
      right: 16px;
      top: 16px;
      z-index: 10;
    }
    .modal-title {
      width: 100%;
    }
  ")),
        
        # 2. Title with Red Warning Icon
        title = div(
          style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
          
          div(
            # Color changed to Bootstrap 'danger' red (#dc3545)
            style = "font-size: 40px; color: #dc3545; margin-bottom: 8px;",
            icon("exclamation-triangle", class = "fas")
          ),
          span("Delete Indicator Set", style = "font-weight: 700; font-size: 20px; text-align: center;")
        ),
        
        size = "s",
        easyClose = TRUE,
        fade = TRUE,
        
        # 3. Centered Warning Message
        div(
          style = "padding: 0 24px; text-align: center;",
          
          p(
            "Are you sure you want to delete the set",
            br(), # Break line for better readability
            strong(current_name, style = "color: #212529; font-size: 16px;"),
            "?",
            style = "margin-bottom: 12px; color: #495057;"
          ),
          
          p(
            "This action cannot be undone.",
            style = "color: #dc3545; font-size: 14px; font-weight: 500;" # Make this warning subtle red
          )
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_delete"), 
            "Delete Set", 
            class = "btn-danger",
            style = "padding: 8px 24px; font-weight: 600;"
          )
        )
      ))
    })
    
    # Confirm delete
    observeEvent(input$confirm_delete, {
      old_name <- active_set_name()
      sets <- indicator_sets()
      
      # Remove the set
      sets[[old_name]] <- NULL
      indicator_sets(sets)
      
      # Switch to first remaining set
      active_set_name(names(sets)[1])
      
      removeModal()
      showNotification(paste("Deleted set:", old_name), type = "message")
    })
    
    # Return reactive values that the main app can use
    return(list(
      sets = indicator_sets,
      active_set = active_set_name,
      get_active_indicators = reactive({
        sets <- indicator_sets()
        active <- active_set_name()
        if (active %in% names(sets)) {
          return(sets[[active]])
        } else {
          return(character())
        }
      }),
      add_to_active = function(indicator_id) {
        sets <- indicator_sets()
        active <- active_set_name()
        if (active %in% names(sets)) {
          sets[[active]] <- union(sets[[active]], indicator_id)
          indicator_sets(sets)
        }
      },
      remove_from_active = function(indicator_id) {
        sets <- indicator_sets()
        active <- active_set_name()
        if (active %in% names(sets)) {
          sets[[active]] <- setdiff(sets[[active]], indicator_id)
          indicator_sets(sets)
        }
      }
    ))
  })
}