options(shiny.autoload.r = FALSE)

library(shiny)
library(dplyr)
library(stringr)
library(bslib)
library(SnowballC)

# Load data
load("data/indicators.RData")

# Source modular components
source("R/globals.R")
source("R/utils.R")
source("R/modules/indicatorCard.R")
source("R/modules/filterPanel.R")
source("R/modules/setManager.R")                    
source("R/modules/selectedIndicatorsMulti.R") 


# UI ----------
ui <- page_navbar(
  
  title = div(
    style = paste0(
      "display: flex; ",
      "align-items: center; ",
      "gap: 15px; ",
      "padding: 0; ",
      "margin: 0; ",
      "height: 40px;"
    ),
    tags$img(
      src = "cgap_logo.png", 
      height = "40px",
      style = "display: block; vertical-align: middle;"
    ),
    div(
      style = paste0(
        "height: 40px; ",
        "width: 1px; ",
        "background-color: #d1d5db;"
      )
    ),
    span(
      style = paste0(
        "font-size: 40px; ",
        "font-weight: 700; ",
        "color: #1A5A80; ",
        "line-height: 40px; ",
        "font-family: 'Figtree', sans-serif; ",
        "display: inline-block; ",
        "vertical-align: middle;"
      ), 
      "LENS"
    ), 
    span(
      style = paste0(
        "font-size: 17px; ",
        "font-weight: 200; ",
        "font-style: Italic;", 
        "color: #1A5A80; ",
        "line-height: 17px; ",
        "font-family: 'Figtree', sans-serif; ",
        "display: inline-block; ",
        "vertical-align: top;"
      ), 
      "Beta"
    )
  ),
  
  header = tagList(
    includeCSS("www/custom.css"),
    tags$style(HTML(generate_sector_styles(SECTOR_COLORS))),
    indicatorCardJS(),

    # Preset memo drawer (slide-in panel, non-blocking)
    div(
      id = "memo-drawer",
      class = "memo-drawer",

      # Drawer header
      div(
        class = "memo-drawer-header",
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          icon("file-lines", class = "fas", style = "font-size: 16px;"),
          span(id = "memo-drawer-title", "Preset Memo", style = "font-weight: 600; font-size: 16px;")
        ),
        tags$button(
          class = "memo-drawer-close",
          onclick = "closePresetMemo();",
          icon("times", class = "fas")
        )
      ),

      # Drawer body (populated by JS)
      div(
        id = "memo-drawer-body",
        class = "memo-drawer-body"
      )
    ),

    # Memo data and JS
    tags$script(HTML(paste0(
      "var presetMemos = ", jsonlite::toJSON(PRESET_MEMOS, auto_unbox = TRUE), ";\n",
      "
      function openPresetMemo(presetId) {
        var memo = presetMemos[presetId];
        if (!memo) return;

        document.getElementById('memo-drawer-title').textContent = memo.title;

        var body = document.getElementById('memo-drawer-body');
        var html = '<p class=\"memo-summary\">' + memo.summary + '</p>';

        memo.sections.forEach(function(section) {
          html += '<div class=\"memo-section\">';
          html += '<h4>' + section.heading + '</h4>';
          html += '<div>' + section.body + '</div>';
          html += '</div>';
        });

        body.innerHTML = html;

        var drawer = document.getElementById('memo-drawer');
        // Toggle: if already open, close it; otherwise open
        if (drawer.classList.contains('open')) {
          drawer.classList.remove('open');
        } else {
          drawer.classList.add('open');
        }
      }

      function closePresetMemo() {
        document.getElementById('memo-drawer').classList.remove('open');
      }

      // Close on Escape key
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') closePresetMemo();
      });
      "
    )))
  ),
  
  sidebar = sidebar(
    width = 400,
    p(style = "font-size: 13px; color: #555; line-height: 1.5;",
      "A curated catalog of regulatory indicators that analysts combine with breakdowns — such as gender, customer type, or provider type — to support disaggregated analysis for more inclusive financial policies. ",
      tags$a("See User Guide", onclick = "document.getElementById('show_about').click(); return false;",
             href = "#", style = "color: #1A5A80; font-weight: 600; text-decoration: none;"),
      " for details."
    ),
    filterPanelUI("filters")
  ),
  
  nav_spacer(),
  
  nav_panel(
    title = "BROWSE",
    
    layout_sidebar(
      sidebar = sidebar(
        position = "right",
        width = 300,
        class = "mandate-sidebar",
        h5("Quick navigation", style = "margin-bottom: 0px;"),
        "Jump to indicators grouped by their primary mandate",
        uiOutput("mandate_links")
      ),
      div(
        uiOutput("navigation_helper"),
        uiOutput("key"),
        div(
          id = "indicator_wrapper",
          class = "loading-overlay",
          div(
            id = "loading_spinner",
            class = "loading-spinner",
            style = "display: none;",
            div(class = "spinner")
          ),
          div(
            id = "indicator_container",
            class = "fade-content"
          )
        )
      )
    )
  ),
  
  nav_panel(
    title = uiOutput("selected_tab_title"),
    selectedIndicatorsMultiUI("selected")
  ),
  
  nav_item(
    actionButton(
      "show_about",
      "ABOUT",
      class = "btn-link",
      style = "color: inherit; text-decoration: none; border: none; background: transparent; font-size: 14px;"
    )
  )
  
)

# SERVER ----------
server <- function(input, output, session) {
  
  # Initialize set manager
  set_manager <- selectedIndicatorsMultiServer(
    "selected",
    indicators_data = indicators,
    sector_colors = SECTOR_COLORS
  )
  
  # Filter module
  filter_values <- filterPanelServer("filters", indicators)
  
  # Filtered indicators with debouncing
  filtered_indicators <- reactive({
    filter_values$filtered_indicators()
  }) %>% 
    debounce(200)
  
  # Track when we need to re-render indicators
  filter_trigger <- reactiveVal(0)
  
  observeEvent(filtered_indicators(), {
    filter_trigger(filter_trigger() + 1)
  })
  
  # Navigation helper with active filters
  output$navigation_helper <- renderUI({
    indicators_data <- filtered_indicators()
    
    active_filters <- list(
      mandates = input$`filters-mandates`,
      objectives = filter_values$selected_objectives(),
      sectors = input$`filters-sectors`,
      use_cases = input$`filters-use_cases`,
      search = input$`filters-search`,
      presets_digital = input$`filters-presets_digital`
    )
    
    # Get the active set name from set_manager
    active_set_name <- set_manager$active_set()
    
    enhanced_navigation_helper(
      indicators_data, 
      indicators, 
      active_filters,
      active_set_name = active_set_name
    )
  })
  
  # Key
  output$key <- renderUI({
    indicator_key()
  })
  
  # Mandate links
  output$mandate_links <- renderUI({
    indicators_data <- filtered_indicators()
    create_mandate_links(indicators_data)
  })
  
 
  # ADD ALL / REMOVE ALL BUTTON HANDLERS -------------

  
  # Add all filtered indicators to the active set
  observeEvent(input$add_all_filtered, {
    req(filtered_indicators())
    filtered_ids <- isolate(filtered_indicators()$indicator_id)

    if (length(filtered_ids) > 0) {
      set_manager$add_many_to_active(filtered_ids)

      showNotification(
        paste("Added", length(filtered_ids), "indicators to your set"),
        type = "message",
        duration = 3
      )

      # Re-render to update button states
      filter_trigger(filter_trigger() + 1)
    }
  })

  # Remove all filtered indicators from the active set
  observeEvent(input$remove_all_filtered, {
    req(filtered_indicators())
    filtered_ids <- isolate(filtered_indicators()$indicator_id)
    current_selected <- isolate(set_manager$get_active_indicators())
    to_remove <- intersect(filtered_ids, current_selected)

    if (length(to_remove) > 0) {
      set_manager$remove_many_from_active(to_remove)

      showNotification(
        paste("Removed", length(to_remove), "indicators from your set"),
        type = "message",
        duration = 3
      )

      filter_trigger(filter_trigger() + 1)
    } else {
      showNotification(
        "No filtered indicators are in your current set",
        type = "warning",
        duration = 3
      )
    }
  })
  

  # END ADD ALL / REMOVE ALL HANDLERS -----------
  
  # Render indicators only when filter_trigger changes
  observeEvent(filter_trigger(), {
    session$sendCustomMessage("showLoading", TRUE)
    
    indicators_data <- isolate(filtered_indicators())
    selected_codes <- isolate(set_manager$get_active_indicators()) 
    
    removeUI(selector = "#indicator_container > *", immediate = TRUE, multiple = TRUE)
    
    if (nrow(indicators_data) == 0) {
      insertUI(
        selector = "#indicator_container",
        ui =  div(br(), span(
          icon("times-circle", class = "text-danger", lib = "font-awesome"),
          "No indicators match filter criteria",
          style = "font-size: 16px;"
        )),
        immediate = TRUE
      )
      session$sendCustomMessage("showLoading", FALSE)
      return()
    }
    
    mandates <- unique(indicators_data$main_mandate)
    
    for (mandate in mandates) {
      mandate_indicators <- indicators_data %>% 
        filter(main_mandate == mandate)
      
      title_case_mandate <- str_to_title(mandate)
      mandate_id <- paste0("mandate_", slugify(title_case_mandate))
      
      insertUI(
        selector = "#indicator_container",
        ui = tags$div(
          id = mandate_id,
          enhanced_mandate_header(title_case_mandate, nrow(mandate_indicators)),
          accordion(
            id = paste0(mandate_id, "_accordion"),
            !!!lapply(seq_len(nrow(mandate_indicators)), function(i) {
              ind <- mandate_indicators[i, ]
              indicatorCardModern(  
                ind$indicator_id,
                ind,
                SECTOR_COLORS,
                ind$indicator_id %in% selected_codes
              )
            }),
            open = FALSE
          )
        ),
        immediate = TRUE
      )
    }
    
    session$sendCustomMessage("setupSelectButtons", list())
    session$sendCustomMessage("hideLoadingDelayed", list(delay = 300))
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  # Handle selection events from JavaScript
  observeEvent(input$indicator_selected, {
    req(input$indicator_selected)
    
    id <- input$indicator_selected$id
    action <- input$indicator_selected$action
    
    isolate({
      if (action == "add") {
        set_manager$add_to_active(id)
      } else {
        set_manager$remove_from_active(id)
      }
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Selected tab title
  output$selected_tab_title <- renderUI({
    active_indicators <- set_manager$get_active_indicators()
    count <- length(active_indicators)
    
    if (count == 0) {
      span(
        icon("clipboard-list", lib = "font-awesome"),
        " YOUR INDICATOR SETS"
      )
    } else {
      span(
        icon("clipboard-check", lib = "font-awesome"),
        " YOUR INDICATOR SETS",
        span(
          paste0(count),
          style = "background-color: #198754; color: white; font-size: 12px; padding: 2px 8px; border-radius: 12px; margin-left: 5px;"
        )
      )
    }
  })
  
  # About modal
  observeEvent(input$show_about, {
    showModal(about_modal_content())
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
