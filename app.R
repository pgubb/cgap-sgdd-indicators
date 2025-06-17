library(shiny)
library(tidyverse)
library(readxl)
library(bslib)
library(janitor)

# Load data
load("data/indicators.RData")

# Source modular components
source("R/globals.R")
source("R/utils.R")
source("R/modules/indicatorCard.R")
source("R/modules/filterPanel.R")
source("R/modules/selectedIndicators.R")


# UI ----------
ui <- page_navbar(
  
  title = span(
    span(style = "font-size: 26px;", "RGDD explorer"), 
    span(style = "font-size: 14px;", " by "), 
    tags$img(src = "cgap_logo.png", height = "26px")
  ),
  
  tags$head(
    includeCSS("www/custom.css"),
    tags$style(HTML(generate_sector_styles(SECTOR_COLORS))),
    # Add the JavaScript for indicator card toggling
    indicatorCardJS()
  ),
  
  sidebar = sidebar(
    width = 400,
    p("Gender disaggregated regulatory data (RGDD) explorer is a catalog of indicators developed by CGAP to support efforts by financial sector authorities and others to use regulatory data to better understand the role of gender in the financial system."),
    
    filterPanelUI("filters")
  ),
  
  nav_spacer(),
  
  nav_panel(
    title = "Browse indicators",
    layout_sidebar(
      sidebar = sidebar(
        position = "right",
        width = 300,
        class = "mandate-sidebar",
        h4("Primary mandate", style = "margin-bottom: 20px;"),
        uiOutput("mandate_links")
      ),
      # Main content area
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
    selectedIndicatorsUI("selected")
  ),
  
  nav_panel(
    "About",
    includeHTML("www/about.html")
  )
)

# SERVER ----------
server <- function(input, output, session) {
  
  # Initialize reactive values
  values <- reactiveValues(
    selected_codes = character(),
    selected_indicators = data.frame(
      indicator_id = character(), 
      indicator_name = character(),
      indicator_description = character(),
      main_mandate = character(),
      main_objectives = character(),
      main_sector = character(),
      high_priority = character(),
      gender_questions = character(), 
      comment = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Filter module
  filter_values <- filterPanelServer("filters", indicators)
  
  # Filtered indicators with debouncing for better performance
  filtered_indicators <- reactive({
    filter_values$filtered_indicators()
  }) %>% 
    debounce(300)  # Wait 300ms after last change before updating
  
  # Track when we need to re-render indicators (only on filter changes)
  filter_trigger <- reactiveVal(0)
  
  observeEvent(filtered_indicators(), {
    filter_trigger(filter_trigger() + 1)
  })
  
  # Navigation helper
  output$navigation_helper <- renderUI({
    indicators_data <- filtered_indicators()
    n <- nrow(indicators_data)
    N <- nrow(indicators)
    
    div(
      style = "font-size: 15px;",
      span(
        icon("list", lib = "font-awesome"), 
        strong(paste(n, " of ", N)), 
        "indicators"
      ),
      span(
        id = "inline_spinner",
        class = "spinner-small",
        style = "display: none;"
      )
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
  
  # Render indicators only when filter_trigger changes
  observeEvent(filter_trigger(), {
    # Show loading spinner
    session$sendCustomMessage("showLoading", TRUE)
    
    indicators_data <- isolate(filtered_indicators())
    selected_codes <- isolate(values$selected_codes)
    
    # Clear existing indicators
    removeUI(selector = "#indicator_container > *", immediate = TRUE, multiple = TRUE)
    
    if (nrow(indicators_data) == 0) {
      insertUI(
        selector = "#indicator_container",
        ui = h4("No indicators available."),
        immediate = TRUE
      )
      # Hide loading spinner
      session$sendCustomMessage("showLoading", FALSE)
      return()
    }
    
    # Group by mandate and render
    mandates <- unique(indicators_data$main_mandate)
    
    for (mandate in mandates) {
      mandate_indicators <- indicators_data %>% 
        filter(main_mandate == mandate)
      
      # Create mandate section
      mandate_id <- paste0("mandate_", slugify(mandate))
      
      insertUI(
        selector = "#indicator_container",
        ui = tags$div(
          id = mandate_id,
          h5(str_to_upper(mandate), 
             style = "margin-top: 30px; margin-bottom: 20px; color: #7E868C;"),
          accordion(
            id = paste0(mandate_id, "_accordion"),
            !!!lapply(seq_len(nrow(mandate_indicators)), function(i) {
              ind <- mandate_indicators[i, ]
              indicatorCardAccordion(
                ind$indicator_id,
                ind,
                SECTOR_COLORS,
                ind$indicator_id %in% selected_codes
              )
            }),
            open = FALSE
          ),
          hr()
        ),
        immediate = TRUE
      )
    }
    
    # Setup button handlers after rendering
    session$sendCustomMessage("setupSelectButtons", list())
    
    # Hide loading spinner after a short delay to ensure smooth transition
    session$sendCustomMessage("hideLoadingDelayed", list(delay = 300))
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  # Handle selection events from JavaScript - isolated from UI rendering
  observeEvent(input$indicator_selected, {
    req(input$indicator_selected)
    
    id <- input$indicator_selected$id
    action <- input$indicator_selected$action
    
    isolate({
      if (action == "add") {
        values$selected_codes <- union(values$selected_codes, id)
      } else {
        values$selected_codes <- setdiff(values$selected_codes, id)
      }
      
      # Update selected indicators dataframe
      values$selected_indicators <- indicators %>%
        filter(indicator_id %in% values$selected_codes) %>%
        select(indicator_id, indicator_name, indicator_description, 
               main_mandate, main_objectives, main_sector, 
               high_priority, gender_questions)
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Selected indicators module
  selectedIndicatorsServer("selected", 
                           reactive(values$selected_indicators),
                           SECTOR_COLORS)
  
  # Selected tab title
  output$selected_tab_title <- renderUI({
    selected <- values$selected_indicators
    
    if (nrow(selected) == 0) {
      span(
        icon("clipboard-list", lib = "font-awesome"),
        " Your Selected Indicators"
      )
    } else {
      span(
        icon("clipboard-check", lib = "font-awesome"),
        " Your Selected Indicators ",
        span(
          paste0(nrow(selected)),
          style = "background-color: #198754; color: white; font-size: 12px; padding: 2px 8px; border-radius: 12px; margin-left: 5px;"
        )
      )
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)