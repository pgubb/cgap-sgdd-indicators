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
  
  title = div(
    style = paste0(
      "display: flex; ",
      "align-items: center; ",
      "gap: 15px; ",
      "padding: 0; ",           # Remove padding
      "margin: 0; ",            # Remove margin
      "height: 40px;"           # Fixed height
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
        "line-height: 40px; ",     # Match container height
        "font-family: 'Figtree', sans-serif; ",
        "display: inline-block; ",
        "vertical-align: middle;"
      ), 
      "LENS"
    )
  ),

  header = tagList(
    includeCSS("www/custom.css"),
    tags$style(HTML(generate_sector_styles(SECTOR_COLORS))),
    # Add the JavaScript for indicator card toggling
    indicatorCardJS()
  ),
  
  sidebar = sidebar(
    width = 400,
    p(style = "font-size: 14px;", "CGAP's LENS presents a curated catalog of indicators for use with regulatory data, enabling financial sector authorities and stakeholders to better understand financial behaviors, patterns, risks, and opportunities. The tool supports segmented analysis by key sociodemographic traits — with a particular focus on gender — helping uncover insights that can inform more inclusive, evidence-based financial policies and supervision, as well as financial service providers strategies."),
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
      preset_foundation = numeric(),
      preset_digital = numeric(),
      indicator_long_description = character(), 
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
    debounce(200)  # Wait 300ms after last change before updating
  
  # Track when we need to re-render indicators (only on filter changes)
  filter_trigger <- reactiveVal(0)
  
  observeEvent(filtered_indicators(), {
    filter_trigger(filter_trigger() + 1)
  })
  
  # Navigation helper with active filters
  output$navigation_helper <- renderUI({
    indicators_data <- filtered_indicators()
    
    # Get active filters from the filter module
    # These should come from the filter panel inputs
    active_filters <- list(
      mandates = input$`filters-mandates`,
      objectives = filter_values$selected_objectives(),  # You'll need to expose this from the filter module
      sectors = input$`filters-sectors`,
      use_cases = input$`filters-use_cases`,
      search = input$`filters-search`,
      presets_foundation = input$`filters-presets_foundation`
    )
    
    enhanced_navigation_helper(indicators_data, indicators, active_filters)
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
        ui =  div(br(), span(
          icon("times-circle", class = "text-danger", lib = "font-awesome"),
          "No indicators match filter criteria",
          style = "font-size: 16px;"
        )),
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
      
      # FIXED: Use consistent casing for both ID and header
      title_case_mandate <- str_to_title(mandate)
      mandate_id <- paste0("mandate_", slugify(title_case_mandate))
      
      insertUI(
        selector = "#indicator_container",
        ui = tags$div(
          id = mandate_id,  # This now matches the navigation links
          # Use the title case version for display
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
        select(indicator_id, indicator_name, indicator_description, indicator_long_description, 
               main_mandate, main_objectives, main_sector, 
               preset_foundation, preset_digital, gender_questions)
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
        " YOUR INDICATOR SET"
      )
    } else {
      span(
        icon("clipboard-check", lib = "font-awesome"),
        " YOUR INDICATOR SET",
        span(
          paste0(nrow(selected)),
          style = "background-color: #198754; color: white; font-size: 12px; padding: 2px 8px; border-radius: 12px; margin-left: 5px;"
        )
      )
    }
  })
  
  
  # About modal ------
  # Add this observer in your server function:
  observeEvent(input$show_about, {
    showModal(
      modalDialog(
        title = NULL,
        size = "xl",
        easyClose = TRUE,
        footer = modalButton("Close"),
        
        # Modal content
        div(
          # Header section
          div(
            style = paste0(
              "background: linear-gradient(135deg, #6F5B9D 0%, #402C60 100%); ",
              "color: white; ",
              "padding: 30px; ",
              "margin: -15px -15px 30px -15px; ",
              "border-radius: 8px 8px 0 0;"
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
            
            # Context & Additional Resources
            div(
              class = "about-section",
              style = "margin-bottom: 30px;",
              h3(
                icon("graduation-cap", class = "fas", style = "margin-right: 8px; color: #667eea;"),
                "Context & Additional Resources",
                style = "color: #333; margin-bottom: 15px; font-size: 20px;"
              ),
              p(
                "LENS is one component of CGAP's project to support the mainstreaming of gender-disaggregated regulatory data in the financial sector and should be consulted jointly with 'Using Gender-Disaggregated Regulatory Data to Improve Policy, Regulation and Supervision: A Technical Guide for Financial Sector Authorities'.",
                style = "line-height: 1.6; color: #555; margin-bottom: 15px;"
              ),
              p(
                "This technical guide describes use cases for Regulatory gender-disaggregated data (RGDD), provides recommendations for Financial Sector Authorities on how to leverage regulatory reporting regimes to maximize the use of gender-disaggregated data for multiple FSA mandates and to support the goals of other actors (policymakers, FSPs, funders and investors), and provide recommendations for FSAs to collect, use and disseminate gender equality data about their own organizations.",
                style = "line-height: 1.6; color: #555; margin-bottom: 15px;"
              ),
              div(
                style = "display: flex; gap: 10px; flex-wrap: wrap;",
                tags$a(
                  href = "https://www.cgap.org",
                  target = "_blank",
                  class = "btn btn-outline-primary btn-sm",
                  icon("book", class = "fas", style = "margin-right: 6px;"),
                  "Technical Guide"
                ),
                tags$a(
                  href = "https://www.worldbank.org",
                  target = "_blank",
                  class = "btn btn-outline-primary btn-sm",
                  icon("file-alt", class = "fas", style = "margin-right: 6px;"),
                  "Companion Notes"
                )
              )
            ),
            
            # Objectives & Approach
            div(
              class = "about-section",
              style = "margin-bottom: 30px;",
              h3(
                icon("bullseye", class = "fas", style = "margin-right: 8px; color: #667eea;"),
                "Objectives & Approach",
                style = "color: #333; margin-bottom: 15px; font-size: 20px;"
              ),
              p(
                "LENS provides a way of interacting with a curated catalog of indicators compiled and developed by CGAP that are relevant for measuring and describing different aspects of the financial system and for supporting common goals of regulatory decision-making, including those relating to financial inclusion, consumer protection, safety, stability and competition. The selection of indicators is informed both by their general relevance to these different dimensions of the financial system as well as their potential relevance for understanding the role of gender within those dimensions. The indicators in LENS, including their definitions and classifications to mandates,  do not reflect a global consensus but rather constitute an attempt at providing working definitions and classification options that each user should adapt to their specific country context and institutional goals.",
                style = "line-height: 1.6; color: #555;"
              )
            ),
            
            # Sources
            div(
              class = "about-section",
              style = "margin-bottom: 30px;",
              h3(
                icon("database", class = "fas", style = "margin-right: 8px; color: #667eea;"),
                "Sources",
                style = "color: #333; margin-bottom: 15px; font-size: 20px;"
              ),
              p(
                "To build LENS, the CGAP team examined a variety of sources, including work from prior CGAP projects and existing indicators from international organizations focused on financial inclusion and regulation.",
                style = "line-height: 1.6; color: #555; margin-bottom: 15px;"
              ),
              div(
                style = "display: flex; gap: 10px; flex-wrap: wrap;",
                tags$a(
                  href = "https://data.imf.org/en/datasets/IMF.STA:FAS",
                  target = "_blank",
                  class = "btn btn-outline-secondary btn-sm",
                  "IMF FAS"
                ),
                tags$a(
                  href = "https://www.gpfi.org",
                  target = "_blank",
                  class = "btn btn-outline-secondary btn-sm",
                  "GPFI"
                ),
                tags$a(
                  href = "https://www.afi-global.org",
                  target = "_blank",
                  class = "btn btn-outline-secondary btn-sm",
                  "AFI"
                ),
                tags$a(
                  href = "https://www.we-fi.org/we-finance-code/#home",
                  target = "_blank",
                  class = "btn btn-outline-secondary btn-sm",
                  "WE Finance Code"
                )
              )
            ),
            
            # Team & Contact
            div(
              class = "about-section",
              style = paste0(
                "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); ",
                "padding: 20px; ",
                "border-radius: 12px; ",
                "margin-bottom: 20px;"
              ),
              h3(
                icon("users", class = "fas", style = "margin-right: 8px; color: #667eea;"),
                "About CGAP",
                style = "color: #333; margin-bottom: 15px; font-size: 20px;"
              ),
              p(
                "CGAP is a global partnership of more than 40 leading development organizations that works to advance the lives of people living in poverty, especially women, through financial inclusion. CGAP works at the frontier of inclusive finance to test solutions, spark innovation, generate evidence, and share insights. Our knowledge enables public and private stakeholders to scale solutions that make financial ecosystems meet the needs of poor, vulnerable, and underserved people and of micro and small enterprises (MSEs), including through advancing women’s economic empowerment.",
                style = "line-height: 1.6; color: #555; margin-bottom: 15px;"
              ),
              div(
                style = "display: flex; gap: 15px; align-items: center; flex-wrap: wrap;",
                tags$a(
                  href = "https://www.cgap.org",
                  target = "_blank",
                  class = "btn btn-primary btn-sm",
                  icon("globe", class = "fas", style = "margin-right: 6px;"),
                  "Visit CGAP.org"
                )
              )
            )
          )
        )
      )
    )
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)