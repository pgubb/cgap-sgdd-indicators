
library(shiny)
library(tidyverse)
library(readxl)
library(bslib)
library(janitor)
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
    indicatorCardJS()
  ),
  
  sidebar = sidebar(
    width = 400,
    p(style = "font-size: 14px;", "CGAP's LENS presents a curated catalog of indicators for use with regulatory data, enabling financial sector authorities and stakeholders to better understand who is using financial services, under what conditions, and with what outcomes. The tool supports segmented analysis by key sociodemographic traits—with a particular focus on gender—as well as by provider, channel, and product type, uncovering insights that can inform more inclusive, evidence-based financial policies, supervision, and market strategies."),
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
      presets_foundation = input$`filters-presets_foundation`,
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
    filtered_ids <- isolate(filtered_indicators()$indicator_id)
    
    if (length(filtered_ids) > 0) {
      for (id in filtered_ids) {
        set_manager$add_to_active(id)
      }
      
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
    filtered_ids <- isolate(filtered_indicators()$indicator_id)
    current_selected <- isolate(set_manager$get_active_indicators())
    to_remove <- intersect(filtered_ids, current_selected)
    
    if (length(to_remove) > 0) {
      for (id in to_remove) {
        set_manager$remove_from_active(id)
      }
      
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
            
            # User Guide Download - Prominent Card
            div(
              class = "about-section",
              style = paste0(
                "margin-bottom: 30px; ",
                "background: linear-gradient(135deg, #1A5A80 0%, #2980b9 100%); ",
                "border-radius: 12px; ",
                "padding: 24px; ",
                "color: white; ",
                "position: relative; ",
                "overflow: hidden;"
              ),
              # Decorative background element
              div(
                style = paste0(
                  "position: absolute; ",
                  "top: -30px; ",
                  "right: -30px; ",
                  "width: 120px; ",
                  "height: 120px; ",
                  "background: rgba(255,255,255,0.1); ",
                  "border-radius: 50%;"
                )
              ),
              div(
                style = "position: relative; z-index: 1;",
                div(
                  style = "display: flex; align-items: flex-start; gap: 20px; flex-wrap: wrap;",
                  # Icon container
                  div(
                    style = paste0(
                      "background: rgba(255,255,255,0.2); ",
                      "border-radius: 12px; ",
                      "padding: 16px; ",
                      "display: flex; ",
                      "align-items: center; ",
                      "justify-content: center;"
                    ),
                    icon("file-pdf", class = "fas", style = "font-size: 32px; color: white;")
                  ),
                  # Text and button container
                  div(
                    style = "flex: 1; min-width: 250px;",
                    h4(
                      "LENS User Guide",
                      style = "margin: 0 0 8px 0; font-size: 20px; font-weight: 600;"
                    ),
                    p(
                      "New to LENS? Download our comprehensive user guide to learn how to navigate the indicator catalog, create custom indicator sets, and generate reports for your analysis.",
                      style = "margin: 0 0 16px 0; font-size: 14px; opacity: 0.9; line-height: 1.5;"
                    ),
                    tags$a(
                      href = "CGAP LENS User Guide.pdf",
                      download = "CGAP LENS User Guide.pdf",
                      style = paste0(
                        "display: inline-flex; ",
                        "align-items: center; ",
                        "gap: 8px; ",
                        "background: white; ",
                        "color: #1A5A80; ",
                        "text-decoration: none; ",
                        "font-size: 14px; ",
                        "font-weight: 600; ",
                        "padding: 12px 24px; ",
                        "border-radius: 8px; ",
                        "transition: all 0.2s ease; ",
                        "box-shadow: 0 4px 12px rgba(0,0,0,0.15);"
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
              h3(
                icon("graduation-cap", class = "fas", style = "margin-right: 8px; color: #667eea;"),
                "Context & Additional Resources",
                style = "color: #333; margin-bottom: 15px; font-size: 20px;"
              ),
              p(
                "LENS is one component of CGAP's project to support the mainstreaming of regulatory gender-disaggregated data (RGDD) in the financial sector and should be consulted jointly with 'Using Disaggregated Data to Improve Policy, Regulation and Supervision: A Technical Guide for Financial Sector Authorities' and accompanying pieces",
                style = "line-height: 1.6; color: #555; margin-bottom: 15px;"
              ),
              p(
                "This Guidance package describes RGDD use cases, provides recommendations for Financial Sector Authorities on how to leverage regulatory reporting regimes to maximize the use of gender-disaggregated data for multiple FSA mandates and to support the goals of other actors (including funders and financial firms).",
                style = "line-height: 1.6; color: #555; margin-bottom: 15px;"
              ),
              div(
                style = "display: flex; gap: 10px; flex-wrap: wrap;",
                tags$a(
                  href = "",
                  target = "_blank",
                  class = "btn btn-outline-primary btn-sm",
                  icon("book", class = "fas", style = "margin-right: 6px;"),
                  "Technical Guide (Forthcoming 2026/Q3)"
                )
              )
            ),
            
            # Tutorial 
            div(
              tags$div(
                class = "video-container",
                tags$iframe(
                  src = "https://www.loom.com/embed/30cd180dc6bb4c31b3ff78a0c85effe3",
                  width = "100%",
                  height = "480",
                  frameborder = "0",
                  webkitallowfullscreen = TRUE,
                  mozallowfullscreen = TRUE,
                  allowfullscreen = TRUE
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
                "LENS provides a way of interacting with a curated catalog of indicators compiled and developed by CGAP that are relevant for measuring and describing different aspects of the financial system and for supporting common goals of regulatory decision-making, including those relating to financial inclusion, consumer protection, stability and soundness, market development and sustainability. The selection of indicators is informed both by their general relevance to these different mandates and goals of financial sector authorities as well as their potential relevance for understanding the role of key sociodemographic traits, such as gender, within those dimensions. The indicators in LENS, including their definitions, assigned mandates/objectives and proposed breakdowns, do not reflect a global consensus but rather constitute an attempt at providing working definitions, classification options and analytical questions that each user should adapt to their specific country context and institutional goals.",
                style = "line-height: 1.6; color: #555;"
              )
            ),
            
            # Sources
            div(
              class = "about-section",
              style = "margin-bottom: 30px;",
              h3(
                icon("database", class = "fas", style = "margin-right: 8px; color: #667eea;"),
                "References and sources",
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
                ), 
                tags$a(
                  href = "https://www.cgap.org/topics/collections/fema-meter",
                  target = "_blank",
                  class = "btn btn-outline-secondary btn-sm",
                  "A2ii FeMa-Meter"
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
