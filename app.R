library(shiny)
library(tidyverse)
library(readxl)
library(bslib)
library(kableExtra)
library(janitor)

# Preparing and loading data
load("data/indicators.RData")

# Globals
VISION <- "A stable, resilient, safe, competitive, efficient, inclusive and responsible financial system 
to support an equitable, just and sustainable/green economy that promotes prosperity and climate resilience for all, 
irrespective of their gender, income level and other personal conditions"

SECTOR_COLORS <- list(
    "Payments" = "#FFD700",
    "Credit" = "#FFA07A",
    "Insurance" = "#98FB98",
    "Investments" = "#87CEFA",
    "Pensions" = "#DDA0DD",
    "Several/Others" = "#FFC0CB",
    "Savings" = "#B0C4DE"
)

USE_CASES <- c("Access to financial services", 
               "Usage of financial services", 
               "Quality of financial services", 
               "Outcomes of the use of financial services", 
               "Public dissemination of information and statistics", 
               "Management and mitigation of consumer risks and foster competition", 
               "Management and mitigation of prudential and stability risks", 
               "Develop capital markets", 
               "Building a sustainable and equitable financial sector", 
               "Efficiency and effectiveness of currency management")

COLUMN_DESCRIPTIONS <- c(
    "indicator_id"= "Unique indicator id",
    "indicator_name" = "Name of the indicator",
    "indicator_description" = "Description of the indicator", 
    "unit_of_analysis" = "Unit of analysis (Individual, SMEs, ", 
    "measurement_type" = "Measurement type",       
    "gender_questions" = "Exploratory questions that can help guide analysis by gender",         
    "formula1_volume" = "Formula for volume indicator",        
    "formula2_value" = "Formula for value indicator",            
    "formula_3_other" = "Formulas used when there is more than one volume or value indicator, or other measurement types.",          
    "main_mandate" = "Main mandate",           
    "secondary_mandates"  = "Other applicable mandates",      
    "main_objectives"  = "Main objectives",         
    "main_sector" = "Main sector",               
    "secondary_sectors" = "Other applicable sectors",      
    "use_cases" = "Applicable use cases",             
    "gender_priority" = "Indicators' prioritization by gender relevance", 
    "costs_regsup" = "Estimated cost of collecting disaggregated data (based on example from National Bank of Rwanda)",             
    "gender_priority_adjusted" =  "Indicator's gender prioritization adjusted by collection feasibility", 
    "in_imf" = "Indicator is also part of IMF-FAS",                  
    "in_gpfi" = "Indicator is also part of GPFI",                  
    "in_afi" = "Indicator is also part of AFI", 
    "in_wef" = "Indicator is also part of WeF",                  
    "references" = "References",               
    "essential_disagg" = "Suggested breakdowns for analysis (essential)",      
    "nonessential_disagg" = "Suggested breakdowns for analysis (useful, but not essential)"
)

# Helpers --------
conditional <- function(condition, success) {
    if (condition) success else TRUE
}

slugify <- function(text) {
    text %>%
        tolower() %>%
        gsub("[^a-z0-9]+", "-", .) %>%
        gsub("-$", "", .) %>%
        gsub("^-", "", .)
}

render_disagg_table <- function(essential, nonessential) {
    essentials <- if (!is.na(essential)) unlist(strsplit(essential, "; ")) else character()
    nonessentials <- if (!is.na(nonessential)) unlist(strsplit(nonessential, "; ")) else character()
    max_len <- max(length(essentials), length(nonessentials))
    essentials <- c(essentials, rep("", max_len - length(essentials)))
    nonessentials <- c(nonessentials, rep("", max_len - length(nonessentials)))
    
    tags$table(
        style = "width: 100%; border-collapse: collapse; margin-top: 10px;",
        tags$thead(
            tags$tr(
                tags$th("Key segments", style = "border: 1px solid #ccc; padding: 4px; background-color: #f2f2f2;"),
                tags$th("Other segments", style = "border: 1px solid #ccc; padding: 4px; background-color: #f2f2f2;")
            )
        ),
        tags$tbody(
            lapply(seq_along(essentials), function(i) {
                tags$tr(
                    tags$td(essentials[i], style = "border: 1px solid #ccc; padding: 4px;"),
                    tags$td(nonessentials[i], style = "border: 1px solid #ccc; padding: 4px;")
                )
            })
        )
    )
}

# UI ----------
gui <- page_navbar(
    title = span(span(style = "font-size: 26px;", "RGDD explorer"), 
                 span(style = "font-size: 14px;", " by "), 
                 tags$img(src = "cgap_logo.png", height = "26px")),
    
    tags$head(
        tags$style(HTML(
            paste0(
                "
                html { scroll-behavior: smooth; }
                .btn-circle {
                    border: none;
                    background: none;
                    padding: 0;
                  }
                .selectize-dropdown-content .option {
          display: flex;
          align-items: center;
          padding: 5px;
          border-radius: 3px;
          color: black;
        }",
                paste(
                    sapply(names(SECTOR_COLORS), function(sector) {
                        paste0(
                            ".selectize-control.multi .selectize-input .item[data-value=\"", sector, "\"] {
                background-color: ", SECTOR_COLORS[[sector]], ";
              }"
                        )
                    }),
                    collapse = "\n"
                )
            )
        ))
    ),
    
    sidebar = sidebar(
        width = 400,
        "Gender disagreggated regulatory data (RGDD) explorer is a catalog of indicators developed by CGAP to support efforts by financial sector authorities and others to use regulatory data to better understand the role of gender in the financial system.",
        
        textInput("search_indicator", "Search", placeholder = "Search by indicator name, sector, use-case..."), 
        
        accordion(
            open = c("Mandates"),
            
            accordion_panel(
                "Mandates", icon = icon("scroll"),
                uiOutput("mandate_links")
            ), 
            accordion_panel(
                style = "font-size: 14px",
                "Filters", icon = icon("filter"),
                
                selectizeInput("sector_select", "Sector", choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
                selectizeInput("gender_priority_select", "Gender Priority", choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
                #selectizeInput("use_cases_select", "Use case", choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
                br(),
                actionButton("reset_filters", "Reset all filters", icon = icon("undo"), class = "btn  btn-sm btn-primary")
            )
        )
    ),
    
    nav_spacer(),
    
    nav_panel(
        title = "Browse indicators",
        uiOutput("navigation_helper"),
        uiOutput("key"),
        uiOutput("indicator_cards")
    ),
    
    nav_panel(
        title = uiOutput("selected_tab_title"),
        div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
            uiOutput("selected_count_badge"),
            downloadButton("download_selected", "Download Selected Indicators", class = "btn btn-success")
        ),
        uiOutput("selected_indicators")
    ),
    
    # nav_panel(
    #     title = "About",
    #     "The table below explains all metadata fields available.",
    #     uiOutput("instructions_table")
    # )
)

# SERVER ----------
server <- function(input, output, session) {
    
    # Reactive value to store selected indicators
    selected_indicators <- reactiveVal(data.frame(
        indicator_name = character(),
        indicator_description = character(),
        main_mandate = character(),
        main_objectives = character(),
        main_sector = character(),
        gender_priority = character(),
        stringsAsFactors = FALSE
    ))
    
    
    # Initialize filters ---------
    observe({
 
        updateSelectizeInput(session, "sector_select",
                             choices = sort(unique(indicators$main_sector)),
                             selected = unique(indicators$main_sector))
        
        updateSelectizeInput(session, "gender_priority_select",
                             choices = sort(unique(indicators$gender_priority)),
                             selected = unique(indicators$gender_priority))
        
        # updateSelectizeInput(session, "use_cases_select",
        #                      choices = USE_CASES,
        #                      selected = USE_CASES)
        
    })
    
    # Reset filters ----------
    observeEvent(input$reset_filters, {
        
        updateSelectizeInput(session, "sector_select",
                             choices = sort(unique(indicators$main_sector)),
                             selected = unique(indicators$main_sector))
        
        updateSelectizeInput(session, "gender_priority_select",
                             choices = sort(unique(indicators$gender_priority)),
                             selected = unique(indicators$gender_priority))
        
        # updateSelectizeInput(session, "use_cases_select",
        #                      choices = USE_CASES,
        #                      selected = USE_CASES)
        
        updateTextInput(session, "search_indicator", value = "")
    })
    
    # Filtered indicators ----------
    filtered_indicators_add <- reactive({
        indicators %>%
            arrange(main_mandate, main_objectives, main_sector) %>%
            filter(
                main_sector %in% input$sector_select,
                gender_priority %in% input$gender_priority_select
            ) %>%
            {
                if (!is.null(input$search_indicator) && input$search_indicator != "") {
                    filter(., str_detect(indicator_name, regex(input$search_indicator, ignore_case = TRUE)) |
                               str_detect(indicator_description, regex(input$search_indicator, ignore_case = TRUE)) | 
                               str_detect(gender_questions, regex(input$search_indicator, ignore_case = TRUE)) | 
                               str_detect(main_sector, regex(input$search_indicator, ignore_case = TRUE)) |
                               str_detect(main_mandate, regex(input$search_indicator, ignore_case = TRUE)) |
                               str_detect(main_objectives, regex(input$search_indicator, ignore_case = TRUE)))
                } else {
                    .
                }
            }
    })
    
    # Navigation helper -----
    output$navigation_helper <- renderUI({
        n <- nrow(filtered_indicators_add())
        N <- nrow(indicators)
        
        div(
            style = "font-size: 15px;",
            span(icon("list", lib = "font-awesome"), 
                 strong(paste(n, " of ", N)), "indicators")
        )
    })
    
    # Key ----- 
    output$key <- renderUI({
        span(
            #style = paste0("background-color: ", sector_color, "; display: flex; align-items: center; padding:5px; border-radius: 4px; color:black; font-weight: bold;"),
            span("Indicator name", style = paste0("background-color: white; display: inline-block; align-items: center; padding:2px; border-radius: 4px; color:black; font-weight: bold; font-size: 14px")), 
            span("Main objective(s)", style = paste0("background-color: #E5E7E6; display: inline-block; align-items: center; padding:2px; border-radius: 4px; color:black; font-weight: normal; font-size: 12px;")), 
            span("Main sector", style = paste0("background-color: #FFD700; display: inline-block; align-items: center; padding:2px; border-radius: 4px; color:black; font-weight: normal; font-size: 12px"))
        )
    })
    
    # Jump-links (right panel) -------
    output$mandate_links <- renderUI({
        indicators_data <- filtered_indicators_add()
        mandates <- unique(indicators_data$main_mandate)
        
        tagList(
            lapply(mandates, function(mandate) {
                tags$a(
                    href = paste0("#mandate_", slugify(mandate)),
                    mandate,
                    style = "display: block;  margin-top: 5px; margin-bottom: 5px; font-size: 14px;"
                )
            })
        )
    })
    
    # Indicator cards grouped by mandate -------
    output$indicator_cards <- renderUI({
        indicators_data <- filtered_indicators_add()
        
        if (nrow(indicators_data) == 0) {
            return(h4("No indicators available."))
        }
        
        mandates <- unique(indicators_data$main_mandate)
        
        mandate_sections <- lapply(mandates, function(mandate) {
            
            indicators_in_mandate <- indicators_data %>% filter(main_mandate == mandate)
            
            indicator_cards <- lapply(1:nrow(indicators_in_mandate), function(i) {
                ind <- indicators_in_mandate[i, ]
                sector_color <- SECTOR_COLORS[[ind$main_sector]]
                indicator_id <- ind$indicator_id
                
                img_tags <- div(
                    style = "display: flex; justify-content: flex-start; gap: 10px; align-items: center; margin-top: 10px;",
                    if (ind$in_imf == 1) span("IMF FAS", tags$img(src = "imf_logo.png", height = "44px")),
                    if (ind$in_gpfi == 1)  tags$img(src = "gpfi_logo.png", height = "44px"),
                    if (ind$in_afi == 1)  tags$img(src = "afi_logo.png", height = "44px")
                )
                
                accordion_panel(
                    value = ind$indicator_name,
                    title = div(
                        span(
                        #style = paste0("background-color: ", sector_color, "; display: flex; align-items: center; padding:5px; border-radius: 4px; color:black; font-weight: bold;"),
                        span(ind$indicator_name, style = paste0("background-color: white; display: inline-block; align-items: center; padding:2px; border-radius: 4px; color:black; font-weight: bold; font-size: 14px")), 
                        span(ind$main_objectives, style = paste0("background-color: #E5E7E6; display: inline-block; align-items: center; padding:2px; border-radius: 4px; color:black; font-weight: normal; font-size: 12px;")), 
                        span(ind$main_sector, style = paste0("background-color: ", sector_color, "; display: inline-block; align-items: center; padding:2px; border-radius: 4px; color:black; font-weight: normal; font-size: 12px"))
                        )
                    ),
                    div(
                    div(
                        class = "card-body",
                        style = "font-size: 13px; padding-top:10px;",
                        p(strong("Description:"), ind$indicator_description),
                        p(strong("Unit of analysis:"), ind$unit_of_analysis),
                        p(strong("Measurement type:"), ind$measurement_type),
                        p(strong("Formula (volume):"), ind$formula1_volume), 
                        p(strong("Formula (value):"), ind$formula2_value), 
                        p(strong("Main sector:"), ind$main_sector),
                        p(strong("Other applicable sectors:"), ind$secondary_sectors),
                        p(strong("Main mandate:"), ind$main_mandate),
                        p(strong("Other applicable mandates:"), ind$secondary_mandates),
                        p(strong("Main objectives:"), ind$main_objectives),
                        p(strong("Applicable use-cases:"), ind$use_cases),
                        p(strong("Gender priority:"), ind$gender_priority), 
                        p(strong("Exploratory questions for gender analysis:"), ind$gender_questions), 
                        render_disagg_table(ind$essential_disagg, ind$nonessential_disagg),
                        br(), 
                        p(strong("References:"), ind$references), 
                        p(strong("Equivalent indicators also in:"), img_tags), 
                    ),
                    div(
                        style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                        actionButton(
                            inputId = paste0("select_", ind$indicator_id),
                            label = "Add indicator to my list",
                            icon = icon("plus-circle", lib = "font-awesome"),
                            class = "btn btn-sm btn-outline-primary"
                        )
                    )
                    )
                )
            })
            
            tagList(
                tags$div(
                    id = paste0("mandate_", slugify(mandate)),
                    h5(str_to_upper(mandate), style = "margin-top: 30px; margin-bottom: 20px; color: #7E868C;"),
                    accordion(!!!indicator_cards, open = FALSE),
                    hr()
                )
            )
        })
        
        div(
            style = "display: flex; align-items: flex-start; gap: 30px;",
            div(
                style = "flex-grow: 2; padding: 10px;",
                do.call(tagList, mandate_sections)
            )
            # div(
            #     style = "flex-grow: 1.5; padding: 10px; border-left: 1px solid #ccc;",
            #     h4("Mandates"), 
            #     uiOutput("mandate_links")
            # )
        )
    })
    
    # Selected indicators -------------
    
    # Track selected indicator codes
    selected_codes <- reactiveVal(character())
    
    observe({
        lapply(unique(indicators$indicator_id), function(code) {
            observeEvent(input[[paste0("select_", code)]], {
                
                selected <- selected_codes()
                
                if (code %in% selected) {
                    # Unselect
                    selected <- setdiff(selected, code)
                    updateActionButton(session, paste0("select_", code),
                                       icon = icon("circle", lib = "font-awesome"))
                } else {
                    # Select
                    selected <- union(selected, code)
                    updateActionButton(session, paste0("select_", code),
                                       icon = icon("check-circle", lib = "font-awesome"))
                }
                
                selected_codes(selected)
                
                # Update the selected_indicators() reactive too
                selected_rows <- indicators %>%
                    filter(indicator_id %in% selected) %>%
                    select(indicator_name, indicator_description, main_mandate, main_objectives, main_sector, gender_priority)
                
                selected_indicators(selected_rows)
            })
        })
    })
    
    # Selected indicators table
    output$selected_indicators <- renderUI({
        selected <- selected_indicators()
        if (nrow(selected) == 0) {
            h4("")
        } else {
            table <- selected %>% arrange(main_mandate, main_objectives, main_sector)
            
            names(table) <- str_to_upper(COLUMN_DESCRIPTIONS[names(table)])
                
            kbl(table) %>%
                column_spec(5, bold = F, background = SECTOR_COLORS[table$main_sector]) %>%
                kable_styling("striped", full_width = FALSE) %>%
                row_spec(0, bold = TRUE, color = "#7E868C") %>%
                collapse_rows(1, valign = "middle") %>%
                as.character() %>%
                HTML()
        }
    })
    
    output$selected_count_badge <- renderUI({
        selected <- selected_indicators()
        
        if (nrow(selected) == 0) {
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
    
    output$selected_tab_title <- renderUI({
        selected <- selected_indicators()
        
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
    
    # Download selected indicators
    output$download_selected <- downloadHandler(
        filename = function() {
            paste0("selected_indicators_", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(selected_indicators(), file, row.names = FALSE)
        }
    )
    
    # Instructions table
    output$instructions_table <- renderUI({
        instructions %>% 
            kbl() %>%
            column_spec(1, bold = TRUE) %>%
            kable_styling("striped", full_width = FALSE) %>%
            as.character() %>%
            HTML()
    })
}

# Run the app
shinyApp(ui = gui, server = server)
