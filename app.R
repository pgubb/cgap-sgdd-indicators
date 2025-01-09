library(shiny)
library(tidyverse)
library(readxl)
library(dplyr)
library(reactable)
library(bslib)
library(kableExtra)

# Preparing and loading data
source("R/dataprep.R")

# Globals
VISION <- "A stable, resilient, safe, competitive, efficient, inclusive and responsible financial system 
to support an equitable, just and sustainable/green economy that promotes prosperity and climate resilience for all, 
irrespective of their gender, income level and other personal conditions"

# Map sectors to colors
sector_colors <- list(
    "Payments" = "#FFD700",
    "Credit" = "#FFA07A",
    "Insurance" = "#98FB98",
    "Investments" = "#87CEFA",
    "Pensions" = "#DDA0DD",
    "Several/Others" = "#FFC0CB",
    "Deposits" = "#B0C4DE",
    "None" = "#E0E0E0"
)

# HELPERS ------

conditional <- function(condition, success) {
    if (condition) success else TRUE
}

# UI ----------

gui <- page_navbar(
        title = "SGDD Indicators",
        tags$head(
            tags$style(HTML(
                paste0(
                    ".selectize-dropdown-content .option {
                    display: flex;
                    align-items: center;
                    padding: 5px;
                    border-radius: 3px;
                    color: black;
                }",
                    paste(
                        sapply(names(sector_colors), function(sector) {
                            paste0(
                                ".selectize-control.multi .selectize-input .item[data-value=\"", sector, "\"] {
                                background-color: ", sector_colors[[sector]], ";
                            }"
                            )
                        }),
                        collapse = "\n"
                    )
                )
            ))
        ),
        sidebar = sidebar(
                    width = 500,
                    "The SGDD Indicators database is a collection of supply-side indicators to encourage gender-disaggregated data collection and analysis using supply-side data from across the financial system.",
                    accordion(
                        open = c("1. Select use-case for measurement"),
                        accordion_panel(
                            "1. Select use-case for measurement",
                            strong("Vision for the financial sector"), 
                            p(VISION),  
                            selectInput(
                                inputId = "challenge_select",
                                label = col_codebook["uc_challenge"],
                                choices = setNames(challenges_codebook$uc_sgdd_challenge_code, challenges_codebook$uc_challenge),
                                selected = challenges_codebook$uc_sgdd_challenge_code[1],
                            ),
                            strong(col_codebook["uc_sgdd_opportunity"]),
                            p(textOutput("challenge_opportunity")),
                            selectInput(
                                inputId = "usecase_select",
                                label = col_codebook["uc_sgdd_usecase"],
                                choices = NULL, 
                                multiple = FALSE
                            ), 
                            strong(col_codebook["uc_explanation"]),
                            p(textOutput("uc_explanation")),
                            strong(col_codebook["uc_analysis_needed"]),
                            p(textOutput("uc_analysis_needed")),
                        ), 
                        accordion_panel(
                            "2. Additional filters", 
                            selectizeInput(
                                inputId = "sector_select",
                                label = col_codebook["sectors"],
                                choices = NULL, 
                                multiple = TRUE, 
                                options= list('plugins' = list('remove_button'))
                            ), 
                            selectizeInput(
                                inputId = "genderpriority_select",
                                label = col_codebook["priority_gender"],
                                choices = NULL, 
                                multiple = TRUE, 
                                options= list('plugins' = list('remove_button'))
                            ), 
                            
                        )
                    ), 
                ),
        nav_spacer(), 
        nav_panel(
            title = "Browse indicators",
            uiOutput("navigation_helper"),
            uiOutput("indicator_cards")
        ), 
        nav_panel(
            title = "Your Selected Indicators",
            uiOutput("selected_indicators")
        )
)


server <- function(input, output, session) {
    
    # Reactive value to store selected indicators
    selected_indicators <- reactiveVal(data.frame(
        sectors = character(),
        indicator_name = character(),
        indicator_measurement = character(),
        mandate_main = character(),
        objective_main = character(),
        priority_gender = character(),
        stringsAsFactors = FALSE
    ))
    
    # Controls for page navbar -------
    
    # Reactive to filter challenge data based on selected challenge
    selected_challenge <- reactive({
        challenges_codebook %>% 
            filter(uc_sgdd_challenge_code == input$challenge_select)
    })
    
    # Display the opportunity text for the selected challenge
    output$challenge_opportunity <- renderText({
        challenge <- selected_challenge()
        if (nrow(challenge) > 0) {
            challenge$uc_sgdd_opportunity
        } else {
            "No opportunity description available."
        }
    })
    
    # Update use case choices based on the selected challenge
    observeEvent(input$challenge_select, {
        challenge <- selected_challenge()
        if (nrow(challenge) > 0) {
            usecases_for_challenge <- usecases %>% 
                filter(uc_sgdd_challenge_code == challenge$uc_sgdd_challenge_code)
            updateSelectInput(
                session,
                inputId = "usecase_select",
                choices = setNames(usecases_for_challenge$uc_sgdd_usecase_code, usecases_for_challenge$uc_sgdd_usecase),
                selected = usecases_for_challenge$uc_sgdd_usecase_code[1]
            )
        }
    })
    
    
    # User-selected use case
    selected_usecase <- reactive({ 
        usecases %>% filter(uc_sgdd_usecase_code %in% input$usecase_select)
    })
    
    # Display the explanation text for the selected use case
    output$uc_explanation <- renderText({
        if (nrow(selected_usecase()) > 0) {
            selected_usecase()$uc_explanation
        } else {
            "No explanation available."
        }
    })
    
    output$uc_analysis_needed <- renderText({
        if (nrow(selected_usecase()) > 0) {
            selected_usecase()$uc_analysis_needed
        } else {
            "No analysis available."
        }
    })
    
    
    # Reactive to filter indicators based on selected use case
    filtered_indicators <- reactive({
        indicators %>% 
            filter(uc_sgdd_indicators_mandate_obj %in% c(selected_usecase()$uc_sgdd_indicators_mandate_obj1, selected_usecase()$uc_sgdd_indicators_mandate_obj2, selected_usecase()$uc_sgdd_indicators_mandate_obj3, selected_usecase()$uc_sgdd_indicators_mandate_obj4, selected_usecase()$uc_sgdd_indicators_mandate_obj5))
    })
    
    # Update sector choices based on the selected use case
    observeEvent(input$usecase_select, {
    if (nrow(filtered_indicators()) > 0) {
        sectors_for_usecase <- unique(filtered_indicators()$sectors)
        updateSelectInput(
            session,
            inputId = "sector_select",
            choices = sectors_for_usecase,
            selected = sectors_for_usecase
        )
    }
    })
    
    
    # Update gender priority based on the selected use case
    observeEvent(input$usecase_select, {
        if (nrow(filtered_indicators()) > 0) {
            genderpriority_for_usecase <- unique(filtered_indicators()$priority_gender)
            updateSelectInput(
                session,
                inputId = "genderpriority_select",
                choices = genderpriority_for_usecase,
                selected = genderpriority_for_usecase
            )
        }
    })
    
    # Additional filtered indicators 
    
    filtered_indicators_add <- reactive({
        filtered_indicators() %>% filter(
            conditional(length(input$sector_select) > 0, sectors %in% input$sector_select),
            conditional(length(input$genderpriority_select) > 0, priority_gender %in% input$genderpriority_select),
        )
    })
        
    # Main output -------
    
    # Navigation Helper -------
    output$navigation_helper <- renderUI({
        challenge <- selected_challenge()
        usecase <- selected_usecase()
        n <- nrow(filtered_indicators())
        N <- nrow(indicators)
        if (nrow(challenge) > 0 && nrow(usecase) > 0) {
                HTML(
                    as.character(
                        div(
                            style = "font-size: 15px;",
                                strong("Challenge:"), paste(challenge$uc_challenge_short, ">"),
                                strong("Use case:"), paste(usecase$uc_sgdd_usecase_short, ">"), 
                                paste(n, "out of", N, "indicators available for this use case.")
                        )
                        )
                )
        } else {
            HTML("Selection: Not specified.")
        }
    })
    
    # Generate cards for each indicator
    output$indicator_cards <- renderUI({
        
        indicators_data <- filtered_indicators_add()
        
        if (nrow(indicators_data) == 0) {
            return(h4("No indicators available for the selected use case."))
        }
        
        # Create a grid of cards
        card_list <- lapply(unique(indicators_data$indicator_code), function(i) {
            ind <- indicators_data[indicators_data$indicator_code == i, ]
            sector_color <- sector_colors[[ind$sectors]]
            
            card <- div(
                class = "card border mb-3",
                style = "width: 100%;",
                div(
                    class = "card-header",
                    style = paste0("background-color: ", sector_color, "; display: flex; justify-content: space-between; align-items: center;"),
                    h5(ind$indicator_name, class = "card-title"),
                    actionButton(
                        inputId = i,
                        label = "+",
                        class = "btn btn-sm btn-outline-dark"
                    )
                ),
                div(
                    class = "card-body",
                    style = "font-size: 14px", 
                    p(strong(col_codebook["indicator_measurement"]), ind$indicator_measurement),
                    p(strong(col_codebook["unit_of_analysis"]), ind$unit_of_analysis),
                    p(strong(col_codebook["measurement_type"]), ind$measurement_type),
                    p(strong(col_codebook["sectors"]), ind$sectors),
                    p(strong(col_codebook["mandate_main"]), ind$mandate_main),
                    p(strong(col_codebook["objective_main"]), ind$objective_main), 
                    p(strong(col_codebook["objective_other"]), ind$objective_other), 
                    p(strong(col_codebook["gender_questions"]), ind$gender_questions),
                    p(strong(col_codebook["breakdowns"]), ind$breakdowns), 
                    p(strong(col_codebook["priority_gender"]), ind$priority_gender),
                    p(strong(col_codebook["costs_regsup"]), ind$costs_regsup),
                    p(strong(col_codebook["costs_fsp"]), ind$costs_fsp),
                    p(strong(col_codebook["priority_feasibility"]), ind$priority_feasibility),
                    #p(strong(col_codebook["imf_fas_relevance"]), ind$imf_fas_relevance),
                )
            )
            
            div(style = "flex: 1 1 45%; margin: 15px;", card)
        })
        
        # Wrap cards in a container with two-column layout
        div(
            style = "display: flex; flex-wrap: wrap; justify-content: space-between;",
            do.call(tagList, card_list)
        )
    })
    
    # Observe add button clicks
 # Observe add button clicks
    observe({
        lapply(unique(indicators$indicator_code), function(i) {
            observeEvent(input[[i]], {
                selected <- selected_indicators()
                new_entry <- data.frame(
                    sectors = indicators[indicators$indicator_code == i, "sectors"],
                    indicator_name = indicators[indicators$indicator_code == i, "indicator_name"],
                    indicator_measurement = indicators[indicators$indicator_code == i, "indicator_measurement"],
                    mandate_main = indicators[indicators$indicator_code == i, "mandate_main"],
                    objective_main = indicators[indicators$indicator_code == i, "objective_main"],
                    priority_gender = indicators[indicators$indicator_code == i, "priority_gender"],
                    stringsAsFactors = FALSE
                )
                selected_indicators(bind_rows(selected, new_entry))
            })
        })
    })
    
    # Display selected indicators
    output$selected_indicators <- renderUI({
        selected <- selected_indicators()
        if (nrow(selected) == 0) {
            h4("No indicators selected yet.")
        } else {
            table <- 
                selected %>%
                arrange(sectors)
            
            desc_names <- col_codebook[names(table)]
            names(table) <- desc_names
                
            table %>% 
                kbl() %>%
                column_spec(1, bold = F, background = sector_colors[table$`Sectors covered`]) %>%
                kable_styling("striped", full_width = FALSE) %>%
                row_spec(0, bold = TRUE, color = "black") %>%
                collapse_rows(1, valign = "middle") %>% 
                as.character() %>%
                HTML()
        }
    })
    
} # Close server logic

# Run the app
shinyApp(ui = gui, server = server)
