# ###################### CHILD-LEVEL SUMMARY DATA (ADMINS) ######################

library(shiny)
library(here)

source(here("common.R"))

# LOAD DATA
admins <- get_administration_data(mode = mode, db_args = db_args, 
                                  include_demographic_info = TRUE, 
                                  include_birth_info = TRUE,
                                  include_language_exposure = TRUE, 
                                  include_health_conditions = TRUE) %>%
  mutate(monolingual = map_lgl(language_exposures, 
                               function(language_exposures) {
                                 is_null(language_exposures) || nrow(language_exposures)==1
                               }), 
         typically_developing = map_lgl(health_conditions, 
                                        function(health_conditions) {
                                          is_null(health_conditions)
                                        }))

# ----------------------- MAIN SHINY SERVER  ----------------------- 
function(input, output, session) {

  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)
  
  # -------------------- UI ELEMENTS
  output$language_selector <- renderUI({
    selectizeInput("language", label = "Language",
                   choices = c("All", sort(unique(admins$language))),
                   selected = "All")
  })
  
  output$form_selector <- renderUI({
    req(input$language)
    
    if (input$language == "All") {
      language_forms <- unique(admins$form)
    } else {
      language_forms <- filter(admins, language == input$language) %>%
        pull(form) %>%
        unique()
    }
    
    selectizeInput("form", label = "Form",
                   choices = c("All", language_forms),
                   selected = "All")
  })
  
  output$age_selector <- renderUI({
    sliderInput("age", label = "Age (Months)",
                min = min(admins$age), max = max(admins$age), step = 1,
                value = c(min(admins$age), max(admins$age)))
  })
  
  output$health_conditions_selector <- renderUI({
    selectizeInput("health_conditions", label = "Health Conditions",
                   choices = c("Typically-developing", "All"),
                   selected = "Typically-developing")
  })
  
  output$language_status_selector <- renderUI({
    selectizeInput("language_status", label = "Language Status",
                   choices = c("Monolingual","All"),
                   selected = "Monolingual")
  })
  
  
  # -------------------- FILTER DATA
  data <- reactive({
    req(input$language)
    req(input$form)
    req(input$health_conditions)
    req(input$language_status)
    req(input$age)
    
    filter_data <- admins
    if (input$language != "All") {
      filter_data %<>% filter(language == input$language)
    }
    if (input$form != "All") {
      filter_data %<>% filter(form == input$form)
    }
    if (input$health_conditions == "Typically-developing") {
      filter_data %<>% filter(typically_developing)
    }
    if (input$language_status == "Monolingual") {
      filter_data %<>% filter(monolingual)
    }
    
    filter_data %>% 
      filter(age >= input$age[[1]], age <= input$age[[2]])
  })

  output$table <- renderDataTable(data(), 
                                  options = list(orderClasses = TRUE))

  # --------------------- DOWNLOAD
  output$download_data <- downloadHandler(
    filename = function() "administration_data.csv",
    content = function(file) write.csv(data(), file, row.names = FALSE)
  )

  output$loaded <- reactive(1)
}