# ###################### CHILD-LEVEL SUMMARY DATA (ADMINS) ######################
flatten_tibble <- function(tib) {
  if (is.null(tib)) return(as.character(NA))
  map(flatten(tib) |> transpose(),
      \(ls) map2(names(ls), ls, \(n, l) paste(n, l, sep = ": ")) |> paste(collapse = ", ")) |>
    paste(collapse = "; ")
}

# LOAD DATA
admins <- get_administration_data(include_demographic_info = TRUE, 
                                  include_birth_info = TRUE,
                                  include_language_exposure = TRUE, 
                                  include_health_conditions = TRUE,
                                  db_args = shiny_db_args) %>%
  mutate(monolingual = map_lgl(language_exposures, 
                               \(le) is_null(le) || nrow(le) == 1),
         typically_developing = map_lgl(health_conditions, \(hc) is_null(hc)),
         language_exposures = map_chr(language_exposures, flatten_tibble),
         health_conditions = map_chr(health_conditions, \(hc) paste(hc$health_condition_name, collapse = "; "))) |>
  select(-dataset_origin_name, -form_type, -data_id, -date_of_test) |>
  select(language, form, dataset_name, child_id, age,
         comprehension, production, is_norming, everything())

# ----------------------- MAIN SHINY SERVER  ----------------------- 
function(input, output, session) {

  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)
  
  # -------------------- UI ELEMENTS
  output$language_selector <- renderUI({
    selectizeInput("language", label = "Language",
                   # choices = sort(unique(admins$language)))
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
                   # choices = language_forms)
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
  filtered_data <- reactive({
    req(input$language)
    req(input$form)
    req(input$health_conditions)
    req(input$language_status)
    req(input$age)
    
    filter_data <- admins
    if (input$language != "All") {
      filter_data <- filter_data %>% filter(language == input$language)
    }
    if (input$form != "All") {
      filter_data <- filter_data %>% filter(form == input$form)
    }
    if (input$health_conditions == "Typically-developing") {
      filter_data <- filter_data %>% filter(typically_developing)
    }
    if (input$language_status == "Monolingual") {
      filter_data <- filter_data %>% filter(monolingual)
    }
    
    filter_data %>% filter(age >= input$age[[1]], age <= input$age[[2]])

  })

  output$table <- DT::renderDataTable(
    filtered_data(),
    server = TRUE, style = "bootstrap", rownames = FALSE, selection = "none",
    options = list(orderClasses = TRUE, processing = TRUE, pageLength = 25)
  )
  
  # output$table <- DT::renderDataTable(filtered_data(),
  #                                     options = list(orderClasses = TRUE))

  # --------------------- DOWNLOAD
  output$download_data <- downloadHandler(
    filename = function() "wordbank_administration_data.csv",
    content = function(file) {
      fd <- filtered_data() |>
        mutate(downloaded = lubridate::today(), .before = everything())
      write.csv(fd, file, row.names = FALSE)
    }
  )

  output$loaded <- reactive(1)
}