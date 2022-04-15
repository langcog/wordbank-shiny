# ###################### ITEM-LEVEL SUMMARY DATA ######################
library(shiny)
library(here)

source(here("common.R"))
source(here("item_trajectories","helper.R"))

function(input, output, session) {

  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)

  # -------------------- SELECTORS FOR DATA
  
  instruments <- get_instruments(mode = mode, db_args = db_args)
  
  languages <- sort(unique(instruments$language))

  instrument <- reactive({
    req(input$language)
    req(input$form)
    
    filter(instruments, language == input$language, 
           form == input$form)
  })

 
  output$language_selector <- renderUI({
    selectizeInput("language", label = h4("Language"),
                   choices = languages, selected = start_language)
  })

  forms <- reactive({
    req(input$language)
    
    filter(instruments, 
           language == input$language) %>%
      pull(form)
  })

  output$form_selector <- renderUI({
    req(forms())
    
    selectizeInput("form", label = h4("Form"),
                   choices = forms(), selected = start_form)
  })

  measures <- reactive({
    req(input$form)
    
    if (input$form %in% "WG") {
      list("Produces" = "produces", "Understands" = "understands")
    } else {
      list("Produces" = "produces")
    }
  })
  
  output$measure_selector <- renderUI({
    selectInput("measure", label = h4("Measure"),
                choices = measures(), selected = input$measure)
  })

  age_min <- reactive(instrument()$age_min)
  age_max <- reactive(instrument()$age_max)

  output$age_selector <- renderUI({
    req(instrument())
    
    sliderInput("age", label = h4("Age (Months)"),
                min = age_min(), max = age_max(), step = 1,
                value = c(age_min(), age_max()))
  })

  # -------------------- GET DATA
  data <- eventReactive(input$go,{
    req(input$language)
    req(input$form)
    req(input$measure)
    
    get_instrument_data(language = input$language, 
                        form = input$form,
                        item_info = TRUE,
                        mode = mode, 
                        db_args = db_args) %>%
      filter(item_kind == "word") %>%
      gather(measure, value, produces, understands) %>%
      filter(measure == input$measure,
             age >= input$age[1], age <= input$age[2]) %>%
      group_by(item_id, item_definition, type, category, age) %>%
      summarise(prop = round(sum(value, na.rm = TRUE) / length(value), 2)) %>%
      spread(age, prop)
  })
  
  output$table <- renderDataTable(
    data(), server = TRUE, filter = "top", style = "bootstrap",
    rownames = FALSE, selection = "multiple",
    options = list(orderClasses = TRUE, processing = TRUE, pageLength = 25)
  )
  
  # -------------------- DOWNLOADS ETC 
  output$download_all <- downloadHandler(
    "item_data.csv",
    content <- function(file) {
      write.csv(data(), file, row.names = FALSE)
    })

  output$download_current <- downloadHandler(
    "item_data.csv",
    content <- function(file) {
      s <- as.numeric(input$table_rows_all)
      write.csv(data()[s, , drop = FALSE], file, row.names = FALSE)
    })

  output$download_selected <- downloadHandler(
    "item_data.csv",
    content <- function(file) {
      s <- as.numeric(input$table_rows_selected)
      write.csv(data()[s, , drop = FALSE], file, row.names = FALSE)
    })

  output$loaded <- reactive(1)
}