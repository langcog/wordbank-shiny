library(shiny)
#library(dplyr)
#library(DT)
#library(wordbankr)
#mode <- "local"
source(here("common.R"))
# input <- list(language = "English (American)", form = "WS", age = c(18, 30))

function(input, output, session) {

  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)

  instruments <- get_instruments(mode = mode, db_args = db_args)
  languages <- sort(unique(instruments$language))
  
  start_language <- "English (American)"
  start_form <- "WS"

  data <- eventReactive(input$get_data, {

    req(input$language, 
        input$form)

    get_instrument_data(language = input$language, 
                        form = input$form,
                        item_info = TRUE,
                        administration_info = TRUE,
                        mode = mode, 
                        db_args = db_args) %>% 
      left_join(get_administration_data(language = input$language, 
                                        form = input$form, 
                                        include_demographic_info = TRUE,
                                        mode = mode, 
                                        db_args = db_args)) %>%
      select(data_id, age, caregiver_education, sex, value, item_id, category,
             item_definition) %>% # also had 'type'?
      arrange(data_id)
  })

  output$table <- DT::renderDataTable(
    data(), server = TRUE, filter = "top", style = "bootstrap", 
    rownames = FALSE, selection = "multiple",
    options = list(orderClasses = TRUE, processing = TRUE, pageLength = 25)
  )

  output$language_selector <- renderUI({
    selectizeInput("language", label = h4("Language"),
                   choices = languages, selected = start_language)
  })

  forms <- reactive({
    req(input$language)
    valid_form <- function(form) {
      form %in% unique(filter(instruments,
                              language == input$language)$form)
    }
    Filter(valid_form, list("Words & Sentences" = "WS",
                            "Words & Gestures" = "WG",
                            "Oxford CDI" = "Oxford CDI"))
  })

  output$form_selector <- renderUI({
    selectizeInput("form", label = h4("Form"),
                   choices = forms(), selected = start_form)
  })

  output$download_button <- renderUI({
    if (!is.null(data())) {
      downloadButton("download_data", "Download Data",
                     class = "btn-xs")
    }
  })

  output$download_data <- downloadHandler(
    filename = function() "instrument_data.csv",
    content = function(file) {
      cat(nrow(data()))
      write.csv(data(), file, row.names = FALSE)
    })

  output$loading <- renderImage(list(src = "../images/loading.gif",
                                     contentType = "image/gif",
                                     alt = "Loading"),
                                deleteFile = FALSE)

  output$loaded <- reactive(1)

}
