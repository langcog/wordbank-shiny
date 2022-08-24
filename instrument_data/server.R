####################### FULL CHILD-BY-WORD DATA ######################
# LOAD DATA

start_language <- "English (American)"
start_form <- "WS"

instruments <- get_instruments()
languages <- sort(unique(instruments$language))

# ----------------------- MAIN SHINY SERVER  ----------------------- 
function(input, output, session) {

  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)

  # -------------------- SELECTORS FOR DATA
  output$language_selector <- renderUI({
    selectizeInput("language", label = h4("Language"),
                   choices = languages, selected = start_language)
  })
  
  output$form_selector <- renderUI({
    req(input$language)
    
    forms <- filter(instruments, 
                    language == input$language) |>
      pull(form)
    
    selectizeInput("form", label = h4("Form"),
                   choices = forms, selected = start_form)
  })

  # -------------------- GET DATA
  data <- eventReactive(input$get_data, {
    req(input$language, 
        input$form)

    get_instrument_data(language = input$language, 
                        form = input$form,
                        item_info = TRUE,
                        administration_info = TRUE) |>
      select(data_id, item_kind, category, item_id, item_definition,
             english_gloss, uni_lemma, child_id, age, value) |>
             # caregiver_education, sex) # include demographics?
      arrange(data_id)
  })

  output$table <- DT::renderDataTable(
    data(), server = TRUE, filter = "top", style = "bootstrap", 
    rownames = FALSE, selection = "none",
    options = list(orderClasses = TRUE, processing = TRUE, pageLength = 25)
  )

  # -------------------- DOWNLOADS ETC 
  output$download_button <- renderUI({
    req(data())
    downloadButton("download_data", "Download data", class = "btn-xs")
  })
  
  output$download_data <- downloadHandler(
    filename = function() "wordbank_instrument_data.csv",
    content = function(file) {
      df <- data() |>
        mutate(downloaded = lubridate::today(), .before = everything())
      message(nrow(df))
      write.csv(df, file, row.names = FALSE)
    })
  
  output$loading <- renderImage(list(src = "../images/loading.gif",
                                     contentType = "image/gif",
                                     alt = "Loading"),
                                deleteFile = FALSE)
  output$loaded <- reactive(1)
}
