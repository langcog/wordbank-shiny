# ###################### ITEM-LEVEL SUMMARY DATA ######################

# choose form with limited data for testing
# start_language <- "American Sign Language"
# start_form <- "FormA"
start_language <- "English (American)"
start_form <- "WS"

# LOAD DATA
instruments <- get_instruments(db_args = shiny_db_args)
languages <- sort(unique(instruments$language))

# ----------------------- MAIN SHINY SERVER  ----------------------- 
function(input, output, session) {

  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)

  # -------------------- SELECTORS FOR DATA
  output$language_selector <- renderUI({
    selectInput("language", label = h4("Language"),
                choices = languages, selected = start_language)
  })
 
  output$form_selector <- renderUI({
    req(input$language)
    
    forms <- instruments |> filter(language == input$language) |> pull(form)
  
    selectInput("form", label = h4("Form"),
                choices = forms, selected = start_form)
  })
  
  # instrument
  instrument <- reactive({
    req(input$language)
    req(input$form)
    
    instruments |> 
      filter(language == input$language, 
             form == input$form)
  })
  
  output$measure_selector <- renderUI({
    req(instrument())
    measures <- if (instrument()$form_type == "WS") list("Produces" = "produces")
    else list("Produces" = "produces", "Understands" = "understands")

    selectInput("measure", label = h4("Measure"),
                choices = measures, selected = "")
  })
  
  output$age_selector <- renderUI({
    req(input$language)
    req(input$form)
    
    # defense against selecting instrument with mismatched form
    if (nrow(instrument()) == 0) {
      age_min = 12
      age_max = 36
      } else {
        age_min <- instrument()$age_min
        age_max <- instrument()$age_max
      }

    # print(age_min)

    sliderInput("age", label = h4("Age (Months)"),
                min = age_min, max = age_max, step = 1,
                value = c(age_min, age_max))
  })

  # -------------------- GET DATA
  
  inst_data <- reactiveVal()
  
  inst_data_get <- observe({
    req(input$language)
    req(input$form)
    req(input$measure)
    req(input$age)
    
    new_data <- get_instrument_data(language = input$language, 
                                    form = input$form,
                                    item_info = TRUE,
                                    administration_info = TRUE,
                                    db_args = shiny_db_args) |>
      filter(item_kind == "word") |>
      gather(measure, value, produces, understands) |>
      filter(measure == input$measure,
             age >= input$age[1], age <= input$age[2]) |>
      mutate(item_id = as.numeric(str_remove(item_id, "item_"))) |>
      group_by(item_id, item_definition, category, age) |>
      summarise(prop = round(sum(value, na.rm = TRUE) / length(value), 2)) |>
      spread(age, prop)
    
    inst_data(new_data)
  }) |>
    bindEvent(input$go)
  
  inst_data_clear <- observe({
    inst_data(NULL)
  }) |>
    bindEvent(input$language, input$form, input$measure)
  
  output$table <- DT::renderDataTable(
    inst_data(), server = TRUE, style = "bootstrap", #filter = "top", 
    rownames = FALSE, selection = "none",
    options = list(orderClasses = TRUE, processing = TRUE, pageLength = 25)
  )
  
  # -------------------- DOWNLOADS ETC 
  
  output$download_button <- renderUI({
    req(inst_data())
    downloadButton("download_all", "Download Data", class = "btn-xs")
  })

  output$download_all <- downloadHandler(
    filename = function() "wordbank_item_data.csv",
    content <- function(file) {
      d <- inst_data() |>
        mutate(downloaded = lubridate::today(), .before = everything())
      write.csv(d, file, row.names = FALSE)
    })
  
  output$loading <- renderImage(list(src = "images/loading.gif",
                                     contentType = "image/gif",
                                     alt = "Loading"),
                                deleteFile = FALSE)

  output$loaded <- reactive(1)
}
