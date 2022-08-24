# ###################### ITEM-LEVEL SUMMARY DATA ######################

# choose form with limited data for testing
# start_language <- "American Sign Language"
# start_form <- "FormA"
start_language <- "English (American)"
start_form <- "WS"

# LOAD DATA
instruments <- get_instruments()
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
  
  output$measure_selector <- renderUI({
    req(input$form)
    
    # TODO: use form_type
    if (input$form %in% "WG") {
      measures <- list("Produces" = "produces", "Understands" = "understands")
    } else {
      measures <- list("Produces" = "produces")
    }
    
    selectInput("measure", label = h4("Measure"),
                choices = measures, selected = "")
  })
  
  output$age_selector <- renderUI({
    req(input$language)
    req(input$form)
    
    instrument <- instruments |> 
      filter(language == input$language, 
             form == input$form) 
    
    # defense against selecting instrument with mismatched form
    if (nrow(instrument) == 0) {
      age_min = 12
      age_max = 36
      } else {
        age_min <- instrument$age_min
        age_max <- instrument$age_max
      }

    # print(age_min)

    sliderInput("age", label = h4("Age (Months)"),
                min = age_min, max = age_max, step = 1,
                value = c(age_min, age_max))
  })

  # -------------------- GET DATA
  data <- eventReactive(input$go, {
    req(input$language)
    req(input$form)
    req(input$measure)
    req(input$age)
    
    get_instrument_data(language = input$language, 
                        form = input$form,
                        item_info = TRUE,
                        administration_info = TRUE) |>
      filter(item_kind == "word") |>
      gather(measure, value, produces, understands) |>
      filter(measure == input$measure,
             age >= input$age[1], age <= input$age[2]) |>
      mutate(item_id = as.numeric(str_remove(item_id, "item_"))) |>
      group_by(item_id, item_definition, category, age) |>
      summarise(prop = round(sum(value, na.rm = TRUE) / length(value), 2)) |>
      spread(age, prop)
  })
  
  output$table <- DT::renderDataTable(
    data(), server = TRUE, filter = "top", style = "bootstrap",
    rownames = FALSE, selection = "multiple",
    options = list(orderClasses = TRUE, processing = TRUE, pageLength = 25)
  )
  
  # -------------------- DOWNLOADS ETC 
  
  output$download_button <- renderUI({
    req(data())
    downloadButton("download_all", "Download Data", class = "btn-xs")
  })

  output$download_all <- downloadHandler(
    filename = function() "wordbank_item_data.csv",
    content <- function(file) {
      d <- data() |>
        mutate(downloaded = lubridate::today(), .before = everything())
      write.csv(d, file, row.names = FALSE)
    })
  
  output$loading <- renderImage(list(src = "../images/loading.gif",
                                     contentType = "image/gif",
                                     alt = "Loading"),
                                deleteFile = FALSE)

  output$loaded <- reactive(1)
}
