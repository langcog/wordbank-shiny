# ###################### UNILEMMAS ######################
all_prop_data <- feather::read_feather("all_prop_data.feather")
uni_lemmas <- sort(unique(all_prop_data$uni_lemma))
start_lemma <- "dog"
start_measure <- "produces"
kid_min <- 10
points_min <- 6

input <- list(uni_lemma = "dog", measure = "produces")

shinyServer(function(input, output, session) {

  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)

  updateSelectizeInput(session, "uni_lemma", server = TRUE,
                       choices = setNames(uni_lemmas, toupper(uni_lemmas)),
                       selected = start_lemma,
                       options = list(maxOptions = length(uni_lemmas)))
  
  output$measure_selector <- renderUI({
    selectInput("measure", label = h4("Measure"),
                choices = c("Produces" = "produces", "Understands" = "understands"),
                selected = start_measure)
  })

  uni_lemma_data <- function() {
    req(input$uni_lemma)

    all_prop_data %>%
      group_by(language) %>%
      filter(uni_lemma == input$uni_lemma,
             measure == input$measure) |>
      filter(sum(prop > 0) > points_min, # more than n non-zero points
             sum(n > kid_min) > points_min) |> # more than n points with some data
      ungroup()
  }

  n_languages <- function() {
    n_distinct(uni_lemma_data()$language)
  }

  n_cols <- 5
  hgt <- reactive(ceiling(n_languages() / n_cols) * 125 + 100)
  wdth <- reactive(min(n_languages(), n_cols) * 125 + 100)
  
  crosslinguistic_plot <- function() {
    req(uni_lemma_data()) 
    
    uld <- uni_lemma_data()
    words_data <- uld |> distinct(language, measure, words)
    
    ggplot(uld, aes(x = age, y = prop)) +
      facet_wrap(vars(language), ncol = min(n_languages(), n_cols),
                 labeller = labeller(language = label_wrap_gen(width = 10))) +
      geom_point(aes(colour = language), alpha = 0.7, size = 0.7) +
      geom_smooth(aes(colour = language, weight = n), se = FALSE, size = 1.5, span = 1,
                  method = "glm", method.args = list(family = "binomial")) +
      geom_label(aes(x = 8, y = 1, label = words), data = words_data,
                 label.padding = unit(0.25, "lines"), family = font,
                 vjust = "inward", hjust = "inward") +
      langcog::scale_colour_solarized(guide = "none") +
      langcog::scale_fill_solarized(guide = "none") +
      scale_y_continuous(name = "Proportion of children", limits = c(0, 1)) +
      scale_x_continuous(name = "Age (months)", limits = c(8, 36),
                         breaks = seq(12, 36, 6))
  }

  output$crosslinguistic <- renderPlot(
    crosslinguistic_plot(), height = hgt, width = wdth,
  )
  
  table_data <- reactive({
    uni_lemma_data() |>
      select(language, measure, uni_lemma, words, age, prop) |>
      mutate(prop = round(prop, 2),
             uni_lemma = str_to_upper(uni_lemma))
  })

  output$table <- renderTable(table_data(), include.rownames = FALSE)

  
  output$download_table <- downloadHandler(
    filename = function() "wordbank_crosslinguistic.csv",
    content = function(file) {
      td <- table_data() |>
        mutate(downloaded = lubridate::today(), .before = everything())
      write_csv(td, file)
    })

  output$download_table_button <- renderUI({
    req(table_data())
    downloadButton("download_table", "Download table", class = "btn-default btn-xs")
  })
  
  res <- 300
  output$download_plot <- downloadHandler(
    filename = function() "wordbank_crosslinguistic.png",
    content = function(file) {
      ggsave(file, plot = crosslinguistic_plot(), device = "png", dpi = res,
             height = hgt() * res / 72, width = wdth() * res / 72, units = "px")
    })

  output$download_plot_button <- renderUI({
    req(crosslinguistic_plot())
    downloadButton("download_plot", "Download plot", class = "btn-default btn-xs")
  })
  
  output$loaded <- reactive(1)

})
