# ###################### ITEM TRAJECTORIES ######################

library(shiny)
library(here)

source(here("common.R"))
source(here("item_trajectories","helper.R"))


# --------------------- STATE PRELIMINARIES ------------------

admins <- get_administration_data(mode = mode, db_args = db_args, 
                                  filter_age = FALSE, 
                                  include_demographic_info = TRUE) |>
  gather(measure, vocab, comprehension, production) |>
  mutate(identity = "All Data")

items <- get_item_data(mode = mode, db_args = db_args) |>
  mutate(item_definition = iconv(item_definition, from = "utf8", to = "utf8")) # why?

instruments <- get_instruments(mode = mode, db_args = db_args)
languages <- sort(unique(instruments$language))

instrument_tables <- instruments |>
  group_by(instrument_id) |>
  do(words_by_definition = list_items_by_definition(
    filter(items, language == .$language, form == .$form, item_kind == "word")
  ),
  words_by_id = list_items_by_id(
    filter(items, language == .$language, form == .$form, item_kind == "word")
  )) |>
  left_join(instruments)

alerted <- FALSE

# ---------------------- BEGIN SHINY SERVER ------------------
function(input, output, session) {

  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)
  
  output$language_selector <- renderUI({
    selectInput("language", label = strong("Language"),
                choices = languages, selected = start_language)
  })
  
  output$form_selector <- renderUI({
    selectInput("form", label = strong("Form"),
                choices = forms(), selected = start_form)
  })
  
  output$measure_selector <- renderUI({
    selectInput("measure", label = strong("Measure"),
                choices = measures(), selected = start_measure)
  })
# 
#   input_language <- reactive({
#     if (is.null(input$language)) start_language else input$language
#   })

  # input_form <- reactive ({r
  #   a <- if (is.null(input$form)) start_form else input$form
  #   possible_forms <- unique(filter(instrument_tables,
  #                                   language == input$language())$form)
  #   if (a %in% possible_forms | (a == "WG WS" & all(c("WG","WS") %in% possible_forms)))
  #     a else forms()[[1]]
  # })
  # input_form <- reactive ({
  #   a <- if (is.null(input$form)) start_form else input$form
  #   possible_forms <- unique(filter(instrument_tables,
  #                                   language == input_language())$form)
  #   if (a %in% possible_forms | (a == "WG WS" & all(c("WG","WS") %in% possible_forms)))
  #     a else forms()[[1]]
  # })

  input_forms <- reactive(

    if (input$language() != "English (British)") {
      strsplit(input$form, " ")[[1]]}
    else {
      input$form}
  )

  # input_measure <- reactive({
  #   if (is.null(input$measure)) start_measure else input$measure
  # })
# 
#   input_cross_sectional <- reactive({
#     'cross_sectional' %in% input$data_filter
#   })
# 
#   input_norming <- reactive({
#     'norming' %in% input$data_filter
#   })
# 
#   input_words <- reactive({
#     if (is.null(input$words)) word_options()[1] else input$words
#   })

 

  # ---------------------- UI ELEMENTS
  
  word_options <- reactive({
    if (length(input_forms()) == 1) {
      words <- names(filter(instrument_tables,
                            language == input$language,
                            form == input_forms())$words_by_definition[[1]])
    } else {
      words1 <- names(filter(instrument_tables,
                             language == input$language,
                             form == input_forms()[1])$words_by_definition[[1]])
      words2 <- names(filter(instrument_tables,
                             language == input$language,
                             form == input_forms()[2])$words_by_definition[[1]])
      words <- intersect(words1, words2)
    }
  })
  
  observe({
    words <- word_options()
    updateSelectInput(session, "words", choices = words,
                      selected = if (length(words)) words[1] else "")
    #    isolate({
    #         select_words <- input_words()
    #         select_words <- select_words[select_words %in% words]
    #         updateSelectInput(session, "words", choices = words, selected = input_words())
    #    })
  })
  
  forms <- reactive({
    valid_form <- function(form) {
      form %in% unique(filter(instrument_tables,
                              language == input$language)$form)
    }
    form_opts <- Filter(valid_form,
                        list("Words & Sentences" = "WS",
                             "Words & Gestures" = "WG",
                             "FormA" = "FormA",
                             "FormBOne" = "FormBOne",
                             "FormBTwo" = "FormBTwo",
                             "FormC" = "FormC",
                             "TEDS Twos" = "TEDS Twos",
                             "TEDS Threes" = "TEDS Threes",
                             "Toddler Checklist" = "TC",
                             "Infant Checklist" = "IC",
                             "Oxford CDI" = "Oxford CDI"))
    if (all(c("WS", "WG") %in% form_opts)) {
      form_opts$"Both" <- "WG WS"
    }
    form_opts
  })
  
  measures <- reactive({
    if (all(input %in% c("WG","FormA","IC","Oxford CDI"))) {
      list("Produces" = "produces", "Understands" = "understands")
    } else {
      list("Produces" = "produces")
    }
  })
  
 
  
  
  output$data_filter <- renderUI({
    
    possible_filters =  c("cross-sectional only" = "cross_sectional",
                          "normative sample" = "norming")
    
    available_filters <- Filter(
      function(data_filter) !all(is.na(form_admins()[[data_filter]]) |
                                   form_admins()[[data_filter]] == FALSE),
      possible_filters
    )
    
    
    
    checkboxGroupInput("data_filter", "Choose Data",
                       choices = available_filters,
                       selected = "cross_sectional")
    
  })
  
  many_words <- observe({
    word_limit <- 9
    if (length(input_words()) >= word_limit & !alerted) {
      createAlert(session, "many_words", "alert",
                  content = HTML(sprintf("For a large number of words, consider using the %s app instead.",
                                         a(href = "http://wordbank.stanford.edu/analyses?name=item_data",
                                           "Item Data"))),
                  style = "warning", dismiss = FALSE)
      alerted <<- TRUE
    }
    if (length(input_words()) < word_limit & alerted) {
      closeAlert(session, "alert")
      alerted <<- FALSE
    }
  })
  
  instrument <- reactive({
    filter(instrument_tables, language == input$language(),
           form %in% input$forms)
  })
  
  # ---------------------- DATA LOADING
  
  form_admins <- reactive({
    # req(input_language())
    # req(input_forms())
    
    form_specific_admins <- admins |>
      filter(language == input$language)

    if(length(input_forms()) == 1)
      form_specific_admins %<>% filter(form == input_forms())

    #Compute cross-sectional as first entry for a child in a source
    first_longitudinals <- form_specific_admins |>
      group_by(dataset_name, child_id) |>
      arrange(age) |>
      slice(1)

    form_specific_admins |>
      mutate(cross_sectional = data_id %in% first_longitudinals$data_id)
  })

  filtered_admins <- reactive({
    # req(form_admins())
    # req(input_cross_sectional())
    # req(input_norming())

    filtered_admins <- form_admins()

    if (input_cross_sectional())
      filtered_admins <- filter(filtered_admins, cross_sectional == TRUE)

    if (input_norming())
      filtered_admins <- filter(filtered_admins, norming == TRUE)

    filtered_admins
  })

  trajectory_data <- reactive({
    if (all(input_words() %in% word_options())) {
      trajectory_data_fun(filtered_admins(), instrument(), input_measure(), input_words()) |>
        mutate(item = factor(item, levels = input_words()))
    } else {
      data.frame()
    }
  })

  mean_data <- reactive({
    trajectory_data() |>
      group_by(form, type, age) |>
      summarise(prop = mean(prop),
                total = sum(total)) |>
      mutate(item = "mean")
  })

  # ------------------------------ PLOTTING OUTPUT
  ylabel <- reactive({
    if (input_measure() == "understands") "Proportion of Children Understanding"
    else if (input_measure() == "produces") "Proportion of Children Producing"
  })

  age_min <- reactive(min(instrument()$age_min))
  age_max <- reactive(max(instrument()$age_max))

  trajectory_plot <- function() {
    traj <- trajectory_data()
    if (nrow(traj) == 0) {
      ggplot(traj) +
        geom_point() +
        scale_x_continuous(name = "\nAge (months)",
                           breaks = age_min():age_max(),
                           limits = c(age_min(), age_max() + 3)) +
        scale_y_continuous(name = sprintf("%s\n", ylabel()),
                           limits = c(-0.01, 1),
                           breaks = seq(0, 1, 0.25))
    } else {
      amin <- age_min()
      amax <- age_max()
      g <- ggplot(traj, aes(x = age, y = prop, colour = item, fill = item, label = item)) +
        # geom_smooth(aes(linetype = type, weight = total), method = "glm",
        #             method.args = list(family = "binomial")) +
        geom_smooth(aes(linetype = type, weight = total), method = "loess",
                    se = FALSE) +
        geom_point(aes(shape = form)) +
        scale_shape_manual(name = "", values = c(20, 1), guide = FALSE) +
        scale_linetype_discrete(guide = FALSE) +
        scale_x_continuous(name = "\nAge (months)",
                           breaks = amin:amax,
                           limits = c(amin, amax + 3)) +
        scale_y_continuous(name = sprintf("%s\n", ylabel()),
                           limits = c(-0.01, 1),
                           breaks = seq(0, 1, 0.25)) +
        scale_colour_solarized(guide = FALSE) +
        scale_fill_solarized(guide = FALSE) +
        geom_dl(method = list(dl.trans(x = x + 0.3), "last.qp", cex = 1,
                              fontfamily = font))
      if (input$mean) {
        g +
          geom_smooth(aes(linetype = type, weight = total), method = "loess",
                      se = FALSE, colour = "black", data = mean_data()) +
          geom_point(aes(shape = form), colour = "black", data = mean_data())
      } else {
        g
      }
    }
  }
  
  output$trajectory_plot <- renderPlot(trajectory_plot(), height = function() {
    session$clientData$output_trajectory_plot_width * 0.7
  })
  
  
  # ------------------------ TABLE AND OTHER DATA DOWNLOAD

  table_data <- reactive({
    traj <- trajectory_data()
    if (nrow(traj) == 0) {
      expand.grid(age = age_min():age_max(), form = input_forms()) |>
        select(form, age)
    } else {
      traj |>
        select(form, age, item, prop) |>
        spread(item, prop)
    }
  })

  output$table <- renderTable(table_data(),
                              include.rownames = FALSE, digits = 2)

  output$download_table <- downloadHandler(
    filename = function() "item_trajectory_table.csv",
    content = function(file) {
      td <- table_data()
      extra_cols <- data.frame(language = rep(input$language, nrow(td)),
                               measure = rep(input$measure, nrow(td)))
      write.csv(bind_cols(extra_cols, td), file, row.names = FALSE)
    })

  output$download_plot <- downloadHandler(
    filename = function() "item_trajectory.pdf",
    content = function(file) {
      cairo_pdf(file, width = 10, height = 7)
      print(trajectory_plot())
      dev.off()
    })

  output$loaded <- reactive(1)

}
