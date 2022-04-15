# ###################### ITEM TRAJECTORIES ######################

library(shiny)
library(here)

source(here("common.R"))
source(here("item_trajectories","helper.R"))


# --------------------- STATE PRELIMINARIES ------------------

admins <- get_administration_data(mode = mode, db_args = db_args, 
                                  filter_age = FALSE, 
                                  include_demographic_info = TRUE, 
                                  include_birth_info = TRUE,
                                  include_language_exposure = TRUE, 
                                  include_health_conditions = TRUE) %>%
  mutate(monolingual = map_lgl(language_exposures, 
                               function(language_exposures) {
                                 is_null(language_exposures) || nrow(language_exposures) == 1
                               }), 
         typically_developing = map_lgl(health_conditions, 
                                        function(health_conditions) {
                                          is_null(health_conditions)
                                        }))

items <- get_item_data(mode = mode, db_args = db_args) 
# |>
#   mutate(item_definition = iconv(item_definition, from = "utf8", to = "utf8")) # why?

instruments <- get_instruments(mode = mode, db_args = db_args)
languages <- sort(unique(instruments$language))


instrument_tables <- items %>%
  group_by(language, form) %>%
  filter(item_kind == "word") %>%
  nest()

alerted <- FALSE

# ---------------------- BEGIN SHINY SERVER ------------------
function(input, output, session) {

  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)
  
  
  # --------------- UI ELEMENTS
  output$language_selector <- renderUI({
    selectInput("language", label = strong("Language"),
                choices = languages, selected = start_language)
  })
  
  forms <- reactive({
    req(input$language)
    
    filter(instruments,
                    language == input$language) |>
      pull(form) |>
      unique() 
  })
  
  
  output$form_selector <- renderUI({
    req(forms())
    
    selectInput("form", label = strong("Form"),
                multiple = TRUE,
                choices = forms(), selected = start_form)
  })
  
  # note that this should eventually rely on form type? 
  # maybe then we don't need to do the admins stuff
  measures <- reactive({
    req(input$form)
    
    form_admins <- filter(admins, 
           language == input$language,
           form %in% input$form) 
    
    if (!all(is.na(form_admins$comprehension))) {
      c("Understands","Produces") 
    } else {
      "Produces"
    }
    
  })
  
  output$measure_selector <- renderUI({
    req(measures())
    
    selectInput("measure", label = strong("Measure"),
                choices = measures(), selected = start_measure)
  })
  
  
  word_options <- reactive({
    req(input$language)
    req(input$form)
    
    filter(instrument_tables, 
           language == input$language, 
           form %in% input$form) %>%
      unnest(cols = c(data)) %>%
      group_by(item_definition) %>%
      count() %>%
      filter(n == length(input$form)) %>%
      pull(item_definition)
  })

  output$word_selector <- renderUI({
    req(word_options())
    
    selectInput("words", label = strong("Words"),
                choices = word_options(), multiple = TRUE)
  })
    

  # FIXME - NEED NEW FILTERS
  output$data_filter <- renderUI({
    
    possible_filters =  c("cross-sectional only" = "cross_sectional",
                          "normative sample" = "is_norming", 
                          "monolingual" = "monolingual", 
                          "typically developing" = "typically_developing")
    
    # available_filters <- Filter(
    #   function(data_filter) !all(is.na(form_admins()[[data_filter]]) |
    #                                form_admins()[[data_filter]] == FALSE),
    #   possible_filters
    # )
    
    checkboxGroupInput("data_filter", "Choose Data",
                       choices = possible_filters,
                       selected = c("cross_sectional","monolingual","typically_developing"))
    
  })
  
  many_words <- observe({
    word_limit <- 9
    if (length(input$words) >= word_limit & !alerted) {
      createAlert(session, "many_words", "alert",
                  content = HTML(sprintf("For a large number of words, consider using the %s app instead.",
                                         a(href = "http://wordbank.stanford.edu/analyses?name=item_data",
                                           "Item Data"))),
                  style = "warning", dismiss = FALSE)
      alerted <<- TRUE
    }
    if (length(input$words) < word_limit & alerted) {
      closeAlert(session, "alert")
      alerted <<- FALSE
    }
  })
    
  instrument <- reactive({
    req(input$language)
    req(input$form)
    
    filter(instrument_tables, language == input$language,
           form %in% input$form)
  })
  
  # ---------------------- DATA LOADING
  
  form_admins <- reactive({
    req(input$language)
    req(input$form)
    
    form_specific_admins <- admins |>
      filter(language == input$language, 
             form %in% input$form, 
             monolingual == input$monolingual,
             cross_sectional == !input$longitudinal,
             is_norming == input$is_norming, 
             typically_developing == input$typically_developing)
  })

  filtered_admins <- reactive({
    # req(form_admins())
    # req(input_cross_sectional())
    # req(input_norming())

    filtered_admins <- form_admins()

    # if (input_cross_sectional())
    #   filtered_admins <- filter(filtered_admins, cross_sectional == TRUE)
    # 
    # if (input_norming())
    #   filtered_admins <- filter(filtered_admins, norming == TRUE)

    filtered_admins
  })

  trajectory_data <- reactive({
    if (all(input$words %in% word_options())) {
      trajectory_data_fun(filtered_admins(), instrument(), input$measure, input$words) |>
        mutate(item = factor(item, levels = input$words))
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
    if (input$measure == "understands") "Proportion of Children Understanding"
    else if (input$measure == "produces") "Proportion of Children Producing"
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
      expand.grid(age = age_min():age_max(), form = input$form) |>
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
