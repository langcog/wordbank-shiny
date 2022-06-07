# ###################### ITEM TRAJECTORIES ######################
# library(profvis)
library(shiny)
library(here)
library(directlabels)

source(here("common.R"))
source(here("item_trajectories","helper.R"))


# --------------------- STATE PRELIMINARIES ------------------
print("loading data")
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
                                        }))  |>
  group_by(child_id) |>
  arrange(age) |>
  mutate(n = 1:n(), 
         longitudinal = n > 1,
         first_administration = n == 1)
  


items <- get_item_data(mode = mode, db_args = db_args) 
# |>
#   mutate(item_definition = iconv(item_definition, from = "utf8", to = "utf8")) # why?

instruments <- get_instruments(mode = mode, db_args = db_args)
languages <- sort(unique(instruments$language))


instrument_tables <- items  |>
  left_join(instruments) |>
  group_by(language, form, instrument_id) |>
  filter(item_kind == "word")  |>
  nest()

alerted <- FALSE

print("begin shiny app")

# ---------------------- BEGIN SHINY SERVER ------------------
function(input, output, session) {

  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)
  
  
  # --------------- UI ELEMENTS
  output$language_selector <- renderUI({
    print("language_selector")
    
    selectInput("language", label = strong("Language"),
                choices = languages, selected = start_language)
  })
  
  forms <- reactive({
    req(input$language)
    
    print("forms")
    
    filter(instruments,
                    language == input$language) |>
      pull(form) |>
      unique() 
  })
  
  
  output$form_selector <- renderUI({
    req(forms())
    
    print("form selector")
    
    selectInput("form", label = strong("Form"),
                multiple = TRUE,
                choices = forms(), selected = start_form)
  })
  
  # note that this should eventually rely on form type? 
  # maybe then we don't need to do the admins stuff
  measures <- reactive({
    req(input$form)
    req(input$language)
    
    print("measures")
    
    form_admins <- filter(admins, 
                          language == input$language,
                          form %in% input$form) 
    
    print("done with filter")
    
    if (!all(is.na(form_admins$comprehension))) {
      m <- c("understands","produces") 
    } else {
      m <-  "produces"
    }
    
    print("done with measures")
    m
  })
  
  output$measure_selector <- renderUI({
    req(measures())
    
    print("measure selector")
    selectInput("measure", label = strong("Measure"),
                choices = measures(), selected = start_measure)
  })
  
  
  word_options <- reactive({
    req(input$language)
    req(input$form)
    
    print("word options") 
    
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
    
    print("word selector")
    selectInput("words", label = strong("Words"),
                choices = word_options(), multiple = TRUE)
  })
    

  # FIXME - NEED NEW FILTERS
  output$data_filter <- renderUI({
    print("filters")
    
    possible_filters =  c("cross-sectional only" = "first_administration",
                          "normative sample only" = "is_norming", 
                          "monolingual only" = "monolingual", 
                          "typically developing only" = "typically_developing")
    
    # available_filters <- Filter(
    #   function(data_filter) !all(is.na(form_admins()[[data_filter]]) |
    #                                form_admins()[[data_filter]] == FALSE),
    #   possible_filters
    # )
    
    checkboxGroupInput("data_filter", "Choose Data",
                       choices = possible_filters,
                       selected = NULL) #c("first_administration","monolingual","typically_developing"))
    
  })
  
  many_words <- observe({
    req(input$words)
    
    print("checking whether there are too many words")
    
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
    
  filtered_instrument_tables <- reactive({
    req(input$language)
    req(input$form)
    
    print("filtered instrument tables")
    
    filter(instrument_tables, language == input$language,
           form %in% input$form)
  })
  
  # ---------------------- DATA LOADING
  
  filtered_admins <- reactive({
    req(input$language)
    req(input$form)
    # req(input$data_filter)
    
    print("filtered admins")
    
    form_specific_admins <- admins |>
      filter(language == input$language, 
             form %in% input$form)
    # 
    # form_specific_admins <- walk(input$data_filter, function(filter_condition) { 
    #   filter_condition <- enquo(filter_condition)
    #   form_specific_admins %<>% filter(!!filter_condition)
    # })

  })

  trajectory_data <- reactive({
    req(filtered_admins()) 
    req(filtered_instrument_tables()) 
    req(input$words)
    
    print("trajectory data")
    # print(input$words)
    # print(word_options())
    # print(filtered_admins())
    # print(filtered_instrument_tables())
    
    # in case you have changed instruments and your words no longer apply, don't crash
    if (all(input$words %in% word_options())) {
      td <- trajectory_data_fun(filtered_admins(), filtered_instrument_tables(), input$measure, input$words) |>
        mutate(item = factor(item_definition, levels = input$words))
    } else {
      td <- data.frame()
    }
    print(td)
    
    td
  })

  # broken
  # removing form type
  mean_data <- reactive({
    print("mean data")
    
    trajectory_data() |>
      group_by(form, age) |>
      summarise(prop = mean(prop),
                total = sum(total)) |>
      mutate(item = "mean")
  })

  # ------------------------------ PLOTTING OUTPUT
  ylabel <- reactive({
    if (input$measure == "understands") "Proportion of Children Understanding"
    else if (input$measure == "produces") "Proportion of Children Producing"
  })

  age_lims <- reactive({
    req(input$form)
    req(input$language)

    print("age limits")
    
    
    filtered_instruments <- filter(instruments, 
           language == input$language,
           form %in% input$form) 
    
    c(min(filtered_instruments$age_min), 
      max(filtered_instruments$age_max))
  })
  
  trajectory_plot <- function() {
    req(trajectory_data())
    
    print("trajectory plot")
    
    print(trajectory_data())
    
    traj <- trajectory_data()
    if (nrow(traj) == 0) {
      ggplot(traj) +
        geom_point() +
        scale_x_continuous(name = "\nAge (months)",
                           breaks = age_lims()[1]:age_lims()[2],
                           limits = c(age_lims()[1], age_lims()[2] + 3)) +
        scale_y_continuous(name = sprintf("%s\n", ylabel()),
                           limits = c(-0.01, 1),
                           breaks = seq(0, 1, 0.25))
    } else {
      amin <- age_lims()[1]
      amax <- age_lims()[2]
      
      # removed linetype = type from the smoother
      g <- ggplot(traj, aes(x = age, y = prop, colour = item, fill = item, label = item)) +
        # geom_smooth(aes(linetype = type, weight = total), method = "glm",
        #             method.args = list(family = "binomial")) +
        geom_smooth(aes(weight = total), method = "loess",
                    se = FALSE) +
        geom_point(aes(shape = form)) +
        scale_shape_manual(name = "", values = c(20, 1), guide = "none") +
        scale_linetype_discrete(guide = "none") +
        scale_x_continuous(name = "\nAge (months)",
                           breaks = amin:amax,
                           limits = c(amin, amax + 3)) +
        scale_y_continuous(name = sprintf("%s\n", ylabel()),
                           limits = c(-0.01, 1),
                           breaks = seq(0, 1, 0.25)) +
        scale_colour_solarized(guide = "none") +
        scale_fill_solarized(guide = "none") +
        geom_dl(method = list(dl.trans(x = x + 0.3), "last.qp", cex = 1,
                              fontfamily = font))
      if (input$mean) {
        # removed linetype = type
        g +
          geom_smooth(aes(weight = total), method = "loess",
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
    req(trajectory_data())
    req(age_lims())
        
    traj <- trajectory_data()
    if (nrow(traj) == 0) {
      expand.grid(age = age_lims()[1]:age_lims()[1], form = input$form) |>
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
