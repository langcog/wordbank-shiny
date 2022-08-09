# ###################### ITEM TRAJECTORIES ######################
source(here("item_trajectories","helper.R"))

# TODO:
# fix color stability
# update copy
# down plot button should only be shown when there's a plot

# --------------------- STATE PRELIMINARIES ------------------
print("loading data")
admins <- get_administration_data(mode = mode, db_args = db_args, 
                                  filter_age = FALSE, 
                                  include_demographic_info = TRUE, 
                                  include_birth_info = TRUE,
                                  include_language_exposure = TRUE, 
                                  include_health_conditions = TRUE) |>
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
         longitudinal = n() > 1, # not used?
         first_administration = n == 1) |>
  ungroup(child_id)


items <- get_item_data(mode = mode, db_args = db_args) 

instruments <- get_instruments(mode = mode, db_args = db_args)
languages <- sort(unique(instruments$language))


instrument_tables <- items  |>
  left_join(instruments) |>
  group_by(language, form, instrument_id) |>
  filter(item_kind == "word")  |>
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
    instruments |>
      filter(language == input$language) |>
      pull(form) |>
      unique() 
  })
  
  
  output$form_selector <- renderUI({
    req(forms())
    selectInput("form", label = strong("Form"),
                multiple = TRUE,
                choices = forms(), selected = start_form)
  })
  
  form_admins <- reactive({
    req(input$form)
    req(input$language)
    form_admins <- admins |>
      filter(language == input$language, form %in% input$form)
  })
  
  # note that this should eventually rely on form type? 
  # maybe then we don't need to do the admins stuff
  measures <- reactive({
    req(form_admins())
    meas <- "produces"
    if (!all(is.na(form_admins()$comprehension))) meas <- c("understands", meas) 
    meas
  })
  
  output$measure_selector <- renderUI({
    req(measures())
    selectInput("measure", label = strong("Measure"),
                choices = measures(), selected = start_measure)
  })
  
  
  word_options <- reactive({
    req(input$language)
    req(input$form)

    instrument_tables |>
      filter(language == input$language, form %in% input$form) |>
      unnest(cols = c(data)) |>
      group_by(item_definition) |>
      count() |>
      filter(n == length(input$form)) |>
      pull(item_definition)
  })

  output$word_selector <- renderUI({
    req(word_options())
    selectInput("words", label = strong("Words"),
                choices = word_options(), multiple = TRUE)
  })
    
  output$data_filter <- renderUI({
    req(form_admins())

    possible_filters =  c("cross-sectional only" = "first_administration",
                          "normative sample only" = "is_norming", 
                          "monolingual only" = "monolingual", 
                          "typically developing only" = "typically_developing")
    
    available_filters <- possible_filters |>
      keep(\(filt) !all(is.na(form_admins()[[filt]]) | !form_admins()[[filt]]))
    
    checkboxGroupInput("data_filter", "Choose Data", choices = possible_filters,
                       selected = c("first_administration", "monolingual",
                                    "typically_developing"))
    
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
    
    filter(instrument_tables, language == input$language,
           form %in% input$form)
  })
  
  # ---------------------- DATA LOADING
  
  filtered_admins <- reactive({
    req(form_admins())
    
    reduce(input$data_filter, .init = form_admins(),
           function(filtered_admins, filter_condition) {
             filtered_admins |> filter(.data[[ filter_condition ]])
           })
  })
  
  trajectory_data <- reactive({
    req(filtered_admins()) 
    req(filtered_instrument_tables()) 
    req(input$words)
    
    # in case you have changed instruments and your words no longer apply, don't crash
    if (all(input$words %in% word_options())) {
      td <- trajectory_data_fun(filtered_admins(), filtered_instrument_tables(), 
                                input$measure, input$words) |>
        mutate(item = factor(item_definition, levels = input$words))
    } else {
      td <- data.frame()
    }
    
    td
  })

  # ------------------------------ PLOTTING OUTPUT
  ylabel <- reactive({
    if (input$measure == "understands") "Proportion of Children Understanding"
    else if (input$measure == "produces") "Proportion of Children Producing"
  })

  age_lims <- reactive({
    req(input$form)
    req(input$language)
    
    filtered_instruments <- instruments |> 
      filter(language == input$language,
             form %in% input$form) 
    
    c(min(filtered_instruments$age_min), 
      max(filtered_instruments$age_max))
  })
  
  # removing form type
  mean_data <- reactive({
    req(age_lims())
    
    trajectory_data() |>
      group_by(form, age) |>
      summarise(prop = mean(prop),
                total = sum(total)) |>
      mutate(item = "mean") |> 
      filter(age >= age_lims()[1],
             age <= age_lims()[2])
  })
  
  trajectory_plot <- function() {
    req(trajectory_data())
    
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
      
      traj <- filter(traj, age >= amin, age <= amax)
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
        langcog::scale_colour_solarized(guide = "none") +
        langcog::scale_fill_solarized(guide = "none") +
        directlabels::geom_dl(method = list(directlabels::dl.trans(x = x + 0.3),
                                            "last.qp", cex = 1, fontfamily = font))
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
        
    traj <- trajectory_data() |> select(-item_id)
    if (nrow(traj) == 0) {
      expand.grid(age = age_lims()[1]:age_lims()[1], form = input$form) |>
        select(form, age)
    } else {
      traj |>
        filter(age >= age_lims()[1],
               age <= age_lims()[2]) |> 
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
