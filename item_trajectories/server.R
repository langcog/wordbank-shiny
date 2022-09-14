# ###################### ITEM TRAJECTORIES ######################
source("helper.R")

# --------------------- STATE PRELIMINARIES ------------------
print("loading data")
admins <- get_administration_data(filter_age = FALSE,
                                  include_demographic_info = TRUE,
                                  include_birth_info = TRUE,
                                  include_language_exposure = TRUE,
                                  include_health_conditions = TRUE,
                                  db_args = shiny_db_args) |>
  mutate(monolingual = map_lgl(language_exposures,
                               function(language_exposures) {
                                 is_null(language_exposures) || nrow(language_exposures) == 1
                               }),
         typically_developing = map_lgl(health_conditions,
                                        function(health_conditions) {
                                          is_null(health_conditions)
                                        })) |>
  group_by(dataset_name, child_id) |>
  arrange(age) |>
  mutate(n = 1:n(),
         longitudinal = n() > 1, # not used?
         first_administration = n == 1) |>
  ungroup(child_id)

# saveRDS(admins, "admins.rds")
# admins <- readRDS("admins.rds")

items <- get_item_data(db_args = shiny_db_args) 

instruments <- get_instruments(db_args = shiny_db_args)
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
  
  measures <- reactive({
    req(form_admins())
    form_type <- unique(form_admins()$form_type)
    if ("WS" %in% form_type) "produces" else c("understands", "produces")
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
    
    checkboxGroupInput("data_filter", "Filter data", choices = possible_filters,
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
    # req(input$words)

    # in case you have changed instruments and your words no longer apply, don't crash
    if (!is.null(input$words) && all(input$words %in% word_options())) {
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
    req(input$measure)
    if (input$measure == "understands") "Proportion of children understanding"
    else if (input$measure == "produces") "Proportion of children producing"
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
  
  solarized_colors <- c("#268bd2", "#cb4b16", "#859900", "#993399", "#d33682",
                        "#b58900", "#2aa198", "#6c71c4", "#dc322f")
  stable_order_palete <- function(num_values) {
    c(rep(solarized_colors, num_values %/% length(solarized_colors)),
      solarized_colors[1:(num_values %% length(solarized_colors))])
  }
  
  # removing form type
  # mean_data <- reactive({
  #   req(age_lims())
  #   
  #   trajectory_data() |>
  #     group_by(form, age) |>
  #     summarise(prop = mean(prop),
  #               total = sum(total)) |>
  #     mutate(item = "mean") |> 
  #     filter(age >= age_lims()[1],
  #            age <= age_lims()[2])
  # })
  
  trajectory_plot <- function() {
    req(trajectory_data())
    
    traj <- trajectory_data()
    amin <- age_lims()[1]
    amax <- age_lims()[2]
    
    g <- ggplot(traj) +
      # geom_point() +
      coord_cartesian(clip = "off") +
      scale_x_continuous(name = "Age (months)",
                         breaks = amin:amax,
                         limits = c(amin, amax), # + 3),
                         expand = expansion(mult = 0.01)) +
      scale_y_continuous(name = ylabel(),
                         limits = c(-0.01, 1),
                         breaks = seq(0, 1, 0.25),
                         expand = expansion(mult = 0.01)) +
      theme(plot.margin = margin(8, 100, 8, 8))
    
    if (nrow(traj) == 0) return(g)
    
    traj <- filter(traj, age >= amin, age <= amax, !is.na(prop))
    dl <- traj |> group_by(item) |> filter(age == max(age))
    # g <- ggplot(traj, aes(x = age, y = prop, colour = item, label = item)) +
    g +
      geom_point(aes(x = age, y = prop, colour = item, shape = form), alpha = 0.7) +
      geom_smooth(aes(x = age, y = prop, colour = item, weight = total),
                  method = "loess", se = FALSE, size = 1.5) +
      scale_shape_manual(name = "", values = c(20, 1), guide = "none") +
      scale_colour_manual(guide = "none",
                          values = stable_order_palete(length(unique(traj$item)))) +
      directlabels::geom_dl(aes(x = age, y = prop, colour = item, label = item),
                            data = dl,
                            method = list(directlabels::dl.trans(x = x + 0.3),
                                          "last.qp", cex = 1, fontfamily = font))
    # if (!input$mean) return(g)
    # 
    # g +
    #   geom_smooth(aes(weight = total), method = "loess", size = 1.5,
    #               se = FALSE, colour = "black", data = mean_data()) +
    #   geom_point(aes(shape = form), data = mean_data(),
    #              colour = "black", size = 0.6, alpha = 0.7)
  }
  
  output$trajectory_plot <- renderPlot(trajectory_plot(), res = res)
  
  output$details <- renderUI({
    req(trajectory_data())
    req(nrow(trajectory_data()) > 0)
    bsCollapse(
      open = NULL,
      bsCollapsePanel("More details...",
                      includeMarkdown("docs/details.md"),
                      style = "default"))
  })
  
  # ------------------------ TABLE AND OTHER DATA DOWNLOAD

  table_data <- reactive({
    req(trajectory_data())
    req(age_lims())
        
    traj <- trajectory_data()
    if (nrow(traj) == 0) {
      expand.grid(age = age_lims()[1]:age_lims()[2], form = input$form) |>
        select(form, age)
    } else {
      traj |>
        select(-item_id) |>
        filter(age >= age_lims()[1],
               age <= age_lims()[2]) |> 
        select(form, age, item, prop) |>
        mutate(prop = round(prop, 2)) |>
        spread(item, prop)
    }
  })

  output$table <- renderTable(table_data(),
                              include.rownames = FALSE, digits = 2)

  output$download_table <- downloadHandler(
    filename = function() "wordbank_item_trajectories.csv",
    content = function(file) {
      td <- table_data()
      td_write <- td |>
        mutate(downloaded = lubridate::today(),
               language = input$language,
               measure = input$measure) |>
        select(downloaded, language, form, measure, age, everything())
      write.csv(td_write, file, row.names = FALSE)
    })
  
  output$download_table_button <- renderUI({
    req(table_data())
    req(nrow(trajectory_data()) > 0)
    downloadButton("download_table", "Download table",
                   class = "btn-default btn-xs")
  })

  output$download_plot <- downloadHandler(
    filename = function() "wordbank_item_trajectories.png",
    content = function(file) {
      ggsave(file, plot = trajectory_plot(), device = "png",
             width = w, height = h)
    })
  
  output$download_plot_button <- renderUI({
    req(trajectory_plot())
    req(nrow(trajectory_data()) > 0)
    downloadButton("download_plot", "Download plot",
                   class = "btn-default btn-xs")
  })
  
  output$loaded <- reactive(1)

}
