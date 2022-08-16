# ###################### VOCABULARY NORMS ######################

## TO FIX:
# - status bar for gamlss
# - form selection is broken somehow

library(gamlss) # needs to go early because of mass dependency
source("helper.R")


# --------------------- STATE PRELIMINARIES ------------------

admins <- get_administration_data(db_args = db_args, 
                                  filter_age = FALSE, 
                                  include_demographic_info = TRUE, 
                                  include_birth_info = TRUE,
                                  include_language_exposure = TRUE, 
                                  include_health_conditions = TRUE) |>
  mutate(identity = "All Data",
         monolingual = map_lgl(language_exposures, 
                               function(language_exposures) {
                                 is_null(language_exposures) || nrow(language_exposures) == 1
                               }), 
         typically_developing = map_lgl(health_conditions, 
                                        function(health_conditions) {
                                          is_null(health_conditions)
                                        }))  |>
  group_by(dataset_name, language, form, child_id) |>
  arrange(age) |>
  mutate(n = 1:n(), 
         first_administration = n == 1) |>
  select(-n) |>
  ungroup(dataset_name, child_id) |>
  gather(measure, vocab, comprehension, production) 


instruments <- get_instruments(db_args = db_args)
languages <- sort(unique(instruments$language))

alerted <- FALSE


# ---------------------- BEGIN SHINY SERVER ------------------
function(input, output, session) {
  
  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)
  
  # ---------------------- DATA LOADING
  
  # quantiles
  input_quantiles <- reactive({
    req(input$quantiles)
    switch(input$quantiles,
           Standard = c(10, 25, 50, 75, 90),
           Deciles = c(10, 20, 30, 40, 50, 60, 70, 80, 90),
           Quintiles = c(20, 40, 60, 80),
           Quartiles = c(25, 50, 75))
  })
  
  # instrument
  instrument <- reactive({
    req(input$language)
    req(input$form)
    
    instruments |> 
      filter(language == input$language, 
             form == input$form)
  })
  
  # ---------------------- UI ELEMENTS
  
  # LANGUAGE SELECTOR
  output$language_selector <- renderUI({
    selectizeInput("language", label = strong("Language"),
                   choices = c("", languages), 
                   selected = "")
  })
  
  
  # FORMS
  forms <- reactive({
    req(input$language)
    
    form_names[form_names %in% unique(filter(instruments, language == input$language)$form)]
  })
  
  # FORM SELECTOR
  output$form_selector <- renderUI({
    req(forms())
    selectizeInput("form", label = strong("Form"),
                   choices = forms(), selected = forms()[1])
  })
  
  # MEASURES
  # stopgap: hard code those forms that have a comprehension variable.
  # all others will be production-only for now.
  # in the end, this will need a specification in the instruments table.
  measures <- reactive({
    req(input$form)
    
    
    if (input$form %in% c("WG", "FormA","IC","Oxford CDI")) {
      list("Produces" = "production", "Understands" = "comprehension")
    } else {
      list("Produces" = "production")
    }
  })
  
  # MEASURE SELECTOR
  output$measure_selector <- renderUI({
    req(measures())
    selectizeInput("measure", label = strong("Measure"),
                   choices = measures(), selected = start_measure)
  })
  
  
  
  # ---------------------- DATA PROCESSING STEP 1: GET ADMINS
  # make demographic and filter selectors
  
  # GET FORM ADMINS
  form_admins <- reactive({
    req(input$language)
    req(input$measure)
    
    print("form_admins")
    
    admins |>
      filter(language == input$language,
             form == input$form,
             measure == input$measure)
  })
  
  # FILTERS
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
  
  # ---------------------- DATA PROCESSING STEP 2: APPLY FILTER AND GET DEMOGRAPHICS
  # actually process the incoming data
  
  # FILTER FORM ADMINS
  # this function is separate so that if you want, you can render the filters 
  # conditional on whether the dataset has them available
  filtered_admins <- reactive({
    req(form_admins())
    req(input$data_filter)
    
    print("filtered_admins")
    
    reduce(input$data_filter, .init = form_admins(),
           function(filtered_admins, filter_condition) {
             filtered_admins |> filter(.data[[ filter_condition ]])
           })
  })
  
  # DEMOGRAPHIC GROUPING
  # get the demographic groups 
  clumped_demo_groups <- function(fun_demo) {
    demo_groups <- filtered_admins() |>
      rename(demo = {{fun_demo}}) |>
      filter(!is.na(demo)) |>
      group_by(demo) |>
      summarise(n = n())
    
    map <- as.list(as.character(demo_groups$demo))
    names(map) <- map
    
    clump_demo_groups(demo_groups, map, fun_demo, min_obs = 100)
  }
  
  # DEMOGRAPHIC SELECTOR
  # this depends on getting the filtered data to group the demographics
  output$demo_selector <- renderUI({
    req(filtered_admins())
    
    available_demos <- Filter(
      function(demo) !all(is.na(filtered_admins()[[demo]])),
      possible_demo_fields
    )
    
    demo_fields <- Filter(
      function(demo) demo == "identity" |
        nrow(clumped_demo_groups(demo)$groups) >= 2,
      available_demos
    )
    
    # actual selector
    selectInput("demo", label = strong("Split Variable"),
                choices = demo_fields, selected = start_demo)
  })
  
  # FINAL DATA FOR PLOTTING
  data <- reactive({
    req(input$demo)
    req(filtered_admins())
    
    print("data")
    groups_map <- clumped_demo_groups(input$demo)
    groups <- groups_map$groups
    groups$clump <- factor(groups$clump, levels = groups$clump)
    groups$demo_label <- factor(groups$demo_label, levels = groups$demo_label)
    map <- groups_map$map
    demo_map <- data.frame(demo = names(map), clump = unlist(map),
                           row.names = NULL)
    demo_map$clump <- factor(demo_map$clump, levels = groups$clump)
    
    filtered_admins() |>
      rename(demo = {input$demo}) |> # this is a glue "injection"
      left_join(demo_map) |>
      right_join(groups) |>
      select(-demo) |>
      rename(demo = clump)
  })
  
  # ------------------------------ PLOTTING OUTPUT
  
  # age min
  age_min <- reactive({
    req(instrument)
    
    instrument()$age_min
  })
  
  # age max
  age_max <- reactive({
    req(instrument)
    
    instrument()$age_max
  })
  
  # y label
  ylabel <- reactive({
    req(input$measure)
    
    if (input$measure == "comprehension") {
      "Size of Receptive Vocabulary"
    } else if (input$measure == "production") {
      "Size of Productive Vocabulary"
    }
  })
  
  # using waiter package to set up waiting screens
  w <- Waiter$new(
    id = "plot",
    html = bs5_spinner(color = "danger"),
    color = transparent(.5),
  )
  
  # CURVES
  # do the gamlss fits
  curves <- reactive({
    req(data())
    req(input_quantiles())
    
    print("curves")
    
    w$show()
    
    # model
    max_vocab <- max(data()$vocab)
    mod_data <- data() |>
      ungroup() |>
      select(vocab, age, demo) |>
      mutate(vocab = vocab / max_vocab) |>
      filter(vocab > 0, vocab < 1) # transformation to 0-1 for beta model
    
    mod_data <- mod_data[complete.cases(mod_data),]
    
    # get predictions - needs to be split because gamlss only predicts centiles 
    # for a single variable model
    spsComps::shinyCatch(
      mod_data |>
        group_by(demo) |>
        nest() |>
        rowwise() |>
        mutate(gam_mod = list(gamlss(vocab ~ pbm(age, lambda = 10000),
                                     sigma.formula = ~ pbm(age, lambda = 10000),
                                     family = BE, 
                                     control = gamlss.control(c.crit = .1),
                                     data = data)),
               centiles = list(centiles.pred(gam_mod, cent = input_quantiles(),
                                             xname = "age", xvalues = age_min():age_max(),
                                             data = data))) |>
        select(-gam_mod, -data) |>
        unnest(cols = c(centiles)) |>
        pivot_longer(-c(x, demo), names_to = "percentile", values_to = "predicted") |>
        mutate(predicted = predicted * max_vocab) |> # uncorrect transformation
        left_join(data() |>
                    select(demo, demo_label), by = "demo")) # merge in labels
      
  })
  
  # PLOT HOLDER
  # the way the plot works is it sits in a reactive value container
  # it is modified whenever language, form, measure, or demographic is changed
  # but it also can be over-ridden by the "fit models" button
  v <- reactiveValues(plot = NULL)
  
  # MAKE PLOT
  observeEvent(
    c(input$language, input$form, input$measure, input$demo, input$data_filter), 
    {
      req(input$demo)
      req(age_min())
      req(age_max())
      req(ylabel()) 
      
      print("basic plot")
      
      # make legend title
      if(input$demo == "identity") {
        demo_label <- "" 
      } else {
        demo_label <- str_to_title(str_replace(input$demo, "_", " "))
      }
      
      # colors
      colour_values <- length(unique(data()$demo)) |>
        langcog::solarized_palette()
      
      # assign plot
      v$plot <- ggplot(data(), aes(x = age, y = vocab)) + 
        geom_jitter(size = .6, color = pt_color, alpha = .7) +
        scale_x_continuous(name = "Age (months)",
                           breaks = seq(age_min(), age_max(), by = 2),
                           limits = c(age_min(), age_max())) +
        scale_y_continuous(name = ylabel(),#paste0(ylabel(), "\n"),
                           limits = c(0, max(data()$vocab))) + 
        geom_smooth(aes(col = demo_label), size = 1.5, 
                    method = "loess", span = 1, se = FALSE) + 
        scale_color_manual(name = demo_label, 
                           values = colour_values) +
        theme(legend.position = "bottom")
    })
  
  # UPDATE PLOT WITH CURVES WITH GO BUTTON
  observeEvent(input$go, {
    req(data())
    req(input_quantiles())
    req(age_min())
    req(age_max())
    req(ylabel())
    req(curves())
    
    colour_values <- length(input_quantiles()) |>
      langcog::solarized_palette() |>
      rev()
    
    on.exit({
      w$hide()
    })
    
    v$plot <- ggplot(data(), aes(x = age, y = vocab)) + 
      facet_wrap(~demo_label) +
      geom_jitter(size = .6, color = pt_color, alpha = .7) +
      scale_x_continuous(name = "Age (months)",
                         breaks = seq(age_min(), age_max(), by = 2),
                         limits = c(age_min(), age_max())) +
      scale_y_continuous(name = ylabel(),#paste0(ylabel(), "\n"),
                         limits = c(0, max(data()$vocab))) + 
      geom_line(data = curves(),
                aes(x = x, y = predicted, col = percentile), size = 1.5) +
      facet_wrap(~demo_label) +
      scale_color_manual(name = "Quantile", values = colour_values) +
      guides(color = guide_legend(reverse = TRUE))
  })
  
  height_fun <- function() session$clientData$output_plot_width * 0.7
  
  output$plot <- renderPlot(v$plot, height = height_fun)
  
  
  table_data <- eventReactive(input$go, {
    # print(curves())
    curves() |>
      select(age = x, quantile = percentile, predicted, demo) |>
      distinct() |>
      spread(quantile, predicted) |>
      arrange(demo) |>
      rename_(.dots = setNames("demo", input$demo))
  })
  
  output$table <- renderTable(table_data(), include.rownames = FALSE,
                              digits = 1)
  
  
  # --------------------- DOWNLOADING HANDLERS ETC
  
  output$download_table <- downloadHandler(
    filename = function() "vocabulary_norms_table.csv",
    content = function(file) {
      td <- table_data()
      extra_cols <- data.frame(language = rep(input$language, nrow(td)),
                               form = rep(input$form, nrow(td)),
                               measure = rep(input$measure, nrow(td)))
      write.csv(bind_cols(extra_cols, td), file, row.names = FALSE)
    })
  
  output$download_data <- downloadHandler(
    filename = function() "vocabulary_norms_data.csv",
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    })
  
  output$download_plot <- downloadHandler(
    filename = function() "vocabulary_norms.pdf",
    content = function(file) {
      cairo_pdf(file, width = 10, height = 7)
      print(plot())
      dev.off()
    })
  
  output$loaded <- reactive(1)
}
