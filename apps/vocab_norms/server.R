# ###################### VOCABULARY NORMS ######################

## TO FIX:
# - status bar for gamlss
# - form selection is broken somehow

library(gamlss) # needs to go early because of mass dependency
source("helper.R")
pops <- jsonlite::read_json("docs/popovers.json")


# --------------------- STATE PRELIMINARIES ------------------

possible_filters =  c("cross-sectional only" = "first_administration",
                      "normative sample only" = "is_norming", 
                      "monolingual only" = "monolingual", 
                      "typically developing only" = "typically_developing")
demo_cols <- unname(c(unlist(possible_demo_fields), possible_filters))

admins <- get_administration_data(filter_age = FALSE,
                                  include_demographic_info = TRUE,
                                  include_birth_info = TRUE,
                                  include_language_exposure = TRUE,
                                  include_health_conditions = TRUE,
                                  db_args = shiny_db_args) |>
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
  ungroup() |>
  pivot_longer(c(comprehension, production),
               names_to = "measure", values_to = "vocab")|>
  select(language, form, form_type, data_id, child_id, age, measure, vocab,
         {{ demo_cols }})


instruments <- get_instruments(db_args = shiny_db_args)
languages <- sort(unique(instruments$language))

alerted <- FALSE

start_plot <- ggplot() +
  scale_x_continuous(limits = c(8, 36), breaks = seq(8, 36, 4)) +
  scale_y_continuous(limits = c(0, 800)) +
  labs(x = "Age (months)", y = "Size of vocabulary")

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
                   selected = "",
                   options = list(placeholder = "Select a language..."))
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
  measures <- reactive({
    req(instrument())
    if (instrument()$form_type == "WS") list("Produces" = "production")
    else list("Produces" = "production", "Understands" = "comprehension")
    
    # if (input$form %in% c("WG", "FormA","IC","Oxford CDI")) {
    #   list("Produces" = "production", "Understands" = "comprehension")
    # } else {
    #   list("Produces" = "production")
    # }
  })
  
  # MEASURE SELECTOR
  output$measure_selector <- renderUI({
    req(measures())
    selectizeInput("measure", label = strong("Measure"),
                   choices = measures(), selected = start_measure)
  })
  
  output$quantile_panel <- renderUI({
    req(data())
    wellPanel(
      selectInput("quantiles", label = strong("Quantiles"),
                  choices = list("Standard", "Deciles", "Quintiles",
                                 "Quartiles"),
                  selected = "Standard"),
      bsPopover("quantiles", title = NULL,
                content = HTML(sprintf("<small>%s</small>", pops$quantile)),
                placement = "right"), 
      actionButton("go", "Add model fits"))
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
    
    available_filters <- possible_filters |>
      keep(\(filt) !all(is.na(form_admins()[[filt]]) | !form_admins()[[filt]]))
    
    checkboxGroupInput("data_filter", "Filter data", choices = possible_filters,
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
    selectInput("demo", label = strong("Grouping variable"),
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
    
    filt <- input$data_filter
    df <- filtered_admins() |>
      ungroup() |>
      rename(demo = {input$demo}) |> # this is a glue "injection"
      left_join(demo_map) |>
      right_join(groups) |>
      select(-demo) |>
      rename(demo = clump) |>
      select(language, form, form_type, data_id, child_id, age, measure, vocab,
             demo, n, demo_label, {{ filt }})
    return(df)
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
    html = spin_loader(),
    # html = bs5_spinner(color = "danger"),
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
  v <- reactiveValues(plot = start_plot)
  
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
                           limits = c(age_min(), age_max()),
                           expand = expansion(mult = 0.01)) +
        scale_y_continuous(name = ylabel(),
                           limits = c(0, max(data()$vocab)),
                           expand = expansion(mult = 0.01)) +
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
    
    # write_rds(curves(), "../thumbnails/sample_curves.rds")
    v$plot <- ggplot(data(), aes(x = age, y = vocab)) + 
      facet_wrap(vars(demo_label)) + #, labeller = label_both()) +
      coord_cartesian(clip = "off") +
      geom_jitter(size = .6, color = pt_color, alpha = .7) +
      geom_line(data = curves(),
                aes(x = x, y = predicted, colour = percentile), size = 1.5) +
      directlabels::geom_dl(aes(x = x, y = predicted, label = percentile,
                                colour = percentile), data = curves(),
                            method = list("last.qp", directlabels::dl.trans(x = x + 0.2),
                                          cex = 1, fontfamily = font)) +
      directlabels::geom_dl(aes(x = age_max(), y = Inf, label = label),
                            data = tibble(label = "Quantile"),
                            method = list("last.qp", directlabels::dl.trans(x = x + 1, rot = 270),
                                          cex = 1, fontfamily = font, hjust = 0)) +
      scale_x_continuous(name = "Age (months)",
                         breaks = seq(age_min(), age_max(), by = 2),
                         limits = c(age_min(), age_max()),
                         expand = expansion(mult = 0.01)) +
      scale_y_continuous(name = ylabel(),
                         limits = c(0, max(data()$vocab)),
                         expand = expansion(mult = 0.01)) +
      scale_color_manual(name = "Quantile", values = colour_values, guide = "none") +
      theme(plot.margin = margin(8, 30, 8, 8),
            panel.spacing.x = unit(20, "pt"))
    
    on.exit({
      w$hide()
    })

  })
  
  output$plot <- renderPlot(v$plot, res = res)
  
  table_data <- eventReactive(input$go, {
    demo_name <- input$demo
    td <- curves() |>
      ungroup() |>
      select(age = x, quantile = percentile, predicted, demo) |>
      distinct() |>
      mutate(predicted = round(predicted, 1),
             age = as.integer(age)) |>
      pivot_wider(names_from = quantile, values_from = predicted) |>
      arrange(demo) |>
      rename({{demo_name}} := demo)
    if (demo_name == "identity") td <- td |> select(-identity)
    return(td)
  })
    
  output$table <- renderTable(table_data(), include.rownames = FALSE, digits = 1)

  output$details <- renderUI({
    req(data())
    bsCollapse(id = "details", open = NULL,
               bsCollapsePanel(
                 "More details and important disclaimer...",
                 includeMarkdown("docs/details.md"),
                 style = "default")
    )
  })
  
  # --------------------- DOWNLOADING HANDLERS ETC
  
  output$download_table <- downloadHandler(
    filename = function() "wordbank_vocab_table.csv",
    content = function(file) {
      td <- table_data() |>
        mutate(downloaded = lubridate::today(),
               language = input$language, form = input$form,
               measure = input$measure, .before = everything())
      write.csv(td, file, row.names = FALSE)
    })

  output$download_table_button <- renderUI({
    req(table_data())
    downloadButton("download_table", "Download table",
                   class = "btn-default btn-xs")
  })
  
  output$download_data <- downloadHandler(
    filename = function() "wordbank_vocab_data.csv",
    content = function(file) {
      demo_name <- input$demo
      df <- data() |> rename({{ demo_name }} := demo) |> select(-demo_label) |>
        mutate(downloaded = lubridate::today(), .before = everything())
      if (demo_name == "identity") df <- df |> select(-identity)
      write.csv(df, file, row.names = FALSE)
    })

  output$download_data_button <- renderUI({
    req(data())
    downloadButton("download_data", "Download raw data",
                   class = "btn-default btn-xs")
  })
  
  output$download_plot <- downloadHandler(
    filename = function() "wordbank_vocab_norms.png",
    content = function(file) {
      ggsave(file, plot = v$plot, device = "png", width = wdth, height = hght)
    })
  
  output$download_plot_button <- renderUI({
    downloadButton("download_plot", "Download plot",
                   class = "btn-default btn-xs")
  })
  
  output$loaded <- reactive(1)
}
