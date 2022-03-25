library(shiny)
library(gamlss) # needs to go early because of mass dependency
library(tidyverse)
library(magrittr)
library(rlang)
library(lazyeval)
library(wordbankr)
library(langcog)
library(purrr)
library(here)

select <- dplyr::select

source(here("vocab_norms","helper.R"))
theme_set(theme_mikabr(base_size = 18))

# constants
mode <- "remote"
possible_demo_fields <- list("None" = "identity",
                             "Birth Order" = "birth_order",
                             "Ethnicity" = "ethnicity",
                             "Gender" = "sex",
                             "Mother's Education" = "mom_ed")
pt_color <- "#839496"

start_language <- "English (American)"
start_form <- "WS"
start_measure <- "production"
start_demo <- "identity"



# --------------------- STATE PRELIMINARIES ------------------

admins <- get_administration_data(mode = mode, original_ids = TRUE) |>
  gather(measure, vocab, comprehension, production) |>
  mutate(identity = "All Data")

instruments <- get_instruments(mode)
languages <- sort(unique(instruments$language))



# input <- list(language = "English (American)", form = "WS", measure = "production",
#               quantiles = "Standard", demo = "sex")

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
           Quartiles = c(25, 50, 75),
           Median = c(50))
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
                   choices = languages, selected = start_language)
  })

 
  # FORMS
  forms <- reactive({
    req(input$language)
    
    form_opts <- unique(filter(instruments, language == input$language)$form)
    
    # can we do this functionally? this is simple though
    # still has hard-coded cases to deal with naming the opaque WS and WG designators
    forms <- list()
    for (opt in form_opts) {
      if (opt == "WS") {
        forms[["Words & Sentences"]] = "WS"
      } else if (opt == "WG") {
        forms[["Words & Gestures"]] = "WG"
      } else if (opt == "IC") {
        forms[["Infant Checklist"]] = "IC"
      } else if (opt == "TC") {
        forms[["Toddler Checklist"]] = "TC"
      } else {
        forms[[opt]] <- opt
      }
    }
    
    return(forms)
  })
  
  # FORM SELECTOR
  output$form_selector <- renderUI({
    req(forms())
    selectizeInput("form", label = strong("Form"),
                   choices = forms(), selected = start_form)
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
    form_specific_admins <- admins |>
      filter(language == input$language,
             form == input$form,
             measure == input$measure)
    
    #Compute cross-sectional as first entry for a child in a source
    first_longitudinals <- form_specific_admins |>
      filter(longitudinal) |>
      group_by(source_name, original_id) |>
      arrange(age) |>
      slice(1)
    
    form_specific_admins |>
      mutate(cross_sectional = !longitudinal |
               (longitudinal & (data_id %in% first_longitudinals$data_id)))
  })
  
  # FILTERS
  # due to circularity, this is no longer checking the data before rendering the 
  # filters, but probably we should re-enable this some day?
  
  output$data_filter <- renderUI({
    # req(form_admins())
    possible_filters =  c("cross-sectional only" = "cross_sectional",
                          "normative sample" = "norming")
    
    # available_filters <- Filter(
    #   function(data_filter) !all(is.na(form_admins()[[data_filter]]) |
    #                                form_admins()[[data_filter]] == FALSE),
    #   possible_filters
    # )
    # 
    checkboxGroupInput("data_filter", "Choose Data",
                       choices = possible_filters,
                       selected = "cross_sectional")
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
    filtered_admins <- form_admins()
    
    # if('monolingual' %in% input$data_filter)
    #   filtered_admins <- filter(filtered_admins, monolingual == TRUE)
    # 
    # if('typically-developing' %in% input$data_filter)
    #   filtered_admins <- filter(filtered_admins, td == TRUE)
    
    if('cross_sectional' %in% input$data_filter)
      filtered_admins <- filter(filtered_admins, cross_sectional == TRUE)
    
    if('norming' %in% input$data_filter)
      filtered_admins <- filter(filtered_admins, norming == TRUE)
    
    filtered_admins
  })
  
  # DEMOGRAPHIC GROUPING
  # get the demographic groups 
  clumped_demo_groups <- function(fun_demo) {
    req(filtered_admins())
    
    # print("demo grouping")
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
  # 
  # # CURVE FITTING
  # fit_curves <- function(jitter_vocab = FALSE) {
  #   req(input_quantiles())
  #   req(data())
  #   
  #   models <- data() |>
  #     group_by(demo)
  #   
  #   if (jitter_vocab){
  #     models <- models |>
  #       mutate(vocab = jitter(vocab))
  #   }
  #   
  #   models <- models |>
  #     do(model = gcrq(vocab ~ ps(age, monotone = 1, lambda = 1000),
  #                     data = ., tau = input_quantiles()))
  #   
  #   get_model <- function(demo.value) {
  #     return(filter(models, demo == value)$model[[1]])
  #   }
  #   
  #   predicted_data <- data.frame()
  #   values <- as.character(unique(data()$demo))
  #   
  #   for (value in values) {
  #     ages <- data.frame(age = age_min():age_max())
  #     value_predicted_data <- predictQR_fixed(get_model(value),
  #                                             newdata = ages) |>
  #       as.data.frame() |>
  #       mutate(age = age_min():age_max()) |>
  #       gather(quantile, predicted, -age) |>
  #       mutate(demo = value,
  #              quantile = factor(quantile))
  #     predicted_data <- bind_rows(predicted_data, value_predicted_data)
  #   }
  #   
  #   # TODO: when grcq fixes its bug, get rid of the if "." thing in curves
  #   if (predicted_data$quantile[1] == ".") {
  #     predicted_data$quantile <- factor(.5)
  #   }
  #   
  #   clump_groups <- clumped_demo_groups(input$demo)$groups |>
  #     rename(demo = clump)
  #   
  #   quantile_label <- sprintf(
  #     "%.2f",
  #     as.numeric(levels(predicted_data$quantile))[predicted_data$quantile]
  #   )
  #   
  #   predicted_data |>
  #     left_join(clump_groups) |>
  #     mutate(demo = factor(demo, levels = clump_groups$demo),
  #            demo_label = factor(demo_label, levels = clump_groups$demo_label),
  #            quantile = sprintf(
  #              "%.2f", as.numeric(levels(predicted_data$quantile))[quantile]
  #            ))
  #   
  # }
  # 
  # # PLOT CURVES
  # try_curves <- function() {
  #   curve_output <- NULL
  #   attempt <- 0
  #   
  #   while( is.null(curve_output) && attempt < 50 ) {
  #     if ((attempt %% 2) == 0) {
  #       try(
  #         curve_output <- fit_curves()
  #       )
  #     } else if ((attempt %% 2) == 1){
  #       try(
  #         curve_output <- fit_curves(jitter_vocab = TRUE)
  #       )
  #     }
  #     attempt <- attempt + 1
  #   }
  #   return(curve_output)
  # }
  # 
  # curves <- reactive(tryCatch(try_curves(), error = function(e) NULL))
  # 
  # curves_bug <- observe({
  #   if (is.null(curves()) & !alerted) {
  #     createAlert(session, "curves_bug", "alert",
  #                 content = "The current model does not fit well to this dataset.",
  #                 style = "warning", dismiss = FALSE)
  #     alerted <<- TRUE
  #   }
  #   if (!is.null(curves()) & alerted) {
  #     closeAlert(session, "alert")
  #     alerted <<- FALSE
  #   }
  # })
  
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
  
  # CURVES
  # do the gamlss fits
  curves <- reactive({
    req(data())
    req(input_quantiles())
    
    # model
    max_vocab <- max(data()$vocab)
    mod_data <- data() |>
      select(vocab, age, demo) |>
      mutate(vocab = vocab / max_vocab) |>
      filter(vocab > 0, vocab < 1) # transformation to 0-1 for beta model
    
    mod_data <- mod_data[complete.cases(mod_data),]
    
    # get predictions - needs to be split because gamlss only predicts centiles 
    # for a single variable model
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
                  select(demo, demo_label)) # merge in labels
  })
  
  
  # MAKE PLOT
  plot <- reactive({
    req(data())
    req(curves())
    req(input_quantiles())
    req(age_min())
    req(age_max())
    req(ylabel())
    
    # base plot
    p <- ggplot(data(), aes(x = age, y = vocab)) + 
      geom_jitter(size = .6, color = pt_color, alpha = .7) +
        scale_x_continuous(name = "\nAge (months)",
                           breaks = seq(age_min(), age_max(), by = 2),
                           limits = c(age_min(), age_max())) +
        scale_y_continuous(name = paste0(ylabel(), "\n"),
                           limits = c(0, max(data()$vocab))) 
    
    # most of the time - dealing with quantiles
    if (length(input_quantiles()) > 1) { 
      colour_values <- length(input_quantiles()) |>
        solarized_palette() |>
        rev()
      
      p + geom_line(data = curves(),
                    aes(x = x, y = predicted, col = percentile), size = 1.5) + 
        facet_wrap(~demo_label) + 
        scale_color_manual(name = "Quantile", values = colour_values) +
        guides(color = guide_legend(reverse = TRUE))
      } else { # if it's the median, it's a special case
        colour_values <- length(unique(curves()$demo_label)) |>
          solarized_palette()
        
        p + geom_line(data = curves(),
                      aes(x = x, y = predicted, col = demo_label), size = 1.5) + 
          scale_color_manual(name = input$demo, values = colour_values) 
      }
  })
  
  height_fun <- function() session$clientData$output_plot_width * 0.7
  output$plot <- renderPlot(
    plot(), height = height_fun
  )
  
  table_data <- reactive({
    curves() |>
      select(age, quantile, predicted, demo) |>
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
