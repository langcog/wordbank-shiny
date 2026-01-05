# ###################### SCORING ######################

pops <- jsonlite::read_json("docs/popovers.json")

theme_set(theme_bw(base_size = 8, base_family = .font))
theme_update(panel.grid = ggplot2::element_blank(),
             strip.background = ggplot2::element_blank(),
             legend.key = ggplot2::element_blank(),
             panel.border = ggplot2::element_blank(),
             axis.line = ggplot2::element_line(),
             strip.text = ggplot2::element_text(face = "bold"))

# --------------------- STATE PRELIMINARIES ------------------

# load saved metadata files
norms <- read_rds("norms/norms_tables.rds")
label <- read_csv("norms/metadata/labels.csv")
measure_meta <- read_csv("norms/metadata/measures.csv")
instrument_meta <- read_csv("norms/metadata/instruments.csv")

# separate labels by field
field_labels <- \(f) label |> filter(field == f) |> select(label, value) |> deframe()
language_opts <- field_labels("language")
form_opts <- field_labels("form")
measure_opts <- field_labels("measure")
sex_opts <- field_labels("sex")


# ---------------------- BEGIN SHINY SERVER ------------------
function(input, output, session) {
  
  # ---------------------- UI ELEMENTS
  
  # language selector
  output$language_selector <- renderUI({
    selectizeInput("language", label = strong("Language"),
                   choices = language_opts,
                   # choices = c("", languages),
                   options = list(placeholder = "Select a language..."))
  })
  
  # forms
  forms <- reactive({
    req(input$language)
    form_opts[form_opts %in% unique(filter(norms, language == input$language)$form)]
  })
  
  # form selector
  output$form_selector <- renderUI({
    req(forms())
    selectInput("form", label = strong("Form"),
                choices = forms(), selected = forms()[1])
  })
  
  # measures
  measures <- reactive({
    req(input$language, input$form)
    measure_opts[measure_opts %in% unique(filter(norms, language == input$language, form == input$form)$measure)]
  })
  
  # file upload
  output$file_selector <- renderUI({
    req(measures())
    filebox <- fileInput("user_file", "Choose file", accept = ".csv")
    if (input$sample_data)  shinyjs::disabled(filebox) else filebox
  })
  
  # create input row (checkbox + dropdown) for a given field
  field_input <- \(field, field_label, checked, disabled) {
    check <- checkboxInput(inputId = glue("{field}_include"), label = NULL, value = checked)
    div(class = "field-controls",
        if (disabled) shinyjs::disabled(check) else check,
        selectInput(inputId = field, label = field_label, choices = c("", colnames(user_data()))),
        bsPopover("M3L", title = NULL,
                  content = HTML(sprintf("<small>%s</small>", pops$m3l)),
                  placement = "right"),
    )
  }
  
  # static fields (dropdowns always shown)
  static_fields <- c("Subject ID" = "id", "Age (months)" = "age", "Sex" = "sex")
  
  # input rows for static fields
  output$fields_static <- renderUI({
    req(user_data())
    imap(static_fields[static_fields %in% c("id", "age")], partial(field_input, checked = TRUE, disabled = TRUE))
  })

  # input rows for sex field
  output$fields_sex <- renderUI({
    req(user_data())
    imap(static_fields[static_fields == "sex"], partial(field_input, checked = input$norms_type != "not_sex", disabled = TRUE))
  })
  
  # input rows for dynamic fields (set of measures depending on input instrument)
  output$fields_measures <- renderUI({
    req(user_data())
    imap(measures(), partial(field_input, checked = FALSE, disabled = FALSE))
  })
  
  # create input row (checkbox + dropdown) for a given sex value
  sex_input <- \(value, opts, disabled) {
    need_sex <- input$norms_type != "not_sex"
    check <- checkboxInput(inputId = glue("sex_{value}_included"), label = NULL, value = need_sex)
    div(class = "field-controls",
        if (disabled) shinyjs::disabled(check) else check,
        selectInput(inputId = glue("sex_{value}"), label = glue("Sex – {value}"), choices = c("", opts))
    )
  }
  
  # input rows for sex value mapping
  output$value_map <- renderUI({
    req(user_data())
    sex_opts <- if ("sex" %in% names(static_field_cols())) unique(user_data()[[static_field_cols()[["sex"]]]]) else ""
    list(
      sex_input("male", sex_opts, disabled = TRUE),
      sex_input("female", sex_opts, disabled = TRUE)
    )
  })
  
  # ----- REACTIVES
  
  # read in user data
  user_data <- reactive({
    if (input$sample_data) return(read_csv("data/sample.csv"))
    file <- input$user_file
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "csv", "Please upload a csv file"))
    read_csv(file$datapath)
  })
  
  # min and max value for each measure
  inst_measure_lims <- reactive({
    measure_meta |> filter(language == input$language, form == input$form) |>
      mutate(range = map2(min_value, max_value, c)) |>
      select(measure, range) |>
      deframe()
  })
  
  # given percentile table, look up percentile for given age + score
  get_percentile <- \(percentiles, child_age, child_score) {
    
    if (!(child_age %in% percentiles$age)) return(NA)
    # percentiles and value for given age
    age_values <- percentiles |> filter(age == child_age)
    
    # handle score being at floor or ceiling
    if (child_score >= max(age_values$value)) return("99")
    if (child_score <  min(age_values$value)) return("<1")
    
    # largest percentile such that corresponding value is less than or equal to score
    age_values |>
      mutate(above_score = value > child_score) |>
      # depends on percentile sort order being ascending!
      filter(!above_score & lead(above_score)) |>
      pull(percentile) |>
      as.character()
  }
  
  # given percentile table and min/max value vector for a measure
  # return function that takes a child's age, sex, and score, and return percentile
  get_subject_percentile <- \(measure_norms, measure_lims) {
    \(age, sex, value) {
      # if score is missing or outside bounds, return NA
      if (is.na(value) || value < measure_lims[1] || value > measure_lims[2]) return(NA)
      
      # map input sex values to norm labels and retrieve matching norms table
      sex_map <- set_names(c(input$sex_male, input$sex_female), c("boys", "girls"))
      child_sex <- if (is.na(sex)) "both" else as.character(suppressWarnings(fct_recode(sex, !!!sex_map)))
      sex_norms <- measure_norms[[child_sex]]
      
      # look up percentile
      get_percentile(sex_norms, age, value)
    }
  }
  
  # add percentiles for a given measure to data
  add_measure_percentiles <- \(df, meas) {

    # if measure isn't checked, pass
    if (!(meas %in% names(measure_field_cols()))) return(df)
    
    # get strings for column names for age, sex, and measure values
    age_col <- static_field_cols()[["age"]]
    age_clamp_col <- glue("{age_col}_benchmark")
    age_target <- if (input$handle_age == "clamp") age_clamp_col else age_col
    measure_col <- measure_field_cols()[[meas]]
    
    # subset norms to tables for input language + form + given measure
    measure_norms <- norms |>
      filter(language == input$language, form == input$form, measure == meas) |>
      select(sex, percentiles) |> deframe()
    # get measure limits
    measure_lims <- field_lims()[[meas]]
    
    if (input$norms_type %in% c("sex", "both")) {
      sex_col <- static_field_cols()[["sex"]]
      # add corresponding percentile column
      df <- df |> mutate("{measure_col}_percentile_sex" := pmap_chr(
        # map over clamped age, sex, and measure value
        list(.data[[age_target]], .data[[sex_col]], .data[[measure_col]]),
        # apply percentile look up function for measure norms + limits
        get_subject_percentile(measure_norms, measure_lims))
      )
    }
    
    if (input$norms_type %in% c("not_sex", "both")) {
      # add corresponding percentile column
      df <- df |> mutate("{measure_col}_percentile_combined" := pmap_chr(
        # map over clamped age, sex, and measure value
        list(.data[[age_target]], rep("both", nrow(df)), .data[[measure_col]]),
        # apply percentile look up function for measure norms + limits
        get_subject_percentile(measure_norms, measure_lims))
      )
    }
    
    df
  }
  
  # subset static fields to ones that have a mapped column selected
  static_field_cols <- reactive({
    static_fields |> unname() |> set_names() |> map(\(f) input[[f]]) |> unlist() |> discard(\(s) s == "")
  })
  
  # subset measure fields to ones that have a mapped column selected
  measure_field_cols <- reactive({
    measures() |> unname() |> set_names() |> map(\(f) input[[f]]) |> unlist() |> discard(\(s) s == "")
  })
  
  # subset measures to ones whose corresponding input checkbox is checked
  include_m <- \(m) input[[glue("{m}_include")]]
  measures_included <- reactive(measures() |> unname() |> keep(\(m) !is.null(include_m(m)) && include_m(m)))
  
  # construct limits (min/max values) for each field (measures + age)
  field_lims <- reactive({
    instrument <- instrument_meta |> filter(language == input$language, form == input$form)
    all_lims <- inst_measure_lims()
    all_lims$age <- c(instrument$min_age, instrument$max_age)
    all_lims
  })
  
  # validation messages for each value of a given field (value outside of field's limits)
  validation_low <- \(vals, field, field_col) if_else(vals < field_lims()[[field]][1], glue("{field_col} < {field_lims()[[field]][1]}"), NA)
  validation_high <- \(vals, field, field_col) if_else(vals > field_lims()[[field]][2], glue("{field_col} > {field_lims()[[field]][2]}"), NA)
  
  # indicator for whether computing scores is possible in current state
  scorable <- reactive({
    sex_mapped <- "sex" %in% names(static_field_cols()) &&
      "sex_male" %in% names(input) && "sex_female" %in% names(input) &&
      input$sex_male != "" && input$sex_female != ""

    req_fields <- "age"
    # all static fields need to be mapped
    # all(static_fields %in% names(static_field_cols())) &&
    all(req_fields %in% names(static_field_cols())) &&
      # at least one measure field needs to be mapped
      any(measures() %in% names(measure_field_cols())) &&
      # at least one measure needs to be included (checked)
      length(measures_included()) > 0 &&
      # sex values need to be mapped unless only non-sex-specific norms are used
      (input$norms_type == "not_sex" || sex_mapped)
  })
  
  # input data with percentiles added
  scores <- reactive({
    
    # select only relevant columns from input data
    field_cols <- c(unname(static_field_cols()), unname(measure_field_cols()))
    start_data <- user_data() |> select(any_of(field_cols))
    
    # if age column mapped, add a secondary age column clamped to instrument's limits
    if ("age" %in% names(static_field_cols()) && input$handle_age == "clamp") {
      age_col <- static_field_cols()[["age"]]
      start_data <- start_data |>
        mutate("{age_col}_benchmark" := .data[[age_col]] |>
                 map_dbl(\(age) age |> max(field_lims()$age[1]) |> min(field_lims()$age[2])),
               .after = all_of(age_col))
    }
    
    # if any measures columns mapped, add validation messages for measure
    if (length(measure_field_cols()) > 0) {
      # iterate over measures
      errors <- imap(measure_field_cols(), \(meas_col, meas) {
        # iterate over values for given measure
        map(start_data[[meas_col]],
            # combine too low and too high validation messages
            \(v) c(validation_low(v, meas, meas_col), validation_high(v, meas, meas_col)))
        # restructure into single list of non-empty messages
      }) |> transpose() |> map(unlist) |> map(unname) |> map(\(v) discard(v, is.na))
      
      # add messages to data, collapsed into single string per row
      start_data <- start_data |> mutate(errors = map_chr(errors, \(e) paste(e, collapse = ", ")))
    }
    
    # if scoring is possible, add percentiles for each included measure
    if (scorable()) {
      reduce(measures_included(), add_measure_percentiles, .init = start_data)
    } else {
      start_data
    }
    
  })
  
  # ------------------------------ OUTPUTS
  
  # output datatable
  output$scores_table <- renderDT({
    req(user_data())
    req(scores())

    # add placeholder error column to scores if necessary
    sc <- scores()
    if (!("errors" %in% colnames(scores()))) sc <- scores() |> mutate(errors = "")
    
    # add error presence indicator column, sort scores so invalid rows are up top
    scores_sorted <- sc |>
      mutate(has_errors = str_length(errors) > 0) |>
      arrange(desc(has_errors))
    
    # get indices for invalid rows for setting background color
    error_rows <- which(scores_sorted$has_errors)
  
    # remove error column if there are no invalid rows, remove error presence indicator column
    if (!any(scores_sorted$has_errors)) scores_sorted <- scores_sorted |> select(-errors)
    scores_sorted <- scores_sorted |> select(-has_errors)
  
    # create datatable object
    dt <- datatable(
      scores_sorted,
      rownames = FALSE, style = "bootstrap", selection = "none",
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 25, autoWidth = FALSE,
                     # JS callback to hide pagination controls if there's only one page
                     initComplete = JS(
                       "function(settings, json) {",
                       "  if (this.api().page.info().pages === 1) {",
                       "    $(this).closest('.dataTables_wrapper').find('.dataTables_paginate').hide();",
                       "    $(this).closest('.dataTables_wrapper').find('.dataTables_info').hide();",
                       "  }",
                       "}"
                     ))
    )
    
    # highlight invalid rows if present
    if (length(error_rows) > 0) dt <- dt |> formatStyle('errors', target = 'row', backgroundColor = styleRow(error_rows, '#f8d7da'))
    
    dt
  }, server = FALSE)
  
  # create thumbnail plot for a measure
  measure_plot <- \(ms) {
    
    # subset to norms for input instrument + given measure
    measure_norms <- norms |> filter(language == input$language, form == input$form, sex == "both", measure == ms) |>
      pull(percentiles) |> pluck(1) |>
      # subset to "standard" percentiles
      filter(percentile %in% c(10, 25, 50, 75, 90))
    
    # get y locations for percentile direct labels
    pct <- measure_norms |> group_by(percentile) |> filter(age == max(age))
    # use y axis range from metadata for full possible range of measure
    y_range <- measure_meta |> filter(language == input$language, form == input$form, measure == ms)
    
    ggplot(measure_norms, aes(x = age, y = value, color = factor(percentile))) +
      coord_cartesian(clip = "off") +
      geom_line(aes(group = percentile)) +
      directlabels::geom_dl(aes(label = percentile), data = pct,
                            method = list("last.qp", directlabels::dl.trans(x = x + 0.2),
                                          cex = 0.4,
                                          fontfamily = .font)) +
      langcog::scale_color_solarized(guide = "none") +
      scale_x_continuous(name = "Age (months)",
                         breaks = c(min(measure_norms$age), max(measure_norms$age)),
                         limits = c(min(measure_norms$age), max(measure_norms$age)),
                         expand = expansion(mult = 0.01)) +
      scale_y_continuous(name = names(measure_opts[measure_opts == ms]),
                         breaks = c(y_range$min_value, y_range$max_value),
                         limits = c(y_range$min_value, y_range$max_value),
                         expand = expansion(mult = 0.01)) +
      theme(plot.margin = margin(8, 30, 8, 8),
            aspect.ratio = 1)
  }
  
  sz <- 200 # plot height
  output$norms_preview <- renderPlot({
    req(measures_included())
    # make row of plots for each included measure
    patchwork::wrap_plots(map(measures_included(), measure_plot), nrow = 1)
  }, height = sz, width = \() sz * if (length(measures_included()) == 0) 1 else length(measures_included()))
  

  # download scores handler
  output$download_scores <- downloadHandler(
    filename = \() {
      basename <- if (input$sample_data) "sample_data" else tools::file_path_sans_ext(input$user_file$name)
      glue("{basename}_percentiles.csv")
    },
    content = \(file) write_csv(scores(), file)
  )
  
  # download scores button
  output$download_table_button <- renderUI({
    req(user_data(), scorable())
    downloadButton("download_scores", "Download scores",
                   class = "btn-default btn-xs")
  })
  
}
