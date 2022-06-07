trajectory_data_fun <- function(fun_admins, fun_instrument, fun_measure,
                                fun_words) {
  
  # get the data from this instrument
  instrument_word_data <- function(inst_id) {
    inst <- filter(fun_instrument, instrument_id == inst_id)
    word_ids <- filter(inst$data[[1]], item_definition %in% fun_words) |>
      pull(item_id)
    get_instrument_data(language = inst$language,
                        form = inst$form,
                        items = word_ids[!is.na(word_ids)],
                        administration_info = fun_admins,
                        mode = mode, 
                        db_args = db_args) %>%
      mutate(instrument_id = inst_id)
  }
  
  if (!is.null(fun_words)) {
    word_data <- map_df(fun_instrument$instrument_id, instrument_word_data) %>%
      gather(measure, value, produces, understands) %>%
      filter(measure == fun_measure) %>%
      filter(!is.na(age)) %>%
      group_by(form, item_id, age) %>%
      summarise(total = n(),
                prop = sum(value, na.rm = TRUE) / total) %>%
      left_join(unnest(fun_instrument, cols = c(data))) %>%
      ungroup()
    # %>%
    #   group_by(form) %>%
    #   rowwise() %>%
    #   mutate(item = fun_instrument[fun_instrument$instrument_id == instrument_id,]$words_by_id[[1]][item_id],
    #          type = "word") %>%
    #   left_join(select(fun_instrument, instrument_id, form)) %>%
    #   select(-instrument_id, -item_id)
  } else {
    word_data <- data.frame()
  }
  
  word_data
}