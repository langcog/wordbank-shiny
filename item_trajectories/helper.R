
list_items_by_definition <- function(item_data) {
  items <- item_data$item_id
  names(items) <- ifelse(is.na(item_data$uni_lemma),
                         item_data$item_definition,
                         paste0(item_data$item_definition,"\n(", item_data$uni_lemma,")")
  )
  return(items)
}

list_items_by_id <- function(item_data) {
  items <- ifelse(is.na(item_data$uni_lemma),
                  item_data$item_definition,
                  paste0(item_data$item_definition,"\n(", item_data$uni_lemma,")")
  )
  names(items) <- item_data$item_id
  return(items)
}

trajectory_data_fun <- function(admins, fun_instrument, fun_measure,
                                fun_words) {
  
  instrument_word_data <- function(inst_id) {
    inst <- filter(fun_instrument, instrument_id == inst_id)
    word_ids <- inst$words_by_definition[[1]][fun_words]
    get_instrument_data(language = inst$language,
                        form = inst$form,
                        items = word_ids[!is.na(word_ids)],
                        administration_info = admins,
                        mode = mode) %>%
      mutate(instrument_id = inst_id)
  }
  
  if (!is.null(fun_words)) {
    word_data <- map(fun_instrument$instrument_id, instrument_word_data) %>%
      bind_rows() %>%
      mutate(produces = value == "produces",
             understands = value == "understands" | value == "produces") %>%
      select(-value) %>%
      gather(measure, value, produces, understands) %>%
      filter(measure == fun_measure) %>%
      filter(!is.na(age)) %>%
      group_by(instrument_id, num_item_id, age) %>%
      summarise(total = n(),
                prop = sum(value, na.rm = TRUE) / total) %>%
      #num_true = sum(value, na.rm = TRUE),
      #num_false = n() - num_true) %>%
      group_by(instrument_id) %>%
      mutate(item_id = sprintf("item_%s", num_item_id)) %>%
      rowwise() %>%
      mutate(item = fun_instrument[fun_instrument$instrument_id == instrument_id,]$words_by_id[[1]][item_id],
             type = "word") %>%
      left_join(select(fun_instrument, instrument_id, form)) %>%
      select(-instrument_id, -item_id)
  } else {
    word_data <- data.frame()
  }
  
  word_data
}