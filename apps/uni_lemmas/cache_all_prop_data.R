source("../common.R")

items <- get_item_data()
uni_lemmas <- items |>
  filter(!is.na(uni_lemma), uni_lemma != "NA") |>
  group_by(uni_lemma) |>
  filter(n_distinct(language) > 1) |>
  ungroup() |>
  select(language, form, form_type, item_kind, item_id, item_definition, uni_lemma)

get_inst_data <- function(inst_language, inst_form) {
  message("\t", inst_form)
  inst_uni <- uni_lemmas |> filter(language == inst_language, form == inst_form)
  inst_data <- get_instrument_data(language = inst_language,
                                   form = inst_form,
                                   administration_info = TRUE,
                                   item_info = inst_uni,
                                   db_args = shiny_db_args)
}

get_lang_props <- function(inst_language) {
  message(inst_language)
  forms <- uni_lemmas |> filter(language == inst_language) |> pull(form) |> unique()
  inst_data <- map_df(forms, \(f) get_inst_data(inst_language, f))
  inst_data |>
    mutate(produces = !is.na(produces) & produces,
           understands = !is.na(understands) & understands) |>
    group_by(language, uni_lemma, age) |>
    summarise(words = item_definition |> strsplit(", ") |> unlist() |> unique() |>
                paste(collapse = ", "),
              produces = mean(produces),
              understands = mean(understands),
              n = n_distinct(data_id)) |>
    ungroup() |>
    pivot_longer(c(produces, understands), names_to = "measure", values_to = "prop")
}

all_prop_data <- unique(uni_lemmas$language) |> sort() |> map_df(get_lang_props)
saveRDS(all_prop_data, "all_prop_data.rds")
