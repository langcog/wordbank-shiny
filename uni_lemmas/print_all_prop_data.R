library(tidyverse)
library(langcog)
library(wordbankr)
library(here)
theme_set(theme_mikabr() +
            theme(panel.grid = element_blank(),
                  strip.background = element_blank()))
font <- "Open Sans"

# Connect to the Wordbank database and pull out the raw data.
# data_mode <- "remote"
db_args <- list(host = "wordbank2-dev.canyiscnpddk.us-west-2.rds.amazonaws.com",
                dbname = "wordbank",
                user = "wordbank_reader",
                password = "ICanOnlyRead@99")


# all_prop_data <- feather::read_feather(here("shiny_apps/uni_lemmas/all_prop_data.feather"))
uni_lemmas <- unique(wordbankr::get_crossling_items(db_args = db_args)$uni_lemma)
start_lemma <- "dog"
kid_min <- 3
points_min <- 3

all_data <- wordbankr::get_instruments(db_args = db_args) %>%
  split(list(.$language, .$form), drop = TRUE) |>
  map_df(function(x){
    print(x)
    y <- wordbankr::get_instrument_data(language = x$language, 
                                        form = x$form,
                                        administration_info = TRUE, 
                                        item_info = TRUE, 
                                        db_args = db_args)
    return(y)
  })  

all_prop_data <- all_data |>
  filter(!is.na(uni_lemma)) |>
  mutate(value = ifelse(!is.na(value), value, "")) |>
  mutate(produces = ifelse(value == "produces", 1, 0),
         understands = ifelse(value %in% c("understands", "produces"), 1, 0)) |>
  group_by(language, uni_lemma, age) |>
  summarise(words = item_definition %>% strsplit(", ") %>% unlist() %>% unique() %>%
              paste(collapse = ", "),
            produces = mean(produces),
            understands = mean(understands),
            n = n_distinct(data_id)) %>%
  gather(key = "measure", value = "prop", produces, understands)

single_language_unilemmas <- all_prop_data %>%
  group_by(uni_lemma) %>%
  summarise(n_langs = length(unique(language))) %>%
  arrange(n_langs)

all_prop_data <- all_prop_data %>%
  filter(uni_lemma %in% filter(single_language_unilemmas, n_langs > 1)$uni_lemma)

feather::write_feather(all_prop_data, here("all_prop_data.feather"))
write_csv(all_prop_data,here("all_prop_data.csv"), na = "")


