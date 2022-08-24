# admin_data

admins <- read_rds("../item_trajectories/admins.rds")

demo_admins <- admins |>
  ungroup() |>
  filter(language == "English (American)", form == "WS") |>
  select(child_id, age, vocabulary = production, sex, birth_order) |> #comprehension
  arrange(child_id) |>
  slice(1:6)

demo_admins |>
  knitr::kable("html") |>
  kableExtra::kable_styling(bootstrap_options = "basic", full_width = FALSE) |>
  kableExtra::save_kable(zoom = 2, file = "admin_data_thumb.png", bs_theme = "cosmo")

file.copy("admin_data_thumb.png", "~/projects/wordbank/wordbank/static/images/sample_plots", overwrite = TRUE)


# item_data

demo_words <- c("dog", "shake", "blue")
demo_data <- get_instrument_data(language = "English (American)", 
                                  form = "WS",
                                  item_info = TRUE,
                                  administration_info = TRUE)
  
demo_items <- demo_data |>
  filter(item_id %in% unique(demo_data$item_id)[1:6]) |>
  # filter(item_definition %in% demo_words) |>
  mutate(item_id = as.numeric(str_remove(item_id, "item_"))) |>
  group_by(item_id, item_definition, category, age) |>
  summarise(prop = round(sum(produces, na.rm = TRUE) / length(produces), 2)) |>
  ungroup() |>
  spread(age, prop)

demo_items |> select(item = item_definition, category, `16`, `20`, `24`, `30`) |>
  knitr::kable("html") |>
  kableExtra::kable_styling(bootstrap_options = "basic", full_width = FALSE) |>
  kableExtra::save_kable(zoom = 2, file = "item_data_thumb.png", bs_theme = "cosmo")

file.copy("item_data_thumb.png", "~/projects/wordbank/wordbank/static/images/sample_plots", overwrite = TRUE)


# instrument_data
inst_data <- get_instrument_data(language = "English (American)",
                                 form = "WS",
                                 item_info = TRUE,
                                 administration_info = TRUE)
demo_inst <- inst_data |>
  select(child_id, age, item = item_definition, category, value) |>
  filter(child_id %in% unique(child_id)[1:2],
         item %in% unique(item)[1:3]) |>
  arrange(child_id)

demo_inst |>
  knitr::kable("html") |>
  kableExtra::kable_styling(bootstrap_options = "basic", full_width = FALSE) |>
  kableExtra::save_kable(zoom = 2, file = "instrument_data_thumb.png", bs_theme = "cosmo")

file.copy("instrument_data_thumb.png", "~/projects/wordbank/wordbank/static/images/sample_plots", overwrite = TRUE)
