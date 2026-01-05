library(dplyr)
library(tidyr)
library(purrr)

norm_index <- read_csv("metadata/norms_index.csv")

load_norms <- \(filename) {
  norms_df <- read_csv(filename, skip = 1, show_col_types = FALSE) |> #as.matrix()
    rename(percentile = age) |>
    pivot_longer(cols = -percentile, names_to = "age", values_to = "value") |>
    mutate(age = as.numeric(age)) |>
    arrange(age, percentile)
  
  ages <- seq(min(norms_df$age), max(norms_df$age))
  assertthat::assert_that(all(unique(norms_df$percentile) == 1:99))
  assertthat::assert_that(all(unique(norms_df$age) == ages))
  assertthat::assert_that(all(norms_df$value >= 0))
  assertthat::assert_that(nrow(norms_df) == 99 * length(ages))
  assertthat::assert_that(!any(is.na(norms_df)))
  
  norms_df
}

norm_files <- list.files("percentiles", recursive = TRUE)
setdiff(norm_files, norm_index$path)
setdiff(norm_index$path, norm_files)

norms <- norm_index |>
  mutate(percentiles = map(path, \(p) load_norms(file.path("percentiles", p))))

write_rds(norms, "norms_tables.rds")
