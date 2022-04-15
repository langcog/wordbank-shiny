library(tidyverse)
library(magrittr)
library(rlang)
library(lazyeval)
library(wordbankr)
library(langcog)
library(purrr)

select <- dplyr::select
font <- theme_mikabr()$text$family
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

db_args <- list(host = "wordbank2-dev.canyiscnpddk.us-west-2.rds.amazonaws.com",
                dbname = "wordbank",
                user = "wordbank_reader",
                password = "ICanOnlyRead@99")


Sys.setlocale(locale = "en_US.UTF-8")
mode <- "remote"
