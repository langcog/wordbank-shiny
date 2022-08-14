library(tidyverse)
library(wordbankr)
library(shiny)
library(shinyBS)

select <- dplyr::select

font <- "Source Sans Pro"
theme_set(theme_bw(base_size = 18, base_family = font))
theme_update(panel.grid = ggplot2::element_blank(),
             strip.background = ggplot2::element_blank(),
             legend.key = ggplot2::element_blank(),
             panel.border = ggplot2::element_blank(),
             axis.line = ggplot2::element_line(),
             strip.text = ggplot2::element_text(face = "bold"))

shiny_theme <- shinythemes::shinytheme("cosmo")

# constants
possible_demo_fields <- list("None" = "identity",
                             "Birth Order" = "birth_order",
                             "Race" = "race",
                             "Ethnicity" = "ethnicity",
                             "Gender" = "sex",
                             "Mother's Education" = "mom_ed")
pt_color <- "#839496"

start_language <- "English (American)"
start_form <- "WS"
start_measure <- "production"
start_demo <- "identity"

# mode <- "remote"
db_args <- list(host = "wordbank2-dev.canyiscnpddk.us-west-2.rds.amazonaws.com",
# db_args <- list(host = "server.wordbank.stanford.edu",
                dbname = "wordbank",
                user = "wordbank_reader",
                password = "ICanOnlyRead@99")


Sys.setlocale(locale = "en_US.UTF-8")
