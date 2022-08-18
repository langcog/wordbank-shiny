library(tidyverse)
library(wordbankr)
library(shiny)
library(shinyBS)
library(waiter) # for waiting bar
# library(spsComps) # for shinyCatch

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

Sys.setlocale(locale = "en_US.UTF-8")

# form substitutions:

form_names <- list("Words and Gestures" = "WG", 
                   "Words and Sentences" = "WS", 
                   "Infant Checklist" = "IC",
                   "Toddler Checklist" = "TC",
                   "Twins Early Development (TEDS) Short Form - 2 years" = "TEDS Twos",
                   "Twins Early Development (TEDS) Short Form - 3 years" = "TEDS Threes",
                   "Form Variant A" = "FormA",
                   "Form Variant B1" = "FormBOne",
                   "Form Variant B2" = "FormBTwo",    
                   "Form Variant C" = "FormC",
                   "Oxford CDI" = "Oxford CDI",
                   "Swingley Form" = "Swingley",
                   "Form Variant 1" = "FormOne",
                   "Form Variant 2A" = "FormTwoA" ,
                   "Form Variant 2B" = "FormTwoB",
                   "Form Variant 3" = "FormThree",
                   "CDI Two" = "CDITwo",
                   "Words and Sentences (Short Form)" = "WSShort",
                   "Words and Gestures (Short Form)" = "WGShort",
                   "Oxford Short Form" = "OxfordShort")

