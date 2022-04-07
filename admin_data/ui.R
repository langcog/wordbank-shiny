###################### CHILD-LEVEL SUMMARY DATA (ADMINS) ######################
library(shiny)
library(shinythemes)

fluidPage(

  theme = shinytheme("spacelab"),

  titlePanel("By-Child Summary Data"),
  br(),

  conditionalPanel(
    condition = "output.loaded != 1",
    h4("Loading...")
  ),

  conditionalPanel(
    condition = "output.loaded == 1",

    fluidRow(
      column(3, uiOutput("language_selector")),
      column(3, uiOutput("form_selector")),
      column(4, uiOutput("age_selector"))
    ),
    fluidRow(
      column(3, uiOutput("health_conditions_selector")),
      column(3, uiOutput("language_status_selector")),
      br(),
      column(5, downloadButton("download_data", "Download Data", class = "btn-xs"),
             align = "right")
    ),

    fluidRow(
      column(11, dataTableOutput(outputId = "table"))
    )
  )
)