# ###################### ITEM-LEVEL SUMMARY DATA ######################

library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("spacelab"),

  titlePanel("By-Word Summary Data"),
  br(),

  sidebarLayout(

    sidebarPanel(
      width = 3,

      conditionalPanel(
        condition = "output.loaded != 1",
        h4("Loading...")
      ),

      conditionalPanel(
        condition = "output.loaded == 1",

        uiOutput("language_selector"),
        uiOutput("form_selector"),
        uiOutput("measure_selector"),
        uiOutput("age_selector"),
        actionButton("go", "Get Data!"),
        br(),
        br(),
        p("Note that loading large datasets can take 30 seconds or more, please be patient.")
      )
    ),

    mainPanel(
      width = 9,
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      conditionalPanel(
        condition = "output.loaded == 1",
        downloadButton("download_all", "Download data as CSV",
                       class = "btn-default btn-xs"),
        br(), br(),
        DT::dataTableOutput("table")
      )
    )

  )
))
