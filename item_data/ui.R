# ###################### ITEM-LEVEL SUMMARY DATA ######################
shinyUI(fluidPage(
  theme = shiny_theme,

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
        condition="($('html').hasClass('shiny-busy'))",
        fluidRow(column(12, tags$h4("Please wait..."),
                        align = "center")),
        fluidRow(column(12, imageOutput("loading"),
                        align = "center"))),
      conditionalPanel(
        condition = "!($('html').hasClass('shiny-busy'))",
        uiOutput("download_button"),
        br(), br(),
        DT::dataTableOutput("table")
      )
    )

  )
))
