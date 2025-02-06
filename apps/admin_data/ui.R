###################### CHILD-LEVEL SUMMARY DATA (ADMINS) ######################

ui <- function(request) {
  fluidPage(
    theme = shiny_theme,
    
    titlePanel("By-Child Summary Data"),
    br(),
    
    conditionalPanel(
      condition = "output.loaded != 1",
      h4("Loading...")
    ),
    
    conditionalPanel(
      condition = "output.loaded == 1",
      
      fluidRow(
        column(2, uiOutput("language_selector")),
        column(2, uiOutput("form_selector")),
        # ),
        # fluidRow(
        column(3, uiOutput("health_conditions_selector")),
        column(2, uiOutput("language_status_selector")),
        column(3, uiOutput("age_selector")),
      ),
      fluidRow(
        column(1, downloadButton("download_data", "Download data", class = "btn-xs"),
               align = "right"),
        column(1, bookmark, align = "right"),
        br(), br()
      ),
      
      fluidRow(
        column(11, DT::dataTableOutput("table"))
      )
    )
  )
}
