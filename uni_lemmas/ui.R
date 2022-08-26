ui <- function(request) {
  
  fluidPage(
    
    theme = shiny_theme,
    
    br(),
    bsCollapse(id = "doc", open = "title",
               bsCollapsePanel(title = h3("Cross-Linguistic Trajectories"),
                               includeMarkdown("docs/description.md"),
                               value = "title",
                               style = "default")),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        conditionalPanel(
          condition = "output.loaded != 1",
          h4("Loading...")
        ),
        
        conditionalPanel(
          condition = "output.loaded == 1",
          selectizeInput("uni_lemma", label = h4("Meaning"), choices = NULL),
          uiOutput("measure_selector"),
          width = 3),
        
        br(), bookmark,
        
      ),
      
      mainPanel(
        width = 9,
        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        tabsetPanel(
          tabPanel("Plot",
                   br(),
                   conditionalPanel(
                     condition = "output.loaded == 1",
                     div(#style='overflow: scroll',
                       plotOutput("crosslinguistic", height = "auto", width = "100%"),
                       br(),
                       uiOutput("download_plot_button"),
                       br()
                     )
                   )
          ),
          tabPanel("Table",
                   br(),
                   uiOutput("download_table_button"),
                   # downloadButton("download_table", "Download Table",
                   #                class = "btn-default btn-xs"),
                   br(), br(),
                   tableOutput("table"))
        )
      )
    )
  )
}
