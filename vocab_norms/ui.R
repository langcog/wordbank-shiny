pops <- jsonlite::read_json("docs/popovers.json")

ui <- function(request) {
  
  fluidPage(
    # autoWaiter(), 
    theme = shiny_theme,
    
    br(),
    bsCollapse(id = "doc", open = "title",
               bsCollapsePanel(title = h3("Vocabulary Norms"),
                               includeMarkdown("docs/description.md"),
                               value = "title",
                               style = "default")),
    
    fluidRow(
      column(3,
             conditionalPanel(
               condition = "output.loaded != 1",
               h4("Loading...")
             ),
             
             conditionalPanel(
               condition = "output.loaded == 1",
               
               wellPanel(
                 tags$style(type = "text/css", ".popover { width: 150px;} .span"),
                 uiOutput("language_selector"),
                 bsPopover("language_selector", title = NULL,
                           content = HTML(sprintf("<small>%s</small>", pops$language)),
                           placement = "right"),
                 uiOutput("form_selector"),
                 bsPopover("form_selector", title = NULL,
                           content = HTML(sprintf("<small>%s</small>", pops$form)),
                           placement = "right"),
                 uiOutput("measure_selector"),
                 bsPopover("measure_selector", title = NULL,
                           content = HTML(sprintf("<small>%s</small>", pops$measure)),
                           placement = "right"),
                 uiOutput("demo_selector"),
                 bsPopover("demo_selector", title = NULL,
                           content = HTML(sprintf("<small>%s</small>", pops$demo)),
                           placement = "right"),
                 uiOutput("data_filter")
               ),
               uiOutput("quantile_panel"),
               br(), bookmark
               # wellPanel(
               #   selectInput("quantiles", label = strong("Quantiles"),
               #               choices = list("Standard", "Deciles", "Quintiles",
               #                              "Quartiles"),
               #               selected = "Standard"),
               #   bsPopover("quantiles", title = NULL,
               #             content = HTML(sprintf("<small>%s</small>", pops$quantile)),
               #             placement = "right"), 
               #   actionButton("go", "Add Model Fits"))
             )),
      
      column(9,
             tags$style(type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"),
             tabsetPanel(
               tabPanel("Plot",
                        br(),
                        conditionalPanel(
                          useWaiter(), 
                          condition = "output.loaded == 1",
                          bsAlert("curves_bug"),
                          # plotOutput("plot", width = "100%", height = "auto"),
                          plotOutput("plot",
                                     width = res * wdth, height = res * hght),
                          uiOutput("download_plot_button"),
                          br(),
                          uiOutput("download_data_button"),
                          # downloadButton("download_data", "Download Raw Data",
                          #                class = "btn-default btn-xs"),
                          br(), br(),
                          uiOutput("details")
                        )),
               tabPanel("Quantile table",
                        br(),
                        tableOutput("table"),
                        br(),
                        uiOutput("download_table_button")
               )
             )
      )
    )
  )
  
}
