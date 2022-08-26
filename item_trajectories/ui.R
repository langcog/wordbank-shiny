pops <- jsonlite::read_json("docs/popovers.json")

ui <- function(request) {
  fluidPage(
    
    theme = shiny_theme,
    
    br(),
    bsCollapse(id = "doc", open = "title",
               bsCollapsePanel(title = h3("Item Trajectories"),
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
                 tags$style(type = "text/css", ".popover { width: 150px; }"),
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
                 uiOutput("data_filter")),
               
               wellPanel(
                 # popify(selectInput("words", label = strong("Words"),
                 #                    choices = NULL, multiple = TRUE),
                 #        title = NULL,
                 #        content = HTML(sprintf("<small>%s</small>", pops$words)),
                 #        placement = "right"),
                 uiOutput("word_selector"),
                 checkboxInput("mean", "Mean of words")
               ),
               br(), bookmark
             )),
      
      column(9,
             tags$style(type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"),
             tabsetPanel(
               tabPanel("Plot",
                        br(),
                        # conditionalPanel(
                        # condition = "output.loaded == 1",
                        bsAlert("many_words"),
                        #br(),
                        plotOutput("trajectory_plot",
                                   width = res * w, height = res * h),
                        # width = "100%", height = "auto"),
                        br(),
                        uiOutput("download_plot_button"),
                        # downloadButton("download_plot", "Download Plot",
                        #                class = "btn-default btn-xs"),
                        br(),br(),
                        uiOutput("details")
                        # )
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
