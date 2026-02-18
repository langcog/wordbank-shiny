library(shinyWidgets)
library(DT)

pops <- jsonlite::read_json("docs/popovers.json")

ui <- function(request) {
  
  fluidPage(
    theme = shiny_theme,
    # theme = bslib::bs_theme(bootswatch = "cosmo"),
    # theme = bslib::bs_theme(),
    includeCSS("www/styles.css"),
    shinyjs::useShinyjs(),
    
    br(),
    bsCollapse(id = "doc", open = "title",
               bsCollapsePanel(title = h3("Scoring"),
                               includeMarkdown("docs/description.md"),
                               value = "title",
                               style = "default")),
    
    fluidRow(
      column(
        width = 4,
        wellPanel(
          uiOutput("language_selector"),
          # bsPopover("language_selector", title = NULL,
          #           content = HTML(sprintf("<small>%s</small>", pops$language)),
          #           placement = "right"),
          uiOutput("form_selector"),
          # bsPopover("form_selector", title = NULL,
          #           content = HTML(sprintf("<small>%s</small>", pops$form)),
          #           placement = "right"),
          hr(),
          materialSwitch("sample_data", label = "Use example data", status = "primary", right = TRUE),
          uiOutput("file_selector"),
          hr(),
          radioGroupButtons(
            inputId = "norms_type",
            label = "Which set(s) of norms to use",
            choices = c("Sex-specific" = "sex", "Non-sex-specific" = "not_sex", "Both" = "both"),
            selected = "both",
            status = "primary",
            size = "sm",
            individual = TRUE,
          ),
          radioGroupButtons(
            inputId = "handle_age",
            label = "How to handle out of range ages",
            choices = c("Benchmark and score" = "clamp", "Drop" = "drop"),
            selected = "clamp",
            status = "primary",
            size = "sm",
            individual = TRUE,
          ),
          hr(),
          div(class = "field-panel-container",
              uiOutput("helptext_req"),
              div(class = "field-panel",
                  div(class = "field-ui-container",
                      uiOutput("fields_static"),
                      uiOutput("fields_sex"),
                      uiOutput("value_map"))
              ),
              div(class = "field-panel",
                  uiOutput("helptext_measures"),
                  div(class = "field-ui-container",
                      uiOutput("fields_measures")
                  )
              )
          )
        ),
      ),
      
      column(
        width = 8,
        plotOutput("norms_preview", width = "auto", height = "auto"),
        DTOutput("scores_table"),
        uiOutput("download_table_button")
      ),
    )
  )
  
}
