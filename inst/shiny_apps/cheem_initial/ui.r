# ./apps/cheem/ui.r -----
#' @author Nicholas Spyrison
#' Aug 2021

## Dependencies -----
#### Leg work
# require("ggplot2")
# require("plotly")
# require("spinifex")
# require("magrittr")
# require("gganimate")
#### Shiny specific
require("shiny")
require("shinythemes") ## Themes for shiny, think preset css styling.
require("shinycssloaders") ## Esp. for renderPlot() %>% withSpinner(type = 8L)
require("DT") ## For html table and buttons
## Load a few app function
require("cheem") ## Previously #load("./data/0local_funcs.RData", envir = globalenv())

## Sourcing more than needed may be more robust.
#source("../trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
#source("../cobs_n_plot_funcs.r")

if(F){ ## Not run, manual source function or open export file.
  load("./apps/cheem_app/data/0local_funcs.RData")
  file.edit("./apps/cheem_app/0export_local_funcs.r")
}

## Load prepared objs -----
#load("./data/1preprocess.RData")
## Load preprocessed objects, see the file.edit() above for details


## UI content ----
### tab1_cheem -----
tab1_cheem <- tabPanel(title = "EDA of SHAP- and data- spaces", fluidPage(
  #### Top text description -----
  fluidRow(
    ## Choose data:
    selectInput("dat_char", "Data:",
                c("triangle simulation", "penguins", "fifa"),
                "triangle simulation"),
    h3("Preprocessing and data description"),
    uiOutput("input__dat_desc"),
    p("3) Extract the SHAP matrix, that is SHAP values of EACH observation, via {treeshap}"),
    p("- Load above objects into shiny app & perform EDA with ggplot2/plotly."),
    p("Top) View PC1:2 of the data and SHAP spaces."),
    p("Bottom) 1d radial tour, the X's projected through the selected SHAP obs ('*').")
  ),
  tags$hr(style = "border-color: grey;"),
  br(),
  
  #### linked_plotly ----
  h4("PC1:2 of the data and SHAP spaces"),
  textOutput("cobs_msg"),
  uiOutput("input__shap.comparison_obs"),
  selectInput("do_include_maha_qq", "Add Mahalanobis distance QQ plots?",
              choices = c(FALSE, TRUE), selected = FALSE),
  p("Color and shape are mapped to the predicted species of the penguin. This was also the target variable of the RF model."),
  p("Red circle around the point indicates a misclassified point."),
  p("Selection: click & drag to select points, double click to remove the selection."),
  p("-- Selecting points will highight them in all facets and display detiled information to the right."),
  ## Set w/h with: ggplotly(p) %>% layout(height = 800, width = 800)
  uiOutput("input__linked_plotly") %>%
    shinycssloaders::withSpinner(type = 8L),
  h4("Selected data:"),
  DT::DTOutput("selected_df")),
  verbatimTextOutput("kurtosis_print"),
  tags$hr(style = "border-color: grey;"),
  br(),
  
  #### Manual tour ----
  h4("Manual tour, data-space projected through the 1d shap values of the specified obs."),
  fluidRow(
    column(6L, uiOutput("input__manip_var_nm")),
    column(6L, selectInput("do_add_pcp_segments", label = "Draw PCP lines on the basis distribution?",
                           c("Yes" = TRUE, "No" = FALSE)))
  ),
  p("Solid grey line: true zero, all X's = 0 projected through SHAP."),
  p("Dashed line: current location of SHAP observation ('*' above)."),
  p("Dotted line: current location of comparison observation ('x' above)."),
  ## Plotly, .html widget, animated radial tour:
  plotly::plotlyOutput(
    "manual_tour_plotly",
    height = "640px", width = "100%") %>%
  ## gganimate, .gif temp file, animated radial tour:
  # shiny::imageOutput("manual_tour_gganimate",
  #                    width = "100%", height = "900px") %>%
    shinycssloaders::withSpinner(type = 8L)
) ## Assign tab1_cheem

### tab_about -----
tab_about <- tabPanel("About", fluidPage(
  h3("Context & motivation:"),
  p("Modern modeling faces a trade of between interprebility and accuracy of a model. 
    Black-box models use increasingly more and complex interaction terms between features. 
    Doing so allows them to be more accurate, but makes them unrealistically complex to parse and interpret the reasoning and weights used. 
    We want to impove the interprebility of black box models."),
  img(src = "lime_nonlinear.PNG"),
  p('Ribeiro, M. et. al. (2017). Why Should I Trust You?. ', a(href = 'https://www.kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', 'https://www.kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")),
  p("Recently, there have been advances in interegating or explaining agnostic models within the local vacinity of a new observation. 
    Some of the original methods of such local explainations of models (Lundberg, 2017) include: LIME, DeepLIFT, and SHAP.
    Here, we build a random foest model (in light of speed), extract SHAP local attributions -- 
    the feature/variable weights in the vasinity of a new observations given the model. 
    Normalizing these features we explore an array of attempts to improve the interprebility of these SHAP-ley values loosely under the name of 'Trees of Cheem'."
  ),
  img(src = "cheem_workflow.png"),
  p('(top) Wickham, H. & Grolemund, G. (2016). R for data science. ', a(href = 'https://r4ds.had.co.nz/', 'https://r4ds.had.co.nz/', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")),
  p('(bottom) Biecek P. & Burzykowski T. (2020). Explanatory Model Analysis. ', a(href = 'http://ema.drwhy.ai/', 'http://ema.drwhy.ai/', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")),
  p(""),
  h3("Namesake"),
  img(src = "cheem_namesake.png")
)) ## Assign tabZ_about

## Combined ui object ----
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
                navbarPage("Cheem",
                           tab1_cheem,
                           tab_about)
)
