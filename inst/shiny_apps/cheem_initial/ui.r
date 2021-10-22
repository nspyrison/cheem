# ./apps/cheem/ui.r -----
#' @author Nicholas Spyrison
#' Aug 2021

## Dependencies -----
# Application only, preprocessing already done.
#### Shiny specific
require(shiny)
require(shinythemes) ## Themes for shiny, think preset css styling.
require(shinycssloaders) ## Esp. for renderPlot() %>% withSpinner(type = 8L)
require(DT) ## For html table and buttons
## Load a few app function
require(cheem) ## Previously #load("./data/0local_funcs.RData", envir = globalenv())
require(spinifex)
require(plotly)
require(gganimate) ## Not in use atm
require(magrittr)
## Load prepared objs
# preared objects now loaded in app.r; layer_ls reactive function.


## UI content ----
### tab1_cheem -----
tab1_cheem <- tabPanel(title = "Data- and SHAP-space", fluidPage(
  #### Top text description -----
  fluidRow(
    ## Choose data:
    selectInput(
      "dat_char", "Data:",
      choices = c("toy classification", "penguins", "fifa", #"apartments",
                  "diabetes (wide)", "diabetes (long)"),
      selected = "fifa"), #"toy classification"),
    h3("Preprocessing and data description"),
    htmlOutput("desc_rows"),
    p("3) Extract the SHAP matrix, that is SHAP values of EACH observation, via {treeshap}"),
    h4("Load above preprocessed objects into shiny app & perform EDA with ggplot2/plotly."),
    p("Global view, top) The first 2 PC of data- and SHAP-spaces. Explore the spaces and enter a primary and comparison points (shown as '*'/'x') to further explore the differnce of their shaply values."),
    p("Comparison tour, bottom) 1d radial tour, starting at the SHAP values of the primary comparison point is shown as a reference (shown as dashed line and dotted line).")
  ),
  tags$hr(style = "border-color: grey;"),
  br(),
  
  #### linked_plotly ----
  h4("PC1:2 of the data- and SHAP-spaces"),
  fluidRow(
    column(4L, numericInput( ## Updated by updateNumericInput
      "primary_obs", label = "Primary observation rownum, ('*' point):",
      min = 1L, max = 1L, step = 1L, value = 1L)),
    column(4L, numericInput( ## Updated by updateNumericInput
      "comparison_obs", label = "Comparison observation rownum, ('x' ponit):",
      min = 1L, max = 8L, step = 1L, value = 8L)),
    column(4L)
  ),
  selectInput("do_include_maha_qq", "Add Mahalanobis distance QQ plots?",
              choices = c(FALSE, TRUE), selected = FALSE),
  p("Color and shape are mapped to the predicted species of the penguin. This was also the target variable of the RF model."),
  p("Red circle around the point indicates a misclassified point."),
  p("Selection: click & drag to select points, double click to remove the selection."),
  p("-- Selecting points will highight them in all facets and display detiled information below."),
  ## Set w/h with: ggplotly(p) %>% layout(height = 800, width = 800)
  fluidRow(
    column(width = 6L,
           plotly::plotlyOutput("linked_plotly") %>%
             shinycssloaders::withSpinner(type = 8L)
    ), column(width = 6L,
              verbatimTextOutput("kurtosis_text"),
    )),
  h4("Selected data:"),
  DT::DTOutput("selected_df")),
  tags$hr(style = "border-color: grey;"),
  br(),
  
  #### Manual tour ----
  h4("Manual tour, data-space projected through the 1d SHAP values of the primary observation."),
  checkboxGroupInput(
    "inc_vars", label = "Inclusion variables",
    choices = c("bdy", "age", "rct", "atk", "def", "acc", "mvm", "pwr", "gk"),
    selected = c("atk", "def", "acc", "mvm", "pwr"),
    inline = TRUE),
  fluidRow(
    column(width = 4L,
           selectInput("manip_var_nm",
                       label = "Manipulation variable:",
                       choices  = NULL)), #"<Set in updateInput()>")),
    column(width = 4L, selectInput("do_add_pcp_segments", label = "Draw PCP lines on the basis distribution?",
                           c("Yes" = TRUE, "No" = FALSE))),
    column(width = 4L)
  ),
  p("Solid grey line: true zero, all X's = 0 projected through SHAP."),
  p("Dashed line: location of primary observation (previously '*')."),
  p("Dotted line: location of comparison observation (previously 'x')."),
  fluidRow(
    ## Plotly, .html widget, animated radial tour:
    column(width = 6L,
           plotly::plotlyOutput(
             "cheem_tour",
             height = "720px", width = "500px") %>%
             shinycssloaders::withSpinner(type = 8L)
    ),
    column(width = 6L,
           plotly::plotlyOutput("residual_plot",
                                height = "400px", width = "400px") %>%
             shinycssloaders::withSpinner(type = 8L)
    )
  ) ## close fluidRow
) ## Assign tab1_cheem

### tab_about -----
tab_about <- tabPanel("About", fluidPage(
  h3("Context & motivation:"),
  p("Modern modeling faces a trade of between interprebility and accuracy of a model. 
    Black-box models use increasingly more and complex interaction terms between features. 
    Doing so allows them to be more accurate, but makes them unrealistically complex to parse and interpret the reasoning and weights used. 
    We want to impove the interprebility of black box models."),
  img(src = "lime_nonlinear.png"),
  p('Ribeiro, M. et. al. (2017). Why Should I Trust You?', a(href = 'https://www.kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', 'https://www.kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")),
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
