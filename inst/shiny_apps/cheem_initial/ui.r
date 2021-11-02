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

toy_ls <- readRDS("./data/2preprocess_toy_classification.rds")
penguins_ls <- readRDS("./data/1preprocess_penguins.rds")
fifa_ls <- readRDS("./data/3preprocess_fifa.rds")
# apartments_ls <- readRDS("./data/4preprocess_apartments.rds")
diabetes_wide_ls <- readRDS("./data/6preprocess_diabetes_wide.rds")
diabetes_long_ls <- readRDS("./data/6preprocess_diabetes_long.rds")

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
      selected = "penguins"), #"toy classification"),
    h2("Preprocessing and data description"),
    htmlOutput("desc_rows"),
    p("2) Extract the SHAP matrix, (the SHAP values at EACH observation), via {treeshap}"),
    p("Load above preprocessed objects into shiny app & perform EDA with ggplot2/plotly:"),
    p("Global view, top) approximations of the data- and SHAP-spaces as their first two principal components. Intentify a primary and comparison point (shown as '*'/'x') to interogate."),
    p("Cheem tour, bottom) 1d radial tour, starting basis is the normalized SHAP values of the primary point. Positions of the primary and comparison points are highlighted (classification: shown as dashed line and dotted line).")
  ),
  tags$hr(style = "border-color: grey;"),
  br(),
  
  #### global_view ----
  h3("Global view: PC1:2 approximations of data- and SHAP-spaces"),
  fluidRow(
    column(4L, numericInput( ## Updated by updateNumericInput
      "primary_obs", label = "Primary observation rownum, ('*' point):",
      min = 1L, max = 1L, step = 1L, value = 1L)),
    column(4L, numericInput( ## Updated by updateNumericInput
      "comparison_obs", label = "Comparison observation rownum, ('x' ponit):",
      min = 1L, max = 8L, step = 1L, value = 8L)),
    column(4L)
  ),
  p("Color and shape are mapped to the predicted species of the penguin. This was also the target variable of the RF model."),
  p("Red circle around the point indicates a misclassified point."),
  p("Selection: click & drag to select points, double click to remove the selection."),
  p("-- Selecting points will highight them in all facets and display detiled information below."),
  ## Set w/h with: ggplotly(p, height = 800, width = 800)
  plotly::plotlyOutput(
    "linked_global_view", width = "100%", height = "480px") %>%
    shinycssloaders::withSpinner(type = 8L),
  h4("Selected data:"),
  DT::DTOutput("selected_df")),
  tags$hr(style = "border-color: grey;"),
  br(),
  
  #### Cheem tour ----
  h3("Cheem tour, data-space projected through the 1d SHAP values of the primary observation."),
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
  p("Longer-dashed and dotted lines: location of primary & comparison observations respectively (previously '*'/'x')."),
  # shiny::imageOutput("cheem_tour_gganimate",
  #                    width = "100%", height = "720px")
  fluidRow(
    ## Plotly, .html widget, animated radial tour:
    column(width = 8L,
           plotly::plotlyOutput(
             "cheem_tour_plotly",
             height = "720px", width = "960px") %>%
             shinycssloaders::withSpinner(type = 8L)
    ),
    # column(width = 4L,
    #        plotly::plotlyOutput("residual_plot",
    #                             height = "400px", width = "400px") %>%
    #          shinycssloaders::withSpinner(type = 8L)
    # )
  ) ## close fluidRow
) ## Assign tab1_cheem

### tab_about -----
tab_about <- tabPanel("About", fluidPage(
  h2("Context & motivation; black-box interpretability"),
  p("Modern modeling faces a trade of between interprebility and accuracy of a model. 
    Black-box models use increasingly more and complex terms. 
    Doing so allows them to be more accurate, but makes them unrealistically complex to parse and interpret the reasoning and weights used."),
  img(src = "lime_nonlinear.png"),
  p('Ribeiro, M. et. al. (2017). Why Should I Trust You?', a(href = 'https://www.kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', 'kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', .noWS = "outside"), .noWS = c("after-begin", "before-end")),
  p("Recently, <em>local explainations</em> approximate the linear variable importances at one particular point, typically an observation.
    Originally, the explainations were then plotted to illustrate those variables contribute to move that observation from the intercept to its prediction."),
  p("Our approach is to select a primary and comparison observation, and use the SHAP value of primary observation to project the data. 
    The explaination can then be interrogated by playng a <em>manual tour</em> by rotating the contribution of a selected variable.
    By altering the the projection basis we can explore how sensitive variable importances are and thus interogate how well supported that explanation is."),
  img(src = "cheem_workflow.png"),
  p('(top) Wickham, H. & Grolemund, G. (2016). R for data science. ', a(href = 'https://r4ds.had.co.nz/', 'https://r4ds.had.co.nz/', .noWS = "outside"), .noWS = c("after-begin", "before-end")),
  p('(bottom) Biecek P. & Burzykowski T. (2020). Explanatory Model Analysis. ', a(href = 'http://ema.drwhy.ai/', 'http://ema.drwhy.ai/', .noWS = "outside"), .noWS = c("after-begin", "before-end")),
  p('(blue overlay) purposed application in terms of workflow and model specificity.'),
  p(''),
  h3("Namesake"),
  p("The Trees of Cheem, are a fictional race of tree-based humaniods in the Dr Who universe. The inital applation applies tree SHAP (a local explain of tree-based modelsm via {treeshap}), and explainations from {DALEX}, a reference to Dr. Who lore."),
  img(src = "cheem_namesake.png")
)) ## Assign tabZ_about

## Combined ui object ----
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
                navbarPage("Cheem",
                           tab1_cheem,
                           tab_about)
)
