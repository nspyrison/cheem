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
## Dependencies
require(cheem) ## Previously #load("./data/0local_funcs.RData", envir = globalenv())
require(spinifex)
require(plotly)
require(gganimate) ## Not in use atm
require(magrittr)
#options(show.error.locations = TRUE) #, error = browser)


## Load prepared cheem_ls() returns
penguins_ls     <- readRDS("./data/preprocess_penguins.rds")
toy_class_ls    <- readRDS("./data/preprocess_toy_classification.rds")
fifa_ls         <- readRDS("./data/preprocess_fifa.rds")
ames2018_ls     <- readRDS("./data/preprocess_ames2018.rds")
toy_quad_reg_ls <- readRDS("./data/preprocess_toy_quad_regression.rds")
toy_trig_reg_ls <- readRDS("./data/preprocess_toy_trig_regression.rds")
toy_mix_reg_ls  <- readRDS("./data/preprocess_toy_mixture_regression.rds")
chocolates_ls   <- readRDS("./data/preprocess_chocolates.rds")

## Create contextLine; App name, version, and sys date.
.wd         <- getwd()
.regex      <- regexpr("\\/[^\\/]*$", .wd)
.local_path <- gsub("_", " ", substr(.wd, .regex + 1L, nchar(.wd)))
.title      <- paste0("cheem --- ", .local_path)
contextLine <- paste0(.local_path, " app, ",
                      " --- cheem (v", packageVersion("cheem"), ")",
                      " --- ", Sys.Date())

## UI content ----
### tab1_cheem -----
expected_data_char <- c(
  "toy classification", "penguins classification", "chocolates classification",
  "toy quad regression", "toy trig regression", "toy mixture model regression", 
  "fifa regression", "ames housing 2018 regression",
  "<Upload saved cheem_ls (.rds only)>")
tab1_cheem <- tabPanel(title = "Data- and attribution-spaces", fluidPage(
  #### Top text description -----
  fluidRow(
    fluidRow(
      column(3L, selectInput("dat_char", "Data:",
                             choices  = expected_data_char,
                             selected = "penguins classification")),
      column(3L, conditionalPanel(
        "input.dat_char == '<Upload saved cheem_ls (.rds only)>'",
        fileInput("in_cheem_ls", "Select a file (return of cheem_ls saved to .rds)",
                  multiple = FALSE, accept = c("text/rds", ".rds")))),
      column(6L, tableOutput("perf_df"))
    ),
    htmlOutput("desc_rows"),
    p("- fit a modest randomForest model, ")
  ),
  tags$hr(style = "border-color: grey;"),
  
  #### global_view ----
  h4("Global view:"),
  #p("Approximations of data- and attribution-spaces (PC1:2) and model predictions by observed y."),
  fluidRow(
    column(3L, numericInput(
      "primary_inst", "Primary instance ('*', dashed line below):", NULL)),
    column(3L, numericInput(
      "comparison_inst", "Comparison instance ('x', dotted line below):", NULL)),
    column(3L, selectInput(
      "glob_view_col", "Global view point color",
      c("default", "log_maha.data", "cor_attr_proj.y", "residual"))),
    column(3L)
  ),
  ## Container display dim
  ## Also see plot dim in: ggplotly(p, height, width)
  plotly::plotlyOutput("global_view", width = "100%", height = "544px") %>%
    shinycssloaders::withSpinner(type = 8L),
  h5("Selected data:"),
  DT::DTOutput("selected_df")),
  tags$hr(style = "border-color: grey;"),
  
  #### Cheem tour ----
  h4("Cheem tour"),
  fluidRow(
    column(width = 3L,
           checkboxGroupInput(
             "inc_feat_nms", label = "Features to include:",
             choices = NULL, selected = NULL, inline = TRUE)),
    column(width = 3L,
           selectInput("manip_feat_nm", "Manipulation feature:",  NULL)),
    column(width = 3L, 
           selectInput("do_add_pcp_segments", "Draw PCP lines on the basis distribution?",
                       c("yes" = TRUE, "no" = FALSE))),
    column(3L)
  ),
  # p("Longer-dashed and dotted lines: location of primary & comparison points respectively ('*'/'x' in global view)."),
  # p("Origin mark: solid grey line or cross, projection 0, all X's = 0 projected through the basis."),
  ## plotly tour
  #### Sometimes this behaves like iframe and others like object itself. 
  plotly::plotlyOutput("cheem_tour_plotly", width = "1440px", height = "620px") %>%
    shinycssloaders::withSpinner(type = 8L),
  br(), br(), br()
) ## Assign tab1_cheem

### tab_about -----
tab_about <- tabPanel("About", fluidPage(
  h2("Context: feature interpretability of nonlinear models"),
  p("Modern modeling faces a trade-off between the interpretability and accuracy of a model. 
    Nonlinear models use increasingly many and complex terms.
    The use of these terms improve the accuracy of the model, but can make the terms difficult to interpret."),
  HTML("Recently, <em>local explainations</em> approximate the linear feature (variable, predictor) importances in the vicinity of one point, typically an instance.
    Originally, the explanations were then plotted to illustrate those features that contribute to the difference between the mean of the explanatory variables and an instances prediction."),
  br(), br(),
  img(src = "lime_nonlinear.png"),
  p("Local explanations can explain which and how features would influence one variable to crossing a classification boundary or lead to an extreme residual."),
  p('Ribeiro, M. et. al. (2017). Why Should I Trust You? ', a(href = 'https://www.kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', 'https://www.kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', .noWS = "outside"), .noWS = c("after-begin", "before-end")),
  br(), br(),
  h2("Cheem analysis:"),
  HTML("Given a nonlinear model we calculate the tree SHAP local explanation for each observation. In the Gobal view we explore Our approach is to select a primary and comparison instance (observation) and use a local explanation's feature-attribution of the primary instance to project the data. 
    The explanation can then be examined by animating a <em>radial tour</em> that rotates the contribution of a selected feature.
    By altering the projection basis we can explore how sensitive a feature is to the structure identified by the local explanation to see under which contibutions the prediction is support."),
  img(src = "cheem_workflow.png"),
  p('(top) Wickham, H. & Grolemund, G. (2016). R for data science. ', a(href = 'https://r4ds.had.co.nz/', 'https://r4ds.had.co.nz/', .noWS = "outside"), .noWS = c("after-begin", "before-end")),
  p('(bottom) Biecek P. & Burzykowski T. (2020). Explanatory Model Analysis. ', a(href = 'https://ema.drwhy.ai/', 'https://ema.drwhy.ai/', .noWS = "outside"), .noWS = c("after-begin", "before-end")),
  p('(blue overlay) Purposed analysis in terms of workflow and model specificity.'),
  br(), br(), br(),
  h2("Namesake:"),
  HTML("The Trees of Cheem, are a fictional race of tree-based humanoids in the Dr. Who universe. The initial application applies tree SHAP (a local explain of tree-based models woth the <b>treeshap</b> package), and explanations from <b>DALEX</b>, a reference to Dr. Who lore."),
  img(src = "cheem_namesake.png"),
  br(), br(), br()
)) ## Assign tabZ_about

## Combined ui object ----
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
                navbarPage(.title,
                           tab1_cheem,
                           tab_about),
                h5(contextLine, style = "color: #A9A9A9")
)
