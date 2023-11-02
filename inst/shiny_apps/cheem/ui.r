# ./apps/cheem/ui.r -----
#' @author Nicholas Spyrison
#' Aug 2021

## Dependencies -----
# Application only, preprocessing already done.
#### Shiny specific
library(shiny,           quietly = TRUE, verbose = FALSE)
library(shinythemes,     quietly = TRUE, verbose = FALSE) ## Themes for shiny, think preset css styling.
library(shinycssloaders, quietly = TRUE, verbose = FALSE) ## Esp. for renderPlot() %>% withSpinner(type = 8L)
library(DT,              quietly = TRUE, verbose = FALSE) ## For html table and buttons
library(cheem,           quietly = TRUE, verbose = FALSE)
library(plotly,          quietly = TRUE, verbose = FALSE)
library(magrittr,        quietly = TRUE, verbose = FALSE)
#conflicted::conflict_prefer("run_app", "cheem", quiet = TRUE)
#library(gganimate,       quietly = TRUE, verbose = FALSE) ## Not in use atm
## Previously #load("./data/0local_funcs.RData", envir = globalenv())
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
#.local_path <- gsub("_", " ", substr(.wd, .regex + 1, nchar(.wd)))
.title      <- "cheem" #paste0("cheem --- ", .local_path)
contextLine <- paste0(#.local_path, " app, ",
                      "cheem (v", packageVersion("cheem"), ")",
                      " --- ", Sys.Date())

## UI content ----
### tab1_cheem -----
expected_data_char <- c(
  "toy classification", "penguins classification", "chocolates classification",
  "toy quad regression", "toy trig regression", "toy mixture model regression", 
  "fifa regression", "ames housing 2018 regression",
  "<Upload saved cheem_ls (rds only)>")
tab1_cheem <- tabPanel(title = "Analysis", fluidPage(
  #### Top text description -----
  fluidRow(
    fluidRow(
      column(3, selectInput("dat_char", "Data",
                            choices  = expected_data_char,
                            selected = "penguins classification")),
      column(3, conditionalPanel(
        "input.dat_char == '<Upload saved cheem_ls (rds only)>'",
        fileInput("in_cheem_ls", "Select a file (return of cheem_ls saved to rds)",
                  multiple = FALSE, accept = c("text/rds", ".rds")))),
      column(5, tableOutput("perf_df")),
      column(1)
    ),
    htmlOutput("desc_rows"),
  ),
  tags$hr(style = "border-color: grey;"),
  
  #### global_view ----
  h4("Global view"),
  #p("Approximations of data- and attribution-spaces (PC1:2) and model predictions by observed y."),
  fluidRow(
    column(3, numericInput(
      "primary_inst", "Primary instance ('*', dashed line below)", NULL)),
    column(3, numericInput(
      "comparison_inst", "Comparison instance ('x', dotted line below)", NULL)),
    column(3, selectInput(
      "glob_view_col", "Point color",
      c("default", "log_maha.data", "cor_attr_proj.y", "residual"))),
    column(3, actionButton("go_global_view", label = "Update global view")),
    #column(4)
  ),
  
  
  ## Container display dim
  ## Also see plot dim in: ggplotly(p, height, width)
  plotly::plotlyOutput("global_view", width = "1004px", height = "342px") %>% #, width = "100%", height = "544px"
    shinycssloaders::withSpinner(type = 8),
  h5("Selected data"),
  DT::DTOutput("selected_df")),
  tags$hr(style = "border-color: grey;"),
  
  #### Cheem tour ----
  h4("Tour"),
  fluidRow(
    column(3, checkboxGroupInput("inc_feat_nms", label = "Features to include",
                                 choices = NULL, selected = NULL, inline = TRUE)),
    column(3, selectInput("manip_feat_nm", "Manipulation feature", NULL)),
    column(3, selectInput("do_add_pcp_segments", "Draw PCP lines?",
                          c("yes" = TRUE, "no" = FALSE))),
    column(3, actionButton("go_tour", label = "Update tour")),
    #column(4)
  ),
  
  # p("Longer-dashed and dotted lines: location of primary & comparison points respectively ('*'/'x' in global view)."),
  # p("Origin mark: solid grey line or cross, projection 0, all X's = 0 projected through the basis."),
  ## plotly tour
  #### Sometimes this behaves like iframe and others like object itself. 
  plotly::plotlyOutput("cheem_tour_plotly", width = "1004px", height = "502px") %>% #, width = "1440px", height = "620px") %>%
    shinycssloaders::withSpinner(type = 8),
  br(), br(), br()
) ## Assign tab1_cheem

### tab_about -----
tab_about <- tabPanel("About", fluidPage(
  h2("Context: feature interpretability of nonlinear models"),
  p("Modern modeling faces a trade-off between the interpretability and accuracy of a model. 
    Nonlinear models use increasingly many complex terms.
    The use of these terms improves the accuracy of the model but can make the terms challenging to interpret."),
  HTML("Recently, <em>local explanations</em> approximate the linear feature (variable, predictor) importances in the vicinity of one point.
    These explanations can be plotted to illustrate an explanation of the features contribution to the difference between the mean of the explanatory variables and an instance's (observation's) prediction."),
  br(), br(),
  img(src = "lime_nonlinear.png"),
  p("Local explanations can explain which features would influence one variable to cross a classification boundary or lead to an extreme residual."),
  p('Ribeiro, M. et al. (2017). Why Should I Trust You? ', 
    a(href = 'https://www.kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', 
      'https://www.kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', .noWS = "outside"), 
    .noWS = c("after-begin", "before-end")),
  br(), br(),
  h2("Cheem analysis:"),
  HTML("Given a nonlinear model we calculate the tree SHAP local explanation for each observation. 
    In the Global view, we explore the separation of the data space, attribution space, and residual plot as a coordinated view. 
    This facilitates the selection of a primary and comparison instance to examine further. 
    The normalize attribution from the local explanation of the primary instance is used as a projection basis.
    With the use of a <em>radial tour</em> we can select a feature to its contribution.
    By altering the projection basis, we can explore how sensitive a feature is to the structure identified by the local explanation.
    We use this to examine under which contributions the prediction is supported."),
  br(), br(),
  img(src = "cheem_workflow.png"),
  p('(top) Wickham, H. & Grolemund, G. (2016). R for data science. ', a(href = 'https://r4ds.had.co.nz/', 'https://r4ds.had.co.nz/', .noWS = "outside"), .noWS = c("after-begin", "before-end")),
  p('(bottom) Biecek P. & Burzykowski T. (2020). Explanatory Model Analysis. ', a(href = 'https://ema.drwhy.ai/', 'https://ema.drwhy.ai/', .noWS = "outside"), .noWS = c("after-begin", "before-end")),
  p('(blue overlay) Purposed analysis in terms of workflow and model specificity.'),
  br(), br(),
  h2("Namesake:"),
  HTML("The Trees of Cheem are a fictional race of tree-based humanoids in the Dr. Who universe. 
       The initial application applies tree SHAP (a local explanation of tree-based models with the <b>treeshap</b> package) 
       and was inspired by explainer from <b>DALEX</b>, a reference to Dr. Who lore."),
  br(),
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
