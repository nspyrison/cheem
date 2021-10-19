# Penguin classification -----
## ./apps/cheem_classification/app.r
#' 
#' @author Nicholas Spyrison
#' Aug 2021
source("ui.r", local = TRUE, encoding = "utf-8")


server <- function(input, output, session){
  ## Reactives ----
  #### No eager evaluation of reactive functions, only outputs.
  load_ls <- reactive({
    req(input$dat_char, cancelOutput = TRUE)
    dat <- input$dat_char
    if(!(dat %in% c("toy classification", "penguins", "fifa", "appartments")))
      stop("data string not matched.")
    if(dat == "toy classification")
      load("./data/2preprocess_simulation.RData", envir = globalenv())
    if(dat == "penguins")
      load("./data/1preprocess_penguins.RData", envir = globalenv())
    if(dat == "fifa")
      load("./data/3preprocess_fifa.RData", envir = globalenv())
    if(dat == "appartments")
      load("./data/4preprocess_appt.RData", envir = globalenv())
    return(layer_ls)
  })
  
  output$input__dat_desc <- renderUI({
    req(input$dat_char, cancelOutput = TRUE)
    dat <- input$dat_char
    if(!(dat %in% c("toy classification", "penguins", "fifa", "appartments")))
      stop("data string not matched.")
    ## Load data:
    if(dat == "toy classification")
      desc_rows <- list(
        h4("Simulated triangle vertices"),
        p("1) 420 obsvations of 4 dimensions (2 signal, 2 noise, X's), and cluster grouping (Classification Y)"),
        p("   - Each cluster is spherical and has 140 observations"),
        p("2) Create a RF model classifying cluster level, given the continuous variables.")
      )
    if(dat == "penguins")
      desc_rows <- list(
        h4("Palmer penguins"),
        p("1) 214 penguin observations of 4 continuous physical measurements (X's) and species of penguin (Classification Y)."),
        p("2) Create a RF model classifying species from the physical measurements.")
      )
    if(dat == "fifa")
      desc_rows <- list(
        h4("FIFA soccer players, 2020 season"),
        p("1) 5000 player observations of 9 explanatory skill 'aspects' (X's) and wages [2020 Euros] (Regression Y)"),
        p("2) Create a RF model regressing continuous wages from the skill aggregates.")
      )
    if(dat == "appartments")
      desc_rows <- list(
        h4("DALEX::appartments, sinthetic data of appartment prices"),
        p("1) 1000 appartment observations, of 4 explanatory variables, 1 class, Y is price per square meter."),
        p("2) Create a RF model regressing appartment price (/sq_m) the 4 X and the district's rank of price variation.")
      )
    return(desc_rows)
  })
  
  bas <- reactive({
    req(load_ls(), cancelOutput = TRUE)
    shap_df <- load_ls()$shap_df
    bas <- basis_local_attribution(shap_df, primary_obs_d())
    return(bas)
  })
  
  ## output: inputs in the ui -----
  output$input__shap.comparison_obs <- renderUI({
    req(load_ls(), cancelOutput = TRUE)
    .n <- load_ls()$decode_df %>% nrow()
    req(input$dat_char, cancelOutput = TRUE)
    dat <- input$dat_char
    if(!(dat %in% c("toy classification", "penguins", "fifa", "appartments")))
      stop("data string not matched.")
    
    ## Initialize to hard-coded hand picked examples.
    if(dat == "toy classification"){
      primary_obs    <- 18L
      comparison_obs <- 111L
    }
    if(dat == "penguins"){
      primary_obs    <- 177L
      comparison_obs <- 131L
    }
    if(dat == "fifa"){ ## TODO: WILL BE WRONG OBS AFTER THINNING:
      primary_obs    <- 1L ## L Messi
      comparison_obs <- 8L ## V. van Dijk
    }
    if(dat == "appartments"){
      primary_obs    <- 1L
      comparison_obs <- 2L
    }
    
    ## Return
    fluidRow(
      column(4L, numericInput(
        "primary_obs", label = "SHAP values of row number, '*' shape:",
        min = 1L, max = .n, step = 1L, value = primary_obs)),
      column(4L, numericInput(
        "comparison_obs", label = "Comparison row number, 'x' shape:",
        min = 1L, max = .n, step = 1L, value = comparison_obs)),
      column(4L)
    )
  })
  outputOptions(output, "input__shap.comparison_obs", suspendWhenHidden = FALSE) ## Eager evaluation
  ##"Debounce" shap/comparison_obs; 
  #### ie, Reduces making multiple animations as someone types in a 3 digit number 
  primary_obs <- reactive({
    req(input$primary_obs, cancelOutput = TRUE)
    input$primary_obs
  })
  primary_obs_d <- primary_obs %>% debounce(millis = 1000L)
  comparison_obs <- reactive({
    req(input$comparison_obs, cancelOutput = TRUE)
    input$comparison_obs
  })
  comparison_obs_d <- comparison_obs %>% debounce(millis = 1000L)
  
  output$input__manip_var_nm <- renderUI({
    req(bas(), cancelOutput = TRUE)
    bas <- bas()
    
    opts <- rownames(bas)
    shap_df <- load_ls()$shap_df[, -ncol(load_ls()$shap_df)]
    clas <- load_ls()$decode_df$class
    
    ## Median values of the observed class.
    expect_bas <- apply(shap_df[clas == clas[primary_obs_d()], ], 2L, median) %>%
      matrix(ncol = 1L, dimnames = list(colnames(shap_df), "SHAP"))
    .diff <- abs(expect_bas - bas)
    sel <- opts[which(.diff == max(.diff))]
    
    selectInput("manip_var_nm",
                label = "Manipulation variable:",
                choices  = opts,
                selected = sel)
  })
  outputOptions(output, "input__manip_var_nm", suspendWhenHidden = FALSE) ## Eager evaluation
  
  ## Plot outputs -----
  output$kurtosis_text <- renderPrint({
    req(load_ls(), cancelOutput = TRUE)
    req(input$do_include_maha_qq, cancelOutput = TRUE)
    .lines <- ""
    if(as.logical(input$do_include_maha_qq) == TRUE)
      .lines <- c("Moments of the Mahalanobis distances of data- and SHAP-space respectively:", "",
        unique(load_ls()$plot_df[, c("ggtext")])[-1L])
    writeLines(.lines)
  })
  outputOptions(output, "kurtosis_text", suspendWhenHidden = FALSE) ## Eager evaluation
  
  output$linked_plotly <- plotly::renderPlotly({
    req(load_ls(), cancelOutput = TRUE)
    req(primary_obs_d(), cancelOutput = TRUE)
    req(comparison_obs_d(), cancelOutput = TRUE)
    
    linked_plotly_func(
      load_ls(), primary_obs_d(), comparison_obs_d(),
      do_include_maha_qq = as.logical(input$do_include_maha_qq))
  })
  outputOptions(output, "linked_plotly", suspendWhenHidden = FALSE) ## Eager evaluation
  
  output$input__linked_plotly = renderUI({
    ## This is dimension of spacer, figure dim's set in args of cobs_n_plot_func::linked_plotly_func
    height = 640L ## Init, height with qq maha.
    if(as.logical(input$do_include_maha_qq) == FALSE) height <- height / 2L
    plotly::plotlyOutput("linked_plotly", width = "100%", height = paste0(height))
  })
  outputOptions(output, "input__linked_plotly", suspendWhenHidden = FALSE) ## Eager evaluation
  
  output$residual_plot <- plotly::renderPlotly({
    req(load_ls(), cancelOutput = TRUE)
    req(primary_obs_d(), cancelOutput = TRUE)
    req(comparison_obs_d(), cancelOutput = TRUE)
    decode_df <- load_ls()$decode_df
    
    ## Index of selected data:
    .d <- plotly::event_data("plotly_selected") ## What plotly sees as selected
    .idx_rownums <- TRUE
    if(is.null(.d) == FALSE)
      .idx_rownums <- decode_df$rownum %in% .d$key
    
    ## Filter to rownum and select columns
    decode_df <- decode_df[.idx_rownums, ]
    df <- decode_df[
      .idx_rownums, c("y", "residual", "class", "tooltip")]
    .pred_clas <- as.factor(FALSE) ## If regression; dummy pred_clas
    .is_classification <- problem_type(df$y) == "classification"
    
    ## Red misclassified points, if applicable
    pts_highlight <- list()
    if(.is_classification == TRUE){
      .pred_clas <- layer_ls$decode_df$predicted_class
      .idx_misclas <- which(decode_df$is_misclassified == TRUE)
      if(sum(.idx_misclas) > 0L)
        pts_highlight <- c(
          pts_highlight,
          ggplot2::geom_point(
            ggplot2::aes(y, residual), df[.idx_misclas, ],
            color = "red", fill = NA, shape = 21L, size = 3L, alpha = .alpha)
        )
    }
    ## Primary point
    prim_obs <- primary_obs_d()
    if(is.null(prim_obs) == FALSE)
      pts_highlight <- c(
        pts_highlight,
        geom_point(#aes(color = .pred_clas[decode_df$rownum == prim_obs]),
                   data = decode_df[decode_df$rownum == prim_obs, ],
                   color = "black", size = 5L, shape = 4L, alpha = 0.5))
    ## Comp point
    comp_obs <- comparison_obs_d()
    if(is.null(prim_obs) == FALSE)
      pts_highlight <- c(
        pts_highlight,
        geom_point(#aes(color = .pred_clas[.idx_comp]),
                   data = decode_df[decode_df$rownum == comp_obs, ],
                   color = "black", size = 3L, shape = 8L, alpha = 1L))
    
    ## Plot
    gg <- ggplot(df, aes(y, residual, label = tooltip,
                         color = .pred_clas, shape = .pred_clas)) +
      geom_point() +
      pts_highlight +
      theme_bw() +
      scale_color_brewer(palette = "Dark2") +
      labs(x = "Y, response variable", y = "Residual, Y - predition")
    
    ## Return
    plotly::ggplotly(p = gg, tooltip = "label") %>%
      ## Remove button bar and zoom box
      plotly::config(displayModeBar = FALSE,
                     modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")) %>%
      ## Remove legends and axis lines
      plotly::layout(showlegend = FALSE, dragmode = FALSE,
                     xaxis = list(scaleanchor = "y", scalaratio = 1L))
  })
  outputOptions(output, "residual_plot", suspendWhenHidden = FALSE) ## Eager evaluation
  
  
  output$cheem_tour <- plotly::renderPlotly({
    req(bas(), cancelOutput = TRUE)
    req(load_ls(), cancelOutput = TRUE)
    req(input$manip_var_nm, cancelOutput = TRUE)
    req(primary_obs_d(), cancelOutput = TRUE)
    req(comparison_obs_d(), cancelOutput = TRUE)
    req(input$do_add_pcp_segments, cancelOutput = TRUE)
    
    ## Filter to only selected data:
    .d <- plotly::event_data("plotly_selected") ## What plotly sees as selected
    .idx_rownums <- TRUE
    if(is.null(.d) == FALSE)
      .idx_rownums <- load_ls()$decode_df$rownum %in% .d$key
    
    # if(input$dat_char == "fifa"){ ## If fifa data
    #   ## Want to browse 2D tour of fifa data
    #   browser()
    #   debugonce(radial_cheem_ggtour)
    # }
    
    ggt <- radial_cheem_ggtour(
      load_ls(), bas(), input$manip_var_nm,
      primary_obs_d(), comparison_obs_d(),
      do_add_pcp_segments = as.logical(input$do_add_pcp_segments),
      rownum_idx = .idx_rownums)
    spinifex::animate_plotly(ggt)
  }) ## Lazy eval, heavy work, let the other stuff calculate first.
  
  ## Data selected in pca_embed_plotly -----
  output$selected_df <- DT::renderDT({ ## Original data of selection
    .d <- plotly::event_data("plotly_selected") ## What plotly sees as selected
    if(is.null(.d)) return(NULL)
    .df <- load_ls()$decode_df
    return(DT::datatable(.df[.df$rownum %in% .d$key, ], rownames = FALSE))
  })
  outputOptions(output, "selected_df", suspendWhenHidden = FALSE) ## Eager evaluation
  
  ## Message of the cobs row numbers
  output$cobs_msg <- renderText(attr(load_ls(), "cobs_msg"))
  outputOptions(output, "cobs_msg", suspendWhenHidden = FALSE) ## Eager evaluation
} ## Close function, assigning server object.

shinyApp(ui = ui, server = server)
