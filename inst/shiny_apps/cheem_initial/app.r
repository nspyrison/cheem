# Penguin classification -----
## ./apps/cheem_classification/app.r
#' 
#' @author Nicholas Spyrison
#' Aug 2021
source("ui.r", local = TRUE, encoding = "utf-8")


server <- function(input, output, session){
  ## Reactives ----
  
  ## LOAD_LS, pass the layer_ls from the selected data
  load_ls <- reactive({
    req(input$dat_char)
    dat <- input$dat_char
    if(!(dat %in% c("toy classification", "penguins", "fifa",
                    "apartments", "diabetes (wide)", "diabetes (long)")))
      stop("data string not matched.")
    if(dat == "toy classification")
      load("./data/2preprocess_toy_classification.RData", envir = globalenv())
    if(dat == "penguins")
      load("./data/1preprocess_penguins.RData", envir = globalenv())
    if(dat == "fifa")
      load("./data/3preprocess_fifa.RData", envir = globalenv())
    if(dat == "apartments")
      load("./data/4preprocess_apartments.RData", envir = globalenv())
    if(dat == "diabetes (wide)")
      load("./data/6preprocess_diabetes_wide.RData", envir = globalenv())
    if(dat == "diabetes (long)")
      load("./data/6preprocess_diabetes_long.RData", envir = globalenv())
    return(layer_ls)
  })
  load_ls_d <- load_ls %>%  debounce(millis = 100L)
 
  ## PIMARY & COMPARISON OBS, & thier debounced versions
  #### debounce; try to reduce multiple renders as a 3 digit number is typed
  primary_obs <- reactive({
    req(input$primary_obs)
    input$primary_obs
  })
  primary_obs_d <- primary_obs %>% debounce(millis = 1000L)
  comparison_obs <- reactive({
    req(input$comparison_obs)
    input$comparison_obs
  })
  comparison_obs_d <- comparison_obs %>% debounce(millis = 1000L)
  
  ## BASIS FROM local explanation of promary obs
  bas <- reactive({
    req(load_ls())
    shap_df <- load_ls()$shap_df
    bas <- basis_local_attribution(shap_df, primary_obs())
    return(bas)
  })
  
  
  ## Observers: updating inputs in the ui -----
  
  ## UPDATE PRIMARY/COMPARISON OBSERVATIONS
  observe({
    req(load_ls())
    .n <- load_ls()$decode_df %>% nrow()
    req(input$dat_char)
    dat <- input$dat_char
    if(!(dat %in% c("toy classification", "penguins", "fifa",
                    "apartments", "diabetes (wide)", "diabetes (long)")))
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
    if(dat == "apartments"){
      primary_obs    <- 485L
      comparison_obs <- 487L
    }
    if(dat == "diabetes (wide)"){
      primary_obs    <- 121L
      comparison_obs <- 201L
    }
    if(dat == "diabetes (long)"){
      primary_obs    <- 616L
      comparison_obs <- 215L
    }
    
    ## Return
    updateNumericInput(
      session, "primary_obs",
      label = "Primary observation rownum, ('*' point):",
      min = 1L, max = .n, step = 1L, value = primary_obs)
    updateNumericInput(
      session, "comparison_obs",
      label = "Comparison observation rownum, ('x' ponit):",
      min = 1L, max = .n, step = 1L, value = comparison_obs)
  })
  
  ## UPDATE INCLUSION VARIABLES
  observe({
    req(bas())
    bas <- bas()
    opts <- rownames(bas)
    updateCheckboxGroupInput(session, "inc_vars", label = "Inclusion variables",
                             choices = opts, selected = opts, inline = TRUE)
  })
  
  ## UPDATE MANIPULATION VARIABLE INPUT
  observe({
    req(input$inc_vars)
    req(load_ls())
    req(primary_obs())
    
    opts <- input$inc_vars
    layer_ls <- load_ls()
    shap_df <- layer_ls$shap_df[, -ncol(layer_ls$shap_df)]
    clas <- layer_ls$decode_df$class
    
    ## Median values of the observed class.
    expect_bas <- apply(shap_df[clas == clas[primary_obs()], ], 2L, median) %>%
      matrix(ncol = 1L, dimnames = list(colnames(shap_df), "SHAP"))
    .diff <- abs(expect_bas - bas)
    sel <- opts[which(.diff == max(.diff))]
    
    updateSelectInput(session, "manip_var_nm",
                      label = "Manipulation variable:",
                      choices  = opts,
                      selected = sel)
  })
  
  ## Outputs -----
  
  ### MAHA KURTOSIS TEXT, may be off at the moment.
  output$kurtosis_text <- renderPrint({
    req(load_ls())
    req(input$do_include_maha_qq)
    .lines <- ""
    if(as.logical(input$do_include_maha_qq) == TRUE)
      .lines <- c("Moments of the Mahalanobis distances of data- and SHAP-space respectively:", "",
        unique(load_ls()$plot_df[, c("ggtext")])[-1L])
    writeLines(.lines)
  })
  outputOptions(output, "kurtosis_text",
                suspendWhenHidden = FALSE, priority = -199L) ## Eager evaluation
  
  ### GLOBAL VIEW PLOTLY
  output$linked_plotly <- plotly::renderPlotly({
    req(load_ls())
    req(primary_obs())
    req(comparison_obs())
    
    linked_plotly_func(
      load_ls(), primary_obs(), comparison_obs(),
      do_include_maha_qq = as.logical(input$do_include_maha_qq))
  })
  outputOptions(output, "linked_plotly",
                suspendWhenHidden = FALSE, priority = -200L) ## Eager evaluation
  
  
  ### RESIDUAL PLOT, wants to go to residual tour
  output$residual_plot <- plotly::renderPlotly({
    req(load_ls())
    req(primary_obs())
    req(comparison_obs())
    decode_df <- load_ls()$decode_df
    prim_obs <- primary_obs()
    comp_obs <- comparison_obs()
    
    ## Index of selected data:
    .d <- plotly::event_data("plotly_selected") ## selected in global view
    .idx_rownums <- TRUE
    if(is.null(.d) == FALSE)
      .idx_rownums <- decode_df$rownum %in% c(.d$key, prim_obs, comp_obs)
    
    ## Filter to rownum and select columns
    active_df <- decode_df[.idx_rownums, ]
    bkg_df <- decode_df[!.idx_rownums, ]
    .alpha <- logistic_tform(nrow(layer_ls$decode_df), mid_pt = 500L)
    .is_classification <- problem_type(decode_df$y) == "classification"
    .pred_clas <- as.factor(FALSE) ## dummy pred_clas for regression
    
    ## Red misclassified points, if applicable
    pts_highlight <- list()
    if(.is_classification == TRUE){
      .pred_clas <- active_df$predicted_class
      .idx_misclas <- which(active_df$is_misclassified == TRUE)
      if(sum(.idx_misclas) > 0L)
        pts_highlight <- c(
          pts_highlight,
          ggplot2::geom_point(
            ggplot2::aes(y, residual), active_df[.idx_misclas, ],
            color = "red", fill = NA, shape = 21L, size = 3L)
        )
    }
    ## Comp point
    if(is.null(prim_obs) == FALSE)
      pts_highlight <- c(
        pts_highlight,
        geom_point(aes(y, residual),
          data = active_df[active_df$rownum == comp_obs, ],
          color = "black", size = 3L, shape = 4L, alpha = 0.6))
    ## Primary point
    if(is.null(prim_obs) == FALSE)
      pts_highlight <- c(
        pts_highlight,
        geom_point(aes(prediction, residual),
          data = active_df[active_df$rownum == prim_obs, ],
          color = "black", size = 5L, shape = 8L, alpha = 0.8))
    bkg_pts <- NULL
    if(nrow(bkg_df) > 0L)
      bkg_pts <- geom_point(aes(y, residual, shape = .pred_clas),
                            bkg_df, color = "grey80", alpha = .alpha)
    
    ## Plot
    gg <- ggplot() +
      ## Not selected background
      bkg_pts +
      ## Selected_df
      geom_point(aes(y, residual, label = tooltip,
                     color = .pred_clas, shape = .pred_clas),
                 active_df, alpha = .alpha) +
      pts_highlight +
      theme_bw() +
      scale_color_brewer(palette = "Dark2") +
      labs(x = "Prediction", y = "Residual")
    
    ## Return
    plotly::ggplotly(p = gg, tooltip = "label") %>%
      ## Remove button bar and zoom box
      plotly::config(displayModeBar = FALSE,
                     modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")) %>%
      ## Remove legends and axis lines
      plotly::layout(showlegend = FALSE, dragmode = FALSE,
                     xaxis = list(scaleanchor = "y", scalaratio = 1L))
  })
  outputOptions(output, "residual_plot",
                suspendWhenHidden = FALSE, priority = -300L) ## Eager evaluation
  
  
  ## FIANL TOUR
  output$cheem_tour <- plotly::renderPlotly({
    req(bas())
    req(load_ls())
    req(input$manip_var_nm)
    req(primary_obs())
    req(comparison_obs())
    req(input$do_add_pcp_segments)
    
    ## Filter to only selected data:
    .d <- plotly::event_data("plotly_selected") ## What plotly sees as selected
    .idx_rownums <- TRUE
    if(is.null(.d) == FALSE)
      .idx_rownums <- load_ls()$decode_df$rownum %in% .d$key
    
    bas <- bas()
    mv_nm <- input$manip_var_nm
    if(mv_nm %in% rownames(bas) == FALSE){
      message(paste0(
        "output$cheem_tour: input$manip_var_nm = '", mv_nm,
        "' wasn't in the basis bas(). Shiny tried to update cheem_tour before valid manip_var_nm was passed..."))
      return(NULL)
    }
    
    ggt <- radial_cheem_ggtour(
      load_ls(), bas, mv_nm,
      primary_obs(), comparison_obs(),
      do_add_pcp_segments = as.logical(input$do_add_pcp_segments),
      rownum_idx = .idx_rownums)
    spinifex::animate_plotly(ggt) ## %>% plotly::toWebGL() ## faster, but more issues than plotly...
  }) ## Lazy eval, heavy work, let the other stuff calculate first.
  outputOptions(output, "cheem_tour", ## LAZY eval, do last
                suspendWhenHidden = TRUE, priority = -9999L)
  
  ### DT table of selected data
  output$selected_df <- DT::renderDT({ ## Original data of selection
    .d <- plotly::event_data("plotly_selected") ## What plotly sees as selected
    if(is.null(.d)) return(NULL)
    .df <- load_ls()$decode_df
    .df_r <- data.frame(lapply(
      .df, function(c) if(is.numeric(c)) round(c, 2L) else c))
    return(DT::datatable(.df_r[.df_r$rownum %in% .d$key, ], rownames = FALSE))
  })
  outputOptions(output, "selected_df",
                suspendWhenHidden = FALSE, priority = 100L) ## Eager evaluation
} ## Close function, assigning server object.

shinyApp(ui = ui, server = server)
