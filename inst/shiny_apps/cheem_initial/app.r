# Penguin classification -----
## ./apps/cheem_classification/app.r
#' 
#' @author Nicholas Spyrison
#' Aug 2021
source("ui.r", local = TRUE, encoding = "utf-8")


server <- function(input, output, session){
  ## Reactives ----
  
  ## LOAD_LS, pass the cheem_ls from the selected data
  load_ls <- reactive({
    req(input$dat_char)
    dat <- input$dat_char
    if(!(dat %in% c("toy classification", "penguins", "fifa",
                    "apartments", "diabetes (wide)", "diabetes (long)")))
      stop("data string not matched.")
    
    ### BY PRODUCT: UPDATE PRIM/COMP OBS
    if(dat == "toy classification"){
      primary_obs    <- 18L
      comparison_obs <- 111L
    }
    if(dat == "penguins"){
      primary_obs    <- 15L
      comparison_obs <- 282L
    }
    if(dat == "fifa"){
      primary_obs    <- 1L
      comparison_obs <- 8L
    }
    if(dat == "apartments"){
      primary_obs    <- 485L
      comparison_obs <- 487L
    }
    if(dat == "diabetes (wide)"){
      primary_obs    <- 123L
      comparison_obs <- 237L
    }
    if(dat == "diabetes (long)"){
      primary_obs    <- 479L
      comparison_obs <- 674L
    }
    .n_max <- 5000L
    updateNumericInput(
      session, "primary_obs",
      label = "Primary observation rownum, ('*' point):",
      min = 1L, max = .n_max, step = 1L, value = primary_obs)
    updateNumericInput(
      session, "comparison_obs",
      label = "Comparison observation rownum, ('x' ponit):",
      min = 1L, max = .n_max, step = 1L, value = comparison_obs)
    
    ### CHEEM_LS TO RETRUN
    # all loaded at the top of ui.r
    if(dat == "toy classification")
      ret <- toy_ls
    if(dat == "penguins")
      ret <- penguins_ls
    if(dat == "fifa")
      ret <- fifa_ls
    if(dat == "apartments")
      ret <- apartments_ls
    if(dat == "diabetes (wide)")
      ret <- diabetes_wide_ls
    if(dat == "diabetes (long)")
      ret <- diabetes_long_ls
    
    ## BY PRODUCT: UPDATE INCLUSION VARIABLES
    var_nms <- colnames(ret$attr_df)
    updateCheckboxGroupInput(session, "inc_vars", label = "Inclusion variables",
                             choices = var_nms, selected = var_nms, inline = TRUE)
    
    ## Return loaded cheem_ls
    return(ret)
  })
  #load_ls_d <- load_ls %>%  debounce(millis = 100L)
 
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
    req(input$inc_vars)
    req(primary_obs())
    attr_df <- load_ls()$attr_df
    if(all(input$inc_vars %in% colnames(attr_df)) == FALSE){
      message("bas(): bas tried to react before inc_vars updated...")
      return()
    }
    
    bas <- basis_local_attribution(
      attr_df[, input$inc_vars], primary_obs())
    return(bas)
  })
  
  
  
  ## Observers: updating inputs in the ui -----
  
  ## UPDATE MANIPULATION VARIABLE INPUT
  observeEvent({
    bas()
    primary_obs()
  },{
    req(bas())
    req(input$inc_vars)
    req(load_ls())
    req(primary_obs())
    cheem_ls <- load_ls()
    attr_df <- cheem_ls$attr_df
    opts <- input$inc_vars
    clas <- cheem_ls$decode_df$class
    bas <- bas()
    .prob_type <- cheem_ls$problem_type
    .prim_obs <- primary_obs()
    .comp_obs <- comparison_obs
    
    ## Select var with largest diff of median values between classes.
    if(.prob_type == "classification"){
      expect_bas <- apply(
        attr_df[clas == clas[.prim_obs], opts], 2L, median) %>%
        matrix(ncol = 1L, dimnames = list(opts, "SHAP"))
      .diff <- abs(expect_bas - bas)
      sel <- opts[which(.diff == max(.diff))]
    }else if(.prob_type == "regression"){
      prim_bs  <- attr_df[.comp_obs,, drop = FALSE]
      comp_bas <- attr_df[.comp_obs,, drop = FALSE]
      .diff <- abs(comp_bas - prim_bs)
      sel <- opts[which(.diff == max(.diff))]
    } else stop("update manipulation variable: problem type not fit.")
    
    
    
    updateSelectInput(session, "manip_var_nm",
                      label = "Manipulation variable:",
                      choices  = opts,
                      selected = sel)
  }, priority = 50L)
  
  ## Outputs -----
  output$desc_rows <- renderText({
    req(input$dat_char)
    dat <- input$dat_char
    if(!(dat %in% c("toy classification", "penguins", "fifa",
                    "apartments", "diabetes (wide)", "diabetes (long)")))
      stop("data string not matched.")
    ## Load data:
    if(dat == "toy classification"){
      he <- h3("Simulated triangle vertices")
      l1 <- p("- 420 obsvations of 4 dimensions (2 signal, 2 noise, X's), and cluster grouping (Classification Y)")
      l2 <- p("1) Create a random forest model classifying cluster level, given the continuous variables.")
    }
    if(dat == "penguins"){
      he <- h3("Palmer penguins")
      l1 <- p("- 214 penguin observations of 4 continuous physical measurements (X's) and species of penguin (Classification Y).")
      l2 <- p("1) Create a random forest model classifying species from the physical measurements.")
    }
    if(dat == "fifa"){
      he <- h3("FIFA soccer players, 2020 season")
      l1 <- p("- 5000 player observations of 9 explanatory skill 'aspects' (X's) and wages [2020 Euros] (Regression Y)")
      l2 <- p("1) Create a random forest model regressing continuous wages from the skill aggregates.")
    }
    if(dat == "apartments"){
      he <- h3("DALEX::apartments, sinthetic 'anscombe quartet-like' data of appartment prices")
      l1 <- p("- 1000 appartment observations, of 4 explanatory variables, 1 class, Y is price per square meter.")
      l2 <- p("1) Create a random forest model regressing appartment price (/sq_m) the 4 X and the district's rank of price variation.")
    }
    if(dat == "diabetes (wide)"){
      he <- h3("Pima Indians Diabetes (wide)")
      l1 <- p("- 392 observations, of *8* explanatory variables, 1 class/Y; presence/abence of diabetes.")
      l2 <- p("1) Create a random forest model regressing the existence of diabetes from the *8* X variables.")
    }
    if(dat == "diabetes (long)"){
      he <- h3("Pima Indians Diabetes (long)")
      l1 <- p("- *724* observations, of *6* explanatory variables, 1 class/Y; presence/abence of diabetes.")
      l2 <- p("1) Create a random forest model regressing the existence of diabetes from the 6 X variables.")
    }
    ## Return
    HTML(paste(he, l1, l2))
  })
  outputOptions(output, "desc_rows",
                suspendWhenHidden = FALSE, priority = 90L) ## Eager evaluation
  
  ### GLOBAL VIEW PLOTLY
  output$linked_global_view <- plotly::renderPlotly({
    req(load_ls())
    req(primary_obs())
    req(comparison_obs())
    suppressWarnings( ## suppress "Coordinate system already present..." from 2x draw_basis
      linked_global_view(
        load_ls(), primary_obs(), comparison_obs(),
        height_px = 480, width_px = 960L))
  })
  outputOptions(output, "linked_global_view",
                suspendWhenHidden = FALSE, priority = -200L) ## Eager evaluation
  
  
  ### RESIDUAL PLOT, wants to become a residual tour
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
    .alpha <- logistic_tform(nrow(decode_df), mid_pt = 500L)
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
            ggplot2::aes(prediction, residual), active_df[.idx_misclas, ],
            color = "red", fill = NA, shape = 21L, size = 3L)
        )
    }
    ## Comp point
    if(is.null(comp_obs) == FALSE)
      pts_highlight <- c(
        pts_highlight,
        geom_point(aes(prediction, residual),
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
      bkg_pts <- geom_point(aes(prediction, residual, shape = .pred_clas),
                            bkg_df, color = "grey80", alpha = .alpha)
    
    ## Plot
    gg <- ggplot() +
      ## Not selected background
      bkg_pts +
      ## Selected_df
      geom_point(aes(prediction, residual, label = tooltip,
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
  
  
  # ## TOUR, GGANIMATE VERSION
  # output$cheem_tour_gganimate <- renderImage({
  #   req(bas())
  #   req(load_ls())
  #   req(input$manip_var_nm)
  #   req(primary_obs_d())
  #   req(comparison_obs_d())
  #   req(input$do_add_pcp_segments)
  #   load_ls <- load_ls()
  #   bas <- bas()
  #   mv_nm <- input$manip_var_nm
  #   if(mv_nm %in% rownames(bas) == FALSE){
  #     message(paste0(
  #       "output$cheem_tour: input$manip_var_nm = '", mv_nm,
  #       "' wasn't in the basis bas(). Shiny tried to update cheem_tour before valid manip_var_nm was passed..."))
  #     return(NULL)
  #   }
  #   
  #   ## Filter to only selected data:
  #   .d <- plotly::event_data("plotly_selected") ## What plotly sees as selected
  #   .idx_rownums <- TRUE
  #   if(is.null(.d) == FALSE)
  #     .idx_rownums <- load_ls$decode_df$rownum %in% .d$key
  #   
  #   ## Now make the animation
  #   ggt <- radial_cheem_ggtour(
  #     load_ls, bas, mv_nm,
  #     primary_obs(), comparison_obs(),
  #     do_add_pcp_segments = as.logical(input$do_add_pcp_segments),
  #     rownum_idx = .idx_rownums, inc_vars = input$inc_vars,
  #     angle = .2)
  #   ## may improve render time OF PLOTLY:
  #   # %>% toWebGL() ?
  #   # %>% partial_bundle() ?
  #   
  #   ## A temp file to save the output, will be removed later in renderImage
  #   outfile <- tempfile(fileext = ".mp4")
  #   animate_gganimate(
  #     ggt,
  #     # height = 720L, #width = 4,
  #     # units = "px", ## "px", "in", "cm", or "mm."
  #     #res = 300L, ## resolution (dpi)
  #     render = gganimate::av_renderer(outfile))
  #   ## Return a list containing the filename
  #   list(src = outfile, contentType = "image/mp4")
  # }, deleteFile = TRUE)
  # outputOptions(output, "cheem_tour_gganimate", ## LAZY eval, do last
  #               suspendWhenHidden = TRUE, priority = -9999L)
  ## TOUR, PLOTLY VERSION: too many issues trying .mp4 gganimate
  output$cheem_tour_plotly <- plotly::renderPlotly({
    req(bas())
    req(load_ls())
    req(input$manip_var_nm)
    req(primary_obs())
    req(comparison_obs())
    req(input$do_add_pcp_segments)
    req(input$inc_vars)
    
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
        "' wasn't in the basis. Shiny tried to update cheem_tour before manip_var_nm..."))
      return(NULL)
    }
    
    # browser() # CAUSED issue when adding basis to global view.
    # # debugonce(array2df)
    # debugonce(proto_basis1d_distribution)
    ggt <- radial_cheem_ggtour(
      load_ls(), bas, mv_nm,
      primary_obs(), comparison_obs(),
      rownum_idx = .idx_rownums, inc_vars = input$inc_vars)
    spinifex::animate_plotly(ggt) ## %>% plotly::toWebGL() ## faster, but more issues than plotly...
  }) ## Lazy eval, heavy work, let the other stuff calculate first.
  outputOptions(output, "cheem_tour_plotly", ## LAZY eval, do last
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
                suspendWhenHidden = FALSE, priority = 10L) ## Eager evaluation
} ## Close function, assigning server object.

shinyApp(ui = ui, server = server)
