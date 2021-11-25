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
    dat <- req(input$dat_char)
    if(dat %in% expected_data_char == FALSE)
      stop("data string not matched.")
    
    if(dat == "toy classification"){
      ret      <- toy_class_ls
      prim_obs <- 18L
      comp_obs <- 119L
    }else if(dat == "penguins classification"){
      ret      <- penguins_ls
      prim_obs <- 124L
      comp_obs <- 86L
    }else if(dat == "chocolates classification"){
      ret      <- chocolates_ls
      prim_obs <- 22L
      comp_obs <- 34L
    }else if(dat == "toy quad regression"){
      ret      <- toy_quad_reg_ls
      prim_obs <- 11L
      comp_obs <- 121L
    }else if(dat == "toy trig regression"){
      ret      <- toy_trig_reg_ls
      prim_obs <- 87L
      comp_obs <- 102L
    }else if(dat == "fifa regression"){
      ret      <- fifa_ls
      prim_obs <- 1L
      comp_obs <- 8L
    }else if(dat == "ames housing 2018 regression"){
      ret      <- ames2018_ls
      prim_obs <- 170L
      comp_obs <- 220L
    }else{ ## _ie._ user loaded data; no priors of good obs to pick.
      file_path <- req(input$in_cheem_ls$datapath)
      tryCatch(ret <- readRDS(file_path),
               error = function(e) stop(safeError(e)))
      prim_obs <- 1L
      comp_obs <- 2L
    }
    # }else if(dat == "apartments"){
    #   ret            <- apartments_ls
    #   prim_obs <- 485L
    #   comp_obs <- 487L
    # }else if(dat == "diabetes (wide)"){
    #   ret            <- diabetes_wide_ls
    #   prim_obs <- 123L
    #   comp_obs <- 237L
    # }else if(dat == "diabetes (long)"){
    #   ret      <- diabetes_long_ls
    #   prim_obs <- 479L
    #   comp_obs <- 674L
    
    ### BY PRODUCT: UPDATE PRIM/COMP OBS
    updateNumericInput(
      session, "primary_obs",
      label = "Primary observation rownum, ('*' point):",
      min = 1L, max = 1e6L, step = 1L, value = prim_obs)
    updateNumericInput(
      session, "comparison_obs",
      label = "Comparison observation rownum, ('x' point):",
      min = 1L, max = 1e6L, step = 1L, value = comp_obs)
    
    ## BY PRODUCT: UPDATE INCLUSION VARIABLES
    var_nms <- colnames(ret$attr_df)
    
    updateCheckboxGroupInput(session, "inc_var_nms", label = "Variables to include:",
                             choices = var_nms, selected = var_nms, inline = TRUE)
    
    ## Return loaded cheem_ls
    return(ret)
  })
  
  ## PIMARY & COMPARISON OBS
  #### Debounce the plots on the inputs imo.
  primary_obs <- reactive({
    req(input$primary_obs)
    input$primary_obs
  })
  comparison_obs <- reactive({
    req(input$comparison_obs)
    input$comparison_obs
  })

  ## Basis; the local explanation's attributon of the primary obs
  bas <- reactive({
    attr_df     <- req(load_ls()$attr_df)
    inc_var_nms <- req(input$inc_var_nms)
    prim_obs    <- req(primary_obs())
    if(all(inc_var_nms %in% colnames(attr_df)) == FALSE){
      message("bas(): bas tried to react before inc_var_nms updated...")
      return()
    }
    bas <- basis_attr_df(attr_df[, inc_var_nms, drop = FALSE], prim_obs)
    return(tourr::orthonormalise(bas))
  })
  
  sel_rownums <- reactive({
    ## Row NUMBER index of data selected in linked global view
    .d <- plotly::event_data("plotly_selected")
    if(is.null(.d)) return(NULL)
    return(as.integer(.d$key))
  })
  
  ### cheem_ggtour() -----
  cheem_ggtour <- reactive({
    bas         <- req(bas())
    cheem_ls    <- req(load_ls())
    prim_obs    <- req(primary_obs())
    comp_obs    <- req(comparison_obs())
    mv_nm       <- req(input$manip_var_nm)
    add_pcp     <- req(input$do_add_pcp_segments)
    inc_var_nms <- req(input$inc_var_nms)
    #idx_rownum  <- sel_rownums() ## NULL is no selection; all points
    idx_rownum  <- NULL ## all points
    ## sel_rownums() is Leading to a hard to explore plotly method error:
    # Error: object 'x' not found
    ## abandoning and defaulting to full selection.
    
    if(mv_nm %in% rownames(bas) == FALSE){
      message(paste0("output$cheem_tour: input$manip_var_nm = '", mv_nm,
                     "' wasn't in the basis. Shiny tried to update cheem_tour before manip_var_nm..."))
      return(NULL)
    }
    mv <- which(rownames(bas) == mv_nm)
    radial_cheem_tour(
      cheem_ls, bas, mv, prim_obs, comp_obs,
      do_add_pcp_segments = add_pcp, angle = .15,
      row_index = idx_rownum, inc_var_nms = inc_var_nms)
  }) %>% debounce(500L)
  
  ## Observers: updating inputs in the ui -----
  
  ## UPDATE MANIPULATION VARIABLE INPUT
  observeEvent({
    primary_obs()
    comparison_obs()
    input$inc_var_nms
  }, {
    .prim_obs <- req(primary_obs())
    .comp_obs <- req(comparison_obs())
    opts      <- req(input$inc_var_nms)
    attr_df   <- req(load_ls())$attr_df
    if(all(opts %in% colnames(attr_df)) == FALSE){
      message("Update manip_var_nm: not all input$inc_var_nms are in attr_df...")
      return()
    }
    ## Select var with largest difference between primary and comparison obs.
    inc_attr_df <- attr_df[, opts, drop = FALSE]
    mv <- manip_var_of_attr_df(inc_attr_df, .prim_obs, .comp_obs)
    mv_nm <- colnames(inc_attr_df)[mv]
    updateSelectInput(session, "manip_var_nm", label = "Manipulation variable:",
                      choices = opts, selected = mv_nm)
  }, priority = 150L)
  
  ## Outputs -----
  output$desc_rows <- renderText({
    dat <- req(input$dat_char)
    
    ## Load data:
    if(dat == "toy classification"){
      he <- h3("Simulated triangle vertices")
      l1 <- p("- 420 obsvations of 4 dimensions (2 signal, 2 noise, X's), and cluster grouping (Classification Y)")
      l2 <- p("1) Create a random forest model classifying cluster level, given the continuous variables.")
    }else if(dat == "penguins classification"){
      he <- h3("Palmer penguins")
      l1 <- p("- 214 penguin observations of 4 continuous physical measurements (X's) and species of penguin (Classification Y).")
      l2 <- p("1) Create a random forest model classifying species from the physical measurements.")
    }else if(dat == "chocolates classification"){
      he <- h3("Chocolates")
      l1 <- p("- 88 observations of 10 nutrition measures as labeled. These chocolates labels as 'milk' or 'dark'")
      l2 <- p("1) Create a random forest model classifying the type of chocolate from the nutrition label.")
    }else if(dat == "toy quad regression"){
      he <- h3("Toy quadratic regression")
      l1 <- p("- Simulated data, 200 observations, 5 unifrom variable in [0, 5]. y = x1 * x2 + x1 + x2 + (x3 + x4 + x5) / 10 + error")
      l2 <- p("1) Create a random forest model regressing y.")
    }else if(dat == "toy trig regression"){
      he <- h3("Toy trig regression")
      l1 <- p("- Simulated data, 200 observations, 5 unifrom variable in [0, 5]. y = sin(x1) * sin(x2) (x3 + x4 + x5) / 10 + error")
      l2 <- p("1) Create a random forest model regressing y.")      
    }else if(dat == "fifa regression"){
      he <- h3("FIFA soccer players, 2020 season")
      l1 <- p("- 5000 player observations of 9 explanatory skill 'aspects' (X's) and log wages [2020 Euros] (Regression Y)")
      l2 <- p("1) Create a random forest model regressing continuous wages from the skill aggregates.")
    }else if(dat == "ames housing 2018 regression"){
      he <- h3("Ames housing 2018 (North Ames only)")
      l1 <- p("- 338 observations of 9 house attributes. Point aesthetics on zoning subclass, (exogenous to the model).")
      l2 <- p("1) Create a random forest model regression log hose price from the 9 housing variables.")
    }else { ## _ie_ user uploaded data
      he <- h3("User uploaded data")
      l1 <- NULL
      l2 <- p("1) Create a random forest model")
    }
    # }else if(dat == "apartments"){
    #   he <- h3("DALEX::apartments, sinthetic 'anscombe quartet-like' data of appartment prices")
    #   l1 <- p("- 1000 appartment observations, of 4 explanatory variables, 1 class, Y is price per square meter.")
    #   l2 <- p("1) Create a random forest model regressing appartment price (/sq_m) the 4 X and the district's rank of price variation.")
    # }else if(dat == "diabetes (wide)"){
    #   he <- h3("Pima Indians Diabetes (wide)")
    #   l1 <- p("- 392 observations, of *8* explanatory variables, 1 class/Y; presence/abence of diabetes.")
    #   l2 <- p("1) Create a random forest model regressing the existence of diabetes from the *8* X variables.")
    # }else if(dat == "diabetes (long)"){
    #   he <- h3("Pima Indians Diabetes (long)")
    #   l1 <- p("- *724* observations, of *6* explanatory variables, 1 class/Y; presence/abence of diabetes.")
    #   l2 <- p("1) Create a random forest model regressing the existence of diabetes from the 6 X variables.")
    
    ## Return
    HTML(paste(he, l1, l2))
  })
  outputOptions(output, "desc_rows",
                suspendWhenHidden = FALSE, priority = 90L) ## Eager evaluation
  
  ### GLOBAL VIEW PLOTLY
  output$global_view <- plotly::renderPlotly({
    cheem_ls  <- req(load_ls())
    .prim_obs <- req(primary_obs())
    .comp_obs <- req(comparison_obs())
    suppressWarnings( ## suppress "Coordinate system already present..." from 2x draw_basis
      global_view( ## Let global view pick the color/shape
        cheem_ls, .prim_obs, .comp_obs
        #height_px = 480L, width_px = 1440L,
      ))
  })
  outputOptions(output, "global_view",
                suspendWhenHidden = FALSE, priority = -200L) ## Eager evaluation
  
  ### gganimate tour ----
  output$cheem_tour_gganimate <- renderImage({
    ggt <- req(cheem_ggtour())
    
    ### A temp file to save the output, will be removed later in renderImage
    anim <- animate_gganimate(
      ggt, height = 480L, width = 1440L,
      #units = "px", ## "px", "in", "cm", or "mm."
      #res = 72L, ## resolution (dpi)
      render = gganimate::av_renderer())
    
    outfile <- tempfile(fileext = ".mp4")
    gganimate::anim_save("outfile.mp4", anim)
    
    ## Return a list containing the filename of the rendered image file.
    list(src = "outfile.mp4", contentType = "video/mp4")
  }, deleteFile = TRUE)
  outputOptions(output, "cheem_tour_gganimate", ## LAZY eval, do last
                suspendWhenHidden = TRUE, priority = -9999L)
  
  ### plotly tour -----
  output$cheem_tour_plotly <- plotly::renderPlotly({
    ggt      <- req(cheem_ggtour())
    cheem_ls <- req(load_ls())
    
    .anim <- 
      spinifex::animate_plotly(ggt, fps = 4L, width = 1440L, height = 480L) %>%
      suppressWarnings() %>%
      plotly::style(hoverinfo = "none")
    ## %>% plotly::toWebGL() ## maybe faster, may have more issues.
    #### the following hasn't helped:
    ## - starting at frame 11 doesn't help
    ## - hiding gridlines again doesn't remove them.
    
    ## Layout also set in animate_plotly
    if(cheem_ls$type == "classification"){
      .anim %>% plotly::layout(xaxis = list(scaleratio = 4L))
    }else{
      .anim %>% plotly::layout(xaxis = list(scaleratio = 1L))
    }
  }) ## Lazy eval, heavy work, let the other stuff calculate first.
  outputOptions(output, "cheem_tour_plotly", ## LAZY eval, do last
                suspendWhenHidden = TRUE, priority = -9999L)
  
  ### DT table of selected data
  output$selected_df <- DT::renderDT({ ## Original data of selection
    idx_rownum <- sel_rownums() ## NULL is no selection
    if(is.null(idx_rownum)) return(NULL)
    .df <- req(load_ls())$decode_df
    .df_r <- data.frame(lapply(
      .df, function(c) if(is.numeric(c)) round(c, 2L) else c))
    return(DT::datatable(.df_r[idx_rownum,, drop = FALSE], rownames = FALSE))
  })
  outputOptions(output, "selected_df",
                suspendWhenHidden = FALSE, priority = 10L) ## Eager evaluation
} ## Close function, assigning server object.

shinyApp(ui = ui, server = server)
