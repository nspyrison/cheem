### ./inst/shiny_apps/cheem_initial/app.r ###
#' @author Nicholas Spyrison
#' Aug 2021
source("ui.r", local = TRUE, encoding = "utf-8")

server <- function(input, output, session){
  ## Reactives ----
  
  ### load_ls ----
  load_ls <- reactive({
    dat <- req(input$dat_char)
    if(dat %in% expected_data_char == FALSE)
      stop("data string not matched.")
    
    if(dat == "toy classification"){
      ret      <- toy_class_ls
      # prim_obs <- 36L
      # comp_obs <- 23L
    }else if(dat == "penguins classification"){
      ret      <- penguins_ls
      # prim_obs <- 18L  ## Example in milestone pres is 18, 111
      # comp_obs <- 111L
    }else if(dat == "chocolates classification"){
      ret      <- chocolates_ls
      # prim_obs <- 22L
      # comp_obs <- 34L
    }else if(dat == "toy quad regression"){
      ret      <- toy_quad_reg_ls
      # prim_obs <- 11L
      # comp_obs <- 121L
    }else if(dat == "toy trig regression"){
      ret      <- toy_trig_reg_ls
      # prim_obs <- 87L
      # comp_obs <- 102L
    }else if(dat == "toy mixture model regression"){
      ret      <- toy_mix_reg_ls
      # prim_obs <- 23L
      # comp_obs <- 130L
    }else if(dat == "fifa regression"){
      ret      <- fifa_ls
      # prim_obs <- 1L
      # comp_obs <- 8L
    }else if(dat == "ames housing 2018 regression"){
      ret      <- ames2018_ls
      # prim_obs <- 170L
      # comp_obs <- 220L
    }else{ ## _ie._ user loaded data; no priors of good obs to pick.
      file_path <- req(input$in_cheem_ls$datapath)
      tryCatch(ret <- readRDS(file_path),
               error = function(e) stop(safeError(e)))
      prim_obs <- 1L
      comp_obs <- 2L
    }
    
    ## SIDE EFFECT: Update prim/comp_obs
    # updateNumericInput(
    #   session, "primary_obs",
    #   label = "Primary observation rownum, ('*' point):",
    #   min = 1L, max = 1e6L, step = 1L, value = prim_obs)
    # updateNumericInput(
    #   session, "comparison_obs",
    #   label = "Comparison observation rownum, ('x' point):",
    #   min = 1L, max = 1e6L, step = 1L, value = comp_obs)
    # ## SIDE EFFECT: Update inclusion variable names
    # var_nms <- colnames(ret$attr_df)
    # updateCheckboxGroupInput(session, "inc_var_nms", label = "Variables to include:",
    #                          choices = var_nms, selected = var_nms, inline = TRUE)
    
    ## Return loaded cheem_ls
    ret
  })

  
  
  ### bas ----
  bas <- reactive({
    isolate(attr_df <- req(load_ls()$attr_df))
    ## isolated, not eager to eval twice, as prim_obs also updated as 
    #### prim/comp_obs also update as side effects of load_ls()
    inc_var_nms <- req(input$inc_var_nms)
    prim_obs    <- req(input$primary_obs)
    if(all(inc_var_nms %in% colnames(attr_df)) == FALSE){
      cheem:::devMessage("bas(): bas tried to react before inc_var_nms updated...")
      return()
    }
    basis_attr_df(attr_df[, inc_var_nms, drop = FALSE], prim_obs)
  })
  
  ### sel_rownums ----
  sel_rownums <- reactive({
    ## Row NUMBER index of data selected in linked global view
    .d <- plotly::event_data("plotly_selected")
    if(is.null(.d)) return(NULL)
    as.integer(.d$key)
  })
  
  
  ### cheem_ggtour() -----
  cheem_ggtour <- reactive({
    cheem_ls    <- req(load_ls())
    bas         <- req(bas())
    prim_obs    <- req(input$primary_obs)
    comp_obs    <- req(input$comparison_obs)
    mv_nm       <- req(input$manip_var_nm)
    add_pcp     <- req(input$do_add_pcp_segments)
    inc_var_nms <- req(input$inc_var_nms)
    #idx_rownum  <- sel_rownums() ## NULL is no selection; all points
    idx_rownum  <- NULL ## all points
    ## sel_rownums() is Leading to a hard to explore plotly method error:
    # Error: object 'x' not found
    ## abandoning and defaulting to full selection.
    
    if(mv_nm %in% rownames(bas) == FALSE){
      cheem:::devMessage(paste0(
        "output$cheem_tour: input$manip_var_nm = '", mv_nm,
        "' wasn't in the basis. Shiny tried to update cheem_tour before manip_var_nm..."))
      return(NULL)
    }
    mv <- which(rownames(bas) == mv_nm)
    cheem:::radial_cheem_tour_subplots(
      cheem_ls, bas, mv, prim_obs, comp_obs,
      do_add_pcp_segments = add_pcp, angle = .15,
      row_index = idx_rownum, inc_var_nms = inc_var_nms)
  }) %>%
    ## Leaving load_ls out; only load when all are ready
    bindCache(bas(), input$primary_obs, input$comparison_obs,
              input$manip_var_nm, input$do_add_pcp_segments) %>%
    bindEvent(bas(), input$primary_obs, input$comparison_obs,
              input$manip_var_nm, input$do_add_pcp_segments) #%>% debounce(millis = 500L)
  
  
  ## Observe/event -----
  
  ### update prim/comp_obs ----
  observeEvent(req(input$dat_char), {
    dat <- req(input$dat_char)
    if(dat == "toy classification"){
      prim_obs <- 36L
      comp_obs <- 23L
    }else if(dat == "penguins classification"){
      prim_obs <- 124L  ## Last presentation was is 124, 86
      comp_obs <- 76L
    }else if(dat == "chocolates classification"){
      prim_obs <- 22L
      comp_obs <- 34L
    }else if(dat == "toy quad regression"){
      prim_obs <- 11L
      comp_obs <- 121L
    }else if(dat == "toy trig regression"){
      prim_obs <- 87L
      comp_obs <- 102L
    }else if(dat == "toy mixture model regression"){
      prim_obs <- 23L
      comp_obs <- 130L
    }else if(dat == "fifa regression"){
      prim_obs <- 1L
      comp_obs <- 8L
    }else if(dat == "ames housing 2018 regression"){
      prim_obs <- 170L
      comp_obs <- 220L
    }else{ ## _ie._ user loaded data; no priors of good obs to pick.
      file_path <- req(input$in_cheem_ls$datapath)
      tryCatch(ret <- readRDS(file_path),
               error = function(e) stop(safeError(e)))
      prim_obs <- 1L
      comp_obs <- 2L
    }
    
    updateNumericInput(
      session, "primary_obs",
      label = "Primary observation rownum, ('*' point):",
      min = 1L, max = 1e6L, step = 1L, value = prim_obs)
    updateNumericInput(
      session, "comparison_obs",
      label = "Comparison observation rownum, ('x' point):",
      min = 1L, max = 1e6L, step = 1L, value = comp_obs)
    ## SIDE EFFECT: Update inclusion variable names
  })
  
  ### update inc_var_nms -----
  observeEvent(req(load_ls()), {
    var_nms <- colnames(req(load_ls())$attr_df)
    updateCheckboxGroupInput(session, "inc_var_nms", label = "Variables to include:",
                             choices = var_nms, selected = var_nms, inline = TRUE)
  })
  
  ### update manip_var_nm ----
  observeEvent({
    input$primary_obs
    input$comparison_obs
    input$inc_var_nms
  }, {
    attr_df   <- req(load_ls())$attr_df
    .prim_obs <- req(input$primary_obs)
    .comp_obs <- req(input$comparison_obs)
    .inc_nms  <- req(input$inc_var_nms)
    
    if(all(.inc_nms %in% colnames(attr_df)) == FALSE){
      cheem:::devMessage("Update manip_var_nm: not all input$inc_var_nms are in attr_df...")
      return(NULL)
    }
    ## Select var with largest difference between primary and comparison obs.
    inc_attr_df <- attr_df[, .inc_nms, drop = FALSE]
    mv <- manip_var_of_attr_df(inc_attr_df, .prim_obs, .comp_obs)
    mv_nm <- colnames(inc_attr_df)[mv]
    updateSelectInput(session, "manip_var_nm", label = "Manipulation variable:",
                      choices = .inc_nms, selected = mv_nm)
  }, priority = 150L)
  
  ## Outputs -----
  output$desc_rows <- renderText({
    dat <- req(input$dat_char)
    
    ## Load data:
    he <- l1 <- l2 <- NULL
    if(dat == "toy classification"){
      he <- h4("Simulated triangle vertices")
      l1 <- p("- 420 obsvations of 4 dimensions (2 signal, 2 noise, X's), and cluster grouping (Classification Y)")
      # l2 <- p("- Fit a random forest model classifying cluster level and extract .")
    }else if(dat == "penguins classification"){
      he <- h4("Palmer penguins")
      l1 <- p("- 214 penguin observations of 4 continuous physical measurements (X's) and species of penguin (Classification Y).")
      # l2 <- p("- Fit a random forest model classifying species from the physical measurements.")
    }else if(dat == "chocolates classification"){
      he <- h4("Chocolates")
      l1 <- p("- 88 observations of 10 nutrition measures as labeled. These chocolates labels as 'milk' or 'dark'")
      # l2 <- p("- Fit a random forest model classifying the type of chocolate from the nutrition label.")
    }else if(dat == "toy quad regression"){
      he <- h4("Toy quadratic regression")
      l1 <- p("- Simulated data, 200 observations, 5 unifrom variable in [0, 5]. y = x1 * x2 + x1 + x2 + (x3 + x4 + x5) / 10 + error")
    }else if(dat == "toy trig regression"){
      he <- h4("Toy trig regression")
      l1 <- p("- Simulated data, 200 observations, 5 unifrom variable in [0, 4*pi]|[0, 1]. y = sin(x1) + sin(x2) + (x3 + x4 + x5) / 10 + error")
    }else if(dat == "toy mixture model regression"){
      he <- h4("Toy mixture model regression")
      l1 <- p("- Simulated data, 240 observations, 5 unifrom variable in [0, 5], y = {first third: x1^2 + (x2 + x3 + x4 +x5) / 10, second third: x2^2 + (x1 + x3 + x4 +x5) / 10, third third: x3^2 + (x1 + x2 + x4 +x5) / 10} + error.")
    }else if(dat == "fifa regression"){
      he <- h4("FIFA soccer players, 2020 season")
      l1 <- p("- 5000 player observations of 9 explanatory skill 'aspects' (X's) and log wages [2020 Euros] (Regression Y)")
      # l2 <- p("- Fit a random forest model regressing continuous wages from the skill aggregates.")
    }else if(dat == "ames housing 2018 regression"){
      he <- h4("Ames housing 2018 (North Ames only)")
      l1 <- p("- 338 observations of 9 house attributes. Point aesthetics on zoning subclass, (exogenous to the model).")
      # l2 <- p("- Fit a random forest model regression log hose price from the 9 housing variables.")
    }else { ## _ie_ user uploaded data
      he <- h4("User uploaded data")
      .cheem_ls <- req(load_ls())
      .dim <- dim(.cheem_ls$attr_df)
      ## Use dim and type attr to describe data
      l1 <- p(paste0("- ", .dim[1L], " observations of ", .dim[2L], " variables."))
      # l2 <- p("- Fit a ", .cheem_ls$type, " random forest model ")
    }
    
    ## Return
    HTML(paste(he, l1, l2))
  })
  outputOptions(output, "desc_rows",
                suspendWhenHidden = FALSE, priority = 90L) ## Eager evaluation
  
  ### GLOBAL VIEW PLOTLY
  glob_view <- reactive({
    cheem_ls  <- req(load_ls()) 
    .prim_obs <- req(input$primary_obs)
    .comp_obs <- req(input$comparison_obs)
    .col      <- req(input$glob_view_col)
    
    if(all(rownames(req(bas())) %in% colnames(cheem_ls$attr_df)) == FALSE){
      cheem:::devMessage("glob_view(): bas tried to react before inc_var_nms updated...")
      return(NULL)
    }
    
    global_view(cheem_ls, .prim_obs, .comp_obs,
                height_px = 540L, width_px = 1440L,color = .col)
  }) %>%
    bindCache(load_ls(), input$primary_obs, input$comparison_obs,
              input$glob_view_col) %>%
    bindEvent(load_ls(), input$primary_obs, input$comparison_obs,
              input$glob_view_col)# %>%
    #debounce(millis = 200L)
  output$global_view <- plotly::renderPlotly(glob_view())
  ## NO EAGER EVAL, want last
  
  ### plotly tour -----
  output$cheem_tour_plotly <- plotly::renderPlotly({
    cheem_ls <- req(load_ls())
    ggt <- req(cheem_ggtour())
    
    .anim <- ggt %>%
      spinifex::animate_plotly(fps = 4L) %>%
      plotly::layout(showlegend = FALSE) %>%
      plotly::style(hoverinfo = "none")
    ## %>% plotly::toWebGL() ## maybe faster, may have more issues.
    #### the following hasn't helped:
    ## - starting at frame 11 doesn't help
    ## - hiding gridlines again doesn't remove them.
    .anim
  })
  ## Lazy eval, heavy work, let the other stuff calculate first.
  ## NO EAGER EVAL, desired last
  
  ### DT table of selected data
  output$selected_df <- DT::renderDT({  ## Original data of selection
    idx_rownum <- unique(sel_rownums()) ## NULL is no selection
    if(is.null(idx_rownum)) return(NULL)
    .df <- req(load_ls())$decode_df
    .df_r <- data.frame(lapply(
      .df, function(c) if(is.numeric(c)) round(c, 2L) else c))
    DT::datatable(.df_r[idx_rownum,, drop = FALSE], rownames = FALSE)
  })
  outputOptions(output, "selected_df",
                suspendWhenHidden = FALSE, priority = 10L) ## Eager evaluation
} ## Close function, assigning server object.

shinyApp(ui = ui, server = server)
