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
      ret <- toy_class_ls
    }else if(dat == "penguins classification"){
      ret <- penguins_ls
    }else if(dat == "chocolates classification"){
      ret <- chocolates_ls
    }else if(dat == "toy quad regression"){
      ret <- toy_quad_reg_ls
    }else if(dat == "toy trig regression"){
      ret <- toy_trig_reg_ls
    }else if(dat == "toy mixture model regression"){
      ret <- toy_mix_reg_ls
    }else if(dat == "fifa regression"){
      ret <- fifa_ls
    }else if(dat == "ames housing 2018 regression"){
      ret <- ames2018_ls
    }else{ ## _ie._ user loaded data; no priors of good instance to pick.
      file_path <- req(input$in_cheem_ls$datapath)
      tryCatch(ret <- readRDS(file_path),
               error = function(e) stop(safeError(e)))
    }
    
    ## SIDE EFFECT: Update prim/comp_inst
    # updateNumericInput(
    #   session, "primary_inst",
    #   label = "Primary instance rownum, ('*' point):",
    #   min = 1L, max = 1e6L, step = 1L, value = prim_inst)
    # updateNumericInput(
    #   session, "comparison_inst",
    #   label = "Comparison instance rownum, ('x' point):",
    #   min = 1L, max = 1e6L, step = 1L, value = comp_inst)
    # ## SIDE EFFECT: Update inclusion feature names
    # feat_nms <- colnames(ret$attr_df)
    # updateCheckboxGroupInput(session, "inc_feat_nms", label = "features to include:",
    #                          choices = feat_nms, selected = feat_nms, inline = TRUE)
    
    ## Return loaded cheem_ls
    ret
  })

  
  
  ### bas ----
  bas <- reactive({
    isolate(attr_df <- req(load_ls()$attr_df))
    ## isolated, not eager to eval twice, as prim_inst also updated as 
    #### prim/comp_inst also update as side effects of load_ls()
    inc_feat_nms <- req(input$inc_feat_nms)
    prim_inst    <- req(input$primary_inst)
    if(all(inc_feat_nms %in% colnames(attr_df)) == FALSE){
      cheem:::devMessage("bas(): bas tried to react before inc_feat_nms updated...")
      return()
    }
    basis_attr_df(attr_df[, inc_feat_nms, drop = FALSE], prim_inst)
  })
  
  ### sel_rownums ----
  sel_rownums <- reactive({
    ## Row NUMBER index of data selected in linked global view
    .d <- plotly::event_data("plotly_selected")
    if(is.null(.d)) return(NULL)
    unique(as.integer(.d$key))
  })
  
  ### perf_df ----

  ### cheem_ggtour -----
  cheem_ggtour <- reactive({
    cheem_ls     <- req(load_ls())
    bas          <- req(bas())
    prim_inst    <- req(input$primary_inst)
    comp_inst    <- req(input$comparison_inst)
    mv_nm        <- req(input$manip_feat_nm)
    add_pcp      <- req(input$do_add_pcp_segments)
    inc_feat_nms <- req(input$inc_feat_nms)
    idx_rownum   <- sel_rownums() ## NULL is no selection; all points
    #idx_rownum   <- NULL ## all points
    ## sel_rownums() is Leading to a hard to explore plotly method error:
    # Error: object 'x' not found
    ## abandoning and defaulting to full selection.
    
    if(mv_nm %in% rownames(bas) == FALSE){
      cheem:::devMessage(paste0(
        "output$cheem_tour: input$manip_feat_nm = '", mv_nm,
        "' wasn't in the basis. Shiny tried to update cheem_tour before manip_feat_nm..."))
      return()
    }
    mv <- which(rownames(bas) == mv_nm)
    radial_cheem_tour_subplots(
      cheem_ls, bas, mv, prim_inst, comp_inst, do_add_pcp_segments = add_pcp,
      angle = .15, row_index = idx_rownum, inc_var_nms = inc_feat_nms)
  }) %>%
    ## Leaving load_ls out; only load when all are ready
    bindCache(bas(), input$primary_inst, input$comparison_inst,
              input$manip_feat_nm, input$do_add_pcp_segments) %>%
    bindEvent(bas(), input$primary_inst, input$comparison_inst,
              input$manip_feat_nm, input$do_add_pcp_segments) #%>% debounce(millis = 500L)
  
  
  ## Observe/event -----
  
  ### update prim/comp_inst ----
  observeEvent(req(input$dat_char), {
    dat <- req(input$dat_char)
    if(dat == "toy classification"){
      prim_inst <- 36L
      comp_inst <- 23L
    }else if(dat == "penguins classification"){
      prim_inst <- 243L ## Presubmission seminar looked at 124, 86
      comp_inst <- 169L
    }else if(dat == "chocolates classification"){
      prim_inst <- 22L
      comp_inst <- 7L
    }else if(dat == "toy quad regression"){
      prim_inst <- 11L
      comp_inst <- 121L
    }else if(dat == "toy trig regression"){
      prim_inst <- 87L
      comp_inst <- 102L
    }else if(dat == "toy mixture model regression"){
      prim_inst <- 23L
      comp_inst <- 130L
    }else if(dat == "fifa regression"){
      prim_inst <- 1L
      comp_inst <- 8L
    }else if(dat == "ames housing 2018 regression"){
      prim_inst <- 74L
      comp_inst <- 192L
    }else{ ## _ie._ user loaded data; no priors of good instance to pick.
      prim_inst <- 1L
      comp_inst <- 2L
    }
    
    updateNumericInput(
      session, "primary_inst",
      label = "Primary instance ('*', dashed line below):",
      min = 1L, max = 1e6L, step = 1L, value = prim_inst)
    updateNumericInput(
      session, "comparison_inst",
      label = "Comparison instance ('x', dotted line below):",
      min = 1L, max = 1e6L, step = 1L, value = comp_inst)
    ## SIDE EFFECT: Update inclusion feature names
  })
  
  ### update inc_feat_nms -----
  observeEvent(req(load_ls()), {
    feat_nms <- colnames(req(load_ls())$attr_df)
    updateCheckboxGroupInput(session, "inc_feat_nms", label = "Featurtes to include:",
                             choices = feat_nms, selected = feat_nms, inline = TRUE)
  })
  
  ### update manip_feat_nm ----
  observeEvent({
    input$primary_inst
    input$comparison_inst
    input$inc_feat_nms
  }, {
    attr_df    <- req(load_ls())$attr_df
    .prim_inst <- req(input$primary_inst)
    .comp_inst <- req(input$comparison_inst)
    .inc_nms   <- req(input$inc_feat_nms)
    if(all(.inc_nms %in% colnames(attr_df)) == FALSE){
      cheem:::devMessage("Update manip_feat_nm: not all input$inc_feat_nms are in attr_df...")
      return()
    }
    
    inc_attr_df <- attr_df[, .inc_nms]
    bas         <- basis_attr_df(inc_attr_df, .prim_inst) %>%
      tourr::orthonormalise()
    mv          <- manip_var_of(bas)
    mv_nm       <- colnames(inc_attr_df)[mv]
    updateSelectInput(session, "manip_feat_nm", label = "Manipulation feature:",
                      choices = .inc_nms, selected = mv_nm)
  }, priority = 150L)
  
  ## Outputs -----
  output$desc_rows <- renderText({
    dat     <- req(input$dat_char)
    attr_df <- req(load_ls())$attr_df
    
    ## Load data:
    he <- l1 <- NULL
    if(dat == "toy classification"){
      he <- h4("Simulated triangle vertices")
      l1 <- p(paste0(
        "- ", nrow(attr_df), " instances of ", ncol(attr_df),
        " features (2 signal, 2 noise), and cluster membership, the classification target"))
    }else if(dat == "penguins classification"){
      he <- h4("Palmer penguins")
      l1 <-  p(paste0(
        "- ", nrow(attr_df), " instances of ", ncol(attr_df),
        " physical features and species of penguin, the classification target"))
    }else if(dat == "chocolates classification"){
      he <- h4("Chocolates")
      l1 <- p(paste0(
        "- ", nrow(attr_df), " instances of ", ncol(attr_df),
        " nutritional label features. Type of chocolate 'Dark' or 'Milk' is the response"))
    }else if(dat == "toy quad regression"){
      he <- h4("Toy quadratic regression")
      l1 <- p(paste0(
        "- ", nrow(attr_df), " instances of ", ncol(attr_df),
        " uniform features over [0, 5], regression y = x1 * x2 + x1 + x2 + (x3 + x4 + x5) / 10 +  error"))
    }else if(dat == "toy trig regression"){
      he <- h4("Toy trig regression")
      l1 <- p(paste0(
        "- ", nrow(attr_df), " instances of ", ncol(attr_df),
        " uniform features in [0, 4*pi]|[0, 1], regression y = sin(x1) + sin(x2) + (x3 + x4 + x5) / 10 + error"))
    }else if(dat == "toy mixture model regression"){
      he <- h4("Toy mixture model regression")
      l1 <- p(paste0(
        "- ", nrow(attr_df), " instances of ", ncol(attr_df),
        " uniform feature in [0, 5], regression y = {first third: x1^2 + (x2 + x3 + x4 +x5) / 10, second third: x2^2 + (x1 + x3 + x4 +x5) / 10, third third: x3^2 + (x1 + x2 + x4 +x5) / 10} + error."))
    }else if(dat == "fifa regression"){
      he <- h4("FIFA soccer players, 2020 season")
      l1 <- p(paste0(
        "- ", nrow(attr_df), " instances of ", ncol(attr_df),
        " aggregated skill features, regressing on wages [2020 EUR]"))
    }else if(dat == "ames housing 2018 regression"){
      he <- h4("Ames housing 2018 (North Ames only)")
      l1 <- p(paste0(
        "- ", nrow(attr_df), " instances of ", ncol(attr_df),
        " features, regressing on Sale Price [2018 USD]."))
    }else { ## User uploaded data
      he <- h4("User uploaded data")
      l1 <- p(paste0("- ", nrow(attr_df), " instances of ", ncol(attr_df), " features."))
    }
    ## Return
    HTML(paste(he, l1))
  })
  outputOptions(output, "desc_rows",
                suspendWhenHidden = FALSE, priority = 90L) ## Eager evaluation
  
  ### GLOBAL VIEW PLOTLY
  glob_view <- reactive({
    cheem_ls   <- req(load_ls()) 
    .prim_inst <- req(input$primary_inst)
    .comp_inst <- req(input$comparison_inst)
    .col       <- req(input$glob_view_col)
    
    if(all(rownames(req(bas())) %in% colnames(cheem_ls$attr_df)) == FALSE){
      cheem:::devMessage("glob_view(): bas tried to react before inc_feat_nms updated...")
      return()
    }
    
    global_view(cheem_ls, .prim_inst, .comp_inst, color = .col,
                height_px = 540L, width_px = 1440L)
  }) %>%
    bindCache(load_ls(), input$primary_inst, input$comparison_inst,
              input$glob_view_col) %>%
    bindEvent(load_ls(), input$primary_inst, input$comparison_inst,
              input$glob_view_col)# %>%
    #debounce(millis = 200L)
  ## Lazy eval, heavy work, let the other stuff calculate first.
  output$global_view <- plotly::renderPlotly(suppressWarnings(glob_view()))
  
  ### plotly tour -----
  output$cheem_tour_plotly <- plotly::renderPlotly({
    cheem_ls <- req(load_ls())
    ggt <- req(cheem_ggtour())
    
    .anim <- ggt %>%
      spinifex::animate_plotly(fps = 4L) %>%
      plotly::layout(showlegend = FALSE) %>%
      plotly::style(hoverinfo = "none")
    ## the following hasn't helped:
    #### %>% plotly::toWebGL() & plotly::partial_bundle(), not reliably faster and may increase visual issues.
    #### - starting at frame 11 doesn't get arround frame 1 sometimes being skipped.
    #### - redundantly hiding gridlines in plotly doesn't remove them.
    .anim
  })
  ## Lazy eval, heavy work, let the other stuff calculate first.
  
  output$perf_df <- renderTable({
    df <- req(load_ls()$model_performance_df)
    df %>% dplyr::mutate_if(is.numeric, round, digits = 2L)
  })
  outputOptions(output, "perf_df",
                suspendWhenHidden = FALSE, priority = 10L) ## Eager evaluation
  
  ### DT table of selected data
  output$selected_df <- DT::renderDT({ ## Original data of selection
    idx_rownum <- sel_rownums() ## NULL is no selection
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
