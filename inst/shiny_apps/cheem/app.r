### ./inst/shiny_apps/cheem_initial/app.r ###
#' @author Nicholas Spyrison
#' Aug 2021
source("ui.r", local = TRUE, encoding = "utf-8")
do_dev_cat <- FALSE
dev_cat <- function(msg, verbose = do_dev_cat){
  if(do_dev_cat)
    cat(paste0(msg, "\n"))
}

server <- function(input, output, session){
  ## Reactives ----
  
  ### load_ls ----
  load_ls <- reactive({
    dev_cat("top of load_ls")
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
    
    ## Return loaded cheem_ls
    ret
  })
  
  
  ### bas ----
  bas <- reactive({
    dev_cat("top of bas")
    isolate(attr_df <- req(load_ls()$attr_df))
    ## isolated, not eager to eval twice, as prim_inst also updated as 
    #### prim/comp_inst also update as side effects of load_ls()
    inc_feat_nms <- req(input$inc_feat_nms)
    prim_inst    <- req(input$primary_inst)
    if(all(inc_feat_nms %in% colnames(attr_df)) == FALSE){
      cheem:::devMessage("bas(): bas tried to react before inc_feat_nms updated...")
      return()
    }
    sug_basis(attr_df[, inc_feat_nms, drop = FALSE], prim_inst)
  })
  
  ### sel_rownums ----
  sel_rownums <- reactive({
    dev_cat("top of sel_rownums")
    ## Row NUMBER index of data selected in linked global view
    .d <- plotly::event_data("plotly_selected")
    if(is.null(.d)) return(NULL)
    unique(as.integer(.d$key))
  })
  
  ### cheem_ggtour -----
  cheem_ggtour <- reactive({
    dev_cat("top of cheem_ggtour")
    cheem_ls     <- req(load_ls())
    bas          <- req(bas())
    prim_inst    <- req(input$primary_inst)
    comp_inst    <- req(input$comparison_inst)
    mv_nm        <- req(input$manip_feat_nm)
    add_pcp      <- req(input$do_add_pcp_segments)
    inc_feat_nms <- req(input$inc_feat_nms)
    idx_rownum   <- sel_rownums() ## NULL is no selection; all points
    #idx_rownum   <- NULL ## all points
    
    if(mv_nm %in% rownames(bas) == FALSE){
      cheem:::devMessage(paste0(
        "output$cheem_tour: input$manip_feat_nm = '", mv_nm,
        "' wasn't in the basis. Shiny tried to update cheem_tour before manip_feat_nm..."))
      return()
    }
    mv <- which(rownames(bas) == mv_nm)
    radial_cheem_tour(
      cheem_ls, bas, mv, prim_inst, comp_inst, do_add_pcp_segments = add_pcp,
      angle = .10, row_index = idx_rownum, inc_var_nms = inc_feat_nms, 
      pcp_shape = 142)
  })
  
  
  ## Observe/event -----
  
  ### update prim/comp_inst ----
  observeEvent(req(input$dat_char), {
    dev_cat("top of observeEvent(req(input$dat_char)")
    dat <- req(input$dat_char)
    if(dat == "toy classification"){
      prim_inst <- 118
      comp_inst <- 135
    }else if(dat == "penguins classification"){
      prim_inst <- 243
      comp_inst <- 169
    }else if(dat == "chocolates classification"){
      prim_inst <- 64
      comp_inst <- 83
    }else if(dat == "toy quad regression"){
      prim_inst <- 100
      comp_inst <- 188
    }else if(dat == "toy trig regression"){
      prim_inst <- 180
      comp_inst <- 167
    }else if(dat == "toy mixture model regression"){
      prim_inst <- 127
      comp_inst <- 220
    }else if(dat == "fifa regression"){
      prim_inst <- 1
      comp_inst <- 8
    }else if(dat == "ames housing 2018 regression"){
      prim_inst <- 74
      comp_inst <- 141
    }else{ ## _ie._ user loaded data; no priors of good instance to pick.
      prim_inst <- 1
      comp_inst <- 2
    }
    
    ## SIDE EFFECT: Update inclusion feature names
    updateNumericInput(
      session, "primary_inst",
      label = "Primary instance ('*', dashed line below)",
      min = 1, max = 1e6, step = 1, value = prim_inst)
    updateNumericInput(
      session, "comparison_inst",
      label = "Comparison instance ('x', dotted line below)",
      min = 1, max = 1e6, step = 1, value = comp_inst)
  })
  
  ### update inc_feat_nms -----
  observeEvent(req(load_ls()), {
    dev_cat("top of observeEvent(req(load_ls())")
    feat_nms <- colnames(req(load_ls())$attr_df)
    updateCheckboxGroupInput(session, "inc_feat_nms", label = "Featurtes to include",
                             choices = feat_nms, selected = feat_nms, inline = TRUE)
  })
  
  ### update manip_feat_nm ----
  observeEvent({
    input$primary_inst
    input$comparison_inst
    input$inc_feat_nms
  }, {
    dev_cat("top of observeEvent({input$primary_inst...")
    attr_df    <- req(load_ls())$attr_df
    .prim_inst <- req(input$primary_inst)
    .comp_inst <- req(input$comparison_inst)
    .inc_nms   <- req(input$inc_feat_nms)
    if(all(.inc_nms %in% colnames(attr_df)) == FALSE){
      cheem:::devMessage("Update manip_feat_nm: not all input$inc_feat_nms are in attr_df...")
      return()
    }
    
    inc_attr_df <- attr_df[, .inc_nms]
    bas         <- sug_basis(inc_attr_df, .prim_inst) %>%
      tourr::orthonormalise()
    mv          <- sug_manip_var(inc_attr_df, .prim_inst, .comp_inst)
    mv_nm       <- colnames(inc_attr_df)[mv]
    updateSelectInput(session, "manip_feat_nm", label = "Manipulation feature",
                      choices = .inc_nms, selected = mv_nm)
  }, priority = 150)
  
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
        " features (two signal, two noise), and cluster membership, the classification target"))
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
                suspendWhenHidden = FALSE, priority = 90) ## Eager evaluation
  
  ### GLOBAL VIEW PLOTLY
  glob_view <- reactive({
    cheem_ls   <- req(load_ls()) ## will this not trigger BYO-Data make? req(input$dat_char)
    .BYOData   <- req(input$dat_char)
    .prim_inst <- req(input$primary_inst)
    .comp_inst <- req(input$comparison_inst)
    .col       <- req(input$glob_view_col)
    
    if(all(rownames(req(bas())) %in% colnames(cheem_ls$attr_df)) == FALSE){
      cheem:::devMessage("glob_view(): bas tried to react before inc_feat_nms updated...")
      return()
    }
    
    global_view(cheem_ls, .prim_inst, .comp_inst, color = .col,
                height_px = 338, width_px = 1000)
  })

  ## Lazy eval, heavy work, let the other stuff calculate first.
  output$global_view <- plotly::renderPlotly({
    input$load_ls
    input$go_global_view
    
    if(input$go_global_view > -1)
      return(isolate({
      suppressWarnings(glob_view())
    }))
  })
  
  ### plotly tour -----
  output$cheem_tour_plotly <- plotly::renderPlotly({
    ## update on go button, but not other.
    input$load_ls
    #input$go_global_view
    input$go_tour
    
    return(isolate({
      cheem_ls <- req(load_ls())
      ggt <- req(cheem_ggtour())
      
      .anim <- ggt %>%
        spinifex::animate_plotly(fps = 4) %>%
        plotly::layout(showlegend = FALSE) %>%
        plotly::style(hoverinfo = "none")
      ## the following hasn't helped:
      #### %>% plotly::toWebGL() & plotly::partial_bundle(), not reliably faster and may increase visual issues.
      .anim
    }))
  })
  ## Lazy eval, heavy work, let the other stuff calculate first.
  
  output$perf_df <- renderTable({
    ls <- req(load_ls())
    if(ls$type == "regression"){
      ls$model_performance %>%
        dplyr::mutate_if(is.numeric, round, digits = 2)
    }
  })
  outputOptions(output, "perf_df",
                suspendWhenHidden = FALSE, priority = 10) ## Eager evaluation
  
  ### DT table of selected data
  output$selected_df <- DT::renderDT({ ## Original data of selection
    idx_rownum <- sel_rownums() ## NULL is no selection
    if(is.null(idx_rownum)) return(NULL)
    .df <- req(load_ls())$decode_df
    .df_r <- data.frame(lapply(
      .df, function(c) if(is.numeric(c)) round(c, 2) else c))
    DT::datatable(.df_r[idx_rownum,, drop = FALSE], rownames = FALSE)
  })
  outputOptions(output, "selected_df",
                suspendWhenHidden = FALSE, priority = 10) ## Eager evaluation
} ## Close function, assigning server object.

shinyApp(ui = ui, server = server,
         onStart = function(){
           ## Disable verbose and warnings in app
           prevW <- getOption("warn")
           prevV <- getOption("verbose")
           options(warn = -1, verbose = FALSE)
           ## Resume previous verbose and warning options
           shiny::onStop(function() options(warn = prevW, verbose = prevV))
         }
)
