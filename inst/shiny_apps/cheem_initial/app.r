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
      ret      <- toy_ls
      prim_obs <- 18L
      comp_obs <- 111L
    }else if(dat == "penguins"){
      ret      <- penguins_ls
      prim_obs <- 15L
      comp_obs <- 282L
    }else if(dat == "toy regression"){
      ret      <- toy_reg_ls
      prim_obs <- 11L
      comp_obs <- 116L
    }else if(dat == "fifa"){
      ret      <- fifa_ls
      prim_obs <- 1L
      comp_obs <- 8L
    }else if(dat == "ames housing 2018"){
      ret      <- ames2018_ls
      prim_obs <- 128L
      comp_obs <- 93
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
    .n_max <- 1e6
    updateNumericInput(
      session, "primary_obs",
      label = "Primary observation rownum, ('*' point):",
      min = 1L, max = .n_max, step = 1L, value = prim_obs)
    updateNumericInput(
      session, "comparison_obs",
      label = "Comparison observation rownum, ('x' ponit):",
      min = 1L, max = .n_max, step = 1L, value = comp_obs)
    
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
  #primary_obs_d <- primary_obs %>% debounce(millis = 1000L)
  comparison_obs <- reactive({
    req(input$comparison_obs)
    input$comparison_obs
  })
  #comparison_obs_d <- comparison_obs %>% debounce(millis = 1000L)
  
  ## Basis; the local explanation's attributon of the primary obs
  bas <- reactive({
    attr_df  <- req(load_ls()$attr_df)
    inc_vars <- req(input$inc_vars)
    prim_obs <- req(primary_obs())
    if(all(inc_vars %in% colnames(attr_df)) == FALSE){
      message("bas(): bas tried to react before inc_vars updated...")
      return()
    }
    
    return(basis_attr_df(attr_df[, inc_vars], prim_obs))
  })
  
  sel_rownums <- reactive({
    ## Row NUMBER index of data selected in linked global view
    .d <- plotly::event_data("plotly_selected")
    if(is.null(.d)) return(NULL)
    return(.d$key)
  })
  
  ## Observers: updating inputs in the ui -----
  
  ## UPDATE MANIPULATION VARIABLE INPUT
  observeEvent({
    bas()
    primary_obs()
  }, {
    bas       <- req(bas())
    opts      <- req(input$inc_vars)
    .prim_obs <- req(primary_obs())
    .comp_obs <- req(comparison_obs())
    attr_df   <- req(load_ls())$attr_df
    
    ## Select var with largest difference between primary and comparison obs.
    prim_bas <- attr_df[.prim_obs,, drop = FALSE]
    comp_bas <- attr_df[.comp_obs,, drop = FALSE]
    .diff <- abs(comp_bas - prim_bas)
    sel <- opts[which(.diff == max(.diff))]
    
    updateSelectInput(session, "manip_var_nm",
                      label = "Manipulation variable:",
                      choices  = opts,
                      selected = sel)
  }, priority = 150L)
  
  ## Outputs -----
  output$desc_rows <- renderText({
    dat <- req(input$dat_char)
    
    ## Load data:
    if(dat == "toy classification"){
      he <- h3("Simulated triangle vertices")
      l1 <- p("- 420 obsvations of 4 dimensions (2 signal, 2 noise, X's), and cluster grouping (Classification Y)")
      l2 <- p("1) Create a random forest model classifying cluster level, given the continuous variables.")
    }else if(dat == "penguins"){
      he <- h3("Palmer penguins")
      l1 <- p("- 214 penguin observations of 4 continuous physical measurements (X's) and species of penguin (Classification Y).")
      l2 <- p("1) Create a random forest model classifying species from the physical measurements.")
    }else if(dat == "fifa"){
      he <- h3("FIFA soccer players, 2020 season")
      l1 <- p("- 5000 player observations of 9 explanatory skill 'aspects' (X's) and wages [2020 Euros] (Regression Y)")
      l2 <- p("1) Create a random forest model regressing continuous wages from the skill aggregates.")
    }else if(dat == "apartments"){
      he <- h3("DALEX::apartments, sinthetic 'anscombe quartet-like' data of appartment prices")
      l1 <- p("- 1000 appartment observations, of 4 explanatory variables, 1 class, Y is price per square meter.")
      l2 <- p("1) Create a random forest model regressing appartment price (/sq_m) the 4 X and the district's rank of price variation.")
    }else if(dat == "diabetes (wide)"){
      he <- h3("Pima Indians Diabetes (wide)")
      l1 <- p("- 392 observations, of *8* explanatory variables, 1 class/Y; presence/abence of diabetes.")
      l2 <- p("1) Create a random forest model regressing the existence of diabetes from the *8* X variables.")
    }else if(dat == "diabetes (long)"){
      he <- h3("Pima Indians Diabetes (long)")
      l1 <- p("- *724* observations, of *6* explanatory variables, 1 class/Y; presence/abence of diabetes.")
      l2 <- p("1) Create a random forest model regressing the existence of diabetes from the 6 X variables.")
    }else { ## _ie_ user uploaded data
      he <- h3("User uploaded data")
      l1 <- NULL
      l2 <- p("1) Create a random forest model")
    }
    ## Return
    HTML(paste(he, l1, l2))
  })
  outputOptions(output, "desc_rows",
                suspendWhenHidden = FALSE, priority = 90L) ## Eager evaluation
  
  ### GLOBAL VIEW PLOTLY
  output$global_view <- plotly::renderPlotly({
    cheem_ls <- req(load_ls())
    .prim_obs <- req(primary_obs())
    .comp_obs <- req(comparison_obs())
    suppressWarnings( ## suppress "Coordinate system already present..." from 2x draw_basis
      global_view(
        cheem_ls, .prim_obs, .comp_obs,
        height_px = 480, width_px = 960L))
  })
  outputOptions(output, "global_view",
                suspendWhenHidden = FALSE, priority = -200L) ## Eager evaluation
  
  ## Plotly tour
  output$cheem_tour_plotly <- plotly::renderPlotly({
    bas        <- req(bas())
    cheem_ls   <- req(load_ls())
    prim_obs   <- req(primary_obs())
    comp_obs   <- req(comparison_obs())
    mv_nm      <- req(input$manip_var_nm)
    add_pcp    <- req(input$do_add_pcp_segments)
    inc_vars   <- req(input$inc_vars)
    idx_rownum <- sel_rownums() ## NULL is no selection
    
    if(mv_nm %in% rownames(bas) == FALSE){
      message(paste0("output$cheem_tour: input$manip_var_nm = '", mv_nm,
                     "' wasn't in the basis. Shiny tried to update cheem_tour before manip_var_nm..."))
      return(NULL)
    }
    mv <- which(mv_nm %in% rownames(bas))
    
    ggt <- radial_cheem_tour(
      cheem_ls, bas, mv,
      prim_obs, comp_obs,
      do_add_pcp_segments = add_pcp,
      row_index = idx_rownum, inc_vars = inc_vars)
    
    spinifex::animate_plotly(ggt) ## %>% plotly::toWebGL() ## maybe faster, maybe more issues.
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
