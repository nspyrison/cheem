# Penguin classification -----
## ./apps/cheem_classification/app.r
#' 
#' @author Nicholas Spyrison
#' Aug 2021
source("ui.r", local = TRUE, encoding = "utf-8")


server <- function(input, output, session){
  ## Reactives ----
  #### No eager evaluation of reactive functions, only outputs.
  layer_ls <- reactive({
    req(input$dat_char)
    dat <- input$dat_char
    if(!(dat %in% c("triangle simulation", "penguins", "fifa")))
      stop("data string not matched.")
    if(dat == "triangle simulation")
      load("./data/2preprocess_simulation.RData", envir = globalenv())
    if(dat == "penguins")
      load("./data/1preprocess_penguins.RData", envir = globalenv())
    if(dat == "fifa")
      load("./data/3preprocess_fifa.RData", envir = globalenv())
    return(shap_layer_ls)
  })
  
  output$input__dat_desc <- renderUI({
    req(input$dat_char)
    dat <- input$dat_char
    if(!(dat %in% c("triangle simulation", "penguins", "fifa")))
      stop("data string not matched.")
    ## Load data:
    if(dat == "triangle simulation")
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
    return(desc_rows)
  })
  
  bas <- reactive({
    req(layer_ls())
    shap_df <- layer_ls()$shap_df
    bas <- basis_local_attribution(shap_df, shap_obs_d())
    return(bas)
  })
  
  ## output: inputs in the ui -----
  output$input__shap.comp_obs <- renderUI({
    req(layer_ls())
    .n <- layer_ls()$decode_df %>% nrow()
    req(input$dat_char)
    dat <- input$dat_char
    if(!(dat %in% c("triangle simulation", "penguins", "fifa")))
      stop("data string not matched.")
    
    ## Initialize to hard-coded hand picked examples.
    if(dat == "triangle simulation"){
      shap_obs <- 18L
      comp_obs <- 111L
    }
    if(dat == "penguins"){
      shap_obs <- 169L
      comp_obs <- 99L
    }
    if(dat == "fifa"){
      shap_obs <- 1L ## L Messi
      comp_obs <- 8L ## V. van Dijk
    }
    
    ## Return
    fluidRow(
      column(4L, numericInput(
        "shap_obs", label = "SHAP values of row number, '*' shape:",
        min = 1L, max = .n, step = 1L, value = shap_obs)),
      column(4L, numericInput(
        "comp_obs", label = "Comparison row number, 'x' shape:",
        min = 1L, max = .n, step = 1L, value = comp_obs)),
      column(4L)
    )
  })
  outputOptions(output, "input__shap.comp_obs", suspendWhenHidden = FALSE) ## Eager evaluation
  ##"Debounce" shap/comp_obs; 
  #### ie, Reduces making multiple animations as someone types in a 3 digit number 
  shap_obs <- reactive({
    req(input$shap_obs)
    input$shap_obs
  })
  shap_obs_d <- shap_obs %>% debounce(millis = 1000L)
  comp_obs <- reactive({
    req(input$comp_obs)
    input$comp_obs
  })
  comp_obs_d <- comp_obs %>% debounce(millis = 1000L)
  
  output$input__manip_var_nm <- renderUI({
    req(bas())
    bas <- bas()
    
    opts <- rownames(bas)
    shap_df <- layer_ls()$shap_df[, -ncol(layer_ls()$shap_df)]
    clas <- layer_ls()$decode_df$class
    
    ## Median values of the actual class.
    expect_bas <- apply(shap_df[clas == clas[shap_obs_d()], ], 2L, median) %>%
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
  output$kurtosis_print <- renderPrint({
    req(layer_ls())
    req(input$do_include_maha_qq)
    if(as.logical(input$do_include_maha_qq) == FALSE){.lines <- ""
    }else{
      .lines <-
        c("Moments of the Mahalanobis distances of data- and SHAP-space respectively:", "",
          unique(layer_ls()$plot_df[, c("ggtext")])[-1L])
    }
    writeLines(.lines)
  })
  outputOptions(output, "kurtosis_print", suspendWhenHidden = FALSE) ## Eager evaluation
  
  output$linked_plotly <- plotly::renderPlotly({
    req(layer_ls())
    req(shap_obs_d())
    req(comp_obs_d())
    linked_plotly_func(
      layer_ls(), shap_obs_d(), comp_obs_d(),
      do_include_maha_qq = as.logical(input$do_include_maha_qq))
  })
  outputOptions(output, "linked_plotly", suspendWhenHidden = FALSE) ## Eager evaluation
  
  output$input__linked_plotly = renderUI({
    ## This is dimension of spacer, figure dim's set in args of cobs_n_plot_func::linked_plotly_func
    height = 640L ## Init, height with qq maha.
    if(as.logical(input$do_include_maha_qq) == FALSE) height <- height / 2L
    plotlyOutput("linked_plotly", width = "100%", height = paste0(height))
  })
  outputOptions(output, "input__linked_plotly", suspendWhenHidden = FALSE) ## Eager evaluation
  
  output$manual_tour_plotly <- plotly::renderPlotly({
    req(bas())
    req(layer_ls())
    req(input$manip_var_nm)
    
    ggt <- manual_tour1d_func(
      layer_ls(), bas(), input$manip_var_nm,
      shap_obs_d(), comp_obs_d(),
      do_add_pcp_segements = as.logical(input$do_add_pcp_segments))
    animate_plotly(ggt)
  }) ## Lazy eval, heavy work, let the other stuff calculate first.
  output$manual_tour_gganimate <- renderImage({
    req(bas())
    req(layer_ls())
    req(input$manip_var_nm)
    
    ## A temp file to save the output, will be removed later in renderImage
    outfile <- tempfile(fileext = ".gif")
    ## Now make the animation
    ggt <- manual_tour1d_func(
      layer_ls(), bas(), input$manip_var_nm,
      shap_obs_d(), comp_obs_d(),
      do_add_pcp_segements = as.logical(input$do_add_pcp_segments))
    anim <- animate_gganimate(ggt)
    gganimate::anim_save("outfile.gif", anim)
    
    ## Return a list containing the filename
    list(src = "outfile.gif", contentType = "image/gif"
         ## ,alt = "Hover tooltip text here" ## h/w dim set in preprocess func.
    )},
    deleteFile = TRUE
  ) ## Lazy eval, too heavy
  
  ## Data selected in pca_embed_plotly -----
  output$selected_df <- DT::renderDT({ ## Original data of selection
    d <- event_data("plotly_selected") ## What plotly sees as selected
    if (is.null(d)) return(NULL)
    df <- layer_ls()$decode_df
    return(DT::datatable(df[df$rownum %in% d$key, ], rownames = FALSE))
  })
  outputOptions(output, "selected_df", suspendWhenHidden = FALSE) ## Eager evaluation
  
  ## Message of the cobs row numbers
  output$cobs_msg <- renderText(attr(layer_ls(), "cobs_msg"))
  outputOptions(output, "cobs_msg", suspendWhenHidden = FALSE) ## Eager evaluation
} ## Close function, assigning server object.

shinyApp(ui = ui, server = server)
