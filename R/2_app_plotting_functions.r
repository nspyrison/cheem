##TODO: !! This whole file wants to be renamed and recoded as generalized variants
#  Keep in mind extension of RF models. then model and explanation types by DALEX.

##TODO: !! Whole file wants Roxygen descriptions if not examples.


## Other local functions for downstream consumption.
#### Esp to prep for shiny apps.

#' Append corrupted observations and extract SHAP layer list.
#' 
#' Draws `n_cobs` from the `data` of the `target_level` for _each_ other level of 
#' the `class`. This is appended to the `data` and finally applied to 
#' `nested_local_attr_layers()`.
#' 
#' @param data A data.frame or matrix with data from all `class` levels, not just
#' the level sampled from.
#' @param class The variable to group points by. Originally the _predicted_
#'  class.
#' @param y The target variable of the model.
#' @param target_level The number of the level of `class` (cast as.factor) to 
#' sample from. 
#' @param n_cobs The number of Corrupted OBServations (cobs) to draw for _each_ 
#' other non-target level.
#' @param var_coeff Variance coefficient, closer to 0 make points near the 
#' median, above 1 makes more points further away from the median.
#' Defaults to 1.
#' @param verbose Logical, Whether or not the function should print tictoc time
#' info. Defaults to TRUE.
#' @param noisy Logical, Whether of not the function should play a beeper tone
#' upon completion. Defaults to TRUE.
#' @return A list of data.frames, the return of `nested_local_attr_layers()` of the 
#' `data` after appending the new corrupted observations.
#' @export
#' @examples
#' ## Data setup, palmerpenguins::penguins
#' raw <- spinifex::penguins
#' lvls <- levels(raw$species)
#' ## Filter to closest 2 classes
#' raw <- raw[raw$species %in% lvls[1:2], ]
#' dat <- as.data.frame(spinifex::scale_sd(raw[, 1:4]))
#' clas <- factor(raw$species, levels = lvls[1:2]) ## Manually remove 3rd lvl
#' 
#' ## Apply the functions
#' layer_ls <- assign_cobs_layer_ls(
#'   data = dat,
#'   class = clas,
#'   y = clas,
#'   target_level = 1,
#'   n_cobs = 2,
#'   var_coeff = .1)
#' 
#' ## Structure of the list.
#' names(layer_ls)
#' str(layer_ls$plot_df)
#' str(layer_ls$decode_df)
####TODO: EXAMPLE CAUSING ERROR.
assign_cobs_layer_ls <- function(
  data,
  class,
  y, ## Factor implies classification, numeric implies regression
  target_level = 1,
  n_cobs = 2,
  var_coeff = .1,
  verbose = TRUE,
  noisy = TRUE
){
  ## Initialize
  .clas_w_cobs <- as.factor(class)
  .x_w_cobs    <- as.data.frame(data)
  ## Classification/regression
  .prob_type   <- problem_type(y)
  if(.prob_type == "classification") .y_w_cobs <- as.integer(y)
  if(.prob_type == "regression")     .y_w_cobs <- y
  
  ## Append COBS if required ----
  if(n_cobs > 0L){
    set.seed(404L)
    .lvls <- levels(class)
    
    ## Sample cobs from level 1
    #.m <- sapply(1L, function(k){
    k <- target_level
    .not_k <- (1L:length(.lvls))[-k]
    .cobs_df_k <- rnorm_from(data[class == .lvls[k], ],
                             n_obs = n_cobs,
                             var_coeff = var_coeff)
    
    ## Replicate for every non-k level and assign a replicated set to each non-k level
    .cobs_df <- data.frame(NULL)
    .m <- sapply(1L:length(.not_k), function(i){
      .cobs_df <<- rbind(.cobs_df, .cobs_df_k)
    })
    .clas_w_cobs <<- factor(c(
      as.character(.clas_w_cobs), rep(.lvls[.not_k], times = n_cobs)), levels = .lvls)
    .x_w_cobs <<- rbind(.x_w_cobs, .cobs_df)
    if(.prob_type == "classification") ## Classification problem if Y is a factor
      ## treeshap wants integer classes :(.
      .y_w_cobs <<- c(.y_w_cobs, rep(.not_k, times = n_cobs))
    if(.prob_type == "regression") ## Regression problem if Y is numeric
      .y_w_cobs <<- c(.y_w_cobs, stats::rnorm(
        n_cobs, mean(y[class == .lvls[.not_k]]),
        sqrt(var_coeff) * stats::sd(y[class == .lvls[.not_k]])
      ))
  } ## End appending cobs.
  
  ### Create and global assign layer_ls -----
  .layer_ls <- nested_local_attr_layers(
    .x_w_cobs, .y_w_cobs, n_layers = 1L, basis_type = "pca",
    class = .clas_w_cobs, verbose = verbose, noisy = noisy)
  attr(.layer_ls, "n_cobs") <- n_cobs
  attr(.layer_ls, "var_coeff") <- var_coeff
  .n <- nrow(data)
  attr(.layer_ls, "cobs_msg") <- ifelse(n_cobs > 0L, paste0(
    "Level-courrupted observations in rows: ",
    (nrow(data) + 1L), " to ", nrow(.layer_ls$decode_df), "."),
    "") ## ""; no cobs added.
  
  ## Return
  return(.layer_ls)
}


## Shiny plot functions ------
#' Linked `plotly` display, global view of data and attribution space.
#' 
#' Given an attribution layer list, create a linked `plotly`of the global data-
#' and attribution- spaces. Typically consumed directly by shiny app.
#' 
#' @param layer_ls A return from `nested_local_attr_layers()`, a list of data frames.
#' @param primary_obs The rownumber of the primary observation. Its local
#' attribution becomes the 1d projection basis, and the point it highlighted 
#' as a dashed line.
#' @param comparison_obs The rownumber of the comparison observation. Point
#' is highlighted as a dotted line.
#' @param height_px The height in pixels of the returned `plotly` plot.
#' @param width_px The width in pixels of the returned `plotly` plot.
#' @param do_include_maha_qq Logical, whether or not to add the qq plots of the
#' Mahalanobis distance for the data- and attribution- spaces
#' @return `plotly` plot of the global view, first 2 components of the basis of
#' the data- and attribution- spaces.
#' @export
#' @examples
#' sub <- DALEX::apartments[1:200, 1:6]
#' X <- sub[, 2:5]
#' Y <- sub$m2.price
#' clas <- sub$district
#' 
#' layer_ls <- nested_local_attr_layers(
#'   x=X, y=Y, n_layers=1, basis_type="pca", class=clas, verbose=T, noisy=T)
#' 
#' linked_plotly_func(layer_ls, primary_obs = 1, comparison_obs = 2)
##TODO: example is missing comp obs; x and shap */x
linked_plotly_func <- function(
  layer_ls,
  primary_obs = NULL,
  comparison_obs = NULL,
  height_px = 640L,
  width_px = 640L,
  do_include_maha_qq = FALSE
){
  ## Prevent global variable warnings:
  V1 <- V2 <- ggtext <- projection_nm <- layer_nm <- tooltip <- NULL
  .alpha <- ifelse(nrow(layer_ls$decode_df) > 999L, .1, .6)
  .xlab <- ifelse(do_include_maha_qq == FALSE, "PC1",
                  "PC1 | Quantile, chi-squared")
  .ylab <- ifelse(do_include_maha_qq == FALSE, "PC2",
                  "PC2 | Quantile, observed Mahalanobis distance")
  ## Remove QQ maha rows if needed
  plot_df <- layer_ls$plot_df ## Init
  if(do_include_maha_qq == FALSE){
    plot_df <- layer_ls$plot_df[
      layer_ls$plot_df$projection_nm != "QQ Mahalanobis distance", ]
    height_px <- height_px / 2L ## Half height display as qq maha is removed.
  }
  ##TODO:!!! "problem_type" attr is only set in assign_cobs_layer_ls;
  # wants to be set directly in nested_local_attr_layers
  is_classification <- attr(layer_ls, "problem_type") == "classification"
  # ifelse("is_misclassified" %in% colnames(layer_ls$decode_df), TRUE, FALSE)
  pred_clas <- as.factor(FALSE) ## If regression; dummy pred_clas
  if(is_classification == TRUE) pred_clas <-
    layer_ls$decode_df$predicted_class %>%
    rep_len(nrow(plot_df)) %>%
    as.factor()
  
  gg <- ggplot2::ggplot(
    plotly::highlight_key(plot_df, ~rownum),
    ggplot2::aes(V1, V2)
  )
    
  ## Red misclassified points, if present
  if(is_classification == TRUE){
    .rn_misclass <- which(layer_ls$decode_df$is_misclassified == TRUE)
    .idx_misclas <- plot_df$rownum %in% .rn_misclass
    if(sum(.idx_misclas) > 0L){
      .df <- plot_df[.idx_misclas, ] # %>% highlight_key(~rownum)
      gg <- gg +
        ggplot2::geom_point(ggplot2::aes(V1, V2), .df,
                            color = "red", fill = NA,
                            shape = 21L, size = 3L, alpha = .alpha)
    }
  }
  ## Highlight comparison obs, if passed
  if(is.null(comparison_obs) == FALSE){
    .idx_comp <- plot_df$rownum == comparison_obs
    if(sum(.idx_comp) > 0L){
      .df <- plot_df[.idx_comp, ] # %>% highlight_key(~rownum)
      gg <- gg +
        ## Highlight comparison obs
        ggplot2::geom_point(ggplot2::aes(V1, V2, color = pred_clas[.idx_comp]),
                            .df, size = 4L, shape = 4L)
    }
  }
  ## Highlight shap obs, if passed
  if(is.null(primary_obs) == FALSE){
    .idx_shap <- plot_df$rownum == primary_obs
    if(sum(.idx_shap) > 0L){
      .df <- plot_df[.idx_shap, ] # %>% highlight_key(~rownum)
      gg <- gg +
        ggplot2::geom_point(ggplot2::aes(V1, V2, color = pred_clas[.idx_shap]),
                            .df, size = 5L, shape = 8L)
    }
  }
  ## Maha skew text,
  #### geom_text not working with plotly... & annotate() not working with facets...
  if(do_include_maha_qq == TRUE){
    gg <- gg +
      ggplot2::geom_text(ggplot2::aes(x = -Inf, y = Inf, label = ggtext),
                         hjust = 0L, vjust = 1L, size = 4L)
  }
  ## Normal points
  gg <- gg +
    suppressWarnings(ggplot2::geom_point(
      ggplot2::aes(V1, V2, color = pred_clas, shape = pred_clas, tooltip = tooltip),
      alpha = .alpha)) +
    ggplot2::facet_grid(rows = ggplot2::vars(projection_nm),
                        cols = ggplot2::vars(layer_nm), scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = .xlab, y = .ylab) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme(axis.text  = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   legend.position = "off")
  
  ## BOX SELECT
  ggp <- plotly::ggplotly(gg, tooltip = "tooltip",
           height = height_px, width = width_px)
  ggp <- plotly::config(ggp, displayModeBar = FALSE)                  ## Remove html buttons
  ggp <- plotly::layout(ggp, dragmode = "select", showlegend = FALSE) ## Set drag left mouse
  ggp <- plotly::event_register(ggp, "plotly_selected")               ## Reflect "selected", on release of the mouse button.
  ggp <- plotly::highlight(ggp, on = "plotly_selected", off = "plotly_deselect")
  return(ggp)
}


#' Cheem tour; 1D manual tour on the selected attribution
#' 
#' Create a linked `plotly`of the global data-
#' and attribution- spaces. Typically consumed directly by shiny app.
#' 
#' @param layer_ls A return from `nested_local_attr_layers()`, a list of data frames.
#' @param basis A 1D projection basis, typically a return of 
#' `basis_local_attribution()`.
#' @param mv_name The character string, _name_ of the Manipulation Variable.
#' @param primary_obs The rownumber of the primary observation. Its local
#' attribution becomes the 1d projection basis, and the point it highlighted 
#' as a dashed line.
#' @param comparison_obs The rownumber of the comparison observation. Point
#' is highlighted as a dotted line.
#' @param do_add_pcp_segments Logical, whether or not to add parallel coordinate
#' line segments to the basis display.
#' @param pcp_shape The number of the shape character to add. Typically
#' 142 or 124, '|' for `plotly` and `gganimate` respectively. Defaults to 142, 
#' '|' for `plotly`.
#' @param angle The step size between interpolated frames, in radians.
#' @return `plotly` plot of the global view, first 2 components of the basis of
#' the data- and attribution- spaces.
#' @export
#' @examples
#' sub <- DALEX::apartments[1:200, 1:6]
#' X <- sub[, 2:5]
#' Y <- sub$m2.price
#' clas <- sub$district
#' 
#' layer_ls <- nested_local_attr_layers(
#'   x=X, y=Y, n_layers=1, basis_type="pca", class=clas, verbose=T, noisy=T)
#' 
#' tgt_obs <- 1
#' bas <- basis_local_attribution(layer_ls$shap_df, tgt_obs)
#' ggt <- radial_cheem_ggtour(layer_ls, basis=bas, mv_name=colnames(X)[1], 
#'                            primary_obs=1, comparison_obs=2)
#' spinifex::animate_plotly(ggt)
radial_cheem_ggtour <- function(
  layer_ls, basis, mv_name, primary_obs, comparison_obs = NULL,
  do_add_pcp_segments = TRUE,
  pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
  angle = .1
){
  ## Initialization
  .y <- layer_ls$decode_df$y %>% matrix(ncol = 1L)
  .col_idx <- which(!(
    colnames(layer_ls$decode_df) %in%
      c("rownum", "class", "y", "prediction", "residual",
        "predicted_class", "is_misclassified", "tooltip")
  ))
  .x <- layer_ls$decode_df[, .col_idx] ## Numeric X variables
  
  ## Problem type: classification or regression?
  problem_type <- function(y){
    if(length(unique(y)) < 5L) return("classification")
    if(is.numeric(y) == TRUE) return("regression")
    stop("y was expected as a with less than 5 unique values, or numeric indicating a classification or regression problem respectivly.")
  }
  .prob_type <- problem_type(.y) ## Either "classification" or "regression"
  .pred_clas <- as.factor(FALSE) ## Initialize dummy predicted class
  if(.prob_type == "classification")
    .pred_clas <- layer_ls$decode_df$predicted_class
  .alpha <- ifelse(nrow(layer_ls$decode_df) > 999L, .1, 1L)
  
  ## Manual (radial) tour 1d
  .mv <- which(colnames(layer_ls$shap_df) == mv_name)
  .mt_path <- spinifex::manual_tour(basis, manip_var = .mv)
  
  ### Classification problem -----
  if(.prob_type == "classification"){
    .dat <- spinifex::scale_01(.x)
    ggt <- spinifex::ggtour(.mt_path, .dat, angle = angle) +
      spinifex::proto_density(
        aes_args = list(color = .pred_clas, fill = .pred_clas)) +
      spinifex::proto_origin1d() +
      spinifex::proto_basis1d(manip_col = "black") +
      proto_basis1d_distribution(
        layer_ls, group_by = .pred_clas,
        position = "top1d",
        shape = pcp_shape, ## '|' for gganimate/ggplot
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        primary_obs = primary_obs,
        comparison_obs = comparison_obs)
    ## Highlight comparison obs, if passed
    ggt <- ggt +
      spinifex::proto_highlight1d(
        comparison_obs,
        list(color = .pred_clas),
        list(linetype = 3L, alpha = 0.8),
        mark_initial = FALSE) +
      ## Highlight shap obs
      spinifex::proto_highlight1d(
        primary_obs,
        list(color = .pred_clas),
        list(linetype = 2L, alpha = .6, size = .8),
        mark_initial = FALSE)
  }
  
  ### Regression problem -----
  if(.prob_type == "regression"){
    ## Add a 2nd 'd' dimension, full contribution of the y var.
    .tgt_dim <- c(dim(.mt_path) + c(1L, 1L, 0L))
    .array <- array(NA, dim = .tgt_dim)
    .m <- sapply(1L:.tgt_dim[3L], function(i){
      .array[,, i] <<- tourr::orthonormalise(
        matrix(c(.mt_path[,, i], rep(0L, .tgt_dim[1L]), 1L),
               nrow = .tgt_dim[1L], ncol = .tgt_dim[2L]))
    })
    dn <- dimnames(.mt_path)
    dn[[1L]] <- c(rownames(.mt_path), "wage_euro")
    dn[[2L]] <- c("proj", "wage_euro")
    dimnames(.array) <- dn
    attr(.array, "manip_var") <- attr(.mt_path, "manip_var")
    attr(.array, "theta")     <- attr(.mt_path, "theta")
    attr(.array, "phi_start") <- attr(.mt_path, "phi_start")
    attr(.array, "phi_min")   <- attr(.mt_path, "phi_min")
    attr(.array, "phi_max")   <- attr(.mt_path, "phi_max")
    
    ## Add y to .dat to project.
    .dat <- spinifex::scale_01(data.frame(.x, .y))
    ## Plot
    ggt <- spinifex::ggtour(.array, .dat, angle = angle) +
      ## _points would ideally be _hex or _hdr, but:
      #### _hex doesn't work with plotly
      #### _hdr not made atm.
      ##- Down sample?
      spinifex::proto_point(
        aes_args = list(color = .pred_clas, fill = .pred_clas),
        identity_args = list(alpha = .alpha)) +
      spinifex::proto_origin() +
      spinifex::proto_basis1d(manip_col = "black") +
      proto_basis1d_distribution(
        layer_ls, group_by = .pred_clas,
        position = "top1d",
        shape = 142L, ## '|' for gganimate/ggplot
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        primary_obs = primary_obs,
        comparison_obs = comparison_obs) +
      ggplot2::labs(x = "1D SHAP projection", y = "Actual wages (2020 Euros)")
    ## Highlight comparison obs, if passed
    ggt <- ggt +
      spinifex::proto_highlight(
        comparison_obs,
        list(color = .pred_clas),
        list(size = 4L, shape = 4L, alpha = 0.5),
        mark_initial = FALSE) +
      ## Highlight shap obs
      spinifex::proto_highlight(
        primary_obs,
        aes_args = list(color = .pred_clas),
        identity_args = list(size = 5L, shape = 8L, alpha = 1L),
        mark_initial = FALSE)
  }
  
  ## Return the static ggtour, animate in app
  return(ggt)
}


