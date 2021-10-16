## spinifex extensions ----

#' Extract and format the 1D local attribution basis
#' 
#' Internal function, Extract and format the 1D local attribution basis from 
#' the provided local_attribution_df.
#' 
#' @param local_attribution_df Return of a local attribution, such as 
#' treeshap_df.
#' @param rownum The rownumber of the primary observation.
#' @return A matrix of the 1D basis
#' @export
#' @examples
#' la_df <- mtcars ## Pretend this is a local attribution data.frame
#' basis_local_attribution(la_df, rownum = 10)
basis_local_attribution <- function(
  local_attribution_df,
  rownum = nrow(local_attribution_df)
){
  ## Remove last column if layer_name
  if(local_attribution_df[, ncol(local_attribution_df)] %>% is.numeric == TRUE){
    col_idx <- 1L:ncol(local_attribution_df)
  }else{col_idx <- -ncol(local_attribution_df)}
  LA_df <- local_attribution_df[rownum, col_idx]
  ## Extract formatted basis
  LA_bas <- LA_df %>%
    as.numeric %>%
    matrix(ncol = 1L, dimnames = list(colnames(LA_df), "SHAP"))
  return(tourr::orthonormalise(LA_bas))
}


#' Adds the distribution of the row local attributions to a ggtour
#'
#' Adds the distribution of orthonormalized row values of 
#' the specified `local_attribution_df`. Does not at the basis itself, 
#' use in conjunction with `proto_basis1d()`.
#'
#' @param layer_ls A return from `nested_local_attr_layers()`, a list of data frames.
#' @param group_by Vector to group densities by. Originally _predicted_ class.
#' @param position The position for the basis, one of: c("top1d", "floor1d",
#' "top2d", "floor2d", "off"). 
#' Defaults to "top1d"; basis above the density curves.
#' @param shape Number specifying the shape of the basis distribution points. 
#' Typically 142 or 124 indicating '|' for `plotly` and `gganimate` respectively.
#' Defaults to 142, '|' for `plotly`.
#' @param do_add_pcp_segments Logical, whether or not to add to add faint 
#' parallel coordinate lines on the 1D basis. Defaults to TRUE.
#' @param primary_obs The rownumber of the primary observation. Its local
#' attribution becomes the 1d projection basis, and the point it highlighted 
#' as a dashed line.
#' @param comparison_obs The rownumber of the comparison observation. Point
#' is highlighted as a dotted line.
#' @rdname proto_basis
#' @family ggtour proto
#' @export
#' @examples
#' load("./apps/cheem_penguins_classification/data/1preprocess.RData")
#' dat <- layer_ls$decode_df[, 8:11]
#' clas <- layer_ls$decode_df$class
#' shap_df <- layer_ls$shap_df[1:nrow(dat), -5]
#' bas <- basis_local_attribution(shap_df, nrow(dat))
#' mv <- manip_var_of(bas) ## Warning is fine.
#' 
#' ## 1D case:
#' mt_path <- manual_tour(bas, mv)
#' 
#' debugonce(proto_basis1d)
#' ggt <- ggtour(mt_path, dat) +
#'   proto_density(aes_args = list(color = clas, fill = clas)) +
#'   proto_basis1d() +
#'   proto_basis1d_distribution(shap_df, group_by = clas)
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_basis1d_distribution <- function(
  layer_ls, ## Only for distribution of bases.
  group_by = as.factor(FALSE),
  position = c("top1d", "floor1d", "top2d", "floor2d", "off"), ## Needs to match that of `proto_basis1d()`
  shape = c(142, 124), ## '|' for plotly and ggplot respectively
  do_add_pcp_segments = TRUE,
  primary_obs = NULL,
  comparison_obs = NULL
){
  ## Prevent global variable warnings:
  .facet_by <- rownum <- maha_dist <- contribution <- var_name <-
    .map_to_unitbox <- .map_to_data <- .map_to_density <-
    .df_zero <- var_num <- x <- y <- xend <- yend <- NULL
  ## Initialize
  eval(spinifex::.init4proto)
  position <- match.arg(position)
  shape <- shape[1L] ## match.arg only for character values.
  if(shape %in% c(142L, 124L) == FALSE) warning("Unexpected shape used in proto_basis1d_distribution.")
  if(position == "off") return()
  if(is.null(.facet_by) == FALSE) position = "floor1d"
  
  local_attribution_df <- layer_ls$shap_df
  ## Pivot longer:
  ### ensure last col dropped if layer_name
  col_idx <- ifelse(
    local_attribution_df[, ncol(local_attribution_df)] %>% is.numeric,
    1L:ncol(local_attribution_df), -ncol(local_attribution_df))
  LA_df <- local_attribution_df[, col_idx]
  
  ## Force orthonormalize each row.
  .m <- sapply(1L:nrow(LA_df), function(i){
    row_bas <- basis_local_attribution(LA_df, i)
    LA_df[i, ] <<- tourr::orthonormalise(row_bas)
  })
  ## Pivot the SHAP values (columns) longer to rows.
  .n <- nrow(LA_df)
  .p <- ncol(LA_df)
  LA_df$rownum <- 1L:.n
  LA_df$group_by <- as.factor(group_by)
  
  .p_df <- layer_ls$plot_df
  .maha_dist_df <-
    .p_df[.p_df$layer_nm == "data" &
            .p_df$projection_nm == "QQ Mahalanobis distance", c(1L, 4L)]
  colnames(.maha_dist_df) <- c("rownum", "maha_dist")
  .LA_df_lj <- dplyr::left_join(LA_df, .maha_dist_df, by = "rownum")
  .LA_df_longer <- tidyr::pivot_longer(.LA_df_lj,
                                       cols = !c(rownum, group_by, maha_dist),
                                       names_to = "var_name",
                                       values_to = "contribution")
  .df_basis_distr <- dplyr::mutate(
    .LA_df_longer,
    .keep = "none",
    x = contribution,
    ## Must be reverse order; var 1 on top, highest value.
    y = rep_len(.p:1L, .n * .p) + .05 * (as.integer(group_by) - 2L),
    var_name = var_name,
    var_num = rep_len(.p:1L, .n * .p),
    rownum = rownum,
    group_by = group_by,
    maha_dist = maha_dist) %>%
    as.data.frame()
  
  ## Map them to position
  if(.d == 1L){
    .map_to_tgt <- .map_to_density
  } else .map_to_tgt <- .map_to_data
  .df_basis_distr <- spinifex::map_relative(
    .df_basis_distr, position, .map_to_tgt)
  ## Add facet level if needed
  if(is.null(.facet_by) == FALSE){
    .basis_ls <- list(facet_by = rep_len("_basis_", nrow(.df_zero)))
    .df_basis_distr <- spinifex:::.bind_elements2df(.basis_ls, .df_basis_distr)
  }
  
  .alpha <- ifelse(length(unique(.df_basis_distr$rowname)) > 999L, .003, .3)
  ## Basis/attribution distribution of the rows of the LA_df
  ret <- suppressWarnings(ggplot2::geom_point(
    ggplot2::aes(x, y, color = group_by, tooltip = rownum), .df_basis_distr,
    shape = shape, alpha = .alpha, size = 1.5))
  
  ## Add PCP lines if needed.
  if(do_add_pcp_segments == TRUE){
    ## Make the right table to inner join to.
    .df_basis_distr2 <- dplyr::mutate(
      .df_basis_distr, .keep = "none",
      xend = x, yend = y, var_num = var_num - 1L, rownum = rownum)
    ## Inner join to by var_name & rownum(lead1)
    .df_basis_distr_pcp_segments <- dplyr::inner_join(
      .df_basis_distr, .df_basis_distr2,
      by = c("var_num" = "var_num", "rownum" = "rownum"))
    ## Mapping .alpha for pcp lines to maha dist causing err: not of length 1 or data
    bkg_pcp <- suppressWarnings(ggplot2::geom_segment(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                   color = group_by, tooltip = rownum),
      .df_basis_distr_pcp_segments, size = .5, alpha = .alpha / 6L))
    prim_idx <- .df_basis_distr_pcp_segments$rownum == primary_obs
    if(length(primary_obs) > 0L){
      prim_pcp <- suppressWarnings(ggplot2::geom_segment(
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                     color = group_by, tooltip = rownum),
        .df_basis_distr_pcp_segments[prim_idx, ],
        size = 1L, alpha = .8, linetype = 2L))
    }else{prim_pcp <- NULL}
    comp_idx <- .df_basis_distr_pcp_segments$rownum == comparison_obs
    if(length(primary_obs) > 0L){
      comp_pcp <- suppressWarnings(ggplot2::geom_segment(
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                     color = group_by, tooltip = rownum),
        .df_basis_distr_pcp_segments[comp_idx, ],
        size = .8, alpha = .6, linetype = 3L))
    }else{comp_pcp <- NULL}
    ret <- list(bkg_pcp, comp_pcp, prim_pcp, ret)
  }
  
  ## Return proto
  return(ret)
}


##NOTES -----

##TODO: !! This whole file wants to rebased with generalized models and attributiosn
#  Keep in mind extension of RF models. then model and explanation types by DALEX.

##TODO: !! Whole file wants Roxygen descriptions and ideally examples.


## shiny preprocess functions -----
## NOTE: this is outdated and should be removed.

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
      ggplot2::aes(V1, V2, color = pred_clas, shape = pred_clas,
                   tooltip = tooltip), alpha = .alpha)) +
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
  angle = .1,
  rownum_idx = TRUE
){
  ## Initialization Y on basis
  .y <- layer_ls$decode_df[rownum_idx, "y"] %>% matrix(ncol = 1L)
  .col_idx <- which(!(
    colnames(layer_ls$decode_df) %in%
      c("rownum", "class", "y", "prediction", "residual",
        "predicted_class", "is_misclassified", "tooltip")
  ))
  .x <- layer_ls$decode_df[rownum_idx, .col_idx] ## Numeric X variables
  
  ## Problem type: classification or regression?
  problem_type <- function(y){
    if(length(unique(y)) < 16L) return("classification")
    if(is.numeric(y) == TRUE) return("regression")
    stop("y was expected as a with less than 16 unique values, or numeric indicating a classification or regression problem respectivly.")
  }
  .prob_type <- problem_type(layer_ls$decode_df$y) ## Either "classification" or "regression"
  .pred_clas <- as.factor(FALSE) ## Initialize dummy predicted class
  if(.prob_type == "classification")
    .pred_clas <- layer_ls$decode_df$predicted_class
  
  logistic_rownum_tform = function(x, mid_pt = 1000, k_attenuation = 2/mid_pt, max = 1, floor=.1) {
    k_attenuation
    log <- max / (1 + exp(k_attenuation * (x - mid_pt)))
    floor + (1 - floor)*log}
  #' @examples 
  #' x <- 1:5000; plot(x, logistic_rownum_tform(x), col='blue');
  .alpha <- logistic_rownum_tform(nrow(layer_ls$decode_df))
  
  ## Manual (radial) tour 1d
  .mv <- which(colnames(layer_ls$shap_df) == mv_name)
  .mt_path <- spinifex::manual_tour(basis, manip_var = .mv)
  
  ### Classification case -----
  # Classification goes right into vis
  if(.prob_type == "classification"){
    .dat <- spinifex::scale_01(.x)
    ggt <- spinifex::ggtour(.mt_path, .dat, angle = angle) +
      spinifex::proto_density(
        aes_args = list(color = .pred_clas, fill = .pred_clas)) +
      proto_basis1d_distribution(
        layer_ls, group_by = .pred_clas,
        position = "top1d",
        shape = pcp_shape, ## '|' for gganimate/ggplot
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        primary_obs = primary_obs,
        comparison_obs = comparison_obs) +
      spinifex::proto_basis1d(manip_col = "black") +
      spinifex::proto_origin1d() +
      ## Highlight comparison obs, if passed
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
        mark_initial = FALSE) +
      spinifex::proto_frame_cor()
  }
  
  ### Regression case -----
  # Regression does some work to append y or y_hat to the basis
  if(.prob_type == "regression"){
    ## y axis; full contribution of the y, regression var.
    .tgt_dim <- c(dim(.mt_path) + c(1L, 1L, 0L))
    .array <- array(NA, dim = .tgt_dim)
    .m <- sapply(1L:.tgt_dim[3L], function(i){
      .array[,, i] <<- tourr::orthonormalise(
        matrix(
          c(0L, .mt_path[,, i], c(1L, rep(0L, .tgt_dim[1L] - 1L))),
          nrow = .tgt_dim[1L], ncol = .tgt_dim[2L]
        ))
    })
    dn <- dimnames(.mt_path)
    dn[[1L]] <- c("Observed y", rownames(.mt_path))
    dn[[2L]] <- c("Local explanation", "Observed y")
    dimnames(.array) <- dn
    attr(.array, "manip_var") <- attr(.mt_path, "manip_var")
    attr(.array, "theta")     <- attr(.mt_path, "theta")
    attr(.array, "phi_start") <- attr(.mt_path, "phi_start")
    attr(.array, "phi_min")   <- attr(.mt_path, "phi_min")
    attr(.array, "phi_max")   <- attr(.mt_path, "phi_max")
    
    ## Add y to .dat to project
    .dat <- spinifex::scale_01(data.frame(.x, .y))
    ## Plot
    ggt <- spinifex::ggtour(.array, .dat, angle = angle) +
      ## _points would ideally be _hex or _hdr, but:
      #### _hex doesn't work with plotly
      #### _hdr not implented atm.
      ##- thin the data...
      spinifex::proto_point(
        aes_args = list(color = .pred_clas, fill = .pred_clas),
        identity_args = list(alpha = .alpha)) +
      proto_basis1d_distribution(
        layer_ls, group_by = .pred_clas,
        position = "top2d",
        shape = c(142L, 124L), ## '|' for plotly/gganimate.
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        primary_obs = primary_obs,
        comparison_obs = comparison_obs) +
      spinifex::proto_basis1d(
        position = "top2d", manip_col = "black") +
      spinifex::proto_origin() +
      ## Manual axes titles
      # Plotly can't handle text rotation of geom_text/annotate.
      ggplot2::labs(x = "Attribution projection, 1D",
                    y = "Observed y") +
      ggplot2::theme(axis.title.y = ggplot2::element_text(vjust = .25)) +
      ## Highlight comparison obs
      spinifex::proto_highlight(
        comparison_obs,
        list(color = .pred_clas),
        list(size = 4L, shape = 4L, alpha = 0.5),
        mark_initial = FALSE) +
      ## Highlight primary obs
      spinifex::proto_highlight(
        primary_obs,
        aes_args = list(color = .pred_clas),
        identity_args = list(size = 5L, shape = 8L, alpha = 1L),
        mark_initial = FALSE) +
      spinifex::proto_frame_cor()
  }
  
  ## Return the static ggtour, animate in app
  return(ggt)
}


