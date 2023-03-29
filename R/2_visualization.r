## Utilities -----

#' Suggest a 1D Basis
#' 
#' Extract and formats the 1D attribution basis from 
#' the provided local explanation.
#' 
#' @param attr_df A data frame of local explanation attributions.
#' @param rownum The rownumber of the observation. 
#' Typically primary or comparison observations.
#' @return A matrix of the 1D basis.
#' @family cheem utility
#' @seealso [radial_cheem_tour()]
#' @export
#' @examples
#' library(cheem)
#'
#' ## Regression setup
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#'
#' ## Attribution basis of one obs
#' sug_basis(ames_rf_shap, rownum = 1)
#' ## This can be used to find a basis to start the radial tour.
#' # ?radial_cheem_tour
sug_basis <- function(
  attr_df,
  rownum
){
  ## Remove last column if label
  .attr_df <- attr_df[rownum,, drop = FALSE]
  ## Extract formatted basis
  LA_bas <- matrix(as.numeric(.attr_df), ncol = 1,
                   dimnames = list(colnames(attr_df),
                                   utils::tail(class(attr_df), 1))
  )
  ## Ensure orthonormality
  tourr::orthonormalise(LA_bas)
}


#' Suggest a manipulation variable
#' 
#' Suggest the number of the variable with the largest difference in attribution
#' between the primary and comparison observations.
#' 
#' @param attr_df A data frame of local explanation attributions.
#' @param primary_obs The rownumber of the primary observation. Its local
#' attribution becomes the 1d projection basis, and the point it highlighted 
#' as a dashed line.
#' @param comparison_obs The rownumber of the comparison observation. Point
#' is highlighted as a dotted line.
#' @return A single number of the variable with the largest difference of 
#' attribution (basis) variables.
#' @seealso [radial_cheem_tour()]
#' @export
#' @family cheem utility
#' @examples
#' library(cheem)
#' 
#' ## Regression setup
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#'
#' ## Suggest the number of a variable to manipulate
#' sug_manip_var(ames_rf_shap, primary_obs = 1, comparison_obs = 2)
#' ## This can be used to find a basis to start the radial tour.
#' # ?radial_cheem_tour
sug_manip_var <- function(attr_df, primary_obs, comparison_obs){
  .prim <- sug_basis(attr_df, rownum = primary_obs)
  .comp <- sug_basis(attr_df, rownum = comparison_obs)
  .diff <- abs(.prim - .comp)
  which(.diff == max(.diff)) ## Number of the variable with the largest difference
}

## proto_* extensions ----

#' Adds the distribution of the row local attributions to a ggtour
#'
#' A {spinifex} proto_*-like function, that adds the distribution of 
#' orthonormalized row values of the specified local explanation `attr_df`. 
#' Does not draw the basis bars; use in conjunction with `proto_basis1d()`.
#'
#' @param attr_df An data frame, the attributions of a local explanation.
#' @param group_by Vector to group densities by. Originally _predicted_ class.
#' @param position The position for the basis, one of: c("top1d", "floor1d",
#' "bottom1d", "off"). 
#' Defaults to "top1d"; basis above the density curves.
#' @param pcp_shape The number of the shape character to add. Expects
#' 3, 142, or 124, '+', '|' in `plotly`, and '|' in `gganimate`, respectively. 
#' Defaults to 3, '+' in either output.
#' @param do_add_pcp_segments Logical, whether or not to add to add faint 
#' parallel coordinate lines on the 1D basis. Defaults to TRUE.
#' @param primary_obs The rownumber of the primary observation. Its local
#' attribution becomes the 1d projection basis, and the point it highlighted 
#' as a dashed line. Defaults to NULL, no highlighting.
#' @param comparison_obs The rownumber of the comparison observation. Point
#' is highlighted as a dotted line. Defaults to NULL, no highlighting.
#' @param inc_var_nms A character vector, the names of the variables to keep. 
#' Defaults to NULL, all variables kept.
#' @param row_index A numeric or logical vector, the index of the rows to keep.
#' Defaults to NULL, all rows kept.
#' @return A `ggplot` object of the the distribution of the local explanation's
#' attributions.
#' @family ggtour proto
#' @seealso [radial_cheem_tour()]
#' @export
#' @examples
#' library(cheem)
#' library(spinifex)
#' 
#' ## Regression setup
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Basis, manipulation var, manual tour path, & predictions to fix to y-axis
#' bas     <- sug_basis(ames_rf_shap, 1)
#' mv      <- sug_manip_var(ames_rf_shap, 1, 2)
#' mt_path <- manual_tour(bas, mv)
#' ## Also consumed by: ?radial_cheem_tour()
#' 
#' ## Compose and animate the tour
#' ggt <- ggtour(mt_path, scale_sd(X), angle = .3) +
#'   append_fixed_y(fixed_y = scale_sd(Y)) +
#'   proto_point(list(color = clas, shape = clas)) +
#'   proto_basis1d_distribution(
#'     attr_df = ames_rf_shap,
#'     primary_obs = 1, comparison_obs = 2,
#'     position = "top1d", group_by = clas) +
#'   proto_basis1d(position = "bottom1d") +
#'   proto_origin()
#' 
#' if(interactive()){
#'   animate_plotly(ggt)
#' }
proto_basis1d_distribution <- function(
  attr_df, ## Only for distribution of bases.
  primary_obs         = NULL,
  comparison_obs      = NULL,
  position            = c("top1d", "floor1d", "bottom1d", "off"), ## Needs to match that of `proto_basis1d()`
  group_by            = as.factor(FALSE),
  pcp_shape           = c(3, 142, 124), ## '|' plotly, '|' ggplot, '+' (both), respectively
  do_add_pcp_segments = TRUE,
  inc_var_nms         = NULL,
  row_index           = NULL
){
  ## Prevent global variable warnings:
  rownum <- contribution <- var_name <- .is_faceted <- .map_to <-
    .d <-.df_zero <- var_num <- x <- y <- xend <- yend <- NULL
  ## Initialize
  attr_df <- as.data.frame(attr_df)
  eval(spinifex::.init4proto)
  position <- match.arg(position)
  if(position == "off") return()
  pcp_shape <- pcp_shape[1] ## match.arg only for character values.
  if(pcp_shape %in% c(142, 124, 3) == FALSE)
    warning("Unexpected shape used in proto_basis1d_distribution.")
  
  ## Subset rows then columns
  if(is.null(row_index) == FALSE){
    ## Ensure prim/comp obs kept
    row_index[c(primary_obs, comparison_obs)] <- TRUE
    attr_df  <- attr_df[ row_index, ]
    group_by <- group_by[row_index]
  }
  if(is.null(inc_var_nms) == FALSE)
    attr_df <- attr_df[, colnames(attr_df) %in% inc_var_nms]
  .n <- nrow(attr_df)
  .p <- ncol(attr_df)
  
  ## Orthonormalize each row.
  .m <- sapply(1:.n, function(i){
    attr_df[i, ] <<- sug_basis(attr_df, i)
  })
  
  ## Pivot the attr values (columns) longer to rows.
  attr_df$rownum   <- 1:.n
  attr_df$group_by <- as.factor(group_by)
  class(attr_df)   <- "data.frame"
  .attr_df_longer  <- tidyr::pivot_longer(
    attr_df, cols = !c(rownum, group_by),
    names_to = "var_name", values_to = "contribution")
  .df_basis_distr  <- dplyr::mutate(
    .attr_df_longer, .keep = "none", x = contribution,
    ## Must be reverse order; var 1 on top, highest value.
    y = (rep_len(.p:1, .n * .p) + .05 * (as.integer(group_by) - 2)) / .p,
    var_name = var_name, var_num = rep_len(.p:1, .n * .p),
    rownum = rownum, group_by = group_by) %>%
    as.data.frame()
  
  ## IF FACETED:
  if(.is_faceted){
    ## "_basis_" becomes an honorary level of facet_var
    .df_basis_distr <- spinifex:::.bind_elements2df(list(
      facet_var = rep_len("_basis_", nrow(.df_basis_distr))), .df_basis_distr)
  }
  
  ## Map (display dim) to position
  .df_basis_distr[, c("x", "y")] <- spinifex::map_relative(
    .df_basis_distr[, c("x", "y")], position, .map_to)
  
  .alpha <- logistic_tform(.n) / 5
  ## Basis/attribution distribution of the rows of the attr_df
  ret <- list(suppressWarnings(ggplot2::geom_point(
    ggplot2::aes(x, y, color = group_by),
    .df_basis_distr, shape = pcp_shape, alpha = .alpha, size = 1.5)))
  
  #### Add basis rectangle/dashed zero line ----
  ## Initialize data.frames, before scaling
  .df_rect <- data.frame(x = c(-1, 1), y = c(.5, .p + .5) / .p)
  .df_seg0 <- data.frame(x = 0, y = c(.5, .p + .5) / .p)
  .df_rect <- spinifex::map_relative(.df_rect, position, .map_to)
  .df_seg0 <- spinifex::map_relative(.df_seg0, position, .map_to)
  if(.is_faceted){
    .facet_var <- list(facet_var = "_basis_")
    .df_rect   <- spinifex:::.bind_elements2df(.facet_var, .df_rect)
    .df_seg0   <- spinifex:::.bind_elements2df(.facet_var, .df_seg0)
  }
  ## Add protos
  ret <- c(
    ret,
    ggplot2::geom_segment(
      ggplot2::aes(x = min(x), y = min(y), xend = max(x), yend = max(y)),
      .df_seg0, color = "grey80", linetype = 2),
    ## Outside rectangle, grey60, unit - width, (height = p + 1)
    ggplot2::geom_rect(
      ggplot2::aes(xmin = min(x), xmax = max(x), ymin = min(y), ymax = max(y)),
      .df_rect, fill = NA, color = "grey60"))
  
  #### Add PCP lines if needed. ----
  ## Make the right table to inner join to.
  .df_basis_distr2 <- dplyr::mutate(
    .df_basis_distr, .keep = "none",
    xend = x, yend = y, var_num = var_num - 1, rownum = rownum)
  ## Inner join to by var_name & rownum(lead1)
  .df_pcp <- dplyr::inner_join(
    .df_basis_distr, .df_basis_distr2,
    by = c("var_num" = "var_num", "rownum" = "rownum"))
  
  if(do_add_pcp_segments == TRUE){
    ## Background pcp lines
    pcp_lines <- list(
      suppressWarnings(ggplot2::geom_segment(
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                     tooltip = rownum, color = group_by),
        .df_pcp, size = .5, alpha = .alpha / 2)))
    ## ret is the rug distribution atm
    ret <- c(ret, pcp_lines)
  }
  
  ### Regardless of the rest of pcp lines draw prim/comp
  ## Add comp_obs highlight
  if(length(comparison_obs) > 0)
    ret <- c(ret, list(suppressWarnings(ggplot2::geom_segment(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, tooltip = rownum),
      .df_pcp[.df_pcp$rownum == comparison_obs,, drop = FALSE],
      color = "black", size = .8, alpha = .6, linetype = 3))))
  ## Add primary_obs highlight
  if(length(primary_obs) > 0)
    ret <- c(ret, list(suppressWarnings(ggplot2::geom_segment(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, tooltip = rownum),
      .df_pcp[.df_pcp$rownum == primary_obs,, drop = FALSE],
      color = "black", size = 1, alpha = .8, linetype = 2))))
  
  ## Return
  ret
}


## Final visuals ------

#' The legwork behind the scenes for the global view
#' 
#' Not meant for end user. This is an internal legwork function to 
#' keep from repeating global_view code.
#' 
#' @param cheem_ls A return from `cheem_ls()`, a list of data frames.
#' @param primary_obs The rownumber of the primary observation. Its local
#' attribution becomes the 1d projection basis, and the point it highlighted 
#' as a dashed line. Defaults to NULL, no highlighting applied.
#' @param comparison_obs The rownumber of the comparison observation. Point
#' is highlighted as a dotted line. Defaults to NULL, no highlighting applied.
#' @param color The name of the column in cheem_ls$global_view_df to map to 
#' color. Expects c("default", "residual", "log_maha.data", "cor_attr_proj.y").
#' Defaults to "default"; predicted_class for classification, dummy class 
#' for regression.
#' @return a `ggplot2` object, waiting to be rendered with `global_view` or 
#' `global_view_subplots`.
#' @family cheem consumers
# #' @export
# #' @examples
# #' library(cheem)
# #' 
# #' ## Regression setup
# #' dat  <- amesHousing2018_NorthAmes
# #' X    <- dat[, 1:9]
# #' Y    <- dat$SalePrice
# #' clas <- dat$SubclassMS
# #' 
# #' ## Cheem visual
# #' ames_rf_chm <- cheem_ls(X, Y, ames_rf_shap, ames_rf_pred, clas,
# #'                         label = "North Ames, RF, SHAP")
# #' if(interactive()){
# #'   global_view(ames_rf_chm) ## Preview spaces
# #' }
# #' 
# #' ## Other global_view() options
# #' if(interactive()){
# #'   global_view_legwork(ames_rf_chm) ## most of the way there
# #'   global_view(ames_rf_chm, as_ggplot = TRUE) ## early return of ggplot
# #'   global_view(ames_rf_chm) ## uses ggplot facets %>% plotly
# #' 
# #'   ## Different color mappings, especially for regression
# #'   global_view(ames_rf_chm, color = "residual")
# #'   global_view(ames_rf_chm, color = "log_maha.data")
# #'   global_view(ames_rf_chm, color = "cor_attr_proj.y")
# #' }
global_view_legwork <- function(
    cheem_ls,
    primary_obs    = NULL,
    comparison_obs = NULL,
    color          = c("default", "residual", "log_maha.data", "cor_attr_proj.y")
    #,subplot_r_ind  = NULL ## May be needed with subplots.
){
  ## Prevent global variable warnings:
  V1 <- V2 <- ggtext <- projection_nm <- label <- tooltip <- NULL
  height_px <- width_px <- 
  ## Initialize
  global_view_df <- cheem_ls$global_view_df
  prob_type      <- cheem_ls$type
  ## Aesthetics
  .alpha <- logistic_tform(nrow(cheem_ls$decode_df))
  color  <- match.arg(color)
  if(color %in% c("default", colnames(global_view_df)) == FALSE)
    stop(paste0("global_view: `color` column ", color,
                " not in the cheem_ls. Try reprocessing the dataset."))
  ## Classification
  if(prob_type == "classification"){
    if(color == "default") color <- "predicted_class"
    .color <- global_view_df[, color]
    .shape <- global_view_df[, "predicted_class"]
  }
  ## Regression or non-linear embedding
  if(prob_type %in% c("regression", "No y provided")){
    if(color == "default"){
      .color <- factor(FALSE)
    }else .color <- global_view_df[, color]
    .shape   <- factor(FALSE)
  }
  if(color == "cor_attr_proj.y") .lim <- c(-1, 1) else .lim <- NULL
  .col_scale <- color_scale_of(.color, limits = .lim)
  
  ## Proto for main points
  pts_main <- list()
  .u_nms   <- unique(global_view_df$label)
  ## If classification: residual/obs LM line behind pts
  if(prob_type == "regression")
    pts_main <- c(ggplot2::geom_smooth(
      ggplot2::aes(V1, V2), subset(global_view_df, label == .u_nms[3]),
      method = "lm", formula = y ~ x, se = FALSE), pts_main)
  ## If classification circle misclassified points
  if(prob_type == "classification"){
    .rn_misclass <- which(cheem_ls$decode_df$is_misclassified == TRUE)
    .idx_misclas <- global_view_df$rownum %in% .rn_misclass
    if(sum(.idx_misclas) > 0)
      pts_main <- c(pts_main, ggplot2::geom_point(
        ggplot2::aes(V1, V2), data = global_view_df[.idx_misclas, ],
        color = "red", fill = NA, shape = 21, size = 3, alpha = .alpha))
  }
  ## Highlight primary obs, if passed
  if(is.null(primary_obs) == FALSE){
    .idx_shap <- global_view_df$rownum == primary_obs
    if(sum(.idx_shap) > 0)
      pts_main <- c(pts_main, ggplot2::geom_point(
        ggplot2::aes(V1, V2), data = global_view_df[.idx_shap, ],
        size = 5, shape = 8, color = "black"))
  }
  ## Highlight comparison obs, if passed
  if(is.null(comparison_obs) == FALSE){
    .idx_comp  <- global_view_df$rownum == comparison_obs
    if(sum(.idx_comp) > 0)
      pts_main <- c(pts_main,ggplot2::geom_point(
        ggplot2::aes(V1, V2), data = global_view_df[.idx_comp, ],
        size = 3, shape = 4, color = "black"))
  }
  ## Add main points
  pts_main <- c(pts_main, suppressWarnings(ggplot2::geom_point(
    ggplot2::aes(color = .color, shape = .shape, tooltip = tooltip),
    alpha = .alpha)), .col_scale)
  
  ## Visualize
  gg <- global_view_df %>% plotly::highlight_key(~rownum) %>%
    ggplot2::ggplot(ggplot2::aes(V1, V2)) +
    pts_main +
    ggplot2::coord_fixed() +
    ggplot2::theme_bw() +
    ggplot2::labs(color = substitute(color), fill = substitute(color)) +
    ggplot2::theme(axis.text       = ggplot2::element_blank(),
                   axis.ticks      = ggplot2::element_blank(),
                   axis.title      = ggplot2::element_blank(), 
                   legend.position = "off")
  
  gg
}



#' Linked `plotly` display, global view of data and attribution space.
#' 
#' from a cheem_ls() list, create a linked `plotly` of the global data-
#' and attribution- spaces. Typically consumed directly by shiny app.
#' 
#' @param cheem_ls A return from `cheem_ls()`, a list of data frames.
#' @param primary_obs The rownumber of the primary observation. Its local
#' attribution becomes the 1d projection basis, and the point it highlighted 
#' as a dashed line. Defaults to NULL, no highlighting applied.
#' @param comparison_obs The rownumber of the comparison observation. Point
#' is highlighted as a dotted line. Defaults to NULL, no highlighting applied.
#' @param height_px The height in pixels of the returned `plotly` plot.
#' Defaults to 480, does nothing when `as_ggplot` is TRUE.
#' @param width_px The width in pixels of the returned `plotly` plot.
#' Defaults to 1440, does nothing when `as_ggplot` is TRUE.
#' @param color The name of the column in cheem_ls$global_view_df to map to 
#' color. Expects c("default", "residual", "log_maha.data", "cor_attr_proj.y").
#' Defaults to "default"; predicted_class for classification, dummy class 
#' for regression.
#' @param as_ggplot Logical, if TRUE returns the plots before being passed to
#' `plotly` functions.
#' @return A `plotly` plot, an interactive html widget of the global view, 
#' first two components of the basis of the data- and attribution- spaces.
#' @seealso [run_app()]
#' @export
#' @family cheem consumers
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## global_view()
#' ames_rf_chm <- cheem_ls(X, Y, ames_rf_shap, ames_rf_pred, clas,
#'                         label = "North Ames, RF, SHAP")
#' if(interactive()){
#'   cheem:::global_view_legwork(ames_rf_chm) ## most of the way there
#'   global_view(ames_rf_chm, as_ggplot = TRUE) ## early return of ggplot
#'   global_view(ames_rf_chm) ## uses ggplot facets %>% plotly
#' 
#'   ## Different color mappings, especially for regression
#'   global_view(ames_rf_chm, color = "residual")
#'   global_view(ames_rf_chm, color = "log_maha.data")
#'   global_view(ames_rf_chm, color = "cor_attr_proj.y")
#' }
#' ## Also consumed by: ?run_app()
global_view <- function(
  cheem_ls,
  primary_obs    = NULL,
  comparison_obs = NULL,
  color          = c("default", "residual", "log_maha.data", "cor_attr_proj.y"),
  height_px      = 480,
  width_px       = 1440,
  as_ggplot      = FALSE
){
  label <- NULL ## get in from of global variable NOTE
  gg <- global_view_legwork(cheem_ls, primary_obs, comparison_obs, color) +
    ggplot2::facet_grid(cols = ggplot2::vars(label))
  ## adding facet_grid causes: 
  # Error in ggplot2::geom_smooth(ggplot2::aes(V1, V2), subset(global_view_df,  : 
  # Problem while computing stat.
  # ℹ Error occurred in the 1st layer.
  # Caused by error in `seq_len()`:
  #   ! argument must be coercible to non-negative integer
  if(as_ggplot) return(gg + ggplot2::theme(aspect.ratio = 1))
  
  ## Plotly options & box selection
  gg %>%
    plotly::ggplotly(tooltip = "tooltip", height = height_px, width = width_px) %>%
    plotly::config(displayModeBar = FALSE) %>%                  ## Remove html buttons
    plotly::layout(dragmode = "select", showlegend = FALSE) %>% ## Set drag left mouse
    plotly::event_register("plotly_selected") %>%               ## Reflect "selected", on release of the mouse button.
    plotly::highlight(on = "plotly_selected", off = "plotly_deselect") %>%
    suppressWarnings() 
  ## attempt to suppress:
  #Warning: The following aesthetics were dropped during statistical transformation: x_plotlyDomain, y_plotlyDomain
  #??? This can happen when ggplot fails to infer the correct grouping structure in the data.
  #??? Did you forget to specify a `group` aesthetic or to convert a numerical variable into a factor?
}



#' Cheem tour; 1D manual tour on the selected attribution
#' 
#' Create a linked `plotly`of the global data-
#' and attribution- spaces. Typically consumed directly by shiny app.
#' 
#' @param cheem_ls A return from `cheem_ls()`, a list of data frames.
#' @param basis A 1D projection basis, typically a return of 
#' `sug_basis()`.
#' @param manip_var The , _number_ of the manipulation variable.
#' @param primary_obs The rownumber of the primary observation. Its local
#' attribution becomes the 1d projection basis, and the point it highlighted 
#' as a dashed line. Defaults to NULL, no primary observation highlighted.
#' @param comparison_obs The rownumber of the comparison observation. Point
#' is highlighted as a dotted line. Defaults to NULL, 
#' no comparison observation highlighted.
#' @param do_add_pcp_segments Logical, whether or not to add parallel coordinate
#' line segments to the basis display.
#' @param pcp_shape The number of the shape character to add. Expects
#' 3, 142, or 124, '+', '|' in `plotly`, and '|' in `gganimate`, respectively. 
#' Defaults to 3, '+' in either output.
#' @param angle The step size between interpolated frames, in radians. 
#' Defaults to .15.
#' @param row_index Numeric index of selected observations. 
#' Defaults to TRUE; 1:n.
#' @param inc_var_nms A vector of the names of the variables to include in the 
#' projection.
#' @param do_center_frame Whether or not to scale by standard deviations away 
#' from the mean within each frame or not.
#' Defaults to TRUE, helping to keep the animation centered.
#' @param do_add_residual Whether of not to add a facet with a fixed y on 
#' residual. Doing so may cause issues with animation. Defaults to FALSE.
#' @return ggtour (`ggplot2` object with frame info) animation frames of a 
#' radial tour manipulating the contribution of a selected tour. Animated with 
#' `spinifex::animate_*` functions.
#' @seealso [run_app()]
#' @family cheem consumers
#' @export
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## radial_cheem_tour()
#' ames_rf_chm <- cheem_ls(X, Y, ames_rf_shap, ames_rf_pred, clas,
#'                         label = "North Ames, RF, SHAP")
#' bas <- sug_basis(ames_rf_shap, 1)
#' mv  <- sug_manip_var(ames_rf_shap, 1, 2)
#' ggt <- radial_cheem_tour(ames_rf_chm, basis = bas, manip_var = mv)
#' if(interactive()){
#'   ## As a plotly html widget
#'   spinifex::animate_plotly(ggt)
#'   
#'   ## As a gganimation
#'   spinifex::animate_gganimate(ggt, render = gganimate::av_renderer())
#' }
#' ## Also used in: ?run_app()
radial_cheem_tour <- function(
  cheem_ls, basis, manip_var, 
  primary_obs         = NULL,
  comparison_obs      = NULL,
  do_add_pcp_segments = TRUE,
  pcp_shape           = c(3, 142, 124), ## '+', '|' plotly and  '|' gganimate, or  respectively
  angle               = .15,
  row_index           = NULL,
  inc_var_nms         = NULL,
  do_center_frame     = TRUE,
  do_add_residual     = FALSE
){
  if(is.null(row_index) == FALSE)
    if(sum(row_index) == 0)
      stop("radial_cheem_tour: sum of row_index was 0.")
  
  ## Initialize
  x <- y <- NULL
  decode_df <- cheem_ls$decode_df
  .prim_obs <- primary_obs    ## Proto_basis1d_distribution EXPECTS NUMERIC INDEX;
  .comp_obs <- comparison_obs ## Don't coerce to logical index
  .n        <- nrow(decode_df)
  ## column & row indexes
  if(is.null(inc_var_nms)) inc_var_nms <- colnames(cheem_ls$attr_df)
  .col_idx <- colnames(decode_df) %in% inc_var_nms
  if(is.null(row_index) == FALSE){
    ## Change row_index from numeric to logical if needed and replicate
    row_index <- as_logical_index(row_index, .n)
    row_index[c(.prim_obs, .comp_obs)] <- TRUE
  }
  ## Subset columns and scale plot data
  .dat <- decode_df[, .col_idx] %>% spinifex::scale_sd() %>%
    spinifex::scale_01() %>% as.data.frame()
  ## Manual (radial) tour 1d
  .mt_path <- spinifex::manual_tour(basis, manip_var)
  
  ## Problem type & aesthetics
  .prob_type <- cheem_ls$type ## Either "classification" or "regression"
  .alpha <- logistic_tform(.n)
  
  ### Classification case -----
  if(.prob_type == "classification"){
    .pred_clas <- decode_df$predicted_class
    ## ggtour
    ggt <- spinifex::ggtour(.mt_path, .dat, angle = angle,
                            do_center_frame = do_center_frame) +
      ## Density
      spinifex::proto_density(
        aes_args = list(color = .pred_clas, fill = .pred_clas),
        row_index = row_index, rug_shape = pcp_shape) +
      
      #Warning message:
      #In Ops.factor(yscale, x[, 2]) : '*' not meaningful for factors
      ## PCP on Basis, 1D
      proto_basis1d_distribution(
        cheem_ls$attr_df,
        primary_obs = .prim_obs, comparison_obs = .comp_obs,
        position = "bottom1d", group_by = .pred_clas,
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        pcp_shape = pcp_shape, inc_var_nms = inc_var_nms,
       row_index = row_index) +
      ## Basis 1D
      spinifex::proto_basis1d(position = "bottom1d", manip_col = "black") +
      spinifex::proto_origin1d() +
      ## Highlight comparison obs
      spinifex::proto_highlight1d(
        row_index = .comp_obs, mark_initial = FALSE,
        identity_args = list(linetype = 3, alpha = 0.8, color = "black")) +
      ## Highlight shap obs
      spinifex::proto_highlight1d(
        row_index = .prim_obs, mark_initial = FALSE,
        identity_args = list(linetype = 2, alpha = .6, size = .8, color = "black"))
    
    ## return
    ggt
  }
  
  ### Regression case -----
  ## Doubling data to facet on obs and residual.
  if(.prob_type == "regression"){
    ## Scale obs y, resid, df_hline
    .y        <- decode_df$y %>% spinifex::scale_sd() %>% spinifex::scale_01()
    .resid    <- decode_df$residual %>% spinifex::scale_sd() %>% spinifex::scale_01()
    .df_hline <- data.frame(x = FALSE, y = mean(.resid), facet_var = "residual")
    
    # Aesthetics setup
    .class    <- factor(FALSE) #decode_df$class|predicted_class
    .pts_prim_obs <- .pts_comp_obs <- NULL
    ## Condition handle adding residual facet or not
    if(do_add_residual){
      ## Double up data; observed y and residual
      if(is.null(.prim_obs) == FALSE)
        .pts_prim_obs <- c(.prim_obs, .n + .prim_obs)
      if(is.null(.comp_obs) == FALSE)
        .pts_comp_obs <- c(.comp_obs, .n + .comp_obs)
      if(length(.class) > 1){.class_fore <- c(.class, .class)
      } else .class_fore <- .class ## could be dummy factor(FALSE)
      ## Foreground:
      .dat_fore   <- rbind(.dat, .dat)
      .idx_fore   <- c(row_index, row_index)
      .facet_fore <- factor(rep(c("observed y", "residual"), each = 2 * .n))
      .fixed_y    <- c(.y, .resid)
    } else {
      ## not doubled up data; just fixed_observed y
      if(is.null(.prim_obs) == FALSE)
        .pts_prim_obs <- .prim_obs
      if(is.null(.comp_obs) == FALSE)
        .pts_comp_obs <- .comp_obs
      ## Foreground:
      .dat_fore   <- .dat
      .idx_fore   <- row_index
      .facet_fore <- rep("observed y", each = .n)
      .class_fore <- .class
      .fixed_y    <- .y
    }
    
    ## ggtour
    ggt <- spinifex::ggtour(.mt_path, .dat_fore, angle = angle,
                            do_center_frame = do_center_frame) +
      spinifex::facet_wrap_tour(facet_var = .facet_fore, nrow = 1) +
      spinifex::append_fixed_y(fixed_y = .fixed_y) +
      ## Plotly doesn't rotate text in geom_text/annotate.
      ggplot2::theme(
        legend.position = "off",
        axis.title.y = ggplot2::element_text(
          angle = 90, vjust = 0.5),
        panel.spacing = ggplot2::unit(2, "points"), ## Facet spacing
      ) +
      ## Exasperates issues with plotly & geom presence issue.
      #spinifex::proto_frame_cor2(row_index = .idx_fore, position = c(.5, 1.1)) +
      ## Points; 1D proj & fixed y
      spinifex::proto_point(
        aes_args = list(color = .class_fore, shape = .class_fore),
        identity_args = list(alpha = .alpha), row_index = .idx_fore) +
      proto_basis1d_distribution(
        cheem_ls$attr_df, 
        primary_obs = .prim_obs, comparison_obs = .comp_obs,
        position = "floor1d", group_by = .class, pcp_shape = pcp_shape,
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        inc_var_nms = inc_var_nms, row_index = row_index) +
      spinifex::proto_basis1d(position = "floor1d", manip_col = "black") +
      ## Highlight comparison obs
      spinifex::proto_highlight(
        row_index = .pts_comp_obs,
        identity_args = list(size = 3, shape = 4, alpha = 0.6, color = "black")) +
      ## Highlight primary obs
      spinifex::proto_highlight(
        row_index = .pts_prim_obs,
        identity_args = list(size = 5, shape = 8, alpha = .8, color = "black"))
    if(do_add_residual){
      ggt <- ggt +
        ## Use manual geom_hline as proto_hline0 is on all facets.
        ggplot2::geom_hline(ggplot2::aes(yintercept = y), .df_hline, color = "grey40")
    }
  }
  ## Return the static ggtour, animate in app
  ggt
}




# Deprecated plotly subplot visuals -----


#' #' @rdname global_view
#' #' @export
#' global_view_subplots <- function(
#'     cheem_ls,
#'     primary_obs    = NULL,
#'     comparison_obs = NULL,
#'     color          = c("default", "residual", "log_maha.data", "cor_attr_proj.y"),
#'     height_px      = 480,
#'     width_px       = 1440
#' ){
#' 
#' # Creates the ggtour for one facet of the global view display
#' #
#' # @param r_ind logical row index of the data to use
#' # @param data a data frame to subset.
#' global_view_1facet <- function(r_idx, data = global_view_df){
#'   ## Individual ggplots
#'   g1 <- global_view_legwork(
#'     cheem_ls, primary_obs, comparison_obs,
#'     subplot_r_ind = global_view_df$label == .u_nms[1])
#'   g2 <- global_view_legwork(
#'     cheem_ls, primary_obs, comparison_obs,
#'     subplot_r_ind = global_view_df$label == .u_nms[2])
#'   g3 <- global_view_legwork(
#'     cheem_ls, primary_obs, comparison_obs,
#'     subplot_r_ind = global_view_df$label == .u_nms[3])
#'   
#'   ## Individual plotly plots with axes titles
#'   p1 <- plotly::ggplotly(g1) %>%
#'     plotly::layout(xaxis = list(title = paste0(.u_nms[1], " PC1")),
#'                    yaxis = list(title = paste0(.u_nms[1], " PC2")))
#'   p2 <- plotly::ggplotly(g2) %>%
#'     plotly::layout(xaxis = list(title = paste0(.u_nms[2], " PC1")),
#'                    yaxis = list(title = paste0(.u_nms[2], " PC2")))
#'   p3 <- plotly::ggplotly(g3) %>%
#'     plotly::layout(xaxis = list(title = "predicted"),
#'                    yaxis = list(title = "observed"))
#'   sp <- plotly::subplot(p1, p2, p3, titleY = TRUE, titleX = TRUE, margin = 0)
#'   
#'   ## Plotly options & box selection
#'   sp %>%
#'     plotly::ggplotly(tooltip = "tooltip", height = height_px, width = width_px) %>%
#'     plotly::config(displayModeBar = FALSE) %>%                  ## Remove html buttons
#'     plotly::layout(dragmode = "select", showlegend = FALSE) %>% ## Set drag left mouse
#'     plotly::event_register("plotly_selected") %>%               ## Reflect "selected", on release of the mouse button.
#'     plotly::highlight(on = "plotly_selected", off = "plotly_deselect")
#' }




#' #' @rdname radial_cheem_tour
#' #' @export
#' radial_cheem_tour_subplots <- function(
#'     cheem_ls, basis, manip_var,
#'     primary_obs         = NULL, 
#'     comparison_obs      = NULL,
#'     do_add_pcp_segments = TRUE,
#'     pcp_shape           = c(3, 142, 124), ## '+', '|' plotly and  '|' gganimate, or  respectively
#'     angle               = .15,
#'     row_index           = NULL,
#'     inc_var_nms         = NULL,
#'     do_center_frame     = TRUE
#' ){
#'   if(is.null(row_index) == FALSE)
#'     if(sum(row_index) == 0)
#'       stop("radial_cheem_tour: sum of row_index was 0.")
#'   ## Initialize
#'   x <- y <- NULL
#'   decode_df <- cheem_ls$decode_df
#'   .prim_obs <- primary_obs    ## Proto_basis1d_distribution EXPECTS NUMERIC INDEX;
#'   .comp_obs <- comparison_obs ## Don't coerce to logical index
#'   .n <- nrow(decode_df)
#'   ## column & row indexes
#'   if(is.null(inc_var_nms))
#'     inc_var_nms <- colnames(cheem_ls$attr_df)
#'   .col_idx <- colnames(decode_df) %in% inc_var_nms
#'   if(is.null(row_index) == FALSE){
#'     ## Change row_index from numeric to logical if needed and replicate
#'     row_index <- as_logical_index(row_index, nrow(decode_df))
#'     row_index[c(.prim_obs, .comp_obs)] <- TRUE
#'   }
#'   ## Subset columns and scale plot data
#'   .dat <- decode_df[, .col_idx] %>% spinifex::scale_sd() %>%
#'     spinifex::scale_01() %>% as.data.frame()
#'   ## Manual (radial) tour 1d
#'   .mt_path <- spinifex::manual_tour(basis, manip_var)
#'   
#'   ## Problem type & aesthetics
#'   is_classification <- cheem_ls$type == "classification" ## Either "classification" or "regression"
#'   .alpha <- logistic_tform(nrow(decode_df))
#'   ## plotly complains about removed legend when this is inside single_facet.
#'   .t <- ggplot2::theme(
#'     legend.position  = "off", 
#'     legend.direction = "vertical", ## horizontal legends not supported
#'     aspect.ratio     = NULL)       ## aspect.ratio not supported
#'   
#'   ### Classification case -----
#'   if(is_classification){
#'     .pred_clas <- decode_df$predicted_class ## for classification color/shape
#'     ## Left facet, 1D Basis (1/3)
#'     ggt_bas <- spinifex::ggtour(
#'       .mt_path, .dat, angle = angle, do_center_frame = do_center_frame) +
#'       proto_basis1d_distribution(
#'         cheem_ls$attr_df, group_by = .pred_clas, position = "floor1d",
#'         do_add_pcp_segments = as.logical(do_add_pcp_segments),
#'         primary_obs = .prim_obs, comparison_obs = .comp_obs,
#'         pcp_shape = pcp_shape, inc_var_nms = inc_var_nms, row_index = row_index) +
#'       spinifex::proto_basis1d(position = "floor1d", manip_col = "black") + 
#'       .t + ggplot2::xlim(-.1, 1) ## Try to leave space for var names.
#'     ## Right facet, 1D density (2/3)
#'     ggt_dat1d <- spinifex::ggtour(.mt_path, .dat, angle = angle,
#'                                   do_center_frame = do_center_frame) +
#'       spinifex::proto_density(
#'         aes_args = list(color = .pred_clas, fill = .pred_clas),
#'         row_index = row_index, rug_shape = pcp_shape) +
#'       spinifex::proto_origin1d() +
#'       ## Highlight comparison obs, if passed
#'       spinifex::proto_highlight1d(
#'         row_index = .comp_obs, mark_initial = FALSE,
#'         identity_args = list(linetype = 3, alpha = 0.8, color = "black")) +
#'       ## Highlight shap obs
#'       spinifex::proto_highlight1d(
#'         row_index = .prim_obs, mark_initial = FALSE,
#'         identity_args = list(linetype = 2, alpha = .6, size = .8, color = "black")) + .t
#'     ## Plotly
#'     p_bas   <- plotly::ggplotly(ggt_bas)
#'     p_dat1d <- plotly::ggplotly(ggt_dat1d)
#'     ## Return a plotly object, animate_plotly can pass animation options to these.
#'     ggp <- plotly::subplot(p_bas, p_dat1d, titleY = TRUE, titleX = TRUE,
#'                            widths = c(.33, .66), margin = 0) %>%
#'       plotly::layout(showlegend = FALSE)
#'     ## Return a plotly, pass animation options with animate_plotly()
#'     return(ggp)
#'   } else {
#'     ### Regression case -----
#'     
#'     # Creates a ggtour object for one facet of the radial tour
#'     # 
#'     # @param data the data frame to project.
#'     # @param fixed_y Vector of length, n, rows of the data.
#'     radial_tour_1regressionfacet <- function(data = .dat, fixed_y){
#'       spinifex::ggtour(.mt_path, data, angle = angle,
#'                        do_center_frame = do_center_frame) +
#'         spinifex::append_fixed_y(fixed_y = fixed_y) +
#'         # Plotly can't handle text rotation in geom_text/annotate.
#'         spinifex::proto_point(aes_args = list(color = .class, shape = .class),
#'                               identity_args = list(alpha = .alpha),
#'                               row_index = row_index) +
#'         ## Highlight comparison obs
#'         spinifex::proto_highlight(
#'           row_index = .comp_obs,
#'           identity_args = list(size = 3, shape = 4, alpha = 0.6, color = "black")) +
#'         ## Highlight primary obs
#'         spinifex::proto_highlight(
#'           row_index = .prim_obs,
#'           identity_args = list(size = 5, shape = 8, alpha = .8, color = "black"))
#'     }
#'     
#'     ## Doubling data to facet on obs and residual.
#'     .class <- factor(FALSE) #decode_df$class|predicted_class
#'     ## Basis 1D, left
#'     g1 <- spinifex::ggtour(.mt_path, data = .dat, angle = angle,
#'                            do_center_frame = do_center_frame) +
#'       proto_basis1d_distribution(
#'         cheem_ls$attr_df, position = "floor1d", pcp_shape = pcp_shape,
#'         do_add_pcp_segments = as.logical(do_add_pcp_segments),
#'         primary_obs = .prim_obs, comparison_obs = .comp_obs,
#'         inc_var_nms = inc_var_nms, row_index = row_index) +
#'       spinifex::proto_basis1d(position = "floor1d", manip_col = "black") + 
#'       .t + ggplot2::xlim(-.1, 1) ## Try to leave space for var names.
#'     ## Obs y, and residual, (middle and right)
#'     .y     <- decode_df$y %>% spinifex::scale_sd() %>% spinifex::scale_01()
#'     .resid <- decode_df$residual %>% spinifex::scale_sd() %>% spinifex::scale_01()
#'     .df_hline <- data.frame(x = FALSE, y = mean(.resid), facet_var = "residual")
#'     g2 <- radial_tour_1regressionfacet(.dat, .y    ) + .t
#'     g3 <- radial_tour_1regressionfacet(.dat, .resid) + .t +
#'       ggplot2::geom_hline(ggplot2::aes(yintercept = y), .df_hline, color = "grey40")
#'     ## Individual ggplotly with axes titles
#'     p1 <- plotly::ggplotly(g1)
#'     p2 <- plotly::ggplotly(g2) %>%
#'       plotly::layout(yaxis = list(title = "observed y"))
#'     p3 <- plotly::ggplotly(g3) %>%
#'       plotly::layout(yaxis = list(title = "residual"))
#'     ## Return a plotly, pass animation options with animate_plotly()
#'     plotly::subplot(p1, p2, p3, titleY = TRUE, titleX = TRUE, margin = 0) %>%
#'       plotly::layout(showlegend = FALSE)
#'   }
#' }