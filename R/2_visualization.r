## spinifex proto_* extensions ----

#' Extract and format the 1D local attribution basis
#' 
#' Internal function, Extract and format the 1D local attribution basis from 
#' the provided local explanation's attribution.
#' 
#' @param attr_df Return of a local attribution, such as treeshap_df.
#' @param rownum The rownumber of the primary observation. Defaults to 1.
#' @return A matrix of the 1D basis
#' @export
#' @examples
#' library(cheem)
#' ## Regression:
#' sub <- amesHousing2018_thin[1:200, ]
#' X <- sub[, 1:9]
#' Y <- log(sub$SalePrice)
#' clas <- sub$ZoneMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' shap_df <- attr_df_treeshap(rf_fit, X) ## Long runtime!#' basis_local_attribution(attr_df, rownum = 10)
#' basis_local_attribution(shap_df, rownum = 1)
basis_local_attribution <- function(
  attr_df,
  rownum = 1
){
  ## Remove last column if layer_name
  attr_df <- attr_df[rownum, ]
  ## Extract formatted basis
  LA_bas <- attr_df %>%
    as.numeric %>%
    matrix(ncol = 1L, dimnames = list(colnames(attr_df), class(attr_df)[1]))
  return(tourr::orthonormalise(LA_bas))
}


#' Adds the distribution of the row local attributions to a ggtour
#'
#' Adds the distribution of orthonormalized row values of 
#' the specified local explanation `attr_df`. Does not draw the basis itself; 
#' use in conjunction with `proto_basis1d()`.
#'
#' @param cheem_ls A return from `cheem_ls()`, a list of data frames.
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
#' @family ggtour proto
#' @export
#' @examples
#' library(cheem)
#' library(spinifex)
#' ## Classification:
#' X    <- flea[, 1:6]
#' clas <- flea$species
#' Y    <- as.integer(clas)
#' 
#' rf_fit  <- default_rf(X, Y)
#' shap_df <- attr_df_treeshap(rf_fit, X) ## Long runtime!
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                      model = rf_fit,
#'                      attr_df = shap_df)
#' 
#' bas <- basis_local_attribution(shap_df, rownum = 1)
#' mv <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, mv)
#' 
#' X_scl <- spinifex::scale_sd(X)
#' ggt <- ggtour(mt_path, X_scl, angle = .3) +
#'   proto_density(aes_args = list(color = clas, fill = clas)) +
#'   proto_basis1d() +
#'   proto_basis1d_distribution(shap_df, group_by = clas)
#' \dontrun{
#' animate_plotly(ggt)
#' }
#' 
#' library(cheem)
#' library(spinifex)
#' ## Regression:
#' sub <- amesHousing2018_thin[1:200, ]
#' X <- sub[, 1:9]
#' Y <- log(sub$SalePrice)
#' clas <- sub$ZoneMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' shap_df <- attr_df_treeshap(rf_fit, X) ## Long runtime!
#' 
#' bas <- basis_pca(X)
#' mv  <- manip_var_of(bas)
#' mt_path <- manual_tour(bas, mv)
#' 
#' ggt <- ggtour(mt_path, scale_sd(X), angle = .3) +
#'   proto_point() +
#'   proto_basis1d(position = "top2d") +
#'   proto_basis1d_distribution(shap_df, group_by = clas, position = "top2d")
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_basis1d_distribution <- function(
  attr_df, ## Only for distribution of bases.
  group_by = as.factor(FALSE),
  position = c("top1d", "floor1d", "top2d", "floor2d", "off"), ## Needs to match that of `proto_basis1d()`
  shape = c(142, 124), ## '|' for plotly and ggplot respectively
  do_add_pcp_segments = TRUE,
  primary_obs = 1,
  comparison_obs = 2,
  inc_vars = NULL,
  row_index = NULL
){
  ## Prevent global variable warnings:
  rownum <- contribution <- var_name <-
    .map_to_unitbox <- .map_to_data <- .map_to_density <- .d <-
    .df_zero <- var_num <- x <- y <- xend <- yend <- NULL
  ## Initialize
  eval(spinifex::.init4proto)
  position <- match.arg(position)
  if(position == "off") return()
  shape <- shape[1L] ## match.arg only for character values.
  if(shape %in% c(142L, 124L) == FALSE) warning("Unexpected shape used in proto_basis1d_distribution.")
  
  ## Subset rows then columns
  if(is.null(row_index) == FALSE){
    attr_df  <- attr_df[ c(row_index, primary_obs, comparison_obs), ]
    group_by <- group_by[c(row_index, primary_obs, comparison_obs)]
  }
  if(is.null(inc_vars) == FALSE)
    attr_df <- attr_df[, colnames(attr_df) %in% inc_vars]
  .n <- nrow(attr_df)
  .p <- ncol(attr_df)
  
  ## Orthonormalize each row.
  .m <- sapply(1L:.n, function(i){
    row_bas <- basis_local_attribution(attr_df, i)
    attr_df[i, ] <<- tourr::orthonormalise(row_bas)
  })
  
  ## Pivot the attr values (columns) longer to rows.
  attr_df$rownum <- 1L:.n
  attr_df$group_by <- as.factor(group_by)
  class(attr_df) <- "data.frame"
  
  .attr_df_longer <- tidyr::pivot_longer(attr_df,
                                         cols = !c(rownum, group_by),
                                         names_to = "var_name",
                                         values_to = "contribution")
  .df_basis_distr <- dplyr::mutate(
    .attr_df_longer, .keep = "none", x = contribution,
    ## Must be reverse order; var 1 on top, highest value.
    y = (rep_len(.p:1L, .n * .p) + .05 * (as.integer(group_by) - 2L)) / .p,
    var_name = var_name, var_num = rep_len(.p:1L, .n * .p),
    rownum = rownum, group_by = group_by) %>%
    as.data.frame()
  
  ## IF FACETED:
  if(.is_faceted == TRUE){
    ## "_basis_" becomes an honorary level of facet_var
    .df_basis_distr <- spinifex:::.bind_elements2df(list(
      facet_var = rep_len("_basis_", nrow(.df_basis_distr))), .df_basis_distr)
  }
  
  ## Map them to position
  if(position %in% c("top1d, floor1d")){.map_to_tgt <- .map_to_density
  }else .map_to_tgt <- .map_to_data
  .df_basis_distr <- spinifex::map_relative(
    .df_basis_distr, position, .map_to_tgt)
  
  .alpha <- logistic_tform(.n) / 15L
  ## Basis/attribution distribution of the rows of the attr_df
  ret <- list(suppressWarnings(ggplot2::geom_point(
    ggplot2::aes(x, y, color = group_by, tooltip = rownum),
    .df_basis_distr, shape = shape, alpha = .alpha, size = 1.5)))
  
  ## Add PCP lines if needed.
  if(do_add_pcp_segments == TRUE){
    ## Make the right table to inner join to.
    .df_basis_distr2 <- dplyr::mutate(
      .df_basis_distr, .keep = "none",
      xend = x, yend = y, var_num = var_num - 1L, rownum = rownum)
    ## Inner join to by var_name & rownum(lead1)
    .df_pcp <- dplyr::inner_join(
      .df_basis_distr, .df_basis_distr2,
      by = c("var_num" = "var_num", "rownum" = "rownum"))
    
    ## Background pcp lines
    pcp_lines <- list(
      suppressWarnings(ggplot2::geom_segment(
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                     tooltip = rownum, color = group_by),
        .df_pcp, size = .5, alpha = .alpha / 2L)))
    ## Add comp_obs highlight
    if(length(comparison_obs) > 0L)
      pcp_lines <- c(
        pcp_lines, suppressWarnings(ggplot2::geom_segment(
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend, tooltip = rownum),
          .df_pcp[.df_pcp$rownum == comparison_obs, ],
          color = "black", size = .8, alpha = .6, linetype = 3L)))
    ## Add primary_obs highlight
    if(sum(primary_obs) > 0L)
      pcp_lines <- c(
        pcp_lines, suppressWarnings(ggplot2::geom_segment(
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend, tooltip = rownum),
          .df_pcp[.df_pcp$rownum == primary_obs, ],
          color = "black", size = 1L, alpha = .8, linetype = 2L)))
    ## ret is the rug distribution atm
    ret <- c(ret, pcp_lines)
  }
  
  ## Return
  return(ret)
}


## Shiny plot functions ------

#' Linked `plotly` display, global view of data and attribution space.
#' 
#' Given an attribution layer list, create a linked `plotly`of the global data-
#' and attribution- spaces. Typically consumed directly by shiny app.
#' 
#' @param cheem_ls A return from `cheem_ls()`, a list of data frames.
#' @param primary_obs The rownumber of the primary observation. Its local
#' attribution becomes the 1d projection basis, and the point it highlighted 
#' as a dashed line.
#' @param comparison_obs The rownumber of the comparison observation. Point
#' is highlighted as a dotted line.
#' @param height_px The height in pixels of the returned `plotly` plot.
#' @param width_px The width in pixels of the returned `plotly` plot.
#' @param do_include_maha_qq Logical, whether or not to add the qq plots of the
#' Mahalanobis distance for the data- and attribution- spaces.
#' @param color A vector to map to the point color.
#' Classification case defaults to predicted class, regression case defaults to
#' class if passed to cheem_ls(), else residual.
#' @param shape A vector to map to the point shape.
#' Classification case defaults to predicted class, regression case defaults to
#' class.
#' @return `plotly` plot of the global view, first 2 components of the basis of
#' the data- and attribution- spaces.
#' @export
#' @examples
#' library(cheem)
#' ## Regression:
#' sub <- amesHousing2018_thin[1:200, ]
#' X <- sub[, 1:10]
#' Y <- log(sub$SalePrice)
#' clas <- sub$MS.Zoning
#' 
#' rf_fit  <- default_rf(X, Y)
#' shap_df <- attr_df_treeshap(rf_fit, X) ## Long runtime!
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                      model = rf_fit,
#'                      attr_df = shap_df)
#' 
#' linked_global_view(this_ls)
linked_global_view <- function(
  cheem_ls,
  primary_obs = NULL,
  comparison_obs = NULL,
  color = NULL,
  shape = NULL, 
  height_px = 480L,
  width_px = 960L
){
  ## Prevent global variable warnings:
  V1 <- V2 <- ggtext <- projection_nm <- layer_name <- tooltip <- NULL
  ## Initialize
  global_view_df <- cheem_ls$global_view_df 
  is_classification <- cheem_ls$type == "classification"
  ## Aesthetics
  .alpha <- logistic_tform(nrow(cheem_ls$decode_df))
  ## setup shape and color
  if(cheem_ls$type == "classification"){
    if(is.null(color)) color <- cheem_ls$decode_df$predicted_class %>% as.factor()
    if(is.null(shape)) shape <- cheem_ls$decode_df$predicted_class %>% as.factor()
  }else{
    ## Regression or unexpected type
    if(length(unique(cheem_ls$decode_df$class)) > 1L){
      ## Class defined
      if(is.null(color)) color <- cheem_ls$decode_df$class %>% as.factor()
      if(is.null(shape)) shape <- cheem_ls$decode_df$class %>% as.factor()
    }else{
      ## Class not defined
      if(is.null(color)) color <- cheem_ls$decode_df$residual
      if(is.null(shape)) shape <- as.factor(FALSE)
    }
  }
  global_view_df$color <- color %>% rep_len(nrow(global_view_df))
  global_view_df$shape <- shape %>% rep_len(nrow(global_view_df))
  
  ## Get the bases of the global view, map them
  u_nms <- unique(global_view_df$layer_name)
  .bas_data <- data.frame(cheem_ls$global_view_basis_ls[[1L]],
                          layer_name = u_nms[1L])
  .map_to_data <- global_view_df[global_view_df$layer_name == u_nms[1L], c("V1", "V2")]
  .map_to_data[, 1L] <- .map_to_data[, 1L] / 3L
  .bas_attr <- data.frame(cheem_ls$global_view_basis_ls[[2L]],
                          layer_name = u_nms[2L])
  .map_to_attr <- global_view_df[global_view_df$layer_name == u_nms[2L], c("V1", "V2")]
  .map_to_attr[, 1L] <- .map_to_attr[, 1L] / 3L
  
  ## Proto for main points
  pts_main <- list()
  if(is_discrete(color) == TRUE){
    ### Discrete color mapping
    pts_main <- list(
      suppressWarnings(ggplot2::geom_point(
        ggplot2::aes(color = color, shape = shape,
                     tooltip = tooltip), alpha = .alpha)),
      ggplot2::scale_color_brewer(palette = "Dark2"))
  }
  if(is_discrete(global_view_df$color) == FALSE){
    ### continuous color mapping
    pts_main <- list(
      suppressWarnings(ggplot2::geom_point(
        ggplot2::aes(color = color, shape = shape,
                     tooltip = tooltip),  alpha = .alpha)),
      ggplot2::scale_colour_gradient2(low = scales::muted("blue"),
                                      mid = "grey80",
                                      high = scales::muted("red")))
  }
  
  ## Proto for highlighted points
  pts_highlight <- list()
  ## Red misclassified points, if present
  if(is_classification == TRUE){
    .rn_misclass <- which(cheem_ls$decode_df$is_misclassified == TRUE)
    .idx_misclas <- global_view_df$rownum %in% .rn_misclass
    if(sum(.idx_misclas) > 0L){
      .df <- global_view_df[.idx_misclas, ]
      pts_highlight <- c(
        pts_highlight,
        ggplot2::geom_point(ggplot2::aes(V1, V2), .df,
                            color = "red", fill = NA,
                            shape = 21L, size = 3L, alpha = .alpha)
      )
    }
  }
  ## Highlight comparison obs, if passed
  if(is.null(comparison_obs) == FALSE){
    .idx_comp <- global_view_df$rownum == comparison_obs
    if(sum(.idx_comp) > 0L){
      .df <- global_view_df[.idx_comp, ]
      pts_highlight <- c(
        pts_highlight,
        ## Highlight comparison obs
        ggplot2::geom_point(ggplot2::aes(V1, V2),
                            .df, size = 3L, shape = 4L, color = "black")
      )
    }
  }
  ## Highlight shap obs, if passed
  if(is.null(primary_obs) == FALSE){
    .idx_shap <- global_view_df$rownum == primary_obs
    if(sum(.idx_shap) > 0L){
      .df <- global_view_df[.idx_shap, ]
      pts_highlight <- c(
        pts_highlight,
        ggplot2::geom_point(ggplot2::aes(V1, V2),
                            .df, size = 5L, shape = 8L, color = "black")
      )
    }
  }
  
  ## Visualize
  gg <- ggplot2::ggplot(
    data = plotly::highlight_key(global_view_df, ~rownum),
    mapping = ggplot2::aes(V1, V2)) +
    pts_main +
    pts_highlight +
    spinifex::draw_basis(.bas_data, .map_to_data, "bottomleft") +
    spinifex::draw_basis(.bas_attr, .map_to_attr, "bottomleft") +
    ggplot2::coord_fixed() +
    ggplot2::facet_grid(#rows = ggplot2::vars(projection_nm),
                        cols = ggplot2::vars(layer_name)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "PC1", y = "PC2") +
    ggplot2::theme(axis.text  = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   legend.position = "off")
  
  ## Plotly options & box selection
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
#' @param cheem_ls A return from `cheem_ls()`, a list of data frames.
#' @param basis A 1D projection basis, typically a return of 
#' `basis_local_attribution()`.
#' @param manip_var The , _number_ of the manipulation variable.
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
#' @param row_index Numeric index of selected observations. 
#' Defaults to TRUE; 1:n.
#' @param inc_vars A vector of the names of the variables to include in the projection.
# #' @param reg_color For regression cases, the color of the points. 
# #' Defaults to class if passed to cheem_ls(), else residual.
# #' @param reg_shape For regression cases, the shape of the points.
# #' Defaults to class.
#' @return `plotly` plot of the global view, first 2 components of the basis of
#' the data- and attribution- spaces.
#' @export
#' @examples
#' library(cheem)
#' ## Regression:
#' sub <- amesHousing2018_thin[1:200, ]
#' X <- sub[, 1:9]
#' Y <- log(sub$SalePrice)
#' clas <- sub$ZoneMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' shap_df <- attr_df_treeshap(rf_fit, X)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                      model = rf_fit,
#'                      attr_df = shap_df)
#' 
#' bas <- basis_local_attribution(shap_df, rownum = 1)
#' ggt <- radial_cheem_ggtour(this_ls, basis = bas, manip_var = 1)
#' \dontrun{
#' spinifex::animate_plotly(ggt)
#' }
#' 
#' ## Classification:
#' X    <- flea[, 1:6]
#' clas <- flea$species
#' Y    <- as.integer(clas)
#' 
#' rf_fit  <- default_rf(X, Y)
#' shap_df <- attr_df_treeshap(rf_fit, X) ## Long runtime!
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                      model = rf_fit,
#'                      attr_df = shap_df)
#' 
#' bas <- basis_local_attribution(shap_df, rownum = 1)
#' ggt <- radial_cheem_ggtour(this_ls, basis = bas, manip_var = 1)
#' \dontrun{
#' animate_plotly(ggt)
#' }
radial_cheem_ggtour <- function(
  cheem_ls, basis, manip_var,
  primary_obs = NULL,
  comparison_obs = NULL,
  do_add_pcp_segments = TRUE,
  pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
  angle = .2,
  row_index = NULL,
  inc_vars = NULL#,
  # reg_color = NULL,
  # reg_shape = NULL
){
  if(is.null(row_index) == FALSE)
    if(sum(row_index) == 0L)
      stop("radial_cheem_ggtour: sum of row_index was 0.")
  decode_df <- cheem_ls$decode_df
  .n <- nrow(decode_df)
  if(is.null(inc_vars) == TRUE)
    inc_vars <- colnames(cheem_ls$attr_df)
  .col_idx <- colnames(decode_df) %in% inc_vars
  if(is.null(row_index)) row_index <- 1L:.n
  .prim_obs <- primary_obs    # Proto_basis1d_distribution EXPECTS NUMERIC INDEX;
  .comp_obs <- comparison_obs # Don't coerce to logical index.
  
  ## Subset columns and scalce plot data
  .dat <- decode_df[, .col_idx] %>% spinifex::scale_sd() %>% as.data.frame()
  ggt <- spinifex::last_ggtour()
  
  ## Change row_index from numeric to logical if needed and replicate
  sel_index <- as_logical_index(
    c(row_index, .prim_obs, .comp_obs), nrow(decode_df))
  
  ## Problem type: classification or regression?
  .prob_type <- cheem_ls$type ## Either "classification" or "regression"
  if(.prob_type == "classification")
    .pred_clas <- decode_df$predicted_class ## for classification color/shape
  .class <- decode_df$class ## for regression color/shape
  .alpha <- logistic_tform(nrow(decode_df))
  ## Manual (radial) tour 1d
  .mt_path <- spinifex::manual_tour(basis, manip_var)
  
  ### Classification case -----
  if(.prob_type == "classification"){
    ggt <- spinifex::ggtour(.mt_path, .dat, angle = angle) +
      spinifex::proto_density(
        aes_args = list(color = .pred_clas, fill = .pred_clas),
        row_index = sel_index) +
      spinifex::proto_basis1d(position = "top1d", manip_col = "black") +
      spinifex::proto_origin1d() +
      ## Highlight comparison obs, if passed
      spinifex::proto_highlight1d(
        row_index = .comp_obs, mark_initial = FALSE,
        identity_args = list(linetype = 3L, alpha = 0.8, color = "black")) +
      ## Highlight shap obs
      spinifex::proto_highlight1d(
        row_index = .prim_obs, mark_initial = FALSE,
        identity_args = list(linetype = 2L, alpha = .6, size = .8, color = "black")) +
      proto_basis1d_distribution(
        cheem_ls$attr_df, group_by = .pred_clas, position = "top1d",
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        primary_obs = .prim_obs, comparison_obs = .comp_obs,
        shape = pcp_shape, inc_vars = inc_vars, row_index = row_index)
  }
  
  ### Regression case -----
  ## Doubling data to facet on obs and residual.
  if(.prob_type == "regression"){
    .fixed_y <- c(spinifex::scale_sd(decode_df$y),
                  spinifex::scale_sd(decode_df$residual))
    scale_col <- list() ## Let spinifex_theme default to it "Dark2"
    ## Set reg_color/reg_shape if null
    # if(length(unique(cheem_ls$decode_df$class)) > 1L){
    #   ## Class defined
    #   if(is.null(reg_color)) reg_color <- cheem_ls$decode_df$class %>% as.factor()
    #   if(is.null(reg_shape)) reg_shape <- cheem_ls$decode_df$class %>% as.factor()
    # }else{ 
    #   ## Class not defined
    #   if(is.null(reg_color)) reg_color <- cheem_ls$decode_df$residual
    #   if(is.null(reg_shape)) reg_shape <- as.factor(FALSE)
    #   scale_col <- ggplot2::scale_colour_gradient2(
    #     low = scales::muted("blue"), mid = "grey80", high = scales::muted("red"))
    # }
    
    .doub_prim_obs <- c(.prim_obs, .n + .prim_obs)
    .doub_comp_obs <- c(.comp_obs, .n + .comp_obs)
    ## Foreground:
    .dat_fore   <- rbind(.dat[sel_index, ], .dat[sel_index, ])
    .facet_fore <- factor(rep(c("observed y", "residual"), each = nrow(.dat_fore)))
    .idx_fore   <- c(sel_index, sel_index)
    .fix_y_fore <- .fixed_y[.idx_fore]
    
    if(F){ ## Testing minimal example
      spinifex::ggtour(.mt_path, .dat_fore, angle = angle) +
        spinifex::facet_wrap_tour(facet_var = .facet_fore, nrow = 1L) +
        spinifex::append_fixed_y(fixed_y = .fix_y_fore) +
        spinifex::proto_frame_cor2(row_index = .idx_fore, position = c(.7, -.1)) +
        ggplot2::labs(x = "Attribution projection", y = "observed y | residual") +
        ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 90, vjust = 0.5)) +
        proto_point() +
        proto_basis1d_distribution(cheem_ls$attr_df, position = "floor2d") +
        proto_basis1d("floor2d") +
        spinifex::proto_highlight(row_index = .doub_comp_obs) +
        spinifex::proto_highlight(row_index = .doub_prim_obs)
    }
    
    ggt <- spinifex::ggtour(.mt_path, .dat_fore, angle = angle) +
      spinifex::facet_wrap_tour(facet_var = .facet_fore, nrow = 1L) +
      spinifex::append_fixed_y(fixed_y = .fix_y_fore) +
      spinifex::proto_frame_cor2(row_index = .idx_fore, position = c(.7, .3)) +
      # Plotly can't handle text rotation in geom_text/annotate.
      ggplot2::labs(x = "Attribution projection", y = "observed y | residual") +
      ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 90L, vjust = 0.5),
                     #legend.position = "off"
                     ) +
      # scale_col +
      ## Foreground
      spinifex::proto_point(
        aes_args = list(color = .class, shape = .class),
        #aes_args = list(color = reg_color, shape = reg_shape),
        identity_args = list(alpha = .alpha), row_index = .idx_fore) +
      proto_basis1d_distribution(
        cheem_ls$attr_df, position = "floor2d", shape = c(142L, 124L),
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        primary_obs = primary_obs, comparison_obs = comparison_obs) +
      spinifex::proto_basis1d(position = "floor2d", manip_col = "black") +
      ## Highlight comparison obs
      spinifex::proto_highlight(
        row_index = .doub_comp_obs,
        identity_args = list(size = 3L, shape = 4L, alpha = 0.6, color = "black")) +
      ## Highlight primary obs
      spinifex::proto_highlight(
        row_index = .doub_prim_obs,
        identity_args = list(size = 5L, shape = 8L, alpha = .8, color = "black"))
  }
  
  ## Return the static ggtour, animate in app
  return(ggt)
}


