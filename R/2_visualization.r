## spinifex proto_* extensions ----

#' Extract and format the 1D local attribution basis
#' 
#' Internal function, Extract and format the 1D local attribution basis from 
#' the provided local explanation's attribution.
#' 
#' @param attr_df Return of a local attribution, such as 
#' treeshap_df.
#' @param rownum The rownumber of the primary observation.
#' @return A matrix of the 1D basis
#' @export
#' @examples
#' la_df <- mtcars ## Pretend this is a local attribution data.frame
#' basis_local_attribution(la_df, rownum = 10)
basis_local_attribution <- function(
  attr_df,
  rownum = nrow(attr_df)
){
  ## Remove last column if layer_name
  attr_df <- attr_df[rownum, ]
  ## Extract formatted basis
  LA_bas <- attr_df %>%
    as.numeric %>%
    matrix(ncol = 1L, dimnames = list(colnames(attr_df), "SHAP"))
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
#' set.dir(~)
#' .cheem_ls <- readRDS(here::here("inst/shiny_apps/cheem_initial/data//1preprocess_penguins.rds"))
#' dat <- .cheem_ls$decode_df[, 8:11]
#' clas <- .cheem_ls$decode_df$class
#' attr_df <- .cheem_ls$attr_df[1:nrow(dat), -5]
#' bas <- basis_local_attribution(attr_df, nrow(dat))
#' mv <- manip_var_of(bas) ## Warning is fine.
#' 
#' ## 1D case:
#' mt_path <- manual_tour(bas, mv)
#' 
#' ggt <- ggtour(mt_path, dat) +
#'   proto_density(aes_args = list(color = clas, fill = clas)) +
#'   proto_basis1d() +
#'   proto_basis1d_distribution(attr_df, group_by = clas)
#' \dontrun{
#' animate_plotly(ggt)
#' }
proto_basis1d_distribution <- function(
  cheem_ls, ## Only for distribution of bases.
  group_by = as.factor(FALSE),
  position = c("top1d", "floor1d", "top2d", "floor2d", "off"), ## Needs to match that of `proto_basis1d()`
  shape = c(142, 124), ## '|' for plotly and ggplot respectively
  do_add_pcp_segments = TRUE,
  primary_obs = NULL,
  comparison_obs = NULL,
  inc_vars = TRUE
){
  ## Prevent global variable warnings:
  rownum <- contribution <- var_name <-
    .map_to_unitbox <- .map_to_data <- .map_to_density <- .d <-
    .df_zero <- var_num <- x <- y <- xend <- yend <- NULL
  ## Initialize
  eval(spinifex::.init4proto)
  position <- match.arg(position)
  shape <- shape[1L] ## match.arg only for character values.
  if(shape %in% c(142L, 124L) == FALSE) warning("Unexpected shape used in proto_basis1d_distribution.")
  if(position == "off") return()
  
  attr_df <- cheem_ls$attr_df
  .n <- nrow(attr_df)
  .p <- ncol(attr_df)
  if(identical(inc_vars, TRUE) == FALSE)
    attr_df <- attr_df[, which(colnames(attr_df) %in% inc_vars)]
  
  ## Orthonormalize each row.
  .m <- sapply(1L:nrow(attr_df), function(i){
    row_bas <- basis_local_attribution(attr_df, i)
    attr_df[i, ] <<- tourr::orthonormalise(row_bas)
  })
  ## Pivot the attr values (columns) longer to rows.
  attr_df$rownum <- 1L:.n
  attr_df$group_by <- as.factor(group_by)
  .attr_df_longer <- tidyr::pivot_longer(attr_df,
                                         cols = !c(rownum, group_by),
                                         names_to = "var_name",
                                         values_to = "contribution")
  .df_basis_distr <- dplyr::mutate(
    .attr_df_longer,
    .keep = "none",
    x = contribution,
    ## Must be reverse order; var 1 on top, highest value.
    y = rep_len(.p:1L, .n * .p) + .05 * (as.integer(group_by) - 2L),
    var_name = var_name,
    var_num = rep_len(.p:1L, .n * .p),
    rownum = rownum,
    group_by = group_by) %>%
    as.data.frame()
  
  ## IF FACETED:
  if(.is_faceted == TRUE){
    position = "floor1d"
    ## "_basis_" becomes an honorary level of facet_var
    .df_basis_distr <- spinifex:::.bind_elements2df( ## basis facet always on first/top level
      list(facet_var = rep_len("_basis_", nrow(.df_basis_distr))), .df_basis_distr)
  }
  
  ## Map them to position
  if(.d == 1L){
    .map_to_tgt <- .map_to_density
  } else .map_to_tgt <- .map_to_data
  .df_basis_distr <- spinifex::map_relative(
    .df_basis_distr, position, .map_to_tgt)
  
  .alpha <- logistic_tform(nrow(cheem_ls$decode_df)) / 15L
  ## Basis/attribution distribution of the rows of the attr_df
  rug_distr <- list(suppressWarnings(ggplot2::geom_point(
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
        pcp_lines,
        suppressWarnings(ggplot2::geom_segment(
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend, tooltip = rownum),
          .df_pcp[.df_pcp$rownum == comparison_obs, ],
          color = "black", size = .8, alpha = .6, linetype = 3L)))
    ## Add primary_obs highlight
    if(sum(primary_obs) > 0L)
      pcp_lines <- c(
        pcp_lines,
        suppressWarnings(ggplot2::geom_segment(
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend, tooltip = rownum),
          .df_pcp[.df_pcp$rownum == primary_obs, ],
          color = "black", size = 1L, alpha = .8, linetype = 2L)))
    ## Return
    return(c(rug_distr, pcp_lines))
  }
  
  ## Return proto
  return(rug_distr)
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
#' cheem_ls <- cheem_ls(
#'   x=X, y=Y, n_layers=1, basis_type="pca", class=clas, verbose=T, noisy=T)
#' 
#' linked_global_view(cheem_ls, primary_obs = 1, comparison_obs = 2)
##TODO: example is missing comp obs; x and shap */x
linked_global_view <- function(
  cheem_ls,
  primary_obs = NULL,
  comparison_obs = NULL,
  height_px = 480L,
  width_px = 960L
){
  ## Prevent global variable warnings:
  V1 <- V2 <- ggtext <- projection_nm <- layer_name <- tooltip <- NULL
  .alpha <- logistic_tform(nrow(cheem_ls$decode_df), mid_pt = 500L)
  global_view_df <- cheem_ls$global_view_df ## Init
  is_classification <- cheem_ls$problem_type == "classification"
  pred_clas <- as.factor(FALSE) ## If regression; dummy pred_clas
  if(is_classification == TRUE) pred_clas <-
    cheem_ls$decode_df$predicted_class %>%
    rep_len(nrow(global_view_df)) %>%
    as.factor()
  
  pts_highlight <- list()
  ## Red misclassified points, if present
  if(is_classification == TRUE){
    .rn_misclass <- which(cheem_ls$decode_df$is_misclassified == TRUE)
    .idx_misclas <- global_view_df$rownum %in% .rn_misclass
    if(sum(.idx_misclas) > 0L){
      .df <- global_view_df[.idx_misclas, ] # %>% highlight_key(~rownum)
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
        ggplot2::geom_point(ggplot2::aes(V1, V2), #, color = pred_clas[.idx_comp]),
                            .df, size = 3L, shape = 4L, color = "black")
      )
    }
  }
  ## Highlight shap obs, if passed
  if(is.null(primary_obs) == FALSE){
    .idx_shap <- global_view_df$rownum == primary_obs
    if(sum(.idx_shap) > 0L){
      .df <- global_view_df[.idx_shap, ] # %>% highlight_key(~rownum)
      pts_highlight <- c(
        pts_highlight,
        ggplot2::geom_point(ggplot2::aes(V1, V2),#, color = pred_clas[.idx_shap]),
                            .df, size = 5L, shape = 8L, color = "black")
      )
    }
  }
  
  .bas_data <- cbind(
    as.data.frame(cheem_ls$basis_ls$data_basis), layer_name = "data")
  .map_to_data <- global_view_df[global_view_df$layer_name == "data", c("V1", "V2")]
  .map_to_data[,1L] <-  .map_to_data[,1L] / 3L
  .bas_attr <- cbind(
    as.data.frame(cheem_ls$basis_ls$attribution_basis), layer_name = "SHAP")
  .map_to_attr <- global_view_df[global_view_df$layer_name == "SHAP", c("V1", "V2")]
  .map_to_attr[,1L] <- .map_to_attr[,1L] / 3L
  ## ggplot
  gg <- ggplot2::ggplot(
    data = plotly::highlight_key(global_view_df, ~rownum),
    mapping = ggplot2::aes(V1, V2)) +
    suppressWarnings(ggplot2::geom_point(
      ggplot2::aes(V1, V2, color = pred_clas, shape = pred_clas,
                   tooltip = tooltip), alpha = .alpha)) +
    pts_highlight +
    spinifex::draw_basis(.bas_data, .map_to_data, "bottomleft") +
    spinifex::draw_basis(.bas_attr, .map_to_attr, "bottomleft") +
    ggplot2::facet_grid(rows = ggplot2::vars(projection_nm),
                        cols = ggplot2::vars(layer_name)) +#, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "PC1", y = "PC2") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
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
#' @param row_idx Numeric index of selected observations. Logial
#' Defaults to TRUE; 1:n.
#' @param inc_vars A vector of the names of the variables to include in the projection.
#' @return `plotly` plot of the global view, first 2 components of the basis of
#' the data- and attribution- spaces.
#' @export
#' @examples
#' sub <- DALEX::apartments[1:200, 1:6]
#' X <- sub[, 2:5]
#' Y <- sub$m2.price
#' clas <- sub$district
#' 
#' cheem_ls <- cheem_ls(
#'   x=X, y=Y, basis_type="pca", class=clas, verbose=T, noisy=T)
#' 
#' bas <- basis_local_attribution(cheem_ls$attr_df, rownum = 1)
#' ggt <- radial_cheem_ggtour(cheem_ls, basis=bas, mv_name=colnames(X)[1], 
#'                            primary_obs=1, comparison_obs=2)
#' spinifex::animate_plotly(ggt)
radial_cheem_ggtour <- function(
  cheem_ls, basis, mv_name, primary_obs, comparison_obs = NULL,
  do_add_pcp_segments = TRUE,
  pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
  angle = .2,
  row_idx = TRUE,
  inc_vars = NULL
){
  if(sum(row_idx) == 0L) stop("radial_cheem_ggtour: sum of row_idx was 0.")
  decode_df <- cheem_ls$decode_df
  .n <- nrow(decode_df)
  ## Initialization Y on basis
  .y <- decode_df$y %>% matrix(ncol = 1L)
  if(is.null(inc_vars) == TRUE) 
    inc_vars <- colnames(cheem_ls$attr_df)
  .col_idx <- which(colnames(decode_df) %in% inc_vars)
  .dat <- decode_df[, .col_idx] %>% spinifex::scale_sd()
  
  ## Change row_idx from numeric to logical if needed and replicate
  row_idx   <- as_logical_index(row_idx, .n)
  .prim_obs <- primary_obs    # proto_basis1d_distribution EXPECTS NUMERIC INDEX;
  .comp_obs <- comparison_obs # don't coerce to logical index.
  
  ## Problem type: classification or regression?
  .prob_type <- problem_type(decode_df$y) ## Either "classification" or "regression"
  .pred_clas <- as.factor(FALSE) ## Initialize dummy predicted class
  if(.prob_type == "classification")
    .pred_clas <- decode_df$predicted_class
  .alpha <- logistic_tform(nrow(decode_df), mid_pt = 500L)
  
  ## Manual (radial) tour 1d
  .mv <- which(colnames(cheem_ls$attr_df) == mv_name)
  .mt_path <- spinifex::manual_tour(basis, manip_var = .mv)
  
  ### Classification case -----
  # Classification goes right into vis
  if(.prob_type == "classification"){
    ## classification; subset to selected data
    .dat <- .dat[row_idx, ]
    # browser()
    # ## seems like proto_basis1d_distribution isn't working atleast as static ggplot2
    # debugonce(proto_basis1d_distribution)
    ggt <- spinifex::ggtour(.mt_path, .dat, angle = angle) +
      spinifex::proto_density(
        aes_args = list(color = .pred_clas, fill = .pred_clas)) +
      spinifex::proto_basis1d(manip_col = "black") +
      spinifex::proto_origin1d() +
      ## Highlight comparison obs, if passed
      spinifex::proto_highlight1d(
        row_index = .comp_obs,
        identity_args = list(linetype = 3L, alpha = 0.8, color = "black"),
        mark_initial = FALSE) +
      ## Highlight shap obs
      spinifex::proto_highlight1d(
        row_index = .prim_obs,
        identity_args = list(linetype = 2L, alpha = .6, size = .8, color = "black"),
        mark_initial = FALSE) +
      proto_basis1d_distribution(
        cheem_ls, group_by = .pred_clas,
        position = "top1d",
        shape = pcp_shape, ## '|' for gganimate/ggplot
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        primary_obs = .prim_obs,
        comparison_obs = .comp_obs,
        inc_vars = inc_vars)
    ## No frame correlation for a 1D projection
  }
  
  ### Regression case -----
  # Regression does some work to append y or y_hat to the basis
  if(.prob_type == "regression"){
    .fixed_y <- c(spinifex::scale_sd(decode_df$y),
                  spinifex::scale_sd(decode_df$residual))
    .col <- decode_df$residual ## Want to be able to color on residual.
    
    ## Background:
    proto_bkg <- list()
    if(sum(!row_idx) > 0L){
      .dat_bgk       <- .dat[!row_idx, ]
      .dat_doub_bgk  <- rbind(.dat_bgk, .dat_bgk)
      .facet_col_bgk <- rep(c("observed y", "residual"), each = nrow(.dat_bgk))
      .fixed_y_bgk   <- .fixed_y[c(!row_idx, !row_idx)]
    }
    .doub_prim_obs <- c(.prim_obs, .n + .prim_obs) ## MUST BE NUMERIC FOR OTHER...
    .doub_comp_obs <- c(.comp_obs, .n + .comp_obs) ## MUST BE NUMERIC FOR OTHER...
    ## Foreground:
    .dat_fore       <- .dat[row_idx, ]
    .dat_doub_fore  <- rbind(.dat_fore, .dat_fore)
    .facet_col_fore <- rep(c("observed y", "residual"), each = nrow(.dat_fore))
    .fixed_y_fore   <- .fixed_y[c(row_idx, row_idx)]
    
    if(sum(!row_idx) > 0L){
      messgae("proto_point.1d_fix_y has data arg, which won't play nice with facet_wrap_tour.
              may need to go to a row_index arg to accomadate.")
      browser()
    }
    ggt <- spinifex::ggtour(.mt_path, .dat_doub_fore, angle = angle) +
      spinifex::facet_wrap_tour(facet_var = .facet_col_fore, nrow = 1L) +
      # Plotly can't handle text rotation in geom_text/annotate.
      ggplot2::labs(x = "Attribution projection, 1D",
                    y = "_basis_ | observed y | residual")
    if(sum(!row_idx) > 0L)
      ggt <- ggt + spinifex::proto_point.1d_fix_y( ## cannot be made before ggtour()
        aes_args = list(shape = .pred_clas),
        identity_args = list(alpha = .alpha, color = "grey80"),
        data = .dat_doub_bgk,
        fixed_y = .fixed_y_bgk)
    ## Foreground
    ggt <- ggt + spinifex::proto_point.1d_fix_y( ## Wants to come early as it appends data$y
      aes_args = list(color = .pred_clas, shape = .pred_clas),
      identity_args = list(alpha = .alpha),
      fixed_y = .fixed_y_fore) +
      # spinifex::proto_frame_cor(data = .dat_fore) + ## Doesn't know of fixed y...
      proto_basis1d_distribution(
        cheem_ls, group_by = .pred_clas,
        position = "top2d",
        shape = c(142L, 124L), ## '|' for plotly/gganimate.
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        primary_obs = primary_obs,
        comparison_obs = comparison_obs) +
      spinifex::proto_basis1d(position = "top2d", manip_col = "black") +
      ## Highlight comparison obs
      spinifex::proto_highlight(
        row_index = .doub_comp_obs,
        identity_args = list(size = 3L, shape = 4L, alpha = 0.6, color = "black"),
        mark_initial = FALSE) +
      ## Highlight primary obs
      spinifex::proto_highlight(
        row_index = .doub_prim_obs,
        identity_args = list(size = 5L, shape = 8L, alpha = .8, color = "black"),
        mark_initial = FALSE)
  }
  
  ## Return the static ggtour, animate in app
  return(ggt)
}


