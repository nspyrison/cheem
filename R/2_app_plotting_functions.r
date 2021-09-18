##TODO: !! This whole file wants to be renamed and recoded as generalized variants
#  Keep in mind extension of RF models. then model and explanation types by DALEX.

##TODO: !! Whole file wants Roxygen descriptions if not examples.


## Other local functions for downstream consumption.
#### Esp to prep for shiny apps.

## create datasets with new, Corrupted OBServations,
#### drawn from the obs each class level 1, then assigned to the other level.
#### then get the shap_layer_ls of them 
assign_cobs_shap_layer_ls <- function(
  data = dat,
  class = clas,
  y = clas, ## Factor implies classification, numeric implies regression
  n_cobs = 5, ## Drawn from level 1, assigned to all other levels.
  sd_coeff = .1 ## Std dev co-efficient of the X/Y values for the cobs. Less is closer to the mean.
){
  ## Initialize
  .clas_w_cobs <- as.factor(class)
  .x_w_cobs    <- as.data.frame(data)
  .n_cobs <- n_cobs[1L] ## Assume length 1.
  ## Classification/regression
  .prob_type   <- problem_type(y)
  if(.prob_type == "classification") .y_w_cobs <- as.integer(y)
  if(.prob_type == "regression")     .y_w_cobs <- y
  
  ## Append COBS if required ----
  if(.n_cobs > 0L){
    set.seed(404L)
    .lvls <- levels(class)
    
    ## Sample cobs from level 1
    #.m <- sapply(1L, function(k){
    k <- 1L
    .not_k <- (1L:length(.lvls))[-k]
    .cobs_df_k <- rnorm_from(dat[class == .lvls[k], ],
                             n_new_obs = .n_cobs,
                             sd_coeff = sd_coeff)
    
    ## Replicate for every non-k level and assign a replicated set to each non-k level
    .cobs_df <- data.frame(NULL)
    .m <- sapply(1L:length(.not_k), function(i){
      .cobs_df <<- rbind(.cobs_df, .cobs_df_k)
    })
    .clas_w_cobs <<- factor(c(
      as.character(.clas_w_cobs), rep(.lvls[.not_k], times = .n_cobs)), levels = .lvls)
    .x_w_cobs <<- rbind(.x_w_cobs, .cobs_df)
    if(.prob_type == "classification") ## Classification problem if Y is a factor
      ## treeshap wants integer classes :(.
      .y_w_cobs <<- c(.y_w_cobs, rep(.not_k, times = .n_cobs))
    if(.prob_type == "regression") ## Regression problem if Y is numeric
      .y_w_cobs <<- c(.y_w_cobs, rnorm(
        .n_cobs, mean(Y[class == .lvls[.not_k]]),
        sd_coeff * sd(Y[class == .lvls[.not_k]])
      ))
  } ## End appending cobs.
  
  ### Create and global assign shap_layer_ls -----
  .shap_layer_ls <- nested_shap_layers(
    .x_w_cobs, .y_w_cobs, n_shap_layers = 1L, class = .clas_w_cobs)
  attr(.shap_layer_ls, "problem_type") <- .prob_type
  attr(.shap_layer_ls, "n_cobs") <- n_cobs
  attr(.shap_layer_ls, "sd_coeff") <- sd_coeff
  .n <- nrow(data)
  attr(.shap_layer_ls, "cobs_msg") <- ifelse(.n_cobs > 0L, paste0(
    "Level-courrupted observations in rows: ",
    (nrow(data) + 1L), " to ", nrow(shap_layer_ls$decode_df), "."),
    "") ## "" being no cobs added.
  assign("shap_layer_ls", .shap_layer_ls, envir = globalenv())
  writeLines(paste0(
    "`shap_layer_ls` assigned to global env (contains ",
    .n_cobs, " cobs drawn from the first level assigned to all other levels).",
    "\nassign_cobs_shap_layer_ls() is complete."))
  
  ## Return NULL, By-product assigns 1 to many "shap_layer_ls_", .n_cobs, "cobs"
  return(NULL)
}
#' @examples
#' ## TESTING ------
#' ## Data setup, palmerpenguins::penguins
#' raw <- palmerpenguins::penguins ## Missing values, visdat::vis_miss(raw)
#' raw_rmna <- raw[!is.na(raw$sex), ]
#' lvls <- levels(raw_rmna$species)
#' ## Filter to closest 2 classes
#' raw_rmna <- raw_rmna[raw_rmna$species %in% lvls[1:2], ]
#' dat <- spinifex::scale_sd(raw_rmna[, 3:6]) %>% as.data.frame()
#' clas <- factor(raw_rmna$species, levels = lvls[1:2]) ## Manually remove 3rd lvl
#' 
#' ## Apply the functions
#' assign_cobs_shap_layer_ls(
#'   data = dat,
#'   class = clas,
#'   Y = clas, ## Factor implies classification, numeric implies regression
#'   n_cobs_per_class_vect = c(5),
#'   sd_coeff = .25)
#' 
#' ## Structure of layer lists.
#' #### 2 layers: data and SHAP of 1 data sets: *_5cobs, 5 cobs per class.
#' names(shap_layer_ls_5cobs)
#' str(shap_layer_ls_5cobs$plot_df)
#' str(shap_layer_ls_5cobs$decode_df)


## Shiny plot functions ------
linked_plotly_func <- function(
  layer_ls,
  shap_obs = NULL,
  comp_obs = NULL,
  height_px = 640L,
  width_px = 640L,
  do_include_maha_qq = FALSE
){
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
  is_classification <- attr(layer_ls, "problem_type") == "classification"
  # ifelse("is_misclassified" %in% colnames(layer_ls$decode_df), TRUE, FALSE)
  pred_clas <- as.factor(FALSE) ## If regression; dummy pred_clas
  if(is_classification == TRUE) pred_clas <-
    layer_ls$decode_df$predicted_class %>%
    rep_len(nrow(plot_df)) %>%
    as.factor()
  
  gg <- plot_df %>%
    plotly::highlight_key(~rownum) %>%
    ggplot(aes(V1, V2))
  ## Red misclassified points, if present
  if(is_classification == TRUE){
    .rn_misclass <- which(layer_ls$decode_df$is_misclassified == TRUE)
    .idx_misclas <- plot_df$rownum %in% .rn_misclass
    if(sum(.idx_misclas) > 0L){
      .df <- plot_df[.idx_misclas, ] %>% plotly::highlight_key(~rownum)
      gg <- gg +
        geom_point(aes(V1, V2), .df,
                   color = "red", fill = NA,
                   shape = 21L, size = 3L, alpha = .alpha)
    }
  }
  ## Highlight comparison obs, if passed
  if(is.null(comp_obs) == FALSE){
    .idx_comp <- plot_df$rownum == comp_obs
    if(sum(.idx_comp) > 0L){
      .df <- plot_df[.idx_comp, ] %>% plotly::highlight_key(~rownum)
      gg <- gg +
        ## Highlight comparison obs
        geom_point(aes(V1, V2, color = pred_clas[.idx_comp]),
                   .df, size = 4L, shape = 4L)
    }
  }
  ## Highlight shap obs, if passed
  if(is.null(shap_obs) == FALSE){
    .idx_shap <- plot_df$rownum == shap_obs
    if(sum(.idx_shap) > 0L){
      .df <- plot_df[.idx_shap, ] %>% plotly::highlight_key(~rownum)
      gg <- gg +
        geom_point(aes(V1, V2, color = pred_clas[.idx_shap]),
                   .df, size = 5L, shape = 8L)
    }
  }
  ## Maha skew text,
  #### geom_text not working with plotly... & annotate() not working with facets...
  if(do_include_maha_qq == TRUE){
    gg <- gg +
      geom_text(aes(x = -Inf, y = Inf, label = ggtext),
                hjust = 0L, vjust = 1L, size = 4L)
  }
  ## Normal points
  gg <- gg +
    suppressWarnings(geom_point(
      aes(V1, V2, color = pred_clas, shape = pred_clas, tooltip = tooltip),
      alpha = .alpha)) +
    facet_grid(rows = vars(projection_nm), cols = vars(layer_nm), scales = "free") +
    theme_bw() +
    labs(x = .xlab, y = .ylab) +
    scale_color_brewer(palette = "Dark2") +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "off")
  
  ## BOX SELECT
  ggplotly(gg, tooltip = "tooltip", height = height_px, width = width_px) %>%
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = "select", showlegend = FALSE) %>% ## Set drag left mouse
    event_register("plotly_selected") %>% ## Reflect "selected", on release of the mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect")
}
#' @examples
#' layer_ls <- shap_layer_ls_5cobs
#' shap_obs <- nrow(shap_layer_ls_5cobs$decode_df)
#' linked_plotly_func(layer_ls, shap_obs)


## radial tour 1d, from shap of obs. default1d + highlight.
manual_tour1d_func <- function(
  layer_ls, basis, mv_name, shap_obs, comp_obs = NULL,
  do_add_pcp_segements = TRUE,
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
  .mt_path <- manual_tour(basis, manip_var = .mv)
  
  ### Classification problem -----
  if(.prob_type == "classification"){
    .dat <- .x %>% scale_01
    ggt <- ggtour(.mt_path, .dat, angle = angle) +
      proto_density(aes_args = list(color = .pred_clas, fill = .pred_clas)) +
      proto_origin1d() +
      proto_basis1d(manip_col = "black") +
      proto_basis1d_distribution(
        layer_ls, group_by = .pred_clas,
        position = "top1d",
        shape = pcp_shape, ## '|' for gganimate/ggplot
        do_add_pcp_segements = as.logical(do_add_pcp_segements),
        shap_obs = shap_obs,
        comp_obs = comp_obs)
      ## Highlight comparison obs, if passed
    ggt <- ggt + 
      proto_highlight1d(
        comp_obs,
        list(color = .pred_clas),
        list(linetype = 3L, alpha = 0.8),
        mark_initial = FALSE) +
      ## Highlight shap obs
      proto_highlight1d(
        shap_obs,
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
      .array[,, i] <<- matrix(c(.mt_path[,, i], rep(0L, .tgt_dim[1L]), 1L),
                              nrow = .tgt_dim[1L], ncol = .tgt_dim[2L]) %>% 
        tourr::orthonormalise()
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
    .dat <- data.frame(.x, .y) %>% scale_01
    ## Plot
    ggt <- ggtour(.array, .dat, angle = angle) +
      ## _points would ideally be _hex or _hdr, but:
      #### _hex doesn't work with plotly
      #### _hdr not made atm.
      ##- Down sample?
      proto_point(
        aes_args = list(color = .pred_clas, fill = .pred_clas),
        identity_args = list(alpha = .alpha)) +
      proto_origin() +
      proto_basis1d(manip_col = "black") +
      proto_basis1d_distribution(
        layer_ls, group_by = .pred_clas,
        position = "top1d",
        shape = 142L, ## '|' for gganimate/ggplot
        do_add_pcp_segements = as.logical(do_add_pcp_segements),
        shap_obs = shap_obs,
        comp_obs = comp_obs) +
      labs(x="1D SHAP projection", y="Actual wages (2020 Euros)")
      ## Highlight comparison obs, if passed
    ggt <- ggt + 
      proto_highlight(
        comp_obs,
        list(color = .pred_clas),
        list(size = 4L, shape = 4L, alpha = 0.5),
        mark_initial = FALSE) +
      ## Highlight shap obs
      proto_highlight(
        shap_obs,
        aes_args = list(color = .pred_clas),
        identity_args = list(size = 5L, shape = 8L, alpha = 1L),
        mark_initial = FALSE)
  }
  
  ## Return the static ggtour, animate in app
  return(ggt)
}
#' @examples
#' layer_ls <- shap_layer_ls_5cobs
#' shap_obs <- nrow(shap_layer_ls_5cobs$decode_df)
#' shap_df <- shap_layer_ls_5cobs$shap_df
#' bas <- basis_local_attribution(shap_df, nrow(shap_df))
#' mv_name <- rownames(bas)[1L]
#' layer_ls=layer_ls;basis=bas;mv_name=mv_name;shap_obs=shap_obs;comp_obs=NULL;
#' manual_tour1d_func(layer_ls, bas, mv_name, shap_obs)

