## Testing plotly existance issues, esp with frame cor2
## Setup ----
require(spinifex)
require(cheem)
#?cheem::radial_cheem_ggtour()

sub <- amesHousing2018_thin[1:200, ]
X <- sub[, 1:9]
Y <- log(sub$SalePrice)
clas <- sub$ZoneMS

## Does adding Sys.sleep make RStudio lesss likely to crash?
rf_fit  <- default_rf(X, Y); Sys.sleep(.01);
shap_df <- attr_df_treeshap(rf_fit, X); Sys.sleep(.01);
this_ls <- cheem_ls(X, Y, class = clas,
                    model = rf_fit,
                    attr_df = shap_df); Sys.sleep(.01);

bas <- basis_local_attribution(shap_df, rownum = 1)
# Error in paste(group, frame, sep = "-") : object 'group' not found
debugonce(radial_cheem_ggtour)
ggt <- radial_cheem_ggtour( 
  this_ls, basis = bas, manip_var = 1, angle = .5)
animate_plotly(ggt)
?animate_gganimate(ggt, ) ## toggles back and forth (with frame cor2)

## local variations of radial_cheem_ggtour:

radial_cheem_ggtour_base <- function(
  cheem_ls = this_ls, basis = bas, manip_var = 1,
  primary_obs = NULL,
  comparison_obs = NULL,
  do_add_pcp_segments = TRUE,
  pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
  angle = .5,
  row_index = NULL,
  inc_vars = NULL
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
  
  ### Regression case -----
  # Regression does some work to append y or y_hat to the basis
  if(.prob_type == "regression"){
    .fixed_y <- c(spinifex::scale_sd(decode_df$y),
                  spinifex::scale_sd(decode_df$residual))
    ## Background:
    proto_bkg <- list()
    if(sum(!sel_index) > 0L){
      .dat_bgk   <- .dat[!sel_index, ]
      .dat_bgk   <- rbind(.dat_bgk, .dat_bgk)
      .facet_bgk <- rep(c("observed y", "residual"), each = nrow(.dat_bgk))
      .idx_bkg   <- c(!sel_index, !sel_index)
      .fix_y_bgk <- .fixed_y[.idx_bkg]
    }
    .doub_prim_obs <- c(.prim_obs, .n + .prim_obs) ## MUST BE NUMERIC FOR OTHER...
    .doub_comp_obs <- c(.comp_obs, .n + .comp_obs) ## MUST BE NUMERIC FOR OTHER...
    ## Foreground:
    .dat_fore   <- rbind(.dat[sel_index, ], .dat[sel_index, ])
    .facet_fore <- rep(c("observed y", "residual"), each = nrow(.dat_fore))
    .idx_fore   <- c(sel_index, sel_index)
    .fix_y_fore <- .fixed_y[.idx_fore]
    ## Facet indexes:
    .idx_obs_y  <- c(sel_index, rep(FALSE, .n))
    .idx_resid  <- c(rep(FALSE, .n), sel_index)
    
    ggt <- spinifex::ggtour(.mt_path, .dat_fore, angle = angle) +
      spinifex::facet_wrap_tour(facet_var = .facet_fore, nrow = 1L) +
      # Plotly can't handle text rotation in geom_text/annotate.
      ggplot2::labs(x = "Attribution projection",
                    y = "observed y | residual")
    ## Background
    if(sum(!sel_index) > 0L)
      ggt <- ggt + spinifex::proto_point.1d_fix_y( ## cannot be made before ggtour()
        aes_args = list(shape = .class),
        identity_args = list(alpha = .alpha, color = "grey80"),
        row_index = .idx_bkg,
        fixed_y = .fix_y_bgk)
    ## Foreground
    #browser()
    ggt <- ggt + spinifex::proto_point.1d_fix_y( ## Wants to come early as it appends data$y
      aes_args = list(color = .class, shape = .class),
      #list(color = reg_color, shape = reg_shape),
      identity_args = list(alpha = .alpha),
      row_index = .idx_fore,
      fixed_y = .fix_y_fore) +
      spinifex::proto_frame_cor2(row_index = .idx_obs_y) +
      spinifex::proto_frame_cor2(row_index = .idx_resid) +
      proto_basis1d_distribution(
        cheem_ls$attr_df,
        position = "floor2d",
        shape = c(142L, 124L), ## '|' for plotly/gganimate.
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        primary_obs = primary_obs,
        comparison_obs = comparison_obs) +
      spinifex::proto_basis1d(position = "floor2d", manip_col = "black") +
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


radial_cheem_ggtour_wo_frame_cor <- function(
  cheem_ls = this_ls, basis = bas, manip_var = 1,
  primary_obs = NULL,
  comparison_obs = NULL,
  do_add_pcp_segments = TRUE,
  pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
  angle = .5,
  row_index = NULL,
  inc_vars = NULL
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
  
  ### Regression case -----
  # Regression does some work to append y or y_hat to the basis
  if(.prob_type == "regression"){
    .fixed_y <- c(spinifex::scale_sd(decode_df$y),
                  spinifex::scale_sd(decode_df$residual))
    ## Background:
    proto_bkg <- list()
    if(sum(!sel_index) > 0L){
      .dat_bgk   <- .dat[!sel_index, ]
      .dat_bgk   <- rbind(.dat_bgk, .dat_bgk)
      .facet_bgk <- rep(c("observed y", "residual"), each = nrow(.dat_bgk))
      .idx_bkg   <- c(!sel_index, !sel_index)
      .fix_y_bgk <- .fixed_y[.idx_bkg]
    }
    .doub_prim_obs <- c(.prim_obs, .n + .prim_obs) ## MUST BE NUMERIC FOR OTHER...
    .doub_comp_obs <- c(.comp_obs, .n + .comp_obs) ## MUST BE NUMERIC FOR OTHER...
    ## Foreground:
    .dat_fore   <- rbind(.dat[sel_index, ], .dat[sel_index, ])
    .facet_fore <- rep(c("observed y", "residual"), each = nrow(.dat_fore))
    .idx_fore   <- c(sel_index, sel_index)
    .fix_y_fore <- .fixed_y[.idx_fore]
    ## Facet indexes:
    .idx_obs_y  <- c(sel_index, rep(FALSE, .n))
    .idx_resid  <- c(rep(FALSE, .n), sel_index)
    
    ggt <- spinifex::ggtour(.mt_path, .dat_fore, angle = angle) +
      spinifex::facet_wrap_tour(facet_var = .facet_fore, nrow = 1L) +
      # Plotly can't handle text rotation in geom_text/annotate.
      ggplot2::labs(x = "Attribution projection",
                    y = "observed y | residual")
    ## Background
    if(sum(!sel_index) > 0L)
      ggt <- ggt + spinifex::proto_point.1d_fix_y( ## cannot be made before ggtour()
        aes_args = list(shape = .class),
        identity_args = list(alpha = .alpha, color = "grey80"),
        row_index = .idx_bkg,
        fixed_y = .fix_y_bgk)
    ## Foreground
    #browser()
    ggt <- ggt + spinifex::proto_point.1d_fix_y( ## Wants to come early as it appends data$y
      aes_args = list(color = .class, shape = .class),
      #list(color = reg_color, shape = reg_shape),
      identity_args = list(alpha = .alpha),
      row_index = .idx_fore,
      fixed_y = .fix_y_fore) +
      # spinifex::proto_frame_cor2(row_index = .idx_obs_y) +
      # spinifex::proto_frame_cor2(row_index = .idx_resid) +
      proto_basis1d_distribution(
        cheem_ls$attr_df,
        position = "floor2d",
        shape = c(142L, 124L), ## '|' for plotly/gganimate.
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        primary_obs = primary_obs,
        comparison_obs = comparison_obs) +
      spinifex::proto_basis1d(position = "floor2d", manip_col = "black") +
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


## Comparisons -----
ggt_b <- radial_cheem_ggtour_base(
  this_ls, basis = bas, manip_var = 1, angle = .5)
ggt_wo_cor <- radial_cheem_ggtour_wo_frame_cor(
  this_ls, basis = bas, manip_var = 1, angle = .5)

animate_plotly(ggt_b)
animate_plotly(ggt_wo_cor)


## from primatives ----
?facet_wrap_tour

library(spinifex)
dat     <- scale_sd(flea[, 1:6])
clas    <- flea$species
bas     <- basis_pca(dat)
mv      <- manip_var_of(bas)
mt_path <- manual_tour(bas, manip_var = mv)

### Facet and density -----
ggt <- ggtour(mt_path, dat, angle = .3) +
  facet_wrap_tour(facet_var = clas, ncol = 2, nrow = 2) +
  proto_density(aes_args = list(color = clas, fill = clas, shape = clas),
                do_add_rug =  FALSE) +
  proto_basis1d(position = "floor1d") +
  proto_origin1d()

animate_plotly(ggt) ## removing rugs helped in this case.
animate_gganimate(ggt, render = gganimate::ffmpeg_renderer())

### facet and append y -----
dummy_y <- scale_sd(as.integer(clas) + rnorm(nrow(dat), 0, .5))

ggt <- ggtour(mt_path, dat, angle = .3) +
  facet_wrap_tour(facet_var = clas, ncol = 2, nrow = 2) +
  append_fixed_y(fixed_y = dummy_y) +
  proto_point(list(fill = clas, color = clas)) +
  proto_basis1d(position = "floor1d") +
  proto_origin()

message("removing rugs helped in this case.")
animate_plotly(ggt)
animate_gganimate(ggt, render = gganimate::ffmpeg_renderer())

### facet, append y, and frame_cor -----
ggt <- ggtour(mt_path, dat, angle = .3) +
  facet_wrap_tour(facet_var = clas, ncol = 2, nrow = 2) +
  append_fixed_y(fixed_y = dummy_y) +
  proto_frame_cor2(position = c(.5, 1.1)) +
  proto_point(list(fill = clas, color = clas)) +
  proto_basis1d(position = "floor1d") +
  proto_origin()

message("order of frame cor is an issue; if we remove facet does that help?")
animate_plotly(ggt) 
animate_gganimate(ggt, render = gganimate::ffmpeg_renderer())

### append y, and frame_cor -----
ggt <- ggtour(mt_path, dat, angle = .3) +
  #facet_wrap_tour(facet_var = clas, ncol = 2, nrow = 2) +
  append_fixed_y(fixed_y = dummy_y) +
  proto_point(list(fill = clas, color = clas)) +
  #proto_basis1d(position = "floor1d") +
  proto_origin() +
  proto_frame_cor2(position = c(.5, 1.1))

animate_plotly(ggt) 
animate_gganimate(ggt, render = gganimate::ffmpeg_renderer())


## Doesn't look like line profiling will be very fruitful..
