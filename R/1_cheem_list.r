##TODO: !! This whole file wants to be renamed and recoded as generalized variants
#  Keep in mind extension of RF models. then model and explanation types by DALEX.

##TODO: !! Whole file wants Roxygen descriptions if not examples.



## SHAP layers --------
#' Create the plot data.frame for the global linked plotly display.
#' 
#' Internal function, the plot data.frame of 1 layer, consumed in 
#' local_attr_layer() and format_nested_layers().
#' 
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model.
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Defaults to "pca".
#' @param class The variable to group points by. Originally the _predicted_
#'  class.
#' @param layer_name Character layer name, typically the type of local 
#' attribution used.
#' @return A data.frame, for the global linked plotly display.
#' @examples
#' sub <- DALEX::apartments[1:200, 1:6]
#' x <- sub[, 2:5]
#' y <- sub$m2.price
#' clas <- sub$district
#' 
#' ret <- global_view_df(x, y, "pca", clas, "typically 'data'/'<attribution name>'")
#' str(ret)
global_view_df <- function(
  x, y, basis_type = c("pca", "olda"),
  class = NULL, ## class req for olda, add to _df's
  layer_name
){
  d = 2 ## Fixed display dimensionality 
  ## maha_vect_of() -----
  #maha_vect_of <- function(x, do_normalize = TRUE){ ## distance from median(x), cov(x)
  if(is.null(class)) class <- as.factor(FALSE)
  .lvls <- levels(class)
  maha <- NULL
  ## For each level of the class, find the in-class maha distances
  .m <- sapply(1L:length(.lvls), function(k){
    .sub <- x[class == .lvls[k], ]
    .sub_maha <- mahalanobis(.sub, apply(.sub, 2L, median), cov(.sub)) %>%
      matrix(ncol = 1L)
    maha <<- c(maha, .sub_maha)
  })
  ## 01 normalize (outside class levels)
  maha <- maha %>% as.matrix(ncol = 1L) %>% spinifex::scale_01()
  ## Maha quantiles
  ### Theoretical chi sq quantiles, x of a QQ plot
  .probs <- seq(.001, .999, length.out = nrow(x))
  .qx <- qchisq(.probs, df = nrow(x) - 1L) %>%
    matrix() %>% spinifex::scale_01()
  ### Sample quantiles, y of a QQ plot
  .qy <- quantile(maha, probs = .probs) %>%
    matrix() %>% spinifex::scale_01()
  .AG_kurt_tst_p <- moments::anscombe.test(as.vector(.qy), "less")$p.value
  .maha_skew_text <- paste0(
    "  Anscombe-Glynn p-value: ",
    format(signif(.AG_kurt_tst_p, 3L), scientific = TRUE), "\n",
    "    (testing 1-tailed kurtosis)\n",
    "  Kurtosis - 3: ", round(moments::kurtosis(.qy) - 3L, 2L), "\n",
    "  Skew: ", round(moments::skewness(.qy), 2L), "\n")
  
  ## projection df -----
  basis_type <- match.arg(basis_type)
  basis <- switch(basis_type,
                  pca  = spinifex::basis_pca(x, d),
                  olda = spinifex::basis_olda(x, class, d))
  proj <- as.matrix(x) %*% basis
  proj <- spinifex::scale_01(proj) %>% as.data.frame()
  
  ## Column bind wider, order by rownum
  tooltip <- 1L:nrow(x) ## Init, 
  #### !!OVERWROTE in format_nested_layers, once we have the model to check misclassification
  .plot_df <- cbind(
    1L:nrow(x), class, proj, y, layer_name, basis_type, tooltip, "")
  ## Row bind longer, adding QQ maha, and kurtosis info.
  .q_idx <- order(maha, decreasing = FALSE) ## Order by maha
  .qq_df <- data.frame(1L:nrow(x), class, .qx, .qy, y, layer_name,
    "QQ Mahalanobis distance", tooltip, .maha_skew_text)
  .qq_df <- .qq_df[.q_idx, ] ## Order by maha distances
  colnames(.qq_df) <- colnames(.plot_df) <-
    c("rownum", "class", paste0("V", 1L:d), "y",
      "layer_nm", "projection_nm", "tooltip", "ggtext")
  return(rbind(.plot_df, .qq_df))
}

#' Create the local attribution layer data.frame
#' 
#' Internal function, the local attribution layer data.frame of 1 layer, consumed in 
#' nested_local_attr_layers().
#' 
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model
#' @param xtext Optional, Out Of Sample data to find the local attribution of.
#' @param ytext Optional, Out Of Sample response to measure xtext with 
#' if provided.
#' @param layer_name Character layer name, typically the type of local 
#' attribution used.
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Defaults to "pca".
#' @param class The variable to group points by. Originally the _predicted_
#'  class.
#' @param verbose Logical, Whether or not the function should print tictoc time
#' info. Defaults to TRUE.
#' @param noisy Logical, Whether of not the function should play a beeper tone
#' upon completion. Defaults to TRUE.
#' @return A data.frame, for the full local attribution matrix.
#' @examples
#' sub <- DALEX::apartments[1:200, 1:6]
#' x <- sub[, 2:5]
#' y <- sub$m2.price
#' clas <- sub$district
#' 
#' ret <- local_attr_layer(
#'   x, y, layer_name = "typically 'data'/'<attribution name>'",
#'   basis_type = "pca", class = clas)
#' names(ret)
local_attr_layer <- function(
  x, y, xtest = NULL, ytest = NULL, layer_name = "UNAMED",
  basis_type = c("pca", "olda"), class = NULL, ## class req for olda
  verbose = TRUE, noisy = TRUE
){
  d = 2
  require("treeshap")
  if(noisy == TRUE) require("beepr")
  if(verbose == TRUE){
    require("tictoc")
    tictoc::tic(paste0("local_attr_layer -- ", layer_name))
  }
  
  ## RF model
  .is_y_disc <- is_discrete(y)
  sec_rf <- system.time({
    .m <- gc()
    .hp_mtry <- if(.is_y_disc == TRUE) sqrt(ncol(x)) else ncol(x) / 3L
    .hp_node <- if(.is_y_disc == TRUE) 1L else 5L
    .hp_node <- max(.hp_node, nrow(x) / 500L)
    .hp_ntree <- 500L / 4L ## A function of the data, atleast not as liberal as the default 500 trees
    suppressWarnings( 
      ## suppresses The response has five or fewer unique values.  Are you sure?
      .rf <- randomForest::randomForest(
        y~., data = data.frame(y, x),
        mtry = .hp_mtry, nodesize = .hp_node, ntree = .hp_ntree))
  })[3L]
  
  ## RF performance
  ### MANUALLY created, different than the performance created by the rf fit...
  .pred <- stats::predict(.rf, x)
  .resid <- y - .pred
  .rss <- sum(.resid^2L)
  .tss <- sum((y - mean(y))^2L)
  .mse <- 1L / nrow(x) * .rss %>% round(2L)
  .rmse <- sqrt(.mse) %>% round(2L)
  .rsq <- 1L - (.rss / .tss) %>% round(4L)
  .performance_df <- data.frame(mse = .mse, rmse = .rmse, rsq = .rsq)
  if(is.null(xtest) == FALSE & is.null(ytest) == FALSE){
    .rss_t <- sum((ytest - predict(.rf, xtest))^2L)
    .tss_t <- sum((ytest - mean(ytest))^2L)
    .mse_t <- 1L / nrow(x) * .rss_t %>% round(2L)
    .rmse_t <- sqrt(.mse_t) %>% round(2L)
    .rsq_t <- 1L - (.rss_t/.tss_t) %>% round(4L)
    .performance_df$test_mse <- .mse_t
    .performance_df$test_rmse <- .rmse_t
    .performance_df$test_rsq <- .rsq_t
  }
  
  ## treeshap
  sec_shap <- system.time({
    .m <- gc()
    .shap <- treeshap_df(.rf, x)
    .shap_xtest <- NULL ## Init
    if(is.null(xtest) == FALSE) .shap_xtest <- treeshap_df(.rf, xtest)
  })[3L]
  
  ## global_view_df() of .shap given .rf
  #### On new shap matrix, data's plot df is initialized in format_nested_layers()
  sec_plot_df <- system.time({
    ## If classification, use .pred_clas over class, when passed to plot_df
    is_classification <- ifelse(length(unique(y)) < 5L, TRUE, FALSE)
    if(is_classification == TRUE){
      .lvls <- levels(class)
      .pred_clas <- as.factor(.lvls[round(.pred)])
    }
    .plot_clas <- if(is_classification == TRUE) .pred_clas else class
    .m <- gc()
    .plot_df <- global_view_df(
      .shap, y, basis_type, .plot_clas,
      layer_name = layer_name) #paste0(layer_name, ", rmse = ", .rmse),
  })[3L]
  
  ## Execution time
  time_df <- data.frame(
    runtime_seconds = c(sec_rf, sec_shap, sec_plot_df),
    task = c("rf model", "rf SHAP {treeshap}", "plot_df (PCA/Maha)"),
    layer = layer_name)
  
  if(verbose == TRUE) tictoc::toc()
  if(noisy == TRUE & sum(time_df$runtime_seconds) > 30L) beepr::beep(1L)
  return(list(plot_df = .plot_df,
              rf_model = .rf,
              performance_df = .performance_df,
              shap_df = .shap,
              shap_xtest_df = .shap_xtest,
              time_df = time_df))
}


#' Format the nested local attribution layers
#' 
#' Internal function, formats all layers of all data.frames, consumed in 
#' nested_local_attr_layers().
#' 
#' @parma shap_layer_ls
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Defaults to "pca".
#' @param class The variable to group points by. Originally the _predicted_
#'  class.
#' @param verbose Logical, Whether or not the function should print tictoc time
#' info.
#' @return A list of formated data frames.
format_nested_layers <- function(
  shap_layer_ls, x, y,
  basis_type = c("pca", "olda"),
  class = NULL,
  verbose = TRUE
){
  d = 2
  if(verbose == TRUE) tictoc::tic("format_nested_layers()")
  ## Init with data layer,
  sec_data_plot_df <- system.time({ ## Init with data layer.
    is_classification <- ifelse(length(unique(y)) < 5L, TRUE, FALSE)
    if(is_classification == TRUE){
      .lvls <- levels(class)
      .rf_mod <- shap_layer_ls[[1L]]$rf_model
      .pred_clas <- as.factor(.lvls[round(predict(.rf_mod))])
    }
    .plot_clas <- ifelse(is_classification == TRUE, .pred_clas, class)
    .m <- utils::capture.output(gc())
    
    data_plot_df <- global_view_df(
      x, y, basis_type, .plot_clas, layer_name = "data")
  })[3L]
   
  ### plot_df, bound longer
  b_plot_df <- data.frame(data_plot_df) ## Init data layer plot_df
  .m <- sapply(1L:length(shap_layer_ls), function(i){
    this_plot_df <- shap_layer_ls[[i]]$plot_df
    b_plot_df <<- rbind(b_plot_df, this_plot_df)
  })
  
  ## decode_df_of -----
  #### Information decode(/display) table
  ## bound wider: add prediction, residuals of every layer
  if(is.null(class)) class <- as.factor(FALSE)
  is_classification <- ifelse(length(unique(y)) < 5L, TRUE, FALSE)
  decode_df <- data.frame(rownum = 1L:nrow(x), class = class, y)
  .m <- sapply(1L:length(shap_layer_ls), function(i){
    .rf_mod <- shap_layer_ls[[i]]$rf_model
    .pred <- predict(.rf_mod)
    decode_df <<- cbind(decode_df, .pred, y - .pred) ## Layer residual
    if(is_classification == TRUE){
      .lvls <- levels(class)
      .pred_clas <- as.factor(.lvls[round(.pred)])
      .is_misclass <- .pred_clas != class
      decode_df <- cbind(decode_df, .pred_clas, .is_misclass)
    }
  })
  ## bind wider, adding x, and tooltip
  tooltip <- 1L:nrow(x) ## Init, if rownames null, just row numbers
  ### Add rownames to tooltip if meaningful
  if(is.null(rownames(x)) == FALSE)
    if(does_contain_nonnumeric(rownames(x)) == TRUE)
      tooltip <- paste0("row: ", tooltip, ", ", rownames(x))
  ### If misclassified, add msg to tooltip.
  is_classification <- ifelse(length(unique(y)) < 5L, TRUE, FALSE)
  if(is_classification == TRUE){
    tooltip[.is_misclass] <- paste0(
      tooltip[.is_misclass],
      "\nMisclassified! predicted: ", .pred_clas[.is_misclass],
      ", actual: ", class[.is_misclass])
    tooltip[!.is_misclass] <- paste0(
      tooltip[!.is_misclass], "\nclass: ", class[!.is_misclass])
  }
  decode_df <- cbind(decode_df, x, tooltip)
  .nms <- c("rownum", "class", "y", ## Init common names.
            "prediction", #paste0("prediction_", names(shap_layer_ls)),
            "residual")   #paste0("residual_",   names(shap_layer_ls)),
  if(is_classification == TRUE){
    .nms <- c(.nms, "predicted_class", "is_misclassified", names(x), "tooltip")
  }else{ ## is regression
    .nms <- c(.nms, names(x), "tooltip")
  }
  names(decode_df) <- .nms
  ## Also add tooltip to plot_df, as it doesn't know of the RF, to check misclass
  b_plot_df$tooltip <- dplyr::left_join(
    b_plot_df, decode_df, c("rownum" = "rownum"))$tooltip.y
  
  ### performance of the layers
  ## manual performance, slightly different than that reported by the rf obj itself
  .nms <- names(shap_layer_ls)
  performance_df <- data.frame(NULL)
  .m <- sapply(1L:length(shap_layer_ls), function(i){
    .row <- shap_layer_ls[[i]]$performance_df
    .row$runtime_seconds <- sum(shap_layer_ls[[i]]$time_df$runtime_seconds)
    performance_df <<- rbind(performance_df, .row)
  })
  row.names(performance_df) <- .nms
  
  ### Rbind shap_df
  b_shap_df <- data.frame()
  .m <- sapply(1L:length(shap_layer_ls), function(i){
    .shap_df <- cbind(shap_layer_ls[[i]]$shap_df, .nms[i])
    b_shap_df <<- rbind(b_shap_df, .shap_df)
  })
  colnames(b_shap_df) <- c(colnames(shap_layer_ls[[1L]]$shap_df), "layer_name")
  
  ### Rbind time_df
  b_time_df <- data.frame( ## Init with data layer.
    runtime_seconds = sec_data_plot_df, task = "plot_df (PCA/Maha)", layer = "data")
  .m <- sapply(1L:length(shap_layer_ls), function(i){
    b_time_df <<- rbind(b_time_df, shap_layer_ls[[i]]$time_df)
  })
  
  if(verbose == TRUE) tictoc::toc()
  return(list(
    plot_df = b_plot_df, decode_df = decode_df,
    performance_df = performance_df, shap_df = b_shap_df, time_df = b_time_df
  )) ## Model obj is most of the weight, not kept, limiting reuse, but much lighter output.
}


#' Format the nested local attribution layers
#' 
#' Internal function, formats all layers of all data.frames, consumed in 
#' nested_local_attr_layers().
#' 
#' @parma shap_layer_ls
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model
#' @param xtext Optional, Out Of Sample data to find the local attribution of.
#' @param ytext Optional, Out Of Sample response to measure xtext with 
#' if provided.
#' @param n_layers The number of local attribution layers deep. Defaults to 1, 
#' the local attribution of just the data. In bulk, I think going more layers is
#' going to overfit the data, so be prepared to optimize OOS measures.
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Defaults to "pca".
#' @param class The variable to group points by. Originally the _predicted_
#'  class.
#' @param verbose Logical, Whether or not the function should print tictoc time
#' info. Defaults to TRUE.
#' @param noisy Logical, Whether or not the function should play a beepr tone
#' upon completion. Defaults to TRUE.
#' @return A list of formatted data frames.
#' @export
#' @examples
#' sub <- DALEX::apartments[1:200, 1:6]
#' set.seed(303)
#' .idx_test <- sample(
#'   1:nrow(sub), size = round(.5 * nrow(sub))) ## 20% index for testing
#' X_train <- sub[-.idx_test, 2:5]
#' Y_train <- sub$m2.price[-.idx_test]
#' clas <- sub$district[-.idx_test]
#' X_test  <- sub[.idx_test, ]
#' Y_test  <- sub$m2.price[.idx_test]
#' 
#' layer_ls <- nested_local_attr_layers(
#'   x=X_train, y=Y_train, xtest=X_test, ytest=Y_test,
#'   n_layers=1, basis_type="pca", class=clas, verbose=T, noisy=T)
#' layer_ls$performance_df
####
# x=X_train;y=Y_train;xtest=X_test;ytest=Y_test;class=clas;
# n_layers = 1;basis_type = "pca";verbose=TRUE;noisy = TRUE;
##TODO: ERROR in maha_dist; solve.default(cov, ...) computationally singular
nested_local_attr_layers <- function(
  x, y, xtest = NULL, ytest = NULL, n_layers = 1,
  basis_type = c("pca", "olda"), class = NULL,
  verbose = TRUE, noisy = TRUE
){
  d = 2
  loc_attr_nm <- "SHAP"
  require("treeshap")
  if(noisy == TRUE) require("beepr")
  if(verbose == TRUE){
    writeLines(paste0("nested_local_attr_layers() started at ", Sys.time()))
    tictoc::tic("nested_local_attr_layers()")
  }
  
  ### Create shap layers in a list
  .next_layers_x <- x ## Init
  .next_layers_xtest <- xtest ## Init, could be NULL
  shap_layer_ls <- list()
  if(n_layers == 1L){
    layer_nms <- loc_attr_nm
  }else layer_nms <- paste0(loc_attr_nm, "^", 1L:n_layers)
  .m <- sapply(1L:n_layers, function(i){
    shap_layer_ls[[i]] <<- local_attr_layer(
      .next_layers_x, y,
      .next_layers_xtest, ytest, ## Could be NULL
      layer_nms[i], basis_type, class,
      verbose, noisy)
    .next_layers_x <<- shap_layer_ls[[i]]$shap_df
    .next_layers_xtest <<- shap_layer_ls[[i]]$shap_xtest_df ## Could be NULL
  })
  names(shap_layer_ls) <- layer_nms
  
  ## Format into one list of formatted df rather than many lists of formatted df
  formated <- format_nested_layers(
    shap_layer_ls, x, y, basis_type, class, verbose)
  .m <- gc()
  if(noisy == TRUE) beepr::beep(2L)
  if(verbose == TRUE){
    tictoc::toc()
    writeLines(paste0("nested_local_attr_layers() finished at ", Sys.time()))
  }
  return(formated)
}



## SHAP matrices/data frames -----
#' Extract the full SHAP matrix of a randomForest model
#' 
#' Currently internal function, wants to be generalized. 
#' Extracts a lighter version of the treeshap *.unify of a randomForest.
#' 
#' @param randomForest_model The return of fitted randomForest::randomForest 
#' model.
#' @param data Data to extract the local attributions of.
#' @return A dataframe of the local attributions.
#' @examples
#' sub <- DALEX::apartments[1:200, 1:5]
#' x <- sub[, 2:5]
#' y <- sub$m2.price
#' 
#' ## Fit a {randomForest} model, 
#' # model fit slightly slower fit than {ranger},
#' # but treeshap iis much faster than {ranger}
#' .rf <- randomForest::randomForest(y ~ ., data = data.frame(y, x))
#' system.time(
#'  df_shap <- treeshap_df(.rf, data = xdat)
#' )[3]
treeshap_df <- function(randomForest_model, data){
  .rfu <- treeshap::randomForest.unify(randomForest_model, data)
  .tshap_ls <- treeshap::treeshap(.rfu, x = data) 
  .tshap_ls <- .tshap_ls[c(1L, 4L)]
  ## Keeping only c(1,4); reduces ~98.5% of the obj size, keep shap values make data attr.
  ## But, we lose the iBreakdown-like plot of treeshap::plot_contribution when we take this apart.
  ret <- .tshap_ls[[1L]]
  
  attr(ret, "class") <- c("treeshap_df", "data.frame")
  attr(ret, "data") <- .tshap_ls[[2L]] ## Also a data.frame
  return(ret)
}

## New for app tour -----
#' Extract and format the 1D local attribution basis
#' 
#' Internal function, Extract and format the 1D local attribution basis from 
#' the provided local_attribution_df.
#' 
#' @param local_attribution_df Return of a local attribution, such as 
#' treeshap_df.
#' @param rownum The rownumber of the primary observation.
#' @return A matrix of the 1D basis
#' @examples
#' la_df <- mtcars ## Pretend this is a local attribution data.frame
#' basis_local_attribution(la_df, rownum = 10)
basis_local_attribution <- function(
  local_attribution_df,
  rownum = nrow(local_attribution_df)
){
  ## Remove last column if layer_name
  col_idx <- if(local_attribution_df[, ncol(local_attribution_df)] %>% is.numeric == TRUE)
      1L:ncol(local_attribution_df) else -ncol(local_attribution_df)
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
#' "off"). Defaults to "top1d"; basis above the density curves.
#' @param shape Number specifying the shape of the basis distribution points. 
#' Typically 142 or 124 indicating '|' for `plotly` and `gganimate` respectively.
#' Defaults to 142, '|' for `plotly.`
#' @param do_add_pcp_segments Logical, whether or not to add to add faint 
#' parallel coordinate lines on the 1D basis. Defaults to TRUE.
#' @param primary_obs The rownumber of the primary observation. Its local
#' attribution becomes the 1d projection basis, and the point it highlighted 
#' as a dashed line.
#' @param comparison_obs The rownumber of the comparison observation. Point
#' is highlighted as a dotted line.
#' @rdname proto_basis
#' @family ggtour proto
#' @examples
#' load("./apps/cheem_penguins_classification/data/1preprocess.RData")
#' dat <- shap_layer_ls_3cobs$decode_df[, 4:7]
#' clas <- shap_layer_ls_3cobs$decode_df$class
#' shap_df <- shap_layer_ls_3cobs$shap_df[1:nrow(dat), -5]
#' bas <- basis_local_attribution(shap_df, nrow(dat))
#' mv <- manip_var_of(bas) ## Warning is fine.
#' 
#' ## 1D case:
#' mt_path <- manual_tour(bas, mv, angle = .3)
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
  position = c("top1d", "floor1d", "off"), ## Needs to match that of `proto_basis1d()`
  shape = c(142, 124), ## '|' for plotly and ggplot respectively
  do_add_pcp_segements = TRUE,
  primary_obs = NULL,
  comparison_obs = NULL
){
  requireNamespace("spinifex")
  ## Initialize
  eval(.init4proto)
  position <- match.arg(position)
  shape <- shape[1L] ## match.arg only for character values.
  if(shape %in% c(142L, 124L) == FALSE) warning("Unexpected shape used in proto_basis1d_distribution.")
  if(position == "off") return()
  if(is.null(.facet_by) == FALSE) position = "floor1d"
  
  local_attribution_df <- layer_ls$shap_df
  ## Pivot longer:
  ### ensure last col dropped if layer_name
  if(local_attribution_df[, ncol(local_attribution_df)] %>% is.numeric == TRUE){
    col_idx <- 1L:ncol(local_attribution_df)
  } else {col_idx <- -ncol(local_attribution_df)}
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
  .df_basis_distr <- LA_df %>%
    dplyr::left_join(.maha_dist_df, by = "rownum") %>%
    tidyr::pivot_longer(
      cols = !c(rownum, group_by, maha_dist),
      names_to = "var_name",
      values_to = "contribution") %>%
    dplyr::mutate(
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
  
  ## Position them
  .df_basis_distr <- map_relative(.df_basis_distr, position, .map_to)
  ## Add facet level if needed
  if(is.null(.facet_by) == FALSE){
    .basis_ls <- list(facet_by = rep_len("_basis_", nrow(.df_zero)))
    .df_basis_distr <- .bind_elements2df(.basis_ls, .df_basis_distr)
  }
  
  if(length(unique(.df_basis_distr$rowname)) > 999L)
    .alpha <- .003 else .alpha <- .3
  ## Basis/attribution distribution of the rows of the LA_df
  ret <- suppressWarnings(ggplot2::geom_point(
    ggplot2::aes(x, y, color = group_by, tooltip = rownum), .df_basis_distr,
    shape = shape, alpha = .alpha, size = 1.5))
  
  ## Add PCP lines if needed.
  if(do_add_pcp_segements == TRUE){
    ## Make the right table to inner join to.
    .df_basis_distr2 <- .df_basis_distr %>% mutate(
      .keep = "none",
      xend = x, yend = y,
      var_num = var_num - 1L, rownum = rownum)
    ## Inner join to by var_name & rownum(lead1)
    .df_basis_distr_pcp_segments <- dplyr::inner_join(
      .df_basis_distr, .df_basis_distr2,
      by = c("var_num" = "var_num", "rownum" = "rownum"))
    ## Mapping .alpha for pcp lines to maha dist causing err: not of length 1 or data
    bkg_pcp <- suppressWarnings(ggplot2::geom_segment(
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                   color = group_by, tooltip = rownum),
      .df_basis_distr_pcp_segments,
      size = .5, alpha = .alpha / 6L))
    prim_idx <- .df_basis_distr_pcp_segments$rownum == primary_obs
    if(length(primary_obs) > 0L){
      prim_pcp <- suppressWarnings(ggplot2::geom_segment(
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                     color = group_by, tooltip = rownum),
        .df_basis_distr_pcp_segments[prim_idx, ],
        size = 1L, alpha = .8, linetype = 2L))
    }else prim_pcp <- NULL
    comp_idx <- .df_basis_distr_pcp_segments$rownum == comparison_obs
    if(length(comparison_obs) > 0L){
      comp_pcp <- suppressWarnings(ggplot2::geom_segment(
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                     color = group_by, tooltip = rownum),
        .df_basis_distr_pcp_segments[comp_idx, ],
        size = .8, alpha = .6, linetype = 3L))
    }else comp_pcp <- NULL
    ret <- list(bkg_pcp, comp_pcp, prim_pcp, ret)
  }
  
  ## Return proto
  return(ret)
}
