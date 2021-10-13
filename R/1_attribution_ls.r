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
#' @export
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
  d = 2L ## Fixed display dimensionality 
  ## maha_vect_of() -----
  #maha_vect_of <- function(x, do_normalize = TRUE){ ## distance from median(x), stats::cov(x)
  if(is.null(class)) class <- as.factor(FALSE)
  .lvls <- levels(class)
  maha <- NULL
  ## For each level of the class, find the in-class maha distances
  .m <- sapply(1L:length(.lvls), function(k){
    .sub <- x[class == .lvls[k], ]
    .sub_maha <- stats::mahalanobis(
      .sub, apply(.sub, 2L, stats::median), stats::cov(.sub)) %>%
      matrix(ncol = 1L)
    maha <<- c(maha, .sub_maha)
  })
  ## 01 normalize (outside class levels)
  maha <- spinifex::scale_01(as.matrix(maha, ncol = 1L))
  ## Maha quantiles
  ### Theoretical chi sq quantiles, x of a QQ plot
  .probs <- seq(.001, .999, length.out = nrow(x))
  .qx <- spinifex::scale_01(matrix(stats::qchisq(.probs, df = nrow(x) - 1L)))
  ### Sample quantiles, y of a QQ plot
  .qy <- spinifex::scale_01(matrix(stats::quantile(maha, probs = .probs)))
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
#' @param xtest Optional, Out Of Sample data to find the local attribution of.
#' @param ytest Optional, Out Of Sample response to measure x with 
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
  d = 2L
  if(verbose == TRUE)
    tictoc::tic(paste0("local_attr_layer -- ", layer_name))
  
  ## RF model
  .is_y_disc <- is_discrete(y)
  sec_rf <- system.time({
    .m <- gc()
    .hp_mtry <- ifelse(.is_y_disc == TRUE, sqrt(ncol(x)), ncol(x) / 3L)
    .hp_node <- ifelse(.is_y_disc == TRUE, 1L, 5L)
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
    .rss_t <- sum((ytest - stats::predict(.rf, xtest))^2L)
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
    is_classification <- problem_type(y) == "classification"
    if(is_classification == TRUE){
      .lvls <- levels(class)
      .pred_clas <- as.factor(.lvls[round(.pred)])
    }
    ## wants if() assignment over ifelse assignment for some reason
    if(is_classification == TRUE)
      .plot_clas <- .pred_clas else .plot_clas <- class
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
#' @param layer_ls A return from `nested_local_attr_layers()`.
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model.
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Defaults to "pca".
#' @param class The variable to group points by. Originally the _predicted_
#'  class.
#' @param verbose Logical, Whether or not the function should print tictoc time
#' info.
#' @return A list of formated data frames.
format_nested_layers <- function(
  layer_ls, x, y,
  basis_type = c("pca", "olda"),
  class = NULL,
  verbose = TRUE
){
  d = 2L
  if(verbose == TRUE) tictoc::tic("format_nested_layers()")
  ## Init with data layer,
  sec_data_plot_df <- system.time({ ## Init with data layer.
    is_classification <- problem_type(y) == "classification"
    if(is_classification == TRUE){
      .lvls <- levels(class)
      .rf_mod <- layer_ls[[1L]]$rf_model
      .pred_clas <- as.factor(.lvls[round(stats::predict(.rf_mod))])
    }
    if(is_classification == TRUE)
      .plot_clas <- .pred_clas else .plot_clas <- class
    .m <- gc()
    
    plot_df <- global_view_df(
      x, y, basis_type, .plot_clas, layer_name = "data")
  })[3L]
   
  ### plot_df, bound longer
  b_plot_df <- data.frame(plot_df) ## Init data layer plot_df
  .m <- sapply(1L:length(layer_ls), function(i){
    this_plot_df <- layer_ls[[i]]$plot_df
    b_plot_df <<- rbind(b_plot_df, this_plot_df)
  })
  
  ## decode_df_of -----
  #### Information decode(/display) table
  ## bound wider: add prediction, residuals of every layer
  if(is.null(class)) class <- as.factor(FALSE)
  is_classification <- ifelse(length(unique(y)) < 5L, TRUE, FALSE)
  decode_df <- data.frame(rownum = 1L:nrow(x), class = class, y)
  .is_misclass <- NULL
  .m <- sapply(1L:length(layer_ls), function(i){
    .rf_mod <- layer_ls[[i]]$rf_model
    .pred <- stats::predict(.rf_mod)
    decode_df <<- cbind(decode_df, .pred, y - .pred) ## Layer residual
    if(is_classification == TRUE){
      .lvls <- levels(class)
      .pred_clas <<- as.factor(.lvls[round(.pred)])
      .is_misclass <<- .pred_clas != class
      decode_df <<- cbind(decode_df, .pred_clas, .is_misclass)
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
      ", observed: ", class[.is_misclass])
    tooltip[!.is_misclass] <- paste0(
      tooltip[!.is_misclass], "\nclass: ", class[!.is_misclass])
  }
  decode_df <- cbind(decode_df, x, tooltip)
  .nms <- c("rownum", "class", "y", ## Init common names.
            "prediction", #paste0("prediction_", names(layer_ls)),
            "residual")   #paste0("residual_",   names(layer_ls)),
  if(is_classification == TRUE){
    .nms <- c(.nms, "predicted_class", "is_misclassified", names(x), "tooltip")
  }else{.nms <- c(.nms, names(x), "tooltip")}
  names(decode_df) <- .nms
  ## Also add tooltip to plot_df, as it doesn't know of the RF, to check misclass
  b_plot_df$tooltip <- dplyr::left_join(
    b_plot_df, decode_df, c("rownum" = "rownum"))$tooltip.y
  
  ### performance of the layers
  ## manual performance, slightly different than that reported by the rf obj itself
  .nms <- names(layer_ls)
  performance_df <- data.frame(NULL)
  .m <- sapply(1L:length(layer_ls), function(i){
    .row <- layer_ls[[i]]$performance_df
    .row$runtime_seconds <- sum(layer_ls[[i]]$time_df$runtime_seconds)
    performance_df <<- rbind(performance_df, .row)
  })
  row.names(performance_df) <- .nms
  
  ### Rbind shap_df
  b_shap_df <- data.frame()
  .m <- sapply(1L:length(layer_ls), function(i){
    .shap_df <- cbind(layer_ls[[i]]$shap_df, .nms[i])
    b_shap_df <<- rbind(b_shap_df, .shap_df)
  })
  colnames(b_shap_df) <- c(colnames(layer_ls[[1L]]$shap_df), "layer_name")
  
  ### Rbind time_df
  b_time_df <- data.frame( ## Init with data layer.
    runtime_seconds = sec_data_plot_df, task = "plot_df (PCA/Maha)", layer = "data")
  .m <- sapply(1L:length(layer_ls), function(i){
    b_time_df <<- rbind(b_time_df, layer_ls[[i]]$time_df)
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
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model
#' @param xtest Optional, Out Of Sample data to find the local attribution of.
#' @param ytest Optional, Out Of Sample response to measure xtext with 
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
#'   1:nrow(sub), size = round(.2 * nrow(sub))) ## 20% index for testing
#' X_train <- sub[-.idx_test, 2:5]
#' Y_train <- sub$m2.price[-.idx_test]
#' clas <- sub$district[-.idx_test]
#' X_test  <- sub[.idx_test, 2:5]
#' Y_test  <- sub$m2.price[.idx_test]
#' 
#' layer_ls <- nested_local_attr_layers(
#'   x=X_train, y=Y_train, xtest=X_test, ytest=Y_test,
#'   n_layers=1, basis_type="pca", class=clas, verbose=T, noisy=T)
#' names(layer_ls)
nested_local_attr_layers <- function(
  x, y, xtest = NULL, ytest = NULL, n_layers = 1,
  basis_type = c("pca", "olda"), class = NULL,
  verbose = TRUE, noisy = TRUE
){
  d = 2L
  loc_attr_nm <- "SHAP"
  if(verbose == TRUE){
    writeLines(paste0("nested_local_attr_layers() started at ", Sys.time()))
    tictoc::tic("nested_local_attr_layers()")
  }
  
  ### Create shap layers in a list
  .next_layers_x <- x ## Init
  .next_layers_xtest <- xtest ## Init, could be NULL
  layer_ls <- list()
  layer_nms <- ifelse(n_layers == 1L, loc_attr_nm,
                      paste0(loc_attr_nm, "^", 1L:n_layers))
  .m <- sapply(1L:n_layers, function(i){
    layer_ls[[i]] <<- local_attr_layer(
      .next_layers_x, y,
      .next_layers_xtest, ytest, ## Could be NULL
      layer_nms[i], basis_type, class,
      verbose, noisy)
    .next_layers_x <<- layer_ls[[i]]$shap_df
    .next_layers_xtest <<- layer_ls[[i]]$shap_xtest_df ## Could be NULL
  })
  names(layer_ls) <- layer_nms
  
  ## Format into one list of formatted df rather than many lists of formatted df
  formated_ls <- format_nested_layers(
    layer_ls, x, y, basis_type, class, verbose)
  .m <- gc()
  if(noisy == TRUE) beepr::beep(2L)
  if(verbose == TRUE){
    tictoc::toc()
    writeLines(paste0("nested_local_attr_layers() finished at ", Sys.time()))
  }
  attr(formated_ls, "problem_type") <- problem_type(y)
  attr(formated_ls, "cobs_msg") <- "" ## Just to be sure in app.
  return(formated_ls)
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
#' @export
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

