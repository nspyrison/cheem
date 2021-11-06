##TODO: !! This whole file wants to be rebased for 1) generalized models and 
# 2) local explanations (language is already generalized)
##TODO: Some functions lacking examples.


#' Create the plot data.frame for the global linked plotly display.
#' 
#' Internal function, the plot data.frame of 1 layer, consumed in 
#' local_attr_ls() and format_ls().
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
#' ret <- global_view_1sp(x, y, basis_type = "pca",
#'                        class = clas, layer_name ="SHAP")
#' str(ret)
global_view_1sp <- function(
  x, y, basis_type = c("pca", "olda"),
  class = NULL, ## class req for olda, add to _df's
  layer_name
){
  d <- 2L ## Fixed display dimensionality
  tooltip <- 1L:nrow(x)
  if(is.null(class)) class <- as.factor(FALSE) 
  
  # ## maha_vect_of() -----
  # .lvls <- levels(class)
  # maha <- NULL
  # ## For each level of the class, find the in-class maha distances
  # .m <- sapply(1L:length(.lvls), function(k){
  #   .sub <- x[class == .lvls[k], ]
  #   .sub_maha <- stats::mahalanobis(
  #     .sub, apply(.sub, 2L, stats::median), stats::cov(.sub)) %>%
  #     matrix(ncol = 1L)
  #   maha <<- c(maha, .sub_maha)
  # })
  # ## [01] normalize (outside class levels)
  # maha <- spinifex::scale_01(as.matrix(maha, ncol = 1L))
  # ## Maha quantiles
  # ### Theoretical chi sq quantiles, x of a QQ plot
  # .probs <- seq(.001, .999, length.out = nrow(x))
  # .qx_chisq <- spinifex::scale_01(matrix(stats::qchisq(.probs, df = nrow(x) - 1L)))
  # ### Sample/obs quantiles, y of a QQ plot
  # ord <- order(maha)
  # .qy_obs <- spinifex::scale_01(matrix(maha[ord]))
  # .AG_kurt_tst_p <- moments::anscombe.test(as.vector(.qy_obs), "less")$p.value
  # .maha_skew_text <- paste0(
  #   "  Anscombe-Glynn p-value: ",
  #   format(signif(.AG_kurt_tst_p, 3L), scientific = TRUE), "\n",
  #   "    (testing 1-tailed kurtosis)\n",
  #   "  Kurtosis - 3: ", round(moments::kurtosis(.qy_obs) - 3L, 2L), "\n",
  #   "  Skew: ", round(moments::skewness(.qy_obs), 2L), "\n")
  # inv_ord <- order(ord)
  # .qq_df <- data.frame(
  #   1L:nrow(x), class, .qx_chisq[inv_ord], .qy_obs[inv_ord], y, layer_name,
  #   "QQ Mahalanobis distance", tooltip, .maha_skew_text)
  
  ## $global_view_df -----
  x_std <- spinifex::scale_sd(x)
  basis_type <- match.arg(basis_type)
  basis <- switch(basis_type,
                  pca  = spinifex::basis_pca(x_std, d),
                  olda = spinifex::basis_olda(x_std, class, d))
  proj <- as.matrix(x_std) %*% basis
  proj <- spinifex::scale_01(proj) %>% as.data.frame()
  
  ## Column bind wider, order by rownum
  .global_view_df <- cbind(
    1L:nrow(x), class, proj, y, layer_name, basis_type, tooltip, "")
  ## Row bind longer, adding QQ maha, and kurtosis info.
  colnames(.global_view_df) <- #colnames(.qq_df) <-
    c("rownum", "class", paste0("V", 1L:d), "y",
      "layer_name", "projection_nm", "tooltip", "ggtext")
  
  ## 1 space's global view with attached basis
  ret <- .global_view_df #rbind(.global_view_df, .qq_df)
  attr(ret, paste0(basis_type, " basis of ", layer_name)) <- basis
  return(ret)
}

#' Create the local attribution layer data.frame
#' 
#' Internal function, the local attribution layer data.frame of 1 layer, consumed in 
#' local_attr_ls().
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
#' @export
#' @examples
#' library(cheem)
#' sub <- DALEX::apartments[1:200, 1:6]
#' x <- sub[, 2:5]
#' y <- sub$m2.price
#' clas <- sub$district
#' 
#' ret <- local_attr_ls(
#'   x, y, layer_name = "typically 'data'/'<attribution name>'",
#'   basis_type = "pca", class = clas)
#' names(ret)
local_attr_ls <- function(
  x, y, xtest = NULL, ytest = NULL, layer_name = "UNAMED",
  basis_type = c("pca", "olda"), class = NULL, ## class req for olda
  verbose = TRUE, noisy = TRUE
){
  if(verbose == TRUE)
    tictoc::tic(paste0("local_attr_ls -- ", layer_name))
  d <- 2L
  basis_type <- match.arg(basis_type)
  
  ## RF model -----
  .is_y_disc <- is_discrete(y)
  sec_mod <- system.time({
    .m <- gc()
    ## Default hyperparameters, (hp)
    .hp_mtry <- ifelse(.is_y_disc == TRUE, sqrt(ncol(x)), ncol(x) / 3L)
    .hp_node <- ifelse(.is_y_disc == TRUE, 1L, 5L)
    .hp_node <- max(.hp_node, nrow(x) / 500L)
    .hp_ntree <- 500L / 4L ## A function of the data, atleast not as liberal as the default 500 trees
    suppressWarnings(
      ## suppresses: The response has five or fewer unique values.  Are you sure?
      .mod <- randomForest::randomForest(
        y~., data = data.frame(y, x),
        mtry = .hp_mtry, nodesize = .hp_node, ntree = .hp_ntree)
    )
  })[3L]
  
  ## $model_performance_df -----
  #### MANUALLY created, different than the performance created by the rf fit...
  .pred <- stats::predict(.mod, x)
  .resid <- y - .pred
  .rss <- sum(.resid^2L)
  .tss <- sum((y - mean(y))^2L)
  .mse <- 1L / nrow(x) * .rss %>% round(2L)
  .rmse <- sqrt(.mse) %>% round(2L)
  .rsq <- 1L - (.rss / .tss) %>% round(4L)
  .model_performance_df <- data.frame(
    model_type = "randomForest",
    hp_mtry = .hp_mtry, hp_node = .hp_node, hp_ntree = .hp_ntree,
    mse = .mse, rmse = .rmse, rsq = .rsq, model_runtime_sec = sec_mod)
  if(is.null(xtest) == FALSE & is.null(ytest) == FALSE){
    .rss_t <- sum((ytest - stats::predict(.mod, xtest))^2L)
    .tss_t <- sum((ytest - mean(ytest))^2L)
    .mse_t <- 1L / nrow(x) * .rss_t %>% round(2L)
    .rmse_t <- sqrt(.mse_t) %>% round(2L)
    .rsq_t <- 1L - (.rss_t/.tss_t) %>% round(4L)
    .model_performance_df$test_mse <- .mse_t
    .model_performance_df$test_rmse <- .rmse_t
    .model_performance_df$test_rsq <- .rsq_t
  }
  row.names(.model_performance_df) <- 1L
  
  ##  attribution matrix, currently treeshap ----
  sec_attr_df <- system.time({
    .m <- gc()
    .attr_df <- treeshap_df(.mod, x)
    .attr_df_xtest <- NULL ## Init
    if(is.null(xtest) == FALSE) .attr_df_xtest <- treeshap_df(.mod, xtest)
  })[3L]
  
  ## global_view of Attribution space ----
  sec_global_view_attr_sp <- system.time({
    ## If classification, use .pred_clas over class
    is_classification <- problem_type(y) == "classification"
    if(is_classification == TRUE){
      .lvls <- levels(class)
      .pred_clas <- as.factor(.lvls[round(.pred)])
    }
    ## wants if() assignment over ifelse assignment for some reason
    if(is_classification == TRUE)
      .plot_clas <- .pred_clas else .plot_clas <- class
    .m <- gc()
    .global_view_df <- global_view_1sp(
      .attr_df, y, basis_type, .plot_clas, layer_name = layer_name)
  })[3L]
  
  ## $time_df, Execution time -----
  runtime_df <- data.frame(
    runtime_seconds = c(sec_mod, sec_attr_df, sec_global_view_attr_sp),
    task = c("model (rF::rF)", "attribution (treeshap)", "global_view_df (PCA)"),
    layer = layer_name)
  if(verbose == TRUE) tictoc::toc()
  if(noisy == TRUE & sum(runtime_df$runtime_seconds) > 30L) beepr::beep(1L)
  
  ## Keep attribution basis and return
  LA_ls <- list(global_view_df = .global_view_df,
                model = .mod, ## Heavy object, dropped but default in format_ls
                model_performance_df = .model_performance_df,
                attr_df = .attr_df,
                attr_xtest_df = .attr_df_xtest, ## Typically NULL
                runtime_df = runtime_df)
  .basis_nm <- paste0(basis_type, " basis of ", layer_name)
  attr(LA_ls, .basis_nm) <- attr(.global_view_df, .basis_nm)
  return(LA_ls)
}


#' Format the nested local attribution layers
#' 
#' Internal function, formats all layers of all data.frames, consumed in 
#' local_attr_ls().
#' 
#' @param local_attr_ls A return from `local_attr_ls()`.
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model.
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Defaults to "pca".
#' @param class The variable to group points by. Originally the _predicted_
#'  class.
#' @param keep_model Whether or not to keep the (sizable) model object. 
#' Defaults to FALSE.
#' @param verbose Logical, Whether or not the function should print tictoc time
#' info.
#' @return A list of formated data frames.
#' @export
format_ls <- function(
  local_attr_ls, x, y,
  basis_type = c("pca", "olda"),
  class = NULL,
  keep_model = FALSE,
  verbose = TRUE
){
  if(verbose == TRUE) tictoc::tic("format_ls")
  layer_name <- "SHAP"
  d <- 2L
  basis_type <- match.arg(basis_type)
  rownum <- V2 <- projection_nm <- NULL
  
  ## Init with data layer,
  sec_global_view_data_sp <- system.time({ ## Init with data layer.
    .mod  <- local_attr_ls$model
    .pred <- stats::predict(.mod)
    .plot_clas <- class # Init regression plot class
    is_classification <- problem_type(y) == "classification"
    if(is_classification == TRUE){
      .lvls <- levels(class)
      .pred_clas <- as.factor(.lvls[round(.pred)])
      .plot_clas <- .pred_clas
    }
    b_global_view_df <- global_view_1sp( ## Only data, but will be bound in next chunck
      x, y, basis_type, .plot_clas, layer_name = "data")
    .m <- gc()
  })[3L]
  .data_basis_nm <- paste0(basis_type, " basis of data")
  .data_basis <- attr(b_global_view_df, .data_basis_nm)
  .attr_basis <- attr(local_attr_ls, paste0(basis_type, " basis of ", layer_name))
  ### global_view_df, bound longer
  b_global_view_df <- rbind(b_global_view_df, local_attr_ls$global_view_df)
  
  ## decode_df_of -----
  #### A decode display table at the observation/instance grain
  if(is.null(class)) class <- as.factor(FALSE)
  is_classification <- ifelse(length(unique(y)) < 5L, TRUE, FALSE)
  decode_df <- data.frame(rownum = 1L:nrow(x), class = class, y)
  .is_misclass <- NULL
  decode_df <- cbind(decode_df, .pred, y - .pred) ## Layer residual
  if(is_classification == TRUE){
    .lvls <- levels(class)
    .pred_clas <- as.factor(.lvls[round(.pred)])
    .is_misclass <- .pred_clas != class
    decode_df <- cbind(decode_df, .pred_clas, .is_misclass)
  }
  
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
  .nms <- c("rownum", "class", "y", "prediction", "residual")
  if(is_classification == TRUE){
    .nms <- c(.nms, "predicted_class", "is_misclassified", names(x), "tooltip")
  }else{.nms <- c(.nms, names(x), "tooltip")}
  names(decode_df) <- .nms
  ## Also add tooltip to global_view
  #### (it doesn't know of the model, to check misclassification)
  b_global_view_df$tooltip <- dplyr::left_join(
    b_global_view_df, decode_df, c("rownum" = "rownum"))$tooltip.y
  .maha_df <- b_global_view_df %>%
    dplyr::filter(projection_nm == "QQ Mahalanobis distance") %>%
    dplyr::select(rownum, V2, layer_name) %>%
    tidyr::pivot_wider(names_from  = layer_name, values_from = V2)
  .lj <-  dplyr::left_join(decode_df, .maha_df, c("rownum" = "rownum"))
  decode_df$maha_data <- .lj$data
  decode_df$maha_SHAP <- .lj$SHAP
  
  ### Rbind runtime_df
  b_runtime_df <- rbind(
    data.frame(runtime_seconds = sec_global_view_data_sp,
               task = "global_view_df (PCA)", layer = "data"),
    local_attr_ls$runtime_df)
  row.names(b_runtime_df) <- 1L:nrow(b_runtime_df)
  
  ## Return:
  ret <- list(
    global_view_df = b_global_view_df, decode_df = decode_df,
    model_performance_df = local_attr_ls$model_performance_df,
    attr_df = local_attr_ls$attr_df, runtime_df = b_runtime_df,
    problem_type = problem_type(y),
    basis_ls = list(data_basis = .data_basis,
                    attribution_basis = .attr_basis)
  )
  ## Keep heavy model object?
  if(keep_model == TRUE) ret <- c(ret, model = local_attr_ls$model)
  if(verbose == TRUE) tictoc::toc()
  return(ret)
}

#' Format the nested local attribution layers
#' 
#' Internal function, formats all layers of all data.frames, consumed in 
#' local_attr_ls().
#' 
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model
#' @param xtest Optional, Out Of Sample data to find the local attribution of.
#' @param ytest Optional, Out Of Sample response to measure xtext with 
#' if provided.
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Defaults to "pca".
#' @param class The variable to group points by. Originally the _predicted_
#'  class.
#' @param loc_attr_nm Name of the layer/attribution. Defaults to "SHAP".
#' @param keep_model Whether or not to keep the (sizable) model object. 
#' Defaults to FALSE.
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
#' .cheem_ls <- cheem_ls(
#'   x = X_train, y = Y_train, xtest = X_test, ytest = Y_test,
#'   basis_type = "pca", class = clas,
#'   keep_model = FALSE, verbose = TRUE, noisy = TRUE)
#' names(.cheem_ls)
#' 
#' ## Save for used with shiny app (expects .rds):
#' if(F) ## don't accidentally save a local file.
#'   saveRDS(.cheem_ls, "./my_cheem_ls.rds")
cheem_ls <- function(
  x, y, xtest = NULL, ytest = NULL, basis_type = c("pca", "olda"),
  class = NULL, loc_attr_nm = "SHAP", keep_model = FALSE,
  verbose = TRUE, noisy = TRUE
){
  if(verbose == TRUE){
    writeLines(paste0("cheem_ls started at ", Sys.time()))
    tictoc::tic("cheem_ls total")
  }
  d <- 2L
  basis_type <- match.arg(basis_type)
  
  ### Create shap layers in a list
  la_ls <- local_attr_ls(
    x, y, xtest, ytest, ## Could be NULL
    loc_attr_nm, basis_type, class, verbose, noisy)
  
  ## Format into one list of formatted df rather than many lists of formatted df
  formated_ls <- format_ls(
    la_ls, x, y, basis_type, class, keep_model, verbose)
  .m <- gc()
  if(noisy == TRUE) beepr::beep(2L)
  if(verbose == TRUE){
    tictoc::toc()
    writeLines(paste0("cheem_ls finished at ", Sys.time()))
  }
  return(formated_ls)
}




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




default_rf <- 




if(F){ ## THERORETICAL DEV -----
  ## TAKEAWAY:
  #-CP and BD, each need DALEX::Explain;
  #-basically want to extract a model-less DALEX::explain before model is removed. 
  
  ## Wrapper function to create a breakdown plot of the 
  bd_plot <- function(explainer, new_obs){
    parts_bd <- DALEX::predict_parts_break_down(explainer = , new_observation )
    plot(parts_bd)
  }
  
  # .explain <- DALEX::explain(model = model,
  #                            data  = x,
  #                            y     = y,
  #                            type  = problem_type)
  
  ## Wrapper function for predict & plot cp profile
  cp_profiles_plots <- function(explainer, new_obs, var_nms){
    .pred_prof <- DALEX::predict_profile(explainer = explainer,
                                         new_observation = new_obs)

    return(
    plot(.pred_prof, variables = var_nms) +
      ggtitle::ggtitle("Ceteris-paribus profile", "")# +
    # + ggplot2::ylim(min(y), max(y))
    )
  }
}