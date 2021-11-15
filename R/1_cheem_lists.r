##TODO: !! This whole file wants to be rebased for 1) generalized models and 
# 2) local explanations (language is already generalized)
##TODO: Some functions lacking examples.

#' Random forest model via randomForest
#' 
#' A wrapper function for `randomForest::randomForest()` with more modest 
#' hyperparameter defaults and consistent arguments with `cheem`.
#' 
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model.
#' @param verbose Logical, if runtime should be printed. Defaults to TRUE.
#' @param hp_ntree Hyper parameter, the number of trees to grow.
#' @param hp_mtry Hyper parameter, the number variables randomly sampled at 
#' each split.
#' @param hp_nodesize Hyperparameter, Minimum size of terminal nodes. 
#' Setting this number larger causes smaller trees to be grown (and thus take less time).
#' @return A randomForest model.
#' @export
#' @examples
#' library(cheem)
#' 
#' sub <- amesHousing2018_thin[1:200, ]
#' X <- sub[, 1:9]
#' Y <- log(sub$SalePrice)
#' clas <- sub$ZoneMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' ## Long runtime for full datasets:
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                     model = rf_fit,
#'                     attr_df = shap_df)
#' global_view(this_ls)
default_rf <- function(
  x, y, verbose = TRUE,
  hp_ntree = 125,
  hp_mtry = ifelse(is_discrete(y), sqrt(ncol(x)), ncol(x) / 3),
  hp_nodesize = max(ifelse(is_discrete(y), 1, 5), nrow(x) / 500)
){
  if(verbose) tictoc::tic("default_rf")
  suppressWarnings(## suppresses: The response has five or fewer unique values.  Are you sure?
    .mod <- randomForest::randomForest(
      y~., data = data.frame(y, x),
      mtry = hp_mtry, nodesize = hp_nodesize, ntree = hp_ntree)
  )
  
  .m <- gc()
  if(verbose) tictoc::toc()
  return(.mod)
}


#' Extract the full treeSHAP data.frame of a randomForest model
#' 
#' A data.frame of each observations treeSHAP variable attributions of a
#' randomForest model. 
#' A wrapper for `treeshap::randomForest.unify` and `treeshap::treeshap`.
#' 
#' @param randomForest_model The return of fitted randomForest::randomForest 
#' model.
#' @param x The explanatory data (without response) to extract the local 
#' attributions of.
#' @param keep_heavy Logical, if the heavy items "interactions",
#'  "unified_model", and "observations" should be kept. Defaults to FALSE.
#' @param verbose Logical, if runtime should be printed. Defaults to TRUE.
#' @param noisy Logical, if a tone should be played on completion. 
#' Defaults to TRUE.
#' @return A dataframe of the local attributions.
#' @export
#' @examples
#' library(cheem)
#' 
#' sub <- amesHousing2018_thin[1:200, ]
#' X <- sub[, 1:9]
#' Y <- log(sub$SalePrice)
#' clas <- sub$ZoneMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' ## Long runtime for full datasets:
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                     model = rf_fit,
#'                     attr_df = shap_df)
#' global_view(this_ls)
attr_df_treeshap <- function(
  randomForest_model,
  x,
  keep_heavy = FALSE,
  verbose = TRUE,
  noisy = TRUE
){
  if(verbose){
    writeLines(paste0("Started attr_df_treeshap() at: ", Sys.time()))
    tictoc::tic("attr_df_treeshap")
  }
  .rfu <- treeshap::randomForest.unify(randomForest_model, x)
  ret  <- treeshap::treeshap(.rfu, x = x)
  if(keep_heavy == FALSE)
    ret <- ret[[1L]]
  ## Keeping only 1; reduces ~99% of the obj size, keep shap values.
  ## But, we lose the iBreakdown-like plot of treeshap::plot_contribution when we take this apart.
  
  class(ret) <- c("data.frame", "treeshap")
  if(verbose) tictoc::toc()
  if(noisy) beepr::beep(1L)
  return(ret)
}

#' Extract higher level model performance
#' 
#' Internal function, used downstream in cheem_ls.
#' 
#' @param model A non-linear model, originally a `randomForest::randomForest`
#' model fit, or a return from `default_rf()`.
#' @return A dataframe of the local attributions.
#' @export
#' @examples
#' library(cheem)
#' 
#' sub <- amesHousing2018_thin[1:200, ]
#' X <- sub[, 1:9]
#' Y <- log(sub$SalePrice)
#' clas <- sub$ZoneMS
#' 
#' rf_fit <- default_rf(X, Y)
#' model_performance_df(rf_fit)
model_performance_df <- function(
  model
){
  #### Following the functions in {Metrics}
  # liable to differ from the performance of the model object (due to adj values?)
  # but at least consistent format and measures.
  y       <- model$y
  .pred   <- stats::predict(model)
  .e      <- y - .pred ## Residual
  .se     <- .e^2L
  .sse    <- sum(.se)
  .mse    <- mean(.se)
  .rmse   <- sqrt(.mse)
  .rse    <- .sse / sum((y - mean(y))^2L)
  .r2     <- 1L - .rse
  .r2_adj <- 1L -(.mse / stats::var(y))
  ## We could be at this all day...
  # .auc    <- Metrics::auc(y, .pred)
  # .mae  <- mean(abs(.e))
  # .mad  <- .mae / length(y)
  # .ROC <- ROCR::
  return(data.frame(
    row.names = NULL,
    model_type = class(model)[length(class(model))],
    sse  = .sse,
    mse  = .mse,
    rmse = .rmse,
    rse  = .rse,
    r2   = .r2,
    r2_adj = .r2_adj
    #,auc = .auc
  ))
}

#' Create the plot data.frame for the global linked plotly display.
#' 
#' Internal function, the plot data.frame of 1 layer, consumed downstream in 
#' cheem_ls.
#' 
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model.
#' @param class The variable to group points by. Originally the _predicted_
#'  class.
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Defaults to "pca".
#' @param layer_name Character layer name, typically the type of local 
#' attribution used. Defaults to the name of the last class of x.
#' @return A data.frame, for the global linked plotly display.
# #' @export
#' @examples
#' library(cheem)
#' 
#' sub <- amesHousing2018_thin[1:200, ]
#' X <- sub[, 1:9]
#' Y <- log(sub$SalePrice)
#' clas <- sub$ZoneMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' ## Long runtime for full datasets:
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                     model = rf_fit,
#'                     attr_df = shap_df)
global_view_df_1layer <- function(
  x, y,
  class = NULL, ## required for olda
  basis_type = c("pca", "olda"),
  layer_name = class(x)[length(class(x))] ## Name of the last class _ie_ `treeshap``
){
  d <- 2L ## Fixed display dimensionality
  basis_type <- match.arg(basis_type)
  if(is.null(class)) class <- as.factor(FALSE)
  tooltip <- 1L:nrow(x) ## placeholder, decode_df makes a better tooltip.
  
  ## Projection
  x_std <- spinifex::scale_sd(x)
  basis <- switch(basis_type,
                  pca  = spinifex::basis_pca(x_std, d),
                  olda = spinifex::basis_olda(x_std, class, d))
  proj <- x_std %*% basis %>% as.data.frame()
  proj <- spinifex::scale_01(proj)
  
  ## Column bind wide
  ret <- data.frame(basis_type, layer_name, 1L:nrow(x), class, tooltip, proj)
  colnames(ret) <- c("basis_type", "layer_name", "rownum",
                     "class", "tooltip", paste0("V", 1L:d))
  attr(ret, paste0(basis_type, ":", layer_name)) <- basis
  return(ret)
}

#' Create the local attribution layer data.frame
#' 
#' Internal function, the local attribution layer data.frame of 1 layer, 
#' consumed 
#' 
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model.
#' @param class The variable to group points by. Originally the _predicted_
#'  class.
#' @param model A non-linear model, originally a `randomForest::randomForest`
#' model fit, or a return from `default_rf()`.
#' @param attr_df A data frame of local explanation attributions,
#' such as a return from `attr_df_treeshap()`.
#' @param layer_name Character layer name, typically the type of local 
#' attribution used. Defaults to the last class of the model.
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Expects "pca" or "olda" (requires `clas`).
#'  Defaults to "pca".
#' @param verbose Logical, if runtime should be printed. Defaults to TRUE.
#' @param keep_model Logical, if the heavy model object should be kept.
#' Defaults to FALSE.
#' @return A data.frame, for the full local attribution matrix.
#' @export
#' @examples
#' library(cheem)
#' library(spinifex)
#' 
#' ## Classification:
#' X    <- penguins[, 1:4]
#' clas <- penguins$species
#' Y    <- as.integer(clas)
#' 
#' rf_fit  <- default_rf(X, Y)
#' ## Long runtime for full datasets:
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                      model = rf_fit,
#'                      attr_df = shap_df)
#' 
#' ## Regression:
#' sub <- amesHousing2018_thin[1:200, ]
#' X <- sub[, 1:9]
#' Y <- log(sub$SalePrice)
#' clas <- sub$ZoneMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' ## Long runtime for full datasets:
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                      model = rf_fit,
#'                      attr_df = shap_df)
#' 
#' ## Save for used with shiny app (expects .rds):
#' if(FALSE) ## Don't accidentally save.
#'   saveRDS(this_ls, "./my_cheem_ls.rds")
cheem_ls <- function(
  x, y, class = NULL,
  model, attr_df,
  basis_type = c("pca", "olda"), ## class req for olda
  layer_name = class(model)[length(class(model))],
  verbose = TRUE,
  keep_model = FALSE
){
  ## Initialize -----
  if(verbose) tictoc::tic("cheem_ls")
  d <- 2L
  basis_type <- match.arg(basis_type)
  is_classification <- is_discrete(y)
  rownum <- V2 <- projection_nm <- NULL
  
  ## Global view -----
  .glob_dat  <- global_view_df_1layer(x, y, class, basis_type, "data")
  .glob_attr <- global_view_df_1layer(attr_df, y, class, basis_type, 
                                      class(attr_df)[length(class(attr_df))])
  .glob_view <- rbind(.glob_dat, .glob_attr)
  ## list of global view bases
  .dat_bas  <- attributes(.glob_dat)[length(attributes(.glob_dat))]
  .attr_bas <- attributes(.glob_attr)[length(attributes(.glob_attr))]
  .glob_basis_ls <- c(.dat_bas, .attr_bas)
  
  ## decode_df ----
  if(is.null(class)) class <- factor(FALSE) ## dummy factor
  .decode_left <- data.frame(
    rownum = 1L:nrow(x), class = class, y,
    prediction = stats::predict(model))
  .decode_right <- data.frame(
    residual = y - stats::predict(model), x) ##, attr_df) ## duplicate col names and long.
  if(is_classification){
    .pred_clas <- factor(
      levels(class)[round(stats::predict(model))], levels = levels(class))
    .is_misclass <- .pred_clas!= class
    .decode_middle <- data.frame(
      predicted_class = .pred_clas,
      is_misclassified = .is_misclass)
    .decode_df <- cbind(.decode_left, .decode_middle, .decode_right)
  }else{
    .decode_df <- cbind(.decode_left, .decode_right)
  }
  ## Round numeric columns for readability.
  .decode_df <- data.frame(lapply(
    .decode_df, function(c) if(is.numeric(c)) round(c, 2L) else c))
  
  ## Add tooltip ----
  tooltip <- paste0("row: ", 1L:nrow(x)) ## Base tooltip
  if(is.null(rownames(x)) == FALSE)
    ### Character rownames?
    if(does_contain_nonnumeric(rownames(x)) == TRUE)
      tooltip <- paste0(tooltip, ", ", rownames(x))
  if(is_classification){
    ### Classification extension
    tooltip[.is_misclass] <- paste0(
      tooltip[.is_misclass],
      "\nMisclassified! predicted: ", .pred_clas[.is_misclass],
      ", observed: ", class[.is_misclass])
    tooltip[!.is_misclass] <- paste0(
      tooltip[!.is_misclass], "\nclass: ", class[!.is_misclass])
  }else{
    ### Regression extension
    tooltip <- paste0(
      tooltip, "\nresidual: ", .decode_df$residual)
  }
  .glob_view$tooltip <- c(tooltip, tooltip)
  .decode_df$tooltip <- tooltip
  
  ## Cleanup and return
  ret_ls <- list(
    type = problem_type(y),
    model_performance_df = model_performance_df(model),
    attr_df = attr_df,
    global_view_df = .glob_view,
    global_view_basis_ls = .glob_basis_ls,
    decode_df = .decode_df)
  .m <- gc()
  if(keep_model) ret_ls <- c(ret_ls, model = model)
  if(verbose) tictoc::toc()
  return(ret_ls)
}


if(FALSE){ ## THERORETICAL DEV -----
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