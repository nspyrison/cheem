# Model fits (treeshap & DALEX) -----

#' Random forest model via randomForest
#' 
#' A wrapper function for `randomForest::randomForest` with more modest 
#' hyperparameter defaults and arguments consistent with `cheem`.
#' 
#' @param x The explanatory variables of the model.
#' @param y The target variable of the model.
#' @param verbose Logical, if runtime should be printed. Defaults to TRUE.
#' @param hp_ntree Hyperparameter, the number of trees to grow.
#' @param hp_mtry Hyperparameter, the number variables randomly sampled at 
#' each split.
#' @param hp_nodesize Hyperparameter, the minimum size of terminal nodes. 
#' Setting this number larger causes smaller trees to be grown (and thus take less time).
#' @return A randomForest model.
#' @export
#' @family cheem preprocessing
#' @examples
#' library(cheem)
#' 
#' ## Regression:
#' dat <- amesHousing2018_NorthAmes
#' X <- dat[, 1:9]
#' Y <- log(dat$SalePrice)
#' clas <- dat$SubclassMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' ## Long runtime for full datasets or complex models:
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                     model = rf_fit,
#'                     attr_df = shap_df)
#' global_view(this_ls)
default_rf <- function(
  x, y, verbose = getOption("verbose"),
  hp_ntree = 125,
  hp_mtry = ifelse(is_discrete(y), sqrt(ncol(x)), ncol(x) / 3),
  hp_nodesize = max(ifelse(is_discrete(y), 1, 5), nrow(x) / 500)
){
  if(verbose) tictoc::tic("default_rf")
  suppressWarnings(## suppresses: The response has five or fewer unique values.  Are you sure?
    .mod <- randomForest::randomForest(
      x, y, mtry = hp_mtry, nodesize = hp_nodesize, ntree = hp_ntree)
  )
  .m <- gc()
  if(verbose) tictoc::toc()
  .mod
}


## Condition handling model types ----

#' Check model type
#' 
#' Check whether or not the model is a 
#' certain model. Checks if a model is made with: 
#' \code{\link[randomForest:randomForest]{randomForest::randomForest}},
#' \code{\link[ranger:ranger]{ranger::ranger}},
#' \code{\link[gbm:gbm]{gbm::gbm}},
#' \code{\link[xgboost:xgb.train]{xgboost::xgb.train}},
#' \code{\link[catboost:catboost.train]{catboost::catboost.train}}, or
#' \code{\link[lightgbm:lightgbm]{lightgbm::lightgbm}}.
#' 
#' @param model A model object to check the class/origin.
#' @return A logical, whether or not the model is of a certain class.
#' @export
#' @family cheem utility
#' @examples
#' library(cheem)
#' 
#' ## Regression:
#' dat <- amesHousing2018_NorthAmes
#' X <- dat[, 1:9]
#' Y <- log(dat$SalePrice)
#' clas <- dat$SubclassMS
#' 
#' # treeshap handles various tree-based models:
#' 
#' ## randomForest
#' fit <- randomForest::randomForest(X, Y, ntree = 25)
#' is_randomForest(fit)
#' 
#' ## ranger
#' fit <- ranger::ranger(Y ~ ., data.frame(X, Y), num.trees = 25)
#' is_ranger(fit)
#' 
#' ## gbm
#' fit <- gbm::gbm(Y ~ ., "gaussian", data.frame(X, Y), n.trees = 25)
#' is_gbm(fit)
#' 
#' ## xgboost
#' fit <- xgboost::xgboost(as.matrix(r_X), r_Y, nrounds = 25,
#'                         params = list(objective = "reg:squarederror"))
#' is_xgboost(fit)
#' 
#' ## catboost R examples error.
#' 
#' ## lightgbm
#' lgbm_params <- list(objective = "regression", num_leaves = 25)
#' fit <- lightgbm::lightgbm(as.matrix(X), Y, params = lgbm_params, nrounds = 2)
#' is_lightgbm(fit)
#' 
#' 
#' # Continue cheem workflow with tree-based models:
#' 
#' ## Long runtime for full datasets or complex models:
#' shap_df <- attr_df_treeshap(fit, X, verbose = TRUE, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                     model = fit,
#'                     attr_df = shap_df)
#' global_view(this_ls)
is_randomForest <- function(model){
  "randomForest" %in% class(model)
}
##' @rdname is_randomForest
##' @export
is_ranger <- function(model){
  "ranger" %in% class(model)
}
##' @rdname is_randomForest
##' @export
is_gbm <- function(model){
  "gbm" %in% class(model)
}
##' @rdname is_randomForest
##' @export
is_xgboost <- function(model){
  "xgb.Booster" %in% class(model)
}
##' @rdname is_randomForest
##' @export
is_lightgbm <- function(model){
  "lgb.Booster" %in% class(model)
}
##' @rdname is_randomForest
##' @export
is_catboost <- function(model){
  "catboost.Model" %in% class(model)
}
#' #' Check is the model is supported by treeshap.
#' #' 
#' #' 
#' #' @param model A model to check the treeshap support of.
#' is_treeshap_supported <- function(model){
#'   any(is_randomForest(model), is_ranger(model), is_gmb(model),
#'     is_xgboost(model), is_catboost(model), is_lightgbm(model))
#' }


#' Unifies models into a standard format
#' 
#' Unifies models supported by treeshap into a standard format.
unify_tree_model <- function(
  model, x
){
  if(is_randomForest(model)){
    ret <- treeshap::randomForest.unify(model, x)
  }else if(is_ranger(model)){
    ret <- treeshap::ranger.unify(model, x)
  }else if(is_gbm(model)){
    ret <- treeshap::gbm.unify(model, x)
  }else if(is_xgboost(model)){
    ret <- treeshap::xgboost.unify(model, x)
  }else if(is_catboost(model)){
    ret <- treeshap::catboost.unify(model, x)
  }else if(is_lightgbm(model)){
    ret <- treeshap::lightgbm.unify(model, x)
  }else
    stop("Wasn't a treeshap supported model.") ## Need dev for DALEX supported packages
  ret
}


# treeshap -----

#' Extract the full treeSHAP data.frame of a randomForest model
#' 
#' A data.frame of each observations treeSHAP variable attributions of a
#' randomForest model. 
#' A wrapper for `treeshap::randomForest.unify` and `treeshap::treeshap`.
#' 
#' @param model A tree based model supported by `treeshap`: 
#' a model from `randomForest::randomForest`, `ranger::ranger`, `gbm::gbm`, 
#' `xgboost::xgb.train`, `catboost::catboost.train`, `lightgbm::lightgbm`.
#' @param x The explanatory data (without response) to extract the local 
#' attributions from.
#' @param keep_heavy Logical, if the heavy items "interactions",
#'  "unified_model", and "observations" should be kept. Defaults to FALSE.
#' @param verbose Logical, if runtime should be printed. Defaults to TRUE.
#' @param noisy Logical, if a tone should be played on completion. 
#' Defaults to TRUE.
#' @return A data.frame of the local attributions for each observation.
#' @export
#' @family cheem preprocessing
#' @examples
#' library(cheem)
#' 
#' ## Regression:
#' dat <- amesHousing2018_NorthAmes
#' X <- dat[, 1:9]
#' Y <- log(dat$SalePrice)
#' clas <- dat$SubclassMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' ## Long runtime for full datasets or complex models:
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                     model = rf_fit,
#'                     attr_df = shap_df)
#' global_view(this_ls)
attr_df_treeshap <- function(
  model,
  x,
  keep_heavy = FALSE,
  verbose    = getOption("verbose"),
  noisy      = getOption("verbose")
){ 
  if(verbose){
    writeLines(paste0("Started attr_df_treeshap at: ", Sys.time()))
    tictoc::tic("attr_df_treeshap")
  }
  ## suppress: Using 25 trees... \n
  suppressMessages(.pred <- stats::predict(model, data = x))
  if(any(is.na(.pred)))
    stop("attr_df_treeshap: model had NA values in its predictions; does the model have enough trees/leaves?")
  .unified_mod <- unify_tree_model(model, x)
  ret <- treeshap::treeshap(.unified_mod, x = x)
  if(keep_heavy == FALSE)
    ret <- ret[[1L]]
  ## Keeping only 1; reduces ~99% of the obj size, keep shap values.
  ## But, we lose the iBreakdown-like plot of treeshap::plot_contribution when we take this apart.
  
  class(ret) <- c("data.frame", "treeshap")
  if(verbose) tictoc::toc()
  if(noisy) beepr::beep(1L)
  ret
}

# cheem workflow -----

#' Extract higher level model performance statistics
#' 
#' Internal function, used downstream in cheem_ls.
#' 
#' @param model A non-linear model, originally a `randomForest::randomForest`
#' model fit, or a return from `default_rf()`.
#' @param x Data to predict, required by ranger models.
#' @param y Observed response, required by ranger models.
#' @return A data.frame of model performance statistics.
#' @examples
#' library(cheem)
#' 
#' ## Regression:
#' dat <- amesHousing2018_NorthAmes
#' X <- dat[, 1:9]
#' Y <- log(dat$SalePrice)
#' clas <- dat$SubclassMS
#' 
#' rf_fit <- default_rf(X, Y)
#' cheem:::model_performance_df(rf_fit)
model_performance_df <- function(
  model, x = NULL, y = NULL
){
  #### Following the functions in {Metrics}
  # liable to differ from the performance of the model object (due to adj values?)
  # but at least consistent format and measures.
  .y <- y
  if(is.null(.y)) .y <- model$y
  ## suppress: Using 25 trees... \n
  suppressMessages(.pred <- stats::predict(model, data = x))
  ## ranger doens't have $y and prediction is a list rather than vector
  if(is.list(.pred)) .pred <- .pred$predictions
  .e      <- .y - .pred ## Residual
  .se     <- .e^2L
  .sse    <- sum(.se)
  .mse    <- mean(.se)
  .rmse   <- sqrt(.mse)
  .rse    <- .sse / sum((.y - mean(.y))^2L)
  .r2     <- 1L - .rse
  .r2_adj <- 1L - (.mse / stats::var(.y))
  ## We could be at this all day...
  # .auc    <- Metrics::auc(y, .pred)
  # .mae  <- mean(abs(.e))
  # .mad  <- .mae / length(y)
  # .ROC <- ROCR::
  data.frame(
    row.names  = NULL,
    model_type = class(model)[length(class(model))],
    sse        = .sse,
    mse        = .mse,
    rmse       = .rmse,
    rse        = .rse,
    r2         = .r2,
    r2_adj     = .r2_adj)
}

#' Create the plot data.frame for the global linked plotly display.
#' 
#' Internal function consumed in the cheem workflow. 
#' Produces the plot data.frame of 1 layer. consumed downstream in cheem_ls.
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
#' @examples
#' library(cheem)
#' 
#' ## Regression:
#' dat <- amesHousing2018_NorthAmes
#' X <- dat[, 1:9]
#' Y <- log(dat$SalePrice)
#' clas <- dat$SubclassMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' ## Long runtime for full datasets or complex models:
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
  
  ## Projection
  x_std <- x %>% spinifex::scale_01()
  basis <- switch(basis_type,
                  pca  = spinifex::basis_pca(x_std, d),
                  olda = spinifex::basis_olda(x_std, class, d))
  proj  <- x_std %*% basis %>% spinifex::scale_01() ## Output mapped to 01 for global view
  
  ## Column bind wide
  ret <- data.frame(basis_type, layer_name, 1L:nrow(x), class, proj)
  colnames(ret) <- c("basis_type", "layer_name", "rownum", "class", paste0("V", 1L:d))
  attr(ret, paste0(basis_type, ":", layer_name)) <- basis
  ret
}

#' Preprocessing for use in shiny app
#' 
#' Performs the preprocessing steps needs to supply the plot functions
#' `global_view()` and `radial_cheem_tour()` used in the shiny app.
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
#' @return A list of data.frames needed for the `shiny` application.
#' @seealso [global_view()] [radial_cheem_tour()] [radial_cheem_tour()]
#' @export
#' @family cheem preprocessing
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
#' ## Long runtime for full datasets or complex models:
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                      model = rf_fit,
#'                      attr_df = shap_df)
#' names(this_ls)
#' 
#' ## Regression:
#' dat <- amesHousing2018_NorthAmes
#' X <- dat[, 1:9]
#' Y <- log(dat$SalePrice)
#' clas <- dat$SubclassMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' ## Long runtime for full datasets or complex models:
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                      model = rf_fit,
#'                      attr_df = shap_df)
#' names(this_ls)
#' global_view(this_ls)
#' 
#' ## Save for used with shiny app (expects .rds):
#' if(FALSE){ ## Don't accidentally save.
#'   saveRDS(this_ls, "./my_cheem_ls.rds")
#'   run_app() ## Select the saved .rds file from the Data dropdown.
#' }
cheem_ls <- function(
  x, y, class = NULL,
  model, attr_df,
  basis_type = c("pca", "olda"), ## class req for olda
  layer_name = tail(class(model))[1],
  verbose    = getOption("verbose"),
  keep_model = FALSE
){
  ## Initialize -----
  if(verbose) tictoc::tic("cheem_ls")
  d <- 2L
  basis_type <- match.arg(basis_type)
  is_classification <- is_discrete(y)
  rownum <- V2 <- projection_nm <- NULL
  
  ## global_view_df -----
  .glob_dat  <- global_view_df_1layer(x, y, class, basis_type, "data")
  .cl        <- tail(class(attr_df), 1L)
  .glob_attr <- global_view_df_1layer(attr_df, y, class, basis_type, .cl)
  .glob_view <- rbind(.glob_dat, .glob_attr)
  ## List of the bases
  .dat_bas   <- tail(attributes(.glob_dat),  1L)
  .attr_bas  <- tail(attributes(.glob_attr), 1L)
  .glob_basis_ls <- c(.dat_bas, .attr_bas)
  ## log maha distance of data sapce
  log_maha.data <- stats::mahalanobis(x, colMeans(x), stats::cov(x))
  ## Calculate correlation of attr_proj
  m <- as.matrix(x)
  cor_attr_proj.y <- NULL
  .m <- sapply(1L:nrow(attr_df), function(i)
    cor_attr_proj.y[i] <<- stats::cor(m %*% basis_attr_df(attr_df, i), y)
  )
  
  ## decode_df ----
  if(is.null(class)) class <- factor(FALSE) ## dummy factor
  ## suppress: Using 25 trees... \n
  suppressMessages(.pred <- stats::predict(model, data = x))
  ## ranger.prediction is a list rather than vector
  if(is.list(.pred)) .pred <- .pred$predictions
  .decode_left <- data.frame(
    rownum = 1L:nrow(x), class = class, y, prediction = .pred)
  .decode_right <- data.frame(residual = y - .pred, cor_attr_proj.y, x)
  ## If classification: append pred class/is_misclass
  if(is_classification){
    .pred_clas <- factor(
      levels(class)[round(.pred)], levels = levels(class))
    .is_misclass <- .pred_clas!= class
    .decode_middle <- data.frame(
      predicted_class  = .pred_clas, is_misclassified = .is_misclass)
    .decode_df <- cbind(.decode_left, .decode_middle, .decode_right)
  }else .decode_df <- cbind(.decode_left, .decode_right)
  ## Round numeric columns for display
  .decode_df <- data.frame(lapply(
    .decode_df, function(c) if(is.numeric(c)) round(c, 2L) else c))

  if(is_classification){
    .vec_yjitter <- stats::runif(nrow(x), -.3, .3)
    .layer_nm    <- "model (w/ y jitter)"
  }else{ ## Regression
    .vec_yjitter <- 0L
    .layer_nm    <- "model"
  }
  ## rbind yhaty to global_view_df ----
  .yhaty_df <- data.frame(V1 = .decode_df$prediction, V2 = .decode_df$y + .vec_yjitter) %>%
    spinifex::scale_01()
  .yhaty_df <- data.frame(basis_type = NA, layer_name = .layer_nm,
                          rownum = 1L:nrow(x), class = .decode_df$class, .yhaty_df)
  .glob_view <- rbind(.glob_view, .yhaty_df)
  
  ## Add tooltips ----
  tooltip <- paste0("row: ", 1L:nrow(x)) ## Base tooltip
  if(is.null(rownames(x)) == FALSE)
    ### Character rownames?
    if(does_contain_nonnumeric(rownames(x)))
      tooltip <- paste0(tooltip, ", ", rownames(x))
  if(is_classification){
    ### Classification tooltip
    tooltip[.is_misclass] <- paste0(
      tooltip[.is_misclass],
      "\nMisclassified! predicted: ", .pred_clas[.is_misclass],
      ", observed: ", class[.is_misclass])
    tooltip[!.is_misclass] <- paste0(
      tooltip[!.is_misclass], "\nclass: ", class[!.is_misclass])
  }else tooltip <- paste0(tooltip, "\nresidual: ", .decode_df$residual)
  
  ## Append model info to global_view
  .N <- nrow(.glob_view)
  .glob_view$log_maha.data     <- rep_len(log_maha.data,       .N)
  .glob_view$cor_attr_proj.y   <- rep_len(cor_attr_proj.y,     .N)
  .glob_view$residual          <- rep_len(.decode_df$residual, .N)
  if(is_classification)
    .glob_view$predicted_class <- rep_len(.pred_clas,          .N)
  .glob_view$class             <- rep_len(.decode_df$residual, .N)
  .glob_view$tooltip           <- rep_len(tooltip,             .N)
  .decode_df$tooltip           <- tooltip
  ## Ensure facet order is kept.
  .glob_view$basis_type <- factor(.glob_view$basis_type, unique(.glob_view$basis_type))
  .glob_view$layer_name <- factor(.glob_view$layer_name, unique(.glob_view$layer_name))
  
  ## Cleanup and return
  ret_ls <- list(
    type = problem_type(y),
    model_performance_df = model_performance_df(model, x, y),
    attr_df = attr_df,
    global_view_df = .glob_view,
    global_view_basis_ls = .glob_basis_ls,
    decode_df = .decode_df)
  .m <- gc()
  if(keep_model) ret_ls <- c(ret_ls, model = model)
  if(verbose) tictoc::toc()
  ret_ls
}


if(FALSE){ ## Extension ideas -----
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
  
    plot(.pred_prof, variables = var_nms) +
      ggtitle::ggtitle("Ceteris-paribus profile", "")# +
    # + ggplot2::ylim(min(y), max(y))
  }
}