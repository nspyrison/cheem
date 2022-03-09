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
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Model, treeSHAP, cheem list, visualize
#' rf_fit  <- default_rf(X, Y)
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
# #' \code{\link[catboost:catboost.train]{catboost::catboost.train}}, or
#' \code{\link[lightgbm:lightgbm]{lightgbm::lightgbm}}.
#' 
#' @param model A model object to check the class/origin.
#' @return A logical, whether or not the model is of a certain class.
#' @export
#' @family cheem unify
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' 
#' # Treeshap handles various tree-based models:
#' 
#' ## randomForest model
#' fit <- randomForest::randomForest(X, Y, ntree = 25)
#' is_randomForest(fit)
#' 
#' ## gbm model
#' if(require(gbm, quietly = TRUE)){
#'   fit <- gbm::gbm(Y ~ ., "gaussian", data.frame(X, Y), n.trees = 25)
#'   is_gbm(fit)
#' }
#' 
#' ## lightgbm
#' if(require(lightgbm, quietly = TRUE)){
#'   param_lgbm <- list(num_leaves = 25, objective = "regression")
#'   fit <- lightgbm::lightgbm(
#'     as.matrix(X), Y, params = param_lgbm, 
#'     nrounds = 2, verbose = 0)
#'   ## Delete model file if it exists
#'   if(file.exists("lightgbm.model"))
#'     file.remove("lightgbm.model")
#'   is_lightgbm(fit)
#' }
#' 
#' ## ranger model
#' if(require(ranger, quietly = TRUE)){
#'   fit <- ranger::ranger(Y ~ ., data.frame(X, Y), num.trees = 25)
#'   is_ranger(fit)
#' }
#' 
#' ## xgboost
#' if(require(xgboost, quietly = TRUE)){
#'   fit <- xgboost::xgboost(as.matrix(X), Y, nrounds = 25, verbose = 0,
#'                           params = list(objective = "reg:squarederror"))
#'   is_xgboost(fit)
#' }
#' 
#' 
#' # Continue cheem workflow with tree-based models:
#' 
#' ## treeSHAP, cheem list, visualize:
#' shap_df <- attr_df_treeshap(fit, X, verbose = TRUE, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                     model = fit,
#'                     attr_df = shap_df)
#' global_view(this_ls)
is_randomForest <- function(model){
  "randomForest" %in% class(model)
}
#' @rdname is_randomForest
#' @export
is_ranger <- function(model){
  "ranger" %in% class(model)
}
#' @rdname is_randomForest
#' @export
is_gbm <- function(model){
  "gbm" %in% class(model)
}
#' @rdname is_randomForest
#' @export
is_xgboost <- function(model){
  "xgb.Booster" %in% class(model)
}
#' @rdname is_randomForest
#' @export
is_lightgbm <- function(model){
  "lgb.Booster" %in% class(model)
}
# #' @rdname is_randomForest
# #' @export
# is_catboost <- function(model){
#   "catboost.Model" %in% class(model)
# }

#' Unify various models/predictions to a standard format
#' 
#' Unifies models/prediction functions for supported by tree-based models 
#' into a standard format.
#' 
#' @param model A tree based model supported by `treeshap`: 
#' a model from `randomForest::randomForest`, `ranger::ranger`, `gbm::gbm`, 
#' `xgboost::xgb.train`, or `lightgbm::lightgbm`.
#' @param x The explanatory data (without response) to extract the local 
#' attributions from.
#' @return A vector of predicted values
#' @family cheem unify
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
#' ## Model, unified prediction from any model (not unified)
#' rf_fit <- default_rf(X, Y)
#' unify_predict(rf_fit, X)
#' 
#' ## Applies the correct treeshap::*.unify for the model type.
#' unified_model <- unify_tree_model(rf_fit, X)
#' str(unified_model)
unify_tree_model <- function(model, x){
if(is_randomForest(model)){
  ret <- randomForest.unify(model, x) ## treeshap:: ported function
}else if(is_ranger(model)){
  ret <- ranger.unify(model, x) ## treeshap:: ported function
}else if(is_gbm(model)){
  ret <- gbm.unify(model, x) ## treeshap:: ported function
}else if(is_xgboost(model)){
  ret <- xgboost.unify(model, x) ## treeshap:: ported function
  # }else if(is_catboost(model)){
  #   ret <- catboost.unify(model, x) ## treeshap:: ported function
}else if(is_lightgbm(model)){
  ret <- lightgbm.unify(model, x) ## treeshap:: ported function
}else
  stop("unify_tree_model: wasn't a treeshap supported model.") ## Need dev for DALEX supported packages
if(nrow(ret[[1L]]) == 0L)
  stop("unify_tree_model: treeshap unified model has 0 rows, model may not be supported.")
## Return
ret
}

#' @rdname unify_tree_model
#' @export
unify_predict <- function(model, x){
  if(is_xgboost(model)){
    ## xgboost:::predict.xgb.Booster expects arg name newdata, rather than data.
    .pred <- stats::predict(model, newdata = as.matrix(x))
  }else if(is_lightgbm(model)){
    .pred <- stats::predict(model, data = as.matrix(x))
  }else suppressMessages(.pred <- stats::predict(model, data = x))
  ## suppress: Using 25 trees... \n
  
  ## ranger predict returns a list
  if(is.list(.pred)) .pred <- .pred$predictions
  
  ## Return
  .pred
}


## treeshap -----

#' Extract the full treeSHAP data.frame of a randomForest model
#' 
#' A data.frame of each observations treeSHAP variable attributions of a
#' randomForest model. 
#' A wrapper for `treeshap::randomForest.unify` and `treeshap::treeshap`.
#' 
#' @param model A tree based model supported by `treeshap`: 
#' a model from `gbm`, `lightgbm`, `randomForest`, `ranger`, or  `xgboost`.
#' @param x The explanatory data (without response) to extract the local 
#' attributions from.
#' @param keep_heavy Logical, if the heavy items "interactions",
#' "unified_model", and "observations" should be kept. Defaults to FALSE.
#' @param verbose Logical, if runtime should be printed. Defaults to TRUE.
#' @param noisy Logical, if a tone should be played on completion. 
#' Defaults to TRUE.
#' @return A data.frame of the local attributions for each observation.
#' @export
#' @family cheem preprocessing
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
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
  .pred <- unify_predict(model, x)
  if(any(is.na(.pred)))
    stop("attr_df_treeshap: model had NA values in its predictions; does the model have enough trees/leaves?")
  .unified_mod <- unify_tree_model(model, x)
  ret <- treeshap(.unified_mod, x = x) ## treeshap:: ported function
  if(keep_heavy == FALSE)
    ret <- ret[[1L]]
  ## Keeping only 1; reduces ~99% of the obj size, while keeping shap values.
  ## But, we lose the iBreakdown-like plot of treeshap::plot_contribution
  
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
# #' @examples
# #' library(cheem)
# #' 
# #' ## Regression setup:
# #' dat  <- amesHousing2018_NorthAmes
# #' X    <- dat[, 1:9]
# #' Y    <- dat$SalePrice
# #' clas <- dat$SubclassMS
# #' 
# #' ## Model and performance:
# #' rf_fit <- default_rf(X, Y)
# #' cheem:::model_performance_df(rf_fit)
model_performance_df <- function(
  model, x = NULL, y = NULL
){
  tryCatch(
    { ## Try
      
      #### Following the functions in {Metrics}
      # liable to differ from the performance of the model object (due to adj values?)
      # but at least consistent format and measures.
      .y <- y
      if(is.null(.y)) .y <- model$y
      .pred   <- unify_predict(model, x)
      .e      <- .y - .pred ## Residual
      .se     <- .e^2L
      .sse    <- sum(.se)
      .mse    <- mean(.se)
      .rmse   <- sqrt(.mse)
      .mad    <- stats::median(abs(.e))
      .rse    <- .sse / sum((.y - mean(.y))^2L)
      .r2     <- 1L - .rse
      .adj_r2 <- 1L - (.mse / stats::var(.y))
      
      ## AUC, only applicable to classification case
      # {## auc, from DALEX:::model_performance_auc
      #   tpr_tmp <- tapply(.y, .pred, sum)
      #   TPR <- c(0L, cumsum(rev(tpr_tmp))) / sum(.y)
      #   fpr_tmp <- tapply(1L - .y, .pred, sum)
      #   FPR <- c(0L, cumsum(rev(fpr_tmp))) / sum(1L - .y)
      #   auc <- sum(diff(FPR) * (TPR[-1L] + TPR[-length(TPR)]) / 2L)
      # }
      
      data.frame(model_type = utils::tail(class(model), 1L),
                 mse        = .mse,
                 rmse       = .rmse,
                 mad        = .mad,
                 r2         = .r2,
                 adj_r2     = .adj_r2,
                 #auc        = auc,
                 row.names  = NULL)
    }, 
    error = function(cond){
      data.frame(model_type = utils::tail(class(model), 1L),
                 result     = "Model performance caused an erorr:",
                 erorr      = cond)
    }
  )
}

#' Create the plot data.frame for the global linked plotly display.
#' 
#' Internal function consumed in the cheem workflow. 
#' Produces the plot data.frame of 1 layer. consumed downstream in cheem_ls.
#' 
#' @param x The explanatory variables of the model.
#' @param class The variable to group points by. Originally the _predicted_
#'  class.
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Defaults to "pca".
#' @param layer_name Character layer name, typically the type of local 
#' attribution used. Defaults to the name of the last class of x.
#' @return A data.frame, for the global linked __plotly__ display.
# #' @examples
# #' library(cheem)
# #' 
# #' ## Regression setup:
# #' dat <- amesHousing2018_NorthAmes
# #' X   <- dat[, 1:9]
# #' 
# #' ## data.frame for one space/panel
# #' cheem:::global_view_df_1layer(X)
global_view_df_1layer <- function(
  x,
  class      = NULL, ## required for olda
  basis_type = c("pca", "olda"),
  layer_name = utils::tail(class(x), 1) ## Name of the last class _ie_ `treeshap``
){
  d <- 2L ## Fixed display dimensionality
  basis_type <- match.arg(basis_type)
  if(is.null(class)) class <- as.factor(FALSE)

  
  ## Projection
  x_std <- spinifex::scale_01(x)
  basis <- switch(basis_type,
                  pca  = spinifex::basis_pca(x_std, d),
                  olda = spinifex::basis_olda(x_std, class, d))
  proj  <- spinifex::scale_01(x_std %*% basis)
  
  ## Column bind wider
  ret <- data.frame(basis_type, layer_name, 1L:nrow(x), class, proj)
  colnames(ret) <-
    c("basis_type", "layer_name", "rownum", "class", paste0("V", 1L:d))
  attr(ret, paste0(basis_type, ":", layer_name)) <- basis
  
  ## Return
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
#' @param keep_model Logical, whether or not the heavy model object should be 
#' kept. Defaults to FALSE.
#' @return A list of data.frames needed for the `shiny` application.
#' @seealso [global_view()] [radial_cheem_tour()] [radial_cheem_tour()]
#' @export
#' @family cheem preprocessing
#' @examples
#' library(cheem)
#' library(spinifex)
#' 
#' ## Classification setup:
#' X    <- penguins_na.rm[, 1:4]
#' clas <- penguins_na.rm$species
#' Y    <- as.integer(clas)
#' 
#' ## Model and treeSHAP explanation:
#' rf_fit  <- default_rf(X, Y)
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                      model = rf_fit,
#'                      attr_df = shap_df)
#' global_view(this_ls) ## Preview spaces
#'
#' ## Save for used with shiny app (expects .rds):
#' if(FALSE){ ## Don't accidentally save.
#'   saveRDS(this_ls, "./my_cheem_ls.rds")
#'   run_app() ## Select the saved .rds file from the Data dropdown.
#' }
#' 
#' 
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Model and treeSHAP explanation:
#' rf_fit  <- default_rf(X, Y)
#' \donttest{
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                      model = rf_fit,
#'                      attr_df = shap_df)
#' global_view(this_ls) ## Preview spaces
#' }
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
  layer_name = utils::tail(class(model), 1),
  verbose    = getOption("verbose"),
  keep_model = FALSE
){
  if(any(sapply(attr_df, function(i) length(unique(i)) == 1L)))
    stop(paste0(
      "cheem_ls: ", layer_name, 
      " had at least one column with one unique level.", 
      " Eigen matrix not be symmetric.",
      " Please review model complexity and attr_df."))
  ## Initialize 
  if(verbose) tictoc::tic("cheem_ls")
  d <- 2L
  basis_type <- match.arg(basis_type)
  is_classification <- is_discrete(y)
  rownum <- V2 <- projection_nm <- NULL
  
  ## global_view_df -----
  .glob_dat  <- global_view_df_1layer(x, class, basis_type, "data")
  .cl        <- utils::tail(class(attr_df), 1L)
  .glob_attr <- global_view_df_1layer(attr_df, class, basis_type, .cl)
  .glob_view <- rbind(.glob_dat, .glob_attr)
  ## List of the bases
  .dat_bas   <- utils::tail(attributes(.glob_dat),  1L)
  .attr_bas  <- utils::tail(attributes(.glob_attr), 1L)
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
  .pred <- unify_predict(model, x)
  .decode_left <- data.frame(
    rownum = 1L:nrow(x), class = class, y, prediction = .pred)
  .decode_right <- data.frame(residual = y - .pred, cor_attr_proj.y, x)
  ## If classification: append pred class/is_misclass
  if(is_classification){
    .pred_clas <- factor(
      levels(class)[round(.pred)], levels = levels(class))
    .is_misclass <- .pred_clas != class
    .decode_middle <- data.frame(
      predicted_class  = .pred_clas, is_misclassified = .is_misclass)
    .decode_df <- cbind(.decode_left, .decode_middle, .decode_right)
  }else .decode_df <- cbind(.decode_left, .decode_right)
  ## Round numeric columns for display
  .decode_df <- data.frame(lapply(
    .decode_df, function(c) if(is.numeric(c)) round(c, 2L) else c))
  
  if(is_classification){
    .vec_yjitter <- stats::runif(nrow(x), -.2, .2)
    .layer_nm    <- "model (w/ y jitter)"
  }else{ ## Regression
    .vec_yjitter <- 0L
    .layer_nm    <- "model"
  }
  ## append yhaty to global_view_df ----
  .yhaty_df <-
    data.frame(V1 = .decode_df$prediction, V2 = .decode_df$y + .vec_yjitter) %>%
    spinifex::scale_01()
  .yhaty_df <- data.frame(basis_type = NA, layer_name = .layer_nm,
                          rownum = 1L:nrow(x), class = .decode_df$class, .yhaty_df)
  .glob_view <- rbind(.glob_view, .yhaty_df)
  
  ## add tooltips ----
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
  if(keep_model) ret_ls <- c(ret_ls, model = model)
  if(verbose) tictoc::toc()
  ret_ls
}





# cheem extension ideas -----
#- Ceteris-paribus profiles for prim/comp obs? 
#-- DALEX::explain(model) %>%
#-- DALEX::predict_profile(prim/comp_obs)
#- Breakdown plots for prim/comp obs? 
#-- Probably now; good for illustrating shap, not really 1:1 with treeshap
#-- DALEX::explain(model) %>%
#-- DALEX::predict_parts(prim/comp_obs, type [NOT INTEROPERABLE WITH treeshap])
