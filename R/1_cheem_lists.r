# Model fits  -----






# cheem workflow -----

#' Extract higher level model performance statistics
#' 
#' Internal function, used downstream in cheem_ls.
#' 
#' @param y Observed response, required by ranger models.
#' @param pred A [n, 1] vector of the predicted values for each observation.
#' Typically obtainable with `stats::predict()`. Exact syntax and arguments 
#' may change across model types.
#' @param label Optionally provide a character label to store reminder 
#' text for the type of model and local explanation used. 
#' Defaults to "label".
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
# #' rf_fit <- randomForest::randomForest(
# #'   X, Y, ntree = 125,
# #'   mtry = ifelse(is_discrete(Y), sqrt(ncol(X)), ncol(X) / 3),
# #'   nodesize = max(ifelse(is_discrete(Y), 1, 5), nrow(X) / 500))
# #' cheem:::model_performance_df(rf_fit)
model_performance_df <- function(
  y, pred, label = "label"
){
  tryCatch({
    .e      <- y - pred ## Residual
    .se     <- .e^2
    .sse    <- sum(.se)
    .mse    <- mean(.se)
    .rmse   <- sqrt(.mse)
    .mad    <- stats::median(abs(.e))
    .rse    <- .sse / sum((y - mean(y))^2)
    .r2     <- 1 - .rse
    .adj_r2 <- 1 - (.mse / stats::var(y))
    
    ## There are a whole host of other performance measurements that  
    ## some analysts may prefer. Hoeveer, it's Hard to unify across models 
    ## and use cases.
    data.frame(label     = label,
               mse       = .mse,
               rmse      = .rmse,
               mad       = .mad,
               r2        = .r2,
               adj_r2    = .adj_r2)
  },
  error = function(cond){
    data.frame(label  = label,
               result = "model_performance_df() caused an erorr:",
               erorr  = cond)
  })
}

#' Create the plot data.frame for the global linked plotly display.
#' 
#' Internal function consumed in the cheem workflow. 
#' Produces the plot data.frame of 1 layer. consumed downstream in cheem_ls.
#' 
#' @param x The explanatory variables of the model.
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Defaults to "pca". 
#' Expects "pca" or "olda" (reqires `class`).
#' @param class Optional, [n x 1] vector, a variable to group points by. 
#' This can be the same as or different from `y`, the target variable.
#' @param label Optionally provide a character label to store reminder 
#' text for the type of model and local explanation used. 
#' Defaults to "label".
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
  label      = "label"
){
  d <- 2 ## Fixed display dimensionality
  basis_type <- match.arg(basis_type)
  if(is.null(class)) class <- as.factor(FALSE)
  
  ## Projection
  x_std <- spinifex::scale_01(x)
  basis <- switch(basis_type,
                  pca  = spinifex::basis_pca(x_std, d),
                  olda = spinifex::basis_olda(x_std, class, d))
  proj  <- spinifex::scale_01(x_std %*% basis)
  
  ## Column bind wider
  ret <- data.frame(basis_type, label, 1:nrow(x), class, proj)
  colnames(ret) <- c("basis_type", "label", "rownum", "class", paste0("V", 1:d))
  attr(ret, paste0(basis_type, ":", label)) <- basis
  
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
#' @param attr_df A data frame or matrix of [n x p] local explanation 
#' attributions.
#' @param pred A [n x 1] vector, the predictions from the accosicated model.
#' @param class Optional, [n x 1] vector, a variable to group points by. 
#' This can be the same as or different from `y`, the target variable.
#' @param label Optionally provide a character label to store reminder 
#' text for the type of model and local explanation used. 
#' Defaults to "label".
#' @param basis_type The type of basis used to approximate the data and 
#' attribution space from. Defaults to "pca". 
#' Expects "pca" or "olda" (reqires `class`).
#' @param verbose Logical, if start time and run duration should be printed. 
#' Defaults to getOption("verbose").
#' @return A list of data.frames needed for the `shiny` application.
#' @seealso [global_view()] [radial_cheem_tour()] [radial_cheem_tour()]
#' @export
#' @family cheem preprocessing
#' @examples
#' library(cheem)
#' 
#' ## Classification setup:
#' X <- spinifex::penguins_na.rm[, 1:4]
#' clas <- spinifex::penguins_na.rm$species
#' Y <- as.integer(clas)
#' 
#' 
#' ## Model and prediction:
#' peng_rf_fit <- randomForest::randomForest(
#'   X, Y, ntree = 125,
#'   mtry = ifelse(is_discrete(Y), sqrt(ncol(X)), ncol(X) / 3),
#'   nodesize = max(ifelse(is_discrete(Y), 1, 5), nrow(X) / 500))
#' peng_rf_pred <- predict(peng_rf_fit)
#' 
#' ## Extract treeshap
#' peng_rf_tshap <- peng_rf_fit %>%
#'   treeshap::randomForest.unify(X) %>%
#'   treeshap::treeshap(X, interactions = FALSE, verbose = FALSE)
#' ## Keep only the [n, p] attr_df
#' peng_rf_tshap <- peng_rf_tshap$shaps
#' 
#' 
#' 
#' ## cheem visual
#' peng_rf_chm <- cheem_ls(X, Y,
#'                         attr_df = peng_rf_tshap,
#'                         pred = peng_rf_pred,
#'                         class = clas,
#'                         label = "Penguin, RF, treeshap")
#' global_view(peng_rf_chm) ## Preview spaces
#'
#' ## Save for use with shiny app (expects .rds):
#' if(FALSE){ ## Don't accidentally save.
#'   saveRDS(peng_rf_chm, "./peng_rf_tshap.rds")
#'   run_app() ## Select the saved .rds file from the Data dropdown.
#' }
#' 
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Model and treeSHAP:
#' rf_fit <- randomForest::randomForest(
#'   X, Y, ntree = 125,
#'   mtry = ifelse(is_discrete(Y), sqrt(ncol(X)), ncol(X) / 3),
#'   nodesize = max(ifelse(is_discrete(Y), 1, 5), nrow(X) / 500))
#' shap_df <- stop("REPLACE ME")
#' 
#' ## Cheem visual:
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                     model = rf_fit,
#'                     attr_df = shap_df)
#' if(interactive()){
#'   global_view(this_ls) ## Preview spaces
#' }
#' 
#' ## Save for use with shiny app (expects .rds):
#' if(FALSE){ ## Don't accidentally save.
#'   saveRDS(this_ls, "./my_cheem_ls.rds")
#'   run_app() ## Select the saved .rds file from the Data drop down.
#' }
cheem_ls <- function(
  x, y,
  attr_df,
  pred,
  class = NULL,
  basis_type = c("pca", "olda"), ## class req for olda
  label = "label",
  verbose = getOption("verbose")
){
  ## Checks
  if(verbose) tictoc::tic("cheem_ls")
  x       <- data.frame(x)
  attr_df <- data.frame(attr_df)
  stopifnot("data.frame" %in% class(x))
  stopifnot("data.frame" %in% class(attr_df))
  if(any(apply(attr_df, 2, function(i) length(unique(i)) == 1)))
    stop(paste0(
      "cheem_ls: ", label,
      " had at least one column with one unique level.",
      " Eigen matrix wont be symmetric.",
      " Please review model complexity and attr_df."))
  ## Initialize
  d <- 2 ## Hard coded display dimensionality
  basis_type <- match.arg(basis_type)
  is_classification <- is_discrete(y)
  rownum <- V2 <- projection_nm <- NULL
  
  ## global_view_df -----
  .glob_dat  <- global_view_df_1layer(x, class, basis_type, "data")
  .glob_attr <- global_view_df_1layer(attr_df, class, basis_type, "attribution")
  .glob_view <- rbind(.glob_dat, .glob_attr)
  ## List of the bases
  .dat_bas   <- utils::tail(attributes(.glob_dat),  1)
  .attr_bas  <- utils::tail(attributes(.glob_attr), 1)
  .glob_basis_ls <- c(.dat_bas, .attr_bas)
  ## log maha distance of data space
  log_maha.data <- stats::mahalanobis(x, colMeans(x), stats::cov(x))
  ## Calculate correlation of attr_proj
  m <- as.matrix(x)
  cor_attr_proj.y <- NULL
  .m <- sapply(1:nrow(attr_df), function(i)
    cor_attr_proj.y[i] <<- stats::cor(m %*% basis_attr_df(attr_df, i), y)
  )
  
  ## decode_df ----
  if(is.null(class)) class <- factor(FALSE) ## dummy factor
  .decode_left <- data.frame(
    rownum = 1:nrow(x), class = class, y, prediction = pred)
  .decode_right <- data.frame(residual = y - pred, cor_attr_proj.y, x)
  ## If classification: append pred class/is_misclass
  if(is_classification){
    .pred_clas <- factor(
      levels(class)[round(pred)], levels = levels(class))
    .is_misclass <- .pred_clas != class
    .decode_middle <- data.frame(
      predicted_class = .pred_clas, is_misclassified = .is_misclass)
    .decode_df <- cbind(.decode_left, .decode_middle, .decode_right)
  }else .decode_df <- cbind(.decode_left, .decode_right)
  ## Round numeric columns for display
  .decode_df <- data.frame(lapply(
    .decode_df, function(c) if(is.numeric(c)) round(c, 2) else c))
  
  if(is_classification){
    .vec_yjitter  <- stats::runif(nrow(x), -.2, .2)
    .y_axis_label <- "model (w/ y jitter)"
  }else{ ## Regression
    .vec_yjitter  <- 0
    .y_axis_label <- "model"
  }
  ## append yhaty to global_view_df ----
  .yhaty_df <-
    data.frame(V1 = .decode_df$prediction, V2 = .decode_df$y + .vec_yjitter) %>%
    spinifex::scale_01()
  .yhaty_df <- data.frame(basis_type = NA, label = .y_axis_label,
                          rownum = 1:nrow(x), class = .decode_df$class, .yhaty_df)
  .glob_view <- rbind(.glob_view, .yhaty_df)
  
  ## add tooltips ----
  tooltip <- paste0("row: ", 1:nrow(x)) ## Base tooltip
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
    tooltip[!.is_misclass] <- paste0(tooltip[!.is_misclass], "\nclass: ", 
                                     class[!.is_misclass])
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
  .glob_view$label      <- factor(.glob_view$label, unique(.glob_view$label))
  
  ## Cleanup and return
  ret_ls <- list(
    type                 = problem_type(y),
    model_performance_df = model_performance_df(y, pred, label),
    attr_df              = attr_df,
    global_view_df       = .glob_view,
    global_view_basis_ls = .glob_basis_ls,
    decode_df            = .decode_df)
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


# Deprecated, generalizing local explanation attribution ----

#' #' Unify various models/predictions to a standard format
#' #' 
#' #' Unifies models/prediction functions for supported by tree-based models 
#' #' into a standard format.
#' #' 
#' #' @param model A tree based model supported by `treeshap`: 
#' #' a model from `randomForest::randomForest`, `ranger::ranger`, `gbm::gbm`, 
#' #' `xgboost::xgb.train`, or `lightgbm::lightgbm`.
#' #' @param x The explanatory data (without response) to extract the local 
#' #' attributions from.
#' #' @return A vector of predicted values
#' #' @family cheem unify
#' #' @export
#' #' @examples
#' #' library(cheem)
#' #' 
#' #' ## Regression setup:
#' #' dat  <- amesHousing2018_NorthAmes
#' #' X    <- dat[, 1:9]
#' #' Y    <- dat$SalePrice
#' #' clas <- dat$SubclassMS
#' #' 
#' #' ## Model, unified prediction from any model (not unified)
#' #' rf_fit <- default_rf(X, Y)
#' #' unify_predict(rf_fit, X)
#' #' 
#' #' ## Applies the correct treeshap::*.unify for the model type.
#' #' unified_model <- unify_tree_model(rf_fit, X)
#' #' str(unified_model)
#' unify_tree_model <- function(model, x){
#' if(is_randomForest(model)){
#'   ret <- randomForest.unify(model, x) ## treeshap:: ported function
#' }else if(is_ranger(model)){
#'   ret <- ranger.unify(model, x) ## treeshap:: ported function
#' }else if(is_gbm(model)){
#'   ret <- gbm.unify(model, x) ## treeshap:: ported function
#' }else if(is_xgboost(model)){
#'   ret <- xgboost.unify(model, x) ## treeshap:: ported function
#'   # }else if(is_catboost(model)){
#'   #   ret <- catboost.unify(model, x) ## treeshap:: ported function
#' }else if(is_lightgbm(model)){
#'   ret <- lightgbm.unify(model, x) ## treeshap:: ported function
#' }else
#'   stop("unify_tree_model: wasn't a treeshap supported model.") ## Need dev for DALEX supported packages
#' if(nrow(ret[[1]]) == 0)
#'   stop("unify_tree_model: treeshap unified model has 0 rows, model may not be supported.")
#' ## Return
#' ret
#' }


#' ## Condition handling model types ----
#' 
#' #' Check model type
#' #' 
#' #' Check whether or not the model is a 
#' #' certain model. Checks if a model is made with: 
#' #' \code{\link[randomForest:randomForest]{randomForest::randomForest}},
#' #' \code{\link[ranger:ranger]{ranger::ranger}},
#' #' \code{\link[gbm:gbm]{gbm::gbm}},
#' #' \code{\link[xgboost:xgb.train]{xgboost::xgb.train}},
#' # #' \code{\link[catboost:catboost.train]{catboost::catboost.train}}, or
#' #' \code{\link[lightgbm:lightgbm]{lightgbm::lightgbm}}.
#' #' 
#' #' @param model A model object to check the class/origin.
#' #' @return A logical, whether or not the model is of a certain class.
#' #' @export
#' #' @family cheem unify
#' #' @examples
#' #' library(cheem)
#' #' 
#' #' ## Regression setup:
#' #' dat  <- amesHousing2018_NorthAmes
#' #' X    <- dat[, 1:9]
#' #' Y    <- dat$SalePrice
#' #' clas <- dat$SubclassMS
#' #' 
#' #' 
#' #' # Treeshap handles various tree-based models:
#' #' 
#' #' ## randomForest model
#' #' fit <- randomForest::randomForest(X, Y, ntree = 25)
#' #' is_randomForest(fit)
#' #' 
#' #' ## gbm model
#' #' if(require(gbm, quietly = TRUE)){
#' #'   fit <- gbm::gbm(Y ~ ., "gaussian", data.frame(X, Y), n.trees = 25)
#' #'   is_gbm(fit)
#' #' }
#' #' 
#' #' ## lightgbm
#' #' if(require(lightgbm, quietly = TRUE)){
#' #'   param_lgbm <- list(num_leaves = 25, objective = "regression")
#' #'   fit <- lightgbm::lightgbm(
#' #'     as.matrix(X), Y, params = param_lgbm, 
#' #'     nrounds = 2, verbose = 0)
#' #'   ## Delete model file if it exists
#' #'   if(file.exists("lightgbm.model"))
#' #'     file.remove("lightgbm.model")
#' #'   is_lightgbm(fit)
#' #' }
#' #' 
#' #' ## ranger model
#' #' if(require(ranger, quietly = TRUE)){
#' #'   fit <- ranger::ranger(Y ~ ., data.frame(X, Y), num.trees = 25)
#' #'   is_ranger(fit)
#' #' }
#' #' 
#' #' ## xgboost
#' #' if(require(xgboost, quietly = TRUE)){
#' #'   fit <- xgboost::xgboost(as.matrix(X), Y, nrounds = 25, verbose = 0,
#' #'                           params = list(objective = "reg:squarederror"))
#' #'   is_xgboost(fit)
#' #' }
#' #' 
#' #' 
#' #' # Continue cheem workflow with tree-based models:
#' #' 
#' #' ## treeSHAP, cheem list, visualize:
#' #' shap_df <- stop("REPLACE ME")
#' #' this_ls <- cheem_ls(X, Y, class = clas,
#' #'                     model = fit,
#' #'                     attr_df = shap_df)
#' #' global_view(this_ls)
#' is_randomForest <- function(model){
#'   "randomForest" %in% class(model)
#' }
#' #' @rdname is_randomForest
#' #' @export
#' is_ranger <- function(model){
#'   "ranger" %in% class(model)
#' }
#' #' @rdname is_randomForest
#' #' @export
#' is_gbm <- function(model){
#'   "gbm" %in% class(model)
#' }
#' #' @rdname is_randomForest
#' #' @export
#' is_xgboost <- function(model){
#'   "xgb.Booster" %in% class(model)
#' }
#' #' @rdname is_randomForest
#' #' @export
#' is_lightgbm <- function(model){
#'   "lgb.Booster" %in% class(model)
#' }
#' # #' @rdname is_randomForest
#' # #' @export
#' # is_catboost <- function(model){
#' #   "catboost.Model" %in% class(model)
#' # }
#' 
#' 
#' 
#' #' @rdname unify_tree_model
#' #' @export
#' unify_predict <- function(model, x){
#'   if(is_xgboost(model)){
#'     ## xgboost:::predict.xgb.Booster expects arg name newdata, rather than data.
#'     .pred <- stats::predict(model, newdata = as.matrix(x))
#'   }else if(is_lightgbm(model)){
#'     .pred <- stats::predict(model, data = as.matrix(x))
#'   }else suppressMessages(.pred <- stats::predict(model, data = x))
#'   
#'   ## ranger predict returns a list
#'   if(is.list(.pred)) .pred <- .pred$predictions
#'   
#'   ## Return
#'   .pred
#' }



#' #' Extract the full treeSHAP data.frame of a randomForest model
#' #' 
#' #' A data.frame of each observations treeSHAP variable attributions of a
#' #' randomForest model. 
#' #' A wrapper for `treeshap::randomForest.unify` and `treeshap::treeshap`.
#' #' 
#' #' @param model A tree based model supported by `treeshap`: 
#' #' a model from `gbm`, `lightgbm`, `randomForest`, `ranger`, or  `xgboost`.
#' #' @param x The explanatory data (without response) to extract the local 
#' #' attributions from.
#' #' @param keep_heavy Logical, if the heavy items "interactions",
#' #' "unified_model", and "observations" should be kept. Defaults to FALSE.
#' #' @param verbose Logical, if runtime should be printed. Defaults to TRUE.
#' #' @param noisy Logical, if a tone should be played on completion. 
#' #' Defaults to TRUE.
#' #' @return A data.frame of the local attributions for each observation.
#' #' @export
#' #' @family cheem preprocessing
#' #' @examples
#' #' library(cheem)
#' #' 
#' #' ## Regression setup:
#' #' dat  <- amesHousing2018_NorthAmes
#' #' X    <- dat[, 1:9]
#' #' Y    <- dat$SalePrice
#' #' clas <- dat$SubclassMS
#' #' 
#' #' rf_fit  <- default_rf(X, Y)
#' #' ## Long runtime for full datasets or complex models:
#' #' shap_df <- attr_df(rf_fit, X, noisy = FALSE)
#' #' this_ls <- cheem_ls(X, Y, class = clas,
#' #'                     model = rf_fit,
#' #'                     attr_df = shap_df)
#' #' global_view(this_ls)
#' attr_df <- function(
#'     attr,
#'     x,
#'     pred,
#'     keep_heavy = FALSE,
#'     verbose    = getOption("verbose"),
#'     noisy      = getOption("verbose")
#' ){ 
#'   if(verbose){
#'     cat(paste0("Started attr_df at: ", Sys.time()))
#'     tictoc::tic("attr_df")
#'   }
#'   .pred <- unify_predict(model, x)
#'   if(any(is.na(.pred)))
#'     stop("attr_df: model had NA values in its predictions; does the model have enough trees/leaves?")
#'   if(keep_heavy == FALSE)
#'     ret <- ret[[1]]
#'   ## Keeping only 1; reduces ~99% of the obj size, while keeping shap values.
#'   ## But, we lose the iBreakdown-like plot of treeshap::plot_contribution
#'   
#'   class(ret) <- c("data.frame", "treeshap")
#'   if(verbose) tictoc::toc()
#'   if(noisy) beepr::beep(1)
#'   ret
#' }


# Deprecated, model simplifers -----

#' #' Modest random forest model (via randomForest)
#' #' 
#' #' A wrapper function for `randomForest::randomForest` with more modest 
#' #' hyperparameter defaults and arguments consistent with `cheem`. We encourage
#' #' you to bring your own model and local explanation or at least adjust the
#' #' hyperparameter (`hp_`) arguments to better fit your data.
#' #' 
#' #' @param x The explanatory variables of the model.
#' #' @param y The target variable of the model.
#' #' @param verbose Logical, if start time and run duration should be printed. 
#' #' Defaults to getOption("verbose").
#' #' @param hp_ntree Hyperparameter, the number of trees to grow.
#' #' @param hp_mtry Hyperparameter, the number variables randomly sampled at 
#' #' each split.
#' #' @param hp_nodesize Hyperparameter, the minimum size of terminal nodes. 
#' #' Setting this number larger causes smaller trees to be grown (and thus take less time).
#' #' @param ... Optionally, other arguments to pass to the
#' #' `randomForest::ranrandomForest()` call.
#' #' @return A randomForest model.
#' #' @export
#' #' @family cheem preprocessing
#' #' @examples
#' #' library(cheem)
#' #' 
#' #' ## Regression setup:
#' #' dat  <- amesHousing2018_NorthAmes
#' #' X    <- dat[, 1:9]
#' #' Y    <- dat$SalePrice
#' #' clas <- dat$SubclassMS
#' #' 
#' #' ## Model, treeSHAP
#' #' rf_fit <- default_rf(X, Y)
#' #' shap_df <- attr_df(rf_fit, X, noisy = FALSE)
#' #' 
#' #' ## cheem list, visualize
#' #' this_ls <- cheem_ls(X, Y, class = clas,
#' #'                     model = rf_fit,
#' #'                     attr_df = shap_df)
#' #' global_view(this_ls)
#' default_rf <- function(
#'     x, y, verbose = getOption("verbose"),
#'     hp_ntree = 125,
#'     hp_mtry = ifelse(is_discrete(y), sqrt(ncol(x)), ncol(x) / 3),
#'     hp_nodesize = max(ifelse(is_discrete(y), 1, 5), nrow(x) / 500)
#' ){
#'   if(verbose) tictoc::tic("default_rf_treeshap")
#'   .fit <- randomForest::randomForest(
#'     x, y, mtry = hp_mtry, nodesize = hp_nodesize, ntree = hp_ntree, ...)
#'   if(verbose) tictoc::toc()
#'   .fit
#' }
#' 
#' 
#' #' Modest random forest model (via randomForest)
#' #' 
#' #' A wrapper function for `randomForest::randomForest` with more modest 
#' #' hyperparameter defaults and arguments consistent with `cheem`. We encourage
#' #' you to bring your own model and local explanation or at least adjust the
#' #' hyperparameter (`hp_`) arguments to better fit your data.
#' #' 
#' #' @param x The explanatory variables of the model.
#' #' @param y The target variable of the model.
#' #' @param verbose Logical, if start time and run duration should be printed. 
#' #' Defaults to getOption("verbose").
#' #' @param hp_ntree Hyperparameter, the number of trees to grow.
#' #' @param hp_mtry Hyperparameter, the number variables randomly sampled at 
#' #' each split.
#' #' @param hp_nodesize Hyperparameter, the minimum size of terminal nodes. 
#' #' Setting this number larger causes smaller trees to be grown (and thus take less time).
#' #' @param ... Optionally, pass arguments into the `xgboost::xgboost()` params list() 
#' #'  randomForest::ranrandomForest() call.
#' #' @return A randomForest model.
#' #' @export
#' #' @family cheem preprocessing
#' #' @examples
#' #' library(cheem)
#' #' 
#' #' ## Regression setup:
#' #' dat  <- amesHousing2018_NorthAmes
#' #' X    <- dat[, 1:9]
#' #' Y    <- dat$SalePrice
#' #' clas <- dat$SubclassMS
#' #' 
#' #' ## Model, treeSHAP
#' #' rf_fit  <- default_xgb(X, Y)
#' #' shap_df <- stop("REPLACE ME")
#' #' 
#' #' ## cheem list, visualize
#' #' this_ls <- cheem_ls(X, Y, class = clas,
#' #'                     model = rf_fit,
#' #'                     attr_df = shap_df)
#' #' global_view(this_ls)
#' default_xgb <- function(
#'     x, y, verbose = getOption("verbose"),
#'     hp_objective = "reg:squarederror",
#'     hp_nrounds = 10,
#'     hp_learn_rt = .3,
#'     ...
#' ){
#'   if(verbose) tictoc::tic("default_xgb")
#'   .dtrain <- data.matrix(x) %>% xgb.DMatrix()
#'   .fit <- xgb.train(
#'     params = list(learning_rate = hp_learn_rt, objective = hp_objective, ...),
#'     data = .dtrain,
#'     nrounds = hp_nrounds)
#'   if(verbose) tictoc::toc()
#'   .fit
#' }

