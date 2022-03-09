## Preface -----
#### Context: trying to submit cheem to CRAN
##
## Have a drat repository hosting current treeshap, but this isn't working with
## for rhub platfroms debian and windows. I Emailed Konrad and then Premek to 
## get their permission to minimally duplicate treeshap code to get around this.
##
## This file minimally duplicates code from treeshap to pass CRAN checks.
##
#### Nicholas Spyrison, 22 Feb 2022



## treeshap and not model specific -----

#' Calculate SHAP values of a tree ensemble model.
#'
#' Calculate SHAP values and optionally SHAP Interaction values.
#'
#'
#' @param unified_model Unified data.frame representation of the model created with a (model).unify function. A \code{\link{model_unified.object}} object.
#' @param x Observations to be explained. A \code{data.frame} or \code{matrix} object with the same columns as in the training set of the model. Keep in mind that objects different than \code{data.frame} or plain \code{matrix} will cause an error or unpredictable behavior.
#' @param interactions Whether to calculate SHAP interaction values. By default is \code{FALSE}. Basic SHAP values are always calculated.
#' @param verbose Whether to print progress bar to the console. Should be logical. Progress bar will not be displayed on Windows.
#'
#' @return A \code{\link{treeshap.object}} object. SHAP values can be accessed with \code{$shaps}. Interaction values can be accessed with \code{$interactions}.
#' @source __treeshap__, \url{https://github.com/ModelOriented/treeshap}
#' @author Konrad Komisarczyk, Przemyslaw Biecek, et al.
#'
#' @export
#' 
#' @importFrom Rcpp sourceCpp
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @useDynLib cheem, .registration = TRUE
#'
#' @seealso
#' \code{\link{unify_tree_model}}, a wrapper function unifying these models.
#' \code{\link{xgboost.unify}} for \code{XGBoost models}
#' \code{\link{lightgbm.unify}} for \code{LightGBM models}
#' \code{\link{gbm.unify}} for \code{GBM models}
# #' \code{\link{catboost.unify}} for \code{catboost models}
#' \code{\link{randomForest.unify}} for \code{randomForest models}
#' \code{\link{ranger.unify}} for \code{ranger models}
#'
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Fit a model:
#' rf_fit <- default_rf(X, Y)
#' unified_model <- unify_tree_model(rf_fit, X)
#'
#' ## Calculate SHAP explanation for X, a subset, or OOS observations. 
#' shaps <- treeshap(unified_model, head(X, 3))
#' str(shaps)
treeshap <- function(unified_model, x, interactions = FALSE, verbose = TRUE) {
  model <- unified_model$model
  
  # argument check
  if (!("matrix" %in% class(x) | "data.frame" %in% class(x))) {
    stop("x parameter has to be data.frame or matrix.")
  }
  
  if (!is.model_unified(unified_model)) {
    stop("unified_model parameter has to of class model_unified. Produce it using *.unify function.")
  }
  
  if (!attr(unified_model, "missing_support") & any(is.na(x))) {
    stop("Given model does not work with missing values. Dataset x should not contain missing values.")
  }
  
  if (!all(model$Feature %in% c(NA, colnames(x)))) {
    stop("Dataset x does not contain all features ocurring in the model.")
  }
  
  if (attr(unified_model, "model") == "LightGBM" & !is.data.frame(x)) {
    stop("For LightGBM models data.frame object is required as x parameter. Please convert.")
  }
  
  if ((!is.numeric(verbose) & !is.logical(verbose)) | is.null(verbose)) {
    warning("Incorrect verbose argument, setting verbose = FALSE (progress will not be printed).")
    verbose <- FALSE
  }
  verbose <- verbose[1] > 0 # so verbose = numeric will work too
  
  # adapting model representation to C++ and extracting from dataframe to vectors
  roots <- which(model$Node == 0) - 1
  yes <- model$Yes - 1
  no <- model$No - 1
  missing <- model$Missing - 1
  feature <- match(model$Feature, colnames(x)) - 1
  split <- model$Split
  decision_type <- unclass(model$Decision.type)
  is_leaf <- is.na(model$Feature)
  value <- model$Prediction
  cover <- model$Cover
  
  x2 <- as.data.frame(t(as.matrix(x))) # transposed to be able to pick a observation with [] operator in Rcpp
  is_na <- is.na(x2) # needed, because dataframe passed to cpp somehow replaces missing values with random values
  
  # calculating SHAP values
  if (interactions) {
    result <- treeshap_interactions_cpp(x2, is_na,
                                        roots, yes, no, missing, feature, split, decision_type, is_leaf, value, cover,
                                        verbose)
    shaps <- result$shaps
    interactions_array <- array(result$interactions,
                                dim = c(ncol(x), ncol(x), nrow(x)),
                                dimnames = list(colnames(x), colnames(x), rownames(x)))
  } else {
    shaps <- treeshap_cpp(x2, is_na,
                          roots, yes, no, missing, feature, split, decision_type, is_leaf, value, cover,
                          verbose)
    interactions_array <- NULL
  }
  if(all(is.nan(shaps))) stop("treeshap: shaps are all NaN, is Rcpp code building properly?")
  
  dimnames(shaps) <- list(rownames(x), colnames(x))
  treeshap_obj <- list(shaps = as.data.frame(shaps), interactions = interactions_array,
                       unified_model = unified_model, observations = x)
  class(treeshap_obj) <- "treeshap"
  return(treeshap_obj)
}





#' treeshap results
#'
#' \code{treeshap} object produced by \code{treeshap} function.
#'
#' @return List consisting of four elements:
#' \describe{
#'   \item{shaps}{A \code{data.frame} with M columns, X rows (M - number of features, X - number of explained observations). Every row corresponds to SHAP values for a observation. }
#'   \item{interactions}{An \code{array} with dimensions (M, M, X) (M - number of features, X - number of explained observations). Every \code{[, , i]} slice is a symmetric matrix - SHAP Interaction values for a observation. \code{[a, b, i]} element is SHAP Interaction value of features \code{a} and \code{b} for observation \code{i}. Is \code{NULL} if interactions where not calculated (parameter \code{interactions} set \code{FALSE}.) }
#'   \item{unified_model}{An object of type \code{\link{model_unified.object}}. Unified representation of a model for which SHAP values were calculated. It is used by some of the plotting functions.}
#'   \item{observations}{Explained dataset. \code{data.frame} or \code{matrix}. It is used by some of the plotting functions.}
#' }
#'
#'
#' @seealso
#' \code{\link{treeshap}},
#'
#'
#' @name treeshap.object
NULL


#' Prints treeshap objects
#'
#' @param x a treeshap object
#' @param ... other arguments
#' 
#' @return A data.frame of $shaps of the treeshap object. Also prints 
#' interactions if used.
#'
#' @export
#'
print.treeshap <- function(x, ...){
  ret <- x$shaps
  print(ret)
  if (!is.null(x$interactions)) {
    print(x$interactions)
  }
  ret
}



#' Set reference dataset
#'
#' Change a dataset used as reference for calculating SHAP values.
#' Reference dataset is initially set with \code{data} argument in unifying function.
#' Usually reference dataset is dataset used to train the model.
#' Important property of reference dataset is that SHAPs for each observation add up to its deviation from mean prediction of reference dataset.
#'
#'
#' @param unified_model Unified model representation of the model created with a (model).unify function. (\code{\link{model_unified.object}}).
#' @param x Reference dataset. A \code{data.frame} or \code{matrix} with the same columns as in the training set of the model.
#'
#' @return  \code{\link{model_unified.object}}. Unified representation of the model as created with a (model).unify function,
#' but with changed reference dataset (Cover column containing updated values).
#'
#' @export
#'
#' @seealso
#' \code{\link{unify_tree_model}}, a wrapper function unifying these models.
#' \code{\link{lightgbm.unify}} for \code{\link[lightgbm:lightgbm]{LightGBM models}}
#' \code{\link{gbm.unify}} for \code{\link[gbm:gbm]{GBM models}}
# #' \code{\link{catboost.unify}} for \code{\link[catboost:catboost.train]{Catboost models}}
#' \code{\link{xgboost.unify}} for \code{\link[xgboost:xgboost]{XGBoost models}}
#' \code{\link{ranger.unify}} for \code{\link[ranger:ranger]{ranger models}}
#' \code{\link{randomForest.unify}} for \code{\link[randomForest:randomForest]{randomForest models}}
#'
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Fit a model:
#' gbm_model <- gbm::gbm(
#'   formula = Y ~ .,
#'   data = data.frame(X, Y),
#'   distribution = "gaussian",
#'   n.trees = 50,
#'   interaction.depth = 4,
#'   n.cores = 1)
#' unified <- gbm.unify(gbm_model, X)
#' set_reference_dataset(unified, X[50:100, ])
set_reference_dataset <- function(unified_model, x) {
  model <- unified_model$model
  data <- x
  
  # argument check
  if (!("matrix" %in% class(x) | "data.frame" %in% class(x))) {
    stop("x parameter has to be data.frame or matrix.")
  }
  
  if (!("model_unified" %in% class(unified_model))) {
    stop("unified_model parameter has to of class model_unified. Produce it using *.unify function.")
  }
  
  if (!all(c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "Prediction") %in% colnames(model))) {
    stop("Given model dataframe is not a correct unified dataframe representation. Use (model).unify function.")
  }
  
  if (!attr(unified_model, "missing_support") && any(is.na(x))) {
    stop("Given model does not work with missing values. Dataset x should not contain missing values.")
  }
  
  if (!all(model$Feature %in% c(NA, colnames(x)))) {
    stop("Dataset does not contain all features ocurring in the model.")
  }
  
  # adapting model representation to C++ and extracting from dataframe to vectors
  roots <- which(model$Node == 0) - 1
  yes <- model$Yes - 1
  no <- model$No - 1
  missing <- model$Missing - 1
  is_leaf <- is.na(model$Feature)
  feature <- match(model$Feature, colnames(x)) - 1
  split <- model$Split
  decision_type <- unclass(model$Decision.type)
  #stopifnot(levels(decision_type) == c("<=", "<"))
  #stopifnot(all(decision_type %in% c(1, 2, NA)))
  
  x <- as.data.frame(t(as.matrix(x)))
  is_na <- is.na(x) # needed, because dataframe passed to cpp somehow replaces missing values with random values
  
  ## NS: Doesn't appear to be needed for cheem, and hitting a Rcpp error:
  ## Error in cheem:::new_covers() : object '_cheem_new_covers' not found
  model$Cover <- new_covers(x, is_na, roots, yes, no, missing, is_leaf, feature, split, decision_type)
  
  ret <- list(model = as.data.frame(model), data = as.data.frame(data))
  #attributes(ret) <- attributes(model_unified)
  class(ret) <- "model_unified"
  attr(ret, "missing_support") <- attr(unified_model, "missing_support")
  attr(ret, 'model') <- attr(unified_model, "model")
  
  return(ret)
}




#' Unified model representation
#'
#' \code{model_unified} object produced by \code{*.unify} function.
#'
#' @return List consisting of two elements:
#'
#'
#' \strong{model} - A \code{data.frame} representing model with following columns:
#'
#' \item{Tree}{0-indexed ID of a tree}
#' \item{Node}{0-indexed ID of a node in a tree. In a tree the root always has ID 0}
#' \item{Feature}{In case of an internal node - name of a feature to split on. Otherwise - NA}
#' \item{Decision.type}{A factor with two levels: "<" and "<=". In case of an internal node - predicate used for splitting observations. Otherwise - NA}
#' \item{Split}{For internal nodes threshold used for splitting observations. All observations that satisfy the predicate Decision.type(Split) ('< Split' / '<= Split') are proceeded to the node marked as 'Yes'. Otherwise to the 'No' node. For leaves - NA}
#' \item{Yes}{Index of a row containing a child Node. Thanks to explicit indicating the row it is much faster to move between nodes}
#' \item{No}{Index of a row containing a child Node}
#' \item{Missing}{Index of a row containing a child Node where are proceeded all observations with no value of the dividing feature}
#' \item{Prediction}{For leaves: Value of prediction in the leaf. For internal nodes: NA}
#' \item{Cover}{Number of observations seen by the internal node or collected by the leaf for the reference dataset}
#'
#' \strong{data} - Dataset used as a reference for calculating SHAP values. A dataset passed to the \code{*.unify} or \code{\link{set_reference_dataset}} function with \code{data} argument. A \code{data.frame}.
#'
#'
#' Object has two also attributes set:
#' \item{\code{model}}{A string. By what package the model was produced.}
#' \item{\code{missing_support}}{A boolean. Whether the model allows missing values to be present in explained dataset.}
#'
#'
#' @seealso
#' \code{\link{unify_tree_model}}, a wrapper function unifying these models.
#' \code{\link{lightgbm.unify}} for \code{\link[lightgbm:lightgbm]{LightGBM models}}
#' \code{\link{gbm.unify}} for \code{\link[gbm:gbm]{GBM models}}
# #' \code{\link{catboost.unify}} for \code{\link[catboost:catboost.train]{Catboost models}}
#' \code{\link{xgboost.unify}} for \code{\link[xgboost:xgboost]{XGBoost models}}
#' \code{\link{ranger.unify}} for \code{\link[ranger:ranger]{ranger models}}
#' \code{\link{randomForest.unify}} for \code{\link[randomForest:randomForest]{randomForest models}}
#'
#'
#' @name model_unified.object
#'
NULL

#' Prints model_unified objects
#'
#' @param x a model_unified object
#' @param ... other arguments
#'
#' @return Prints a data.frame of the $model of the treeshap unified model.
#'
#' @export
#'
print.model_unified <- function(x, ...){
  x$model
}

#' Check whether object is a valid model_unified object
#'
#' Does not check correctness of representation, only basic checks
#'
#' @param x an object to check
#'
#' @return boolean
#'
#' @export
#'
is.model_unified <- function(x) {
  # class checks
  ("model_unified" %in% class(x)) &
    is.data.frame(x$data) &
    is.data.frame(x$model) &
    # attributes check
    is.character(attr(x, "model")) &
    is.logical(attr(x, "missing_support")) &
    # colnames check
    all(c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "Prediction", "Cover") %in% colnames(x$model)) &
    # column types check
    is.numeric(x$model$Tree) &
    is.numeric(x$model$Node) &
    is.character(x$model$Feature) &
    is.factor(x$model$Decision.type) &
    all(levels(x$model$Decision.type) == c("<=", "<")) &
    all(unclass(x$model$Decision.type) %in% c(1, 2, NA)) &
    is.numeric(x$model$Split) &
    is.numeric(x$model$Yes) &
    is.numeric(x$model$No) &
    (!attr(x, "missing_support") | is.numeric(x$model$Missing)) &
    is.numeric(x$model$Prediction) &
    is.numeric(x$model$Cover)
}



# Model specific functions -----


## _catboost.unify ----
# ## I really don't trust catboost or any examples I find, always error or stall Rstudio...
# 
# #' Unify Catboost model
# #'
# #' Convert your Catboost model into a standardized representation.
# #' The returned representation is easy to be interpreted by the user and ready to be used as an argument in \code{treeshap()} function.
# #'
# #' @param catboost_model An object of \code{catboost.Model} class. At the moment, models built on data with categorical features
# #' are not supported - please encode them before training.
# #' @param data Reference dataset. A \code{data.frame} or \code{matrix} with the same columns as in the training set of the model. Usually dataset used to train model. Note that the same order of columns is crucial for unifier to work.
# #' @param recalculate logical indicating if covers should be recalculated according to the dataset given in data. Keep it \code{FALSE} if training data is used.
# #'
# #' @return a unified model representation - a \code{\link{model_unified.object}} object
# #' @source __treeshap__, \url{https://github.com/ModelOriented/treeshap}
# #' @author Konrad Komisarczyk, Przemyslaw Biecek, et al.
# #' 
# #' @export
# #'
# #' @seealso
# #' \code{\link{unify_tree_model}}, a wrapper function unifying these models.
# #'
# #' \code{\link{lightgbm.unify}} for \code{\link[lightgbm:lightgbm]{LightGBM models}}
# #'
# #' \code{\link{gbm.unify}} for \code{\link[gbm:gbm]{GBM models}}
# #'
# #' \code{\link{xgboost.unify}} for \code{\link[xgboost:xgboost]{XGBoost models}}
# #'
# #' \code{\link{ranger.unify}} for \code{\link[ranger:ranger]{ranger models}}
# #'
# #' \code{\link{randomForest.unify}} for \code{\link[randomForest:randomForest]{randomForest models}}
# #'
# #' @examples
# #' library(cheem)
# #' 
# #' ## Regression setup:
# #' dat  <- amesHousing2018_NorthAmes
# #' X    <- dat[, 1:9]
# #' Y    <- dat$SalePrice
# #' 
# #' dt.pool <- catboost::catboost.load_pool(data = X, label = Y)
# #' cat_model <- catboost::catboost.train(
# #'   dt.pool,
# #'   params = list(loss_function = 'RMSE',
# #'                 iterations = 100,
# #'                 logging_level = 'Silent'))
# #' um <- catboost.unify(cat_model, X)
# #' shaps <- treeshap(um, dat[1:2, ])
# catboost.unify <- function(catboost_model, data, recalculate = FALSE) {
#   if (class(catboost_model) != "catboost.Model") {
#     stop('Object catboost_model is not of type "catboost.Model"')
#   }
#   
#   if (!any(c("data.frame", "matrix") %in% class(data))) {
#     stop("Argument data has to be data.frame or matrix.")
#   }
#   
#   if (!requireNamespace("catboost", quietly = TRUE)) {
#     stop("Package \"catboost\" needed for this function to work. Please install it.",
#          call. = FALSE)
#   }
#   
#   if (!requireNamespace("jsonlite", quietly = TRUE)) {
#     stop("Package \"jsonlite\" needed for this function to work. Please install it.",
#          call. = FALSE)
#   }
#   
#   path_to_save <- tempfile("catboost_model", fileext = ".json")
#   catboost::catboost.save_model(catboost_model, path_to_save, 'json')
#   json_data <- jsonlite::read_json(path_to_save)
#   
#   if (!is.null(json_data$features_info$categorical_features)) {
#     stop('catboost.unify() function currently does not support models using categorical features.')
#   }
#   
#   single_trees <- lapply(seq_along(json_data$oblivious_trees),
#                          function(i) one_tree_transform(json_data$oblivious_trees[[i]], (i - 1)))
#   united <- data.table::rbindlist(single_trees)
#   
#   stopifnot(is.numeric(united$float_feature_index)) # to delete in the future
#   
#   #stopifnot(all(sapply(json_data$features_info$float_features, function(x) x$feature_index) == 0:(length(json_data$features_info$float_features) - 1))) #assuming features are ordered
#   
#   # How are missing values treated?:
#   for_missing <- sapply(json_data$features_info$float_features,
#                         function(x) x$nan_value_treatment)[united$float_feature_index + 1]
#   united$Missing <- for_missing
#   united[!is.na(Missing) & Missing == 'AsIs', Missing := NA]
#   united[!is.na(Missing) & Missing == 'AsFalse', Missing := Yes]
#   united[!is.na(Missing) & Missing == 'AsTrue', Missing := No]
#   united[, Missing := as.integer(Missing)]
#   
#   feature_names <- attr(catboost_model$feature_importances, "dimnames")[[1]]
#   stopifnot(all(feature_names == colnames(data))) # this line can be deleted if we are sure feature names from feature_importances is correct
#   united$float_feature_index <- feature_names[united$float_feature_index + 1]
#   
#   colnames(united) <- c('Split', 'Feature', 'Prediction', 'Cover', 'Yes', 'No', 'Node', 'Tree', 'Missing')
#   
#   united$Decision.type <- factor(x = rep("<=", times = nrow(united)), levels = c("<=", "<"))
#   united$Decision.type[is.na(united$Feature)] <- NA
#   
#   ID <- paste0(united$Node, "-", united$Tree)
#   united$Yes <- match(paste0(united$Yes, "-", united$Tree), ID)
#   united$No <- match(paste0(united$No, "-", united$Tree), ID)
#   united$Missing <- match(paste0(united$Missing, "-", united$Tree), ID)
#   
#   united <- united[, c('Tree', 'Node', 'Feature', 'Decision.type', 'Split', 'Yes', 'No', 'Missing', 'Prediction', 'Cover')]
#   
#   # for catboost the model prediction results are calculated as [sum(leaf_values * scale + bias)]
#   # (https://catboost.ai/docs/concepts/python-reference_catboostregressor_set_scale_and_bias.html)
#   # treeSHAP assumes the prediction is sum of leaf values
#   # so here we adjust it
#   scale <- json_data$scale_and_bias[[1]]
#   bias <- json_data$scale_and_bias[[2]]
#   ntrees <- sum(united$Node == 0)
#   united[is.na(united$Feature), ]$Prediction <- united[is.na(united$Feature), ]$Prediction * scale + bias[[1]]/ ntrees
#   
#   ret <- list(model = as.data.frame(united), data = as.data.frame(data))
#   class(ret) <- "model_unified"
#   attr(ret, "missing_support") <- TRUE
#   attr(ret, 'model') <- 'catboost'
#   
#   if (recalculate) {
#     ret <- set_reference_dataset(ret, as.data.frame(data))
#   }
#   
#   return(ret)
# }
# 
# 
# one_tree_transform <- function(oblivious_tree, tree_id) {
#   #stopifnot(!is.null(oblivious_tree$splits))
#   #stopifnot(!is.null(oblivious_tree$leaf_values))
#   #stopifnot(!is.null(oblivious_tree$leaf_weights))
#   frame <- data.table::rbindlist(lapply(oblivious_tree$splits, data.table::as.data.table))
#   if (!all(frame$split_type == 'FloatFeature')) {
#     stop('catboost.unify() function currently does not support models using categorical features. Please encode them before training.')
#   }
#   frame <- frame[nrow(frame):1, ]
#   frame <- frame[, c('border', 'float_feature_index')]
#   
#   #repeat rows representing node at the kth level 2^(k-1) times:
#   frame2 <- frame[rep(seq_len(nrow(frame)), times = 2**(seq_len(nrow(frame)) - 1)), ]
#   
#   #Add columns Score and Cover:
#   frame2[, c('Score', 'Cover')] <- NA
#   frame2$Yes <- as.integer(seq(1, nrow(frame2) * 2, 2))
#   frame2$No <- as.integer(seq(2, nrow(frame2) * 2, 2))
#   leaves_values <- unlist(oblivious_tree$leaf_values)
#   leaves_weights <- as.numeric(unlist(oblivious_tree$leaf_weights))
#   
#   #Create the part of data frame for leaves
#   leaves <- data.table::as.data.table(list(border = NA,
#                                            float_feature_index = NA,
#                                            Score = leaves_values,
#                                            Cover = leaves_weights,
#                                            Yes = NA,
#                                            No = NA))
#   tree_levels <- log2(length(leaves$Cover))
#   #stopifnot(tree_levels == floor(tree_levels))
#   
#   internal_covers <- numeric()
#   for(i in rev(seq(tree_levels))){
#     internal_covers <- c(internal_covers, sapply(split(leaves$Cover, ceiling(seq_along(leaves$Cover) / (2**i))), sum))
#   }
#   names(internal_covers) <- NULL
#   frame2$Cover <- internal_covers
#   frame3 <- rbind(frame2, as.data.frame(leaves))
#   #rownames(frame3) <- seq_len(nrow(frame3))
#   frame3$Node <- as.integer(seq_len(nrow(frame3)) - 1)
#   frame3$Tree <- as.integer(tree_id)
#   return(frame3)
# }



#' Unify GBM model
#'
#' Convert your GBM model into a standardized representation.
#' The returned representation is easy to be interpreted by the user and ready to be used as an argument in \code{treeshap()} function.
#'
#' @param gbm_model An object of \code{gbm} class. At the moment, models built on data with categorical features
#' are not supported - please encode them before training.
#' @param data Reference dataset. A \code{data.frame} or \code{matrix} with the same columns as in the training set of the model. Usually dataset used to train model.
#'
#' @return a unified model representation - a \code{\link{model_unified.object}} object
#' @source __treeshap__, \url{https://github.com/ModelOriented/treeshap}
#' @author Konrad Komisarczyk, Przemyslaw Biecek, et al.
#' 
#' @export
#'
#' @seealso
#' \code{\link{unify_tree_model}}, a wrapper function unifying these models.
#' \code{\link{lightgbm.unify}} for \code{\link[lightgbm:lightgbm]{LightGBM models}}
# #' \code{\link{catboost.unify}} for  \code{\link[catboost:catboost.train]{Catboost models}}
#' \code{\link{xgboost.unify}} for \code{\link[xgboost:xgboost]{XGBoost models}}
#' \code{\link{ranger.unify}} for \code{\link[ranger:ranger]{ranger models}}
#' \code{\link{randomForest.unify}} for \code{\link[randomForest:randomForest]{randomForest models}}
#'
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Fit a model:
#' gbm_model <- gbm::gbm(
#'   formula = Y ~ .,
#'   data = data.frame(X, Y),
#'   distribution = "gaussian",
#'   n.trees = 50,
#'   interaction.depth = 4,
#'   n.cores = 1)
#' unified_model <- gbm.unify(gbm_model, X)
#' 
#' ## Calculate treeSHAP: 
#' shaps <- treeshap(unified_model, X[1:2,])
#' str(shaps)
gbm.unify <- function(gbm_model, data) {
  if(class(gbm_model) != 'gbm') {
    stop('Object gbm_model was not of class "gbm"')
  }
  if(any(gbm_model$var.type > 0)) {
    stop('Models built on data with categorical features are not supported - please encode them before training.')
  }
  x <- lapply(gbm_model$trees, data.table::as.data.table)
  times_vec <- sapply(x, nrow)
  y <- data.table::rbindlist(x)
  data.table::setnames(y, c("Feature", "Split", "Yes",
                            "No", "Missing", "ErrorReduction", "Cover",
                            "Prediction"))
  y[["Tree"]] <- rep(0:(length(gbm_model$trees) - 1), times = times_vec)
  y[["Node"]] <- unlist(lapply(times_vec, function(x) 0:(x - 1)))
  y <- y[, Feature := as.character(Feature)]
  y[y$Feature < 0, "Feature"] <- NA
  y[!is.na(y$Feature), "Feature"] <- attr(gbm_model$Terms, "term.labels")[as.integer(y[["Feature"]][!is.na(y$Feature)]) + 1]
  y[is.na(y$Feature), "ErrorReduction"] <- y[is.na(y$Feature), "Split"]
  y[is.na(y$Feature), "Split"] <- NA
  y[y$Yes < 0, "Yes"] <- NA
  y[y$No < 0, "No"] <- NA
  y[y$Missing < 0, "Missing"] <- NA
  y$Decision.type <- factor(x = rep("<=", times = nrow(y)), levels = c("<=", "<"))
  y[is.na(Feature), Decision.type := NA]
  y <- y[, c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "ErrorReduction", "Cover")]
  colnames(y) <- c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "Prediction", "Cover")
  
  ID <- paste0(y$Node, "-", y$Tree)
  y$Yes <- match(paste0(y$Yes, "-", y$Tree), ID)
  y$No <- match(paste0(y$No, "-", y$Tree), ID)
  y$Missing <- match(paste0(y$Missing, "-", y$Tree), ID)
  
  # Here we lose "Quality" information
  y[!is.na(Feature), Prediction := NA]
  
  # GBM calculates prediction as [initF + sum of predictions of trees]
  # treeSHAP assumes prediction are calculated as [sum of predictions of trees]
  # so here we adjust it
  ntrees <- sum(y$Node == 0)
  y[is.na(Feature), Prediction := Prediction + gbm_model$initF / ntrees]
  
  ret <- list(model = as.data.frame(y), data = as.data.frame(data))
  class(ret) <- "model_unified"
  attr(ret, "missing_support") <- TRUE
  attr(ret, "model") <- "gbm"
  
  # Original covers in gbm_model are not correct
  ret <- set_reference_dataset(ret, as.data.frame(data))
  
  return(ret)
}



# should be preceded with lgb.model.dt.tree
#' Unify LightGBM model
#'
#' Convert your LightGBM model into a standardized representation.
#' The returned representation is easy to be interpreted by the user and ready to be used as an argument in \code{treeshap()} function.
#'
#' @param lgb_model A lightgbm model - object of class \code{lgb.Booster}
#' @param data Reference dataset. A \code{data.frame} or \code{matrix} with the same columns as in the training set of the model. Usually dataset used to train model.
#' @param recalculate logical indicating if covers should be recalculated according to the dataset given in data. Keep it \code{FALSE} if training data are used.
#'
#' @return a unified model representation - a \code{\link{model_unified.object}} object
#' @source __treeshap__, \url{https://github.com/ModelOriented/treeshap}
#' @author Konrad Komisarczyk, Przemyslaw Biecek, et al.
#' 
#' @export
#'
#' @import data.table
#'
#' @seealso
#' \code{\link{unify_tree_model}}, a wrapper function unifying these models.
#' \code{\link{gbm.unify}} for \code{\link[gbm:gbm]{GBM models}}
# #' \code{\link{catboost.unify}} for  \code{\link[catboost:catboost.train]{Catboost models}}
#' \code{\link{xgboost.unify}} for \code{\link[xgboost:xgboost]{XGBoost models}}
#' \code{\link{ranger.unify}} for \code{\link[ranger:ranger]{ranger models}}
#' \code{\link{randomForest.unify}} for \code{\link[randomForest:randomForest]{randomForest models}}
#'
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Fit a model:
#' param_lgbm <- list(num_leaves = 50, objective = "regression")
#' lightgbm_model <- lightgbm::lightgbm(
#'   as.matrix(X), Y, params = param_lgbm, 
#'   nrounds = 2, verbose = 0)
#' unified_model <- lightgbm.unify(lightgbm_model, X)
#' ## Delete model file if it exists
#' if(file.exists("lightgbm.model"))
#'   file.remove("lightgbm.model")
#' 
#' ## Calculate treeSHAP:
#' shaps <- treeshap(unified_model, X[1:2, ])
#' str(shaps)
lightgbm.unify <- function(lgb_model, data, recalculate = FALSE) {
  if (!requireNamespace("lightgbm", quietly = TRUE)) {
    stop("Package \"lightgbm\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  df <- lightgbm::lgb.model.dt.tree(lgb_model)
  stopifnot(c("split_index", "split_feature", "node_parent", "leaf_index", "leaf_parent", "internal_value",
              "internal_count", "leaf_value", "leaf_count", "decision_type") %in% colnames(df))
  df <- data.table::as.data.table(df)
  #convert node_parent and leaf_parent into one parent column
  df[is.na(df$node_parent), "node_parent"] <- df[is.na(df$node_parent), "leaf_parent"]
  #convert values into one column...
  df[is.na(df$internal_value), "internal_value"] <- df[!is.na(df$leaf_value), "leaf_value"]
  #...and counts
  df[is.na(df$internal_count), "internal_count"] <- df[!is.na(df$leaf_count), "leaf_count"]
  df[["internal_count"]] <- as.numeric(df[["internal_count"]])
  #convert split_index and leaf_index into one column:
  max_split_index <- df[, max(split_index, na.rm = TRUE), tree_index]
  rep_max_split <- rep(max_split_index$V1, times = as.numeric(table(df$tree_index)))
  new_leaf_index <- rep_max_split + df[, "leaf_index"] + 1
  df[is.na(df$split_index), "split_index"] <- new_leaf_index[!is.na(new_leaf_index[["leaf_index"]]), 'leaf_index']
  df[is.na(df$split_gain), "split_gain"] <- df[is.na(df$split_gain), "leaf_value"]
  # On the basis of column 'Parent', create columns with childs: 'Yes', 'No' and 'Missing' like in the xgboost df:
  ret.first <- function(x) x[1]
  ret.second <- function(x) x[2]
  tmp <- data.table::merge.data.table(df[, .(node_parent, tree_index, split_index)], df[, .(tree_index, split_index, default_left, decision_type)],
                                      by.x = c("tree_index", "node_parent"), by.y = c("tree_index", "split_index"))
  y_n_m <- unique(tmp[, .(Yes = ifelse(decision_type %in% c("<=", "<"), ret.first(split_index),
                                       ifelse(decision_type %in% c(">=", ">"), ret.second(split_index), stop("Unknown decision_type"))),
                          No = ifelse(decision_type %in% c(">=", ">"), ret.first(split_index),
                                      ifelse(decision_type %in% c("<=", "<"), ret.second(split_index), stop("Unknown decision_type"))),
                          Missing = ifelse(default_left, ret.first(split_index),ret.second(split_index)),
                          decision_type = decision_type),
                      .(tree_index, node_parent)])
  df <- data.table::merge.data.table(df[, c("tree_index", "depth", "split_index", "split_feature", "node_parent", "split_gain",
                                            "threshold", "internal_value", "internal_count")],
                                     y_n_m, by.x = c("tree_index", "split_index"),
                                     by.y = c("tree_index", "node_parent"), all.x = TRUE)
  df[decision_type == ">=", decision_type := "<"]
  df[decision_type == ">", decision_type := "<="]
  df$Decision.type <- factor(x = df$decision_type, levels = c("<=", "<"))
  df[is.na(split_index), Decision.type := NA]
  df <- df[, c("tree_index", "split_index", "split_feature", "Decision.type", "threshold", "Yes", "No", "Missing", "split_gain", "internal_count")]
  colnames(df) <- c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "Prediction", "Cover")
  attr(df, "sorted") <- NULL
  
  ID <- paste0(df$Node, "-", df$Tree)
  df$Yes <- match(paste0(df$Yes, "-", df$Tree), ID)
  df$No <- match(paste0(df$No, "-", df$Tree), ID)
  df$Missing <- match(paste0(df$Missing, "-", df$Tree), ID)
  
  # Here we lose "Quality" information
  df$Prediction[!is.na(df$Feature)] <- NA
  
  ret <- list(model = as.data.frame(df), data = as.data.frame(data))
  class(ret) <- "model_unified"
  attr(ret, "missing_support") <- TRUE
  attr(ret, "model") <- "LightGBM"
  
  if (recalculate) {
    ret <- set_reference_dataset(ret, as.data.frame(data))
  }
  
  return(ret)
}




#' Unify randomForest model
#'
#' Convert your randomForest model into a standardized representation.
#' The returned representation is easy to be interpreted by the user and ready to be used as an argument in \code{treeshap()} function.
#'
#' @param rf_model An object of \code{randomForest} class. At the moment, models built on data with categorical features
#' are not supported - please encode them before training.
#' @param data Reference dataset. A \code{data.frame} or \code{matrix} with the same columns as in the training set of the model. Usually dataset used to train model.
#'
#' @return a unified model representation - a \code{\link{model_unified.object}} object
#'
#' @import data.table
#' @source __treeshap__, \url{https://github.com/ModelOriented/treeshap}
#' @author Konrad Komisarczyk, Przemyslaw Biecek, et al.
#'
#' @export
#'
#' @seealso
#' \code{\link{unify_tree_model}}, a wrapper function unifying these models.
#' \code{\link{lightgbm.unify}} for \code{\link[lightgbm:lightgbm]{LightGBM models}}
#' \code{\link{gbm.unify}} for \code{\link[gbm:gbm]{GBM models}}
# #' \code{\link{catboost.unify}} for  \code{\link[catboost:catboost.train]{Catboost models}}
#' \code{\link{xgboost.unify}} for \code{\link[xgboost:xgboost]{XGBoost models}}
#' \code{\link{ranger.unify}} for \code{\link[ranger:ranger]{ranger models}}
#'
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Fit a model:
#' rf_model <- default_rf(X, Y)
#' unified_model <- randomForest.unify(rf_model, X)
#' 
#' ## Calculate treeSHAP:
#' shaps <- treeshap(unified_model, X[1:2, ])
#' str(shaps)
randomForest.unify <- function(rf_model, data) {
  if(!'randomForest' %in% class(rf_model)){stop('Object rf_model was not of class "randomForest"')}
  if(any(attr(rf_model$terms, "dataClasses") != "numeric")) {
    stop('Models built on data with categorical features are not supported - please encode them before training.')
  }
  n <- rf_model$ntree
  ret <- data.table()
  x <- lapply(1:n, function(tree){
    tree_data <- as.data.table(randomForest::getTree(rf_model, k = tree, labelVar = TRUE))
    tree_data[, c("left daughter", "right daughter", "split var", "split point", "prediction")]
  })
  times_vec <- sapply(x, nrow)
  y <- rbindlist(x)
  y[, Tree := rep(0:(n - 1), times = times_vec)]
  y[, Node := unlist(lapply(times_vec, function(x) 0:(x - 1)))]
  setnames(y, c("Yes", "No", "Feature", "Split",  "Prediction", "Tree", "Node"))
  y[, Feature := as.character(Feature)]
  y[, Yes := Yes - 1]
  y[, No := No - 1]
  y[y$Yes < 0, "Yes"] <- NA
  y[y$No < 0, "No"] <- NA
  y[, Missing := NA]
  y[, Missing := as.integer(Missing)] # seems not, but needed
  
  ID <- paste0(y$Node, "-", y$Tree)
  y$Yes <- match(paste0(y$Yes, "-", y$Tree), ID)
  y$No <- match(paste0(y$No, "-", y$Tree), ID)
  
  y$Cover <- 0
  
  y$Decision.type <- factor(x = rep("<=", times = nrow(y)), levels = c("<=", "<"))
  y[is.na(Feature), Decision.type := NA]
  
  # Here we lose "Quality" information
  y[!is.na(Feature), Prediction := NA]
  
  # treeSHAP assumes, that [prediction = sum of predictions of the trees]
  # in random forest [prediction = mean of predictions of the trees]
  # so here we correct it by adjusting leaf prediction values
  y[is.na(Feature), Prediction := Prediction / n]
  
  
  setcolorder(y, c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "Prediction", "Cover"))
  
  ret <- list(model = as.data.frame(y), data = as.data.frame(data))
  class(ret) <- "model_unified"
  attr(ret, "missing_support") <- FALSE
  attr(ret, "model") <- "randomForest"
  return(set_reference_dataset(ret, as.data.frame(data)))
}



#' Unify ranger model
#'
#' Convert your ranger model into a standardized representation.
#' The returned representation is easy to be interpreted by the user and ready to be used as an argument in \code{treeshap()} function.
#'
#' @param rng_model An object of \code{ranger} class. At the moment, models built on data with categorical features
#' are not supported - please encode them before training.
#' @param data Reference dataset. A \code{data.frame} or \code{matrix} with the same columns as in the training set of the model. Usually dataset used to train model.
#'
#' @return a unified model representation - a \code{\link{model_unified.object}} object
#' @source __treeshap__, \url{https://github.com/ModelOriented/treeshap}
#' @author Konrad Komisarczyk, Przemyslaw Biecek, et al.
#'
#' @import data.table
#'
#' @export
#'
#' @seealso
#' \code{\link{unify_tree_model}}, a wrapper function unifying these models.
#' \code{\link{lightgbm.unify}} for \code{\link[lightgbm:lightgbm]{LightGBM models}}
#' \code{\link{gbm.unify}} for \code{\link[gbm:gbm]{GBM models}}
# #' \code{\link{catboost.unify}} for \code{\link[catboost:catboost.train]{Catboost models}}
#' \code{\link{xgboost.unify}} for \code{\link[xgboost:xgboost]{XGBoost models}}
#' \code{\link{randomForest.unify}} for \code{\link[randomForest:randomForest]{randomForest models}}
#'
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Fit a model:
#' ranger_model  <- ranger::ranger(Y ~ ., data.frame(X, Y), num.trees = 25)
#' unified_model <- ranger.unify(ranger_model, X)
#' 
#' ## Calculate treeSHAP:
#' shaps <- treeshap(unified_model, X[1:2, ])
#' str(shaps)
ranger.unify <- function(rng_model, data) {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("Package \"ranger\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!'ranger' %in% class(rng_model)) {
    stop('Object rng_model was not of class "ranger"')
  }
  n <- rng_model$num.trees
  x <- lapply(1:n, function(tree) {
    tree_data <- as.data.table(ranger::treeInfo(rng_model, tree = tree))
    tree_data[, c("nodeID",  "leftChild", "rightChild", "splitvarName", "splitval", "prediction")]
  })
  times_vec <- sapply(x, nrow)
  y <- rbindlist(x)
  y[, Tree := rep(0:(n - 1), times = times_vec)]
  setnames(y, c("Node", "Yes", "No", "Feature", "Split",  "Prediction", "Tree"))
  y[, Feature := as.character(Feature)]
  y[y$Yes < 0, "Yes"] <- NA
  y[y$No < 0, "No"] <- NA
  y[, Missing := NA]
  y$Cover <- 0
  y$Decision.type <- factor(x = rep("<=", times = nrow(y)), levels = c("<=", "<"))
  y[is.na(Feature), Decision.type := NA]
  
  ID <- paste0(y$Node, "-", y$Tree)
  y$Yes <- match(paste0(y$Yes, "-", y$Tree), ID)
  y$No <- match(paste0(y$No, "-", y$Tree), ID)
  
  # Here we lose "Quality" information
  y[!is.na(Feature), Prediction := NA]
  
  # treeSHAP assumes, that [prediction = sum of predictions of the trees]
  # in random forest [prediction = mean of predictions of the trees]
  # so here we correct it by adjusting leaf prediction values
  y[is.na(Feature), Prediction := Prediction / n]
  
  
  setcolorder(y, c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "Prediction", "Cover"))
  
  ret <- list(model = as.data.frame(y), data = as.data.frame(data))
  class(ret) <- "model_unified"
  attr(ret, "missing_support") <- FALSE
  attr(ret, "model") <- "ranger"
  return(set_reference_dataset(ret, as.data.frame(data)))
}



#' Unify xgboost model
#'
#' Convert your xgboost model into a standardized representation.
#' The returned representation is easy to be interpreted by the user and ready to be used as an argument in \code{treeshap()} function.
#'
#' @param xgb_model A xgboost model - object of class \code{xgb.Booster}
#' @param data Reference dataset. A \code{data.frame} or \code{matrix} with the same columns as in the training set of the model. Usually dataset used to train model.
#' @param recalculate logical indicating if covers should be recalculated according to the dataset given in data. Keep it \code{FALSE} if training data are used.
#'
#' @return a unified model representation - a \code{\link{model_unified.object}} object
#' @source __treeshap__, \url{https://github.com/ModelOriented/treeshap}
#' @author Konrad Komisarczyk, Przemyslaw Biecek, et al.
#'
#' @export
#'
#' @seealso
#' \code{\link{unify_tree_model}}, a wrapper function unifying these models.
#' \code{\link{lightgbm.unify}} for \code{\link[lightgbm:lightgbm]{LightGBM models}}
#' \code{\link{gbm.unify}} for \code{\link[gbm:gbm]{GBM models}}
# #' \code{\link{catboost.unify}} for  \code{\link[catboost:catboost.train]{Catboost models}}
#' \code{\link{ranger.unify}} for \code{\link[ranger:ranger]{ranger models}}
#' \code{\link{randomForest.unify}} for \code{\link[randomForest:randomForest]{randomForest models}}
#'
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Fit a model:
#' xgb_model <- xgboost::xgboost(as.matrix(X), Y, nrounds = 25, verbose = 0,
#'                               params = list(objective = "reg:squarederror"))
#' unified_model <- xgboost.unify(xgb_model, X)
#' 
#' ## Calculate treeSHAP:
#' shaps <- treeshap(unified_model, X[1:2, ])
#' str(shaps)
xgboost.unify <- function(xgb_model, data, recalculate = FALSE) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package \"xgboost\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  xgbtree <- xgboost::xgb.model.dt.tree(model = xgb_model)
  stopifnot(c("Tree", "Node", "ID", "Feature", "Split", "Yes", "No", "Missing", "Quality", "Cover") %in% colnames(xgbtree))
  xgbtree$Yes <- match(xgbtree$Yes, xgbtree$ID)
  xgbtree$No  <- match(xgbtree$No, xgbtree$ID)
  xgbtree$Missing <- match(xgbtree$Missing, xgbtree$ID)
  xgbtree[is.na(xgbtree$Split), 'Feature'] <- NA
  xgbtree$Decision.type <- factor(x = rep("<=", times = nrow(xgbtree)), levels = c("<=", "<"))
  xgbtree$Decision.type[is.na(xgbtree$Feature)] <- NA
  xgbtree <- xgbtree[, c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "Quality", "Cover")]
  colnames(xgbtree) <- c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "Prediction", "Cover")
  
  # Here we lose "Quality" information
  xgbtree$Prediction[!is.na(xgbtree$Feature)] <- NA
  
  ret <- list(model = as.data.frame(xgbtree), data = as.data.frame(data))
  class(ret) <- "model_unified"
  attr(ret, "missing_support") <- TRUE
  attr(ret, "model") <- "xgboost"
  
  if (recalculate) {
    ret <- set_reference_dataset(ret, as.data.frame(data))
  }
  
  return(ret)
}
