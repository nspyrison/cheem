% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1_cheem_lists.r
\name{is_randomForest}
\alias{is_randomForest}
\alias{is_ranger}
\alias{is_gbm}
\alias{is_xgboost}
\alias{is_lightgbm}
\title{Check model type}
\usage{
is_randomForest(model)

is_ranger(model)

is_gbm(model)

is_xgboost(model)

is_lightgbm(model)
}
\arguments{
\item{model}{A model object to check the class/origin.}
}
\value{
A logical, whether or not the model is of a certain class.
}
\description{
Check whether or not the model is a
certain model. Checks if a model is made with:
\code{\link[randomForest:randomForest]{randomForest::randomForest}},
\code{\link[ranger:ranger]{ranger::ranger}},
\code{\link[gbm:gbm]{gbm::gbm}},
\code{\link[xgboost:xgb.train]{xgboost::xgb.train}},
\code{\link[lightgbm:lightgbm]{lightgbm::lightgbm}}.
}
\examples{
library(cheem)

## Regression setup:
dat  <- amesHousing2018_NorthAmes
X    <- dat[, 1:9]
Y    <- dat$SalePrice
clas <- dat$SubclassMS


# Treeshap handles various tree-based models:

## randomForest model
fit <- randomForest::randomForest(X, Y, ntree = 25)
is_randomForest(fit)

## gbm model
if(require(gbm, quietly = TRUE)){
  fit <- gbm::gbm(Y ~ ., "gaussian", data.frame(X, Y), n.trees = 25)
  is_gbm(fit)
}

## lightgbm
if(require(lightgbm, quietly = TRUE)){
  param_lgbm <- list(num_leaves = 25, objective = "regression")
  fit <- lightgbm::lightgbm(
    as.matrix(X), Y, params = param_lgbm, 
    nrounds = 2, verbose = 0)
  ## Delete model file if it exists
  if(file.exists("lightgbm.model"))
    file.remove("lightgbm.model")
  is_lightgbm(fit)
}

## ranger model
if(require(ranger, quietly = TRUE)){
  fit <- ranger::ranger(Y ~ ., data.frame(X, Y), num.trees = 25)
  is_ranger(fit)
}

## xgboost
if(require(xgboost, quietly = TRUE)){
  fit <- xgboost::xgboost(as.matrix(X), Y, nrounds = 25, verbose = 0,
                          params = list(objective = "reg:squarederror"))
  is_xgboost(fit)
}


# Continue cheem workflow with tree-based models:

## treeSHAP, cheem list, visualize:
shap_df <- attr_df_treeshap(fit, X, verbose = TRUE, noisy = FALSE)
this_ls <- cheem_ls(X, Y, class = clas,
                    model = fit,
                    attr_df = shap_df)
global_view(this_ls)
}
\seealso{
Other cheem unify: 
\code{\link{unify_tree_model}()}
}
\concept{cheem unify}
