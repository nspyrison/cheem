% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1_cheem_lists.r
\name{unify_tree_model}
\alias{unify_tree_model}
\alias{unify_predict}
\title{Unify various models/predictions to a standard format}
\usage{
unify_tree_model(model, x)

unify_predict(model, x)
}
\arguments{
\item{model}{A tree based model supported by \code{treeshap}:
a model from \code{randomForest::randomForest}, \code{ranger::ranger}, \code{gbm::gbm},
\code{xgboost::xgb.train}, or \code{lightgbm::lightgbm}.}

\item{x}{The explanatory data (without response) to extract the local
attributions from.}
}
\value{
A vector of predicted values
}
\description{
Unifies models/prediction functions for supported by tree-based models
into a standard format.
}
\examples{
library(cheem)

## Regression setup:
dat  <- amesHousing2018_NorthAmes
X    <- dat[, 1:9]
Y    <- dat$SalePrice
clas <- dat$SubclassMS

## Model, unified prediction from any model (not unified)
rf_fit <- default_rf(X, Y)
unify_predict(rf_fit, X)

## Applies the correct treeshap::*.unify for the model type.
unified_model <- unify_tree_model(rf_fit, X)
str(unified_model)
}
\seealso{
Other cheem unify: 
\code{\link{is_randomForest}()}
}
\concept{cheem unify}
