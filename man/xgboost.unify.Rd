% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_treeshap_minimal_port.r
\name{xgboost.unify}
\alias{xgboost.unify}
\title{Unify xgboost model}
\source{
\strong{treeshap}, \url{https://github.com/ModelOriented/treeshap}
}
\usage{
xgboost.unify(xgb_model, data, recalculate = FALSE)
}
\arguments{
\item{xgb_model}{A xgboost model - object of class \code{xgb.Booster}}

\item{data}{Reference dataset. A \code{data.frame} or \code{matrix} with the same columns as in the training set of the model. Usually dataset used to train model.}

\item{recalculate}{logical indicating if covers should be recalculated according to the dataset given in data. Keep it \code{FALSE} if training data are used.}
}
\value{
a unified model representation - a \code{\link{model_unified.object}} object
}
\description{
Convert your xgboost model into a standardized representation.
The returned representation is easy to be interpreted by the user and ready to be used as an argument in \code{treeshap()} function.
}
\examples{
library(cheem)

## Regression setup:
dat  <- amesHousing2018_NorthAmes
X    <- dat[, 1:9]
Y    <- dat$SalePrice
clas <- dat$SubclassMS

## Fit a model:
xgb_model <- xgboost::xgboost(as.matrix(X), Y, nrounds = 25, verbose = 0,
                              params = list(objective = "reg:squarederror"))
unified_model <- xgboost.unify(xgb_model, X)

## Calculate treeSHAP:
shaps <- treeshap(unified_model, X[1:2, ])
str(shaps)
}
\seealso{
\code{\link{unify_tree_model}}, a wrapper function unifying these models.
\code{\link{lightgbm.unify}} for \code{\link[lightgbm:lightgbm]{LightGBM models}}
\code{\link{gbm.unify}} for \code{\link[gbm:gbm]{GBM models}}
\code{\link{ranger.unify}} for \code{\link[ranger:ranger]{ranger models}}
\code{\link{randomForest.unify}} for \code{\link[randomForest:randomForest]{randomForest models}}
}
\author{
Konrad Komisarczyk, Przemyslaw Biecek, et al.
}
