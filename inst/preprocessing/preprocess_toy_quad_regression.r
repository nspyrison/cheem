## Setup ------
{
  set.seed(20211105)
  X <- data.frame(x1 = runif(200, 0, 5),
                  x2 = runif(200, 0, 5),
                  x3 = runif(200, 0, 5),
                  x4 = runif(200, 0, 5),
                  x5 = runif(200, 0, 5))
  Y <- X$x1 + X$x2 + (X$x1 * X$x2) + .1 * X$x3 + .1 * X$x4 + .1 * X$x5 + rnorm(200)
  clas <- NULL
}

rf_fit <- randomForest::randomForest(
  X, Y, ntree = 125,
  mtry = ifelse(is_discrete(Y), sqrt(ncol(X)), ncol(X) / 3),
  nodesize = max(ifelse(is_discrete(Y), 1, 5), nrow(X) / 500))
rf_pred <- predict(rf_fit, X)
rf_shap <- treeshap::treeshap(
  treeshap::randomForest.unify(rf_fit, X), X, FALSE, FALSE)
rf_shap <- rf_shap$shaps
chm <- cheem_ls(X, Y, rf_shap, rf_pred, clas,
                label = "Toy quadratic regression, RF, treeshap")

## Export ----
NM <- "preprocess_toy_quad_regression.rds"
saveRDS(chm, file = paste0("~/R/cheem/inst/shiny_apps/cheem/data/", NM))
cat("Saved", NM, "\n")

if(F){
  ## Don't run load cheem list
  chm <- readRDS(paste0("./inst/shiny_apps/cheem/data/", NM))
  lapply(chm, object.size)
  
  ## Don't run manual check
  names(chm)
  global_view(chm)
}
