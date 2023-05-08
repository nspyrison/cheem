## Setup ----
{
  set.seed(20211105)
  n_obs <- 240
  X <- tibble::tibble(
    x1 = runif(n_obs, 0, 5),
    x2 = runif(n_obs, 0, 5),
    x3 = runif(n_obs, 0, 5),
    x4 = runif(n_obs, 0, 5),
    x5 = runif(n_obs, 0, 5))
  n_thrid <- n_obs / 3
  idx1 <- 0 * n_thrid + 1:n_thrid
  idx2 <- 1 * n_thrid + 1:n_thrid
  idx3 <- 2 * n_thrid + 1:n_thrid
  Y <- c(
    c(X$x1^2 + (X$x2 + X$x3 + X$x4 + X$x5) / 10)[idx1] + rnorm(n_thrid),
    c(X$x2^2 + (X$x1 + X$x3 + X$x4 + X$x5) / 10)[idx2] + rnorm(n_thrid),
    c(X$x3^2 + (X$x1 + X$x2 + X$x4 + X$x5) / 10)[idx3] + rnorm(n_thrid)
  )
  
  ## Visualize
  if(F){
    library(ggplot2)
    skimr::skim(df)
    
    df <- tibble::tibble(X, Y) %>% spinifex::scale_sd()
    gt_path <- save_history(df, grand_tour(), max_bases = 10, rescale = FALSE)
    
    ggt <- ggtour(gt_path) + proto_default(aes_args = list(color = Y))
    animate_plotly(ggt)
  }
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
                label = "Toy mixture regression, RF, treeshap")

## Export ----
NM <- "preprocess_toy_mixture_regression.rds"
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
