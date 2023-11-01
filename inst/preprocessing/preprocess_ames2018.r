{
  dat  <- amesHousing2018_NorthAmes
  X    <- dat[, 1:9]
  colnames(X) <- c("LtA", "Qlt", "YrB", "LvA", "Bth", "Bdr", "Rms", "GYB", "GrA")
  Y    <- dat$SalePrice
  clas <- amesHousing2018_NorthAmes$SubclassMS ## class is a zone subclass
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
                label = "North Ames, RF, treeshap")

## Export ----
NM <- "preprocess_ames2018.rds"
saveRDS(chm, file = paste0("./inst/shiny_apps/cheem/data/", NM))
cat("Saved", NM, "\n")

if(F){
  ## Don't run load cheem list
  chm <- readRDS(paste0("./inst/shiny_apps/cheem/data/", NM))
  lapply(chm, object.size)
  
  ## Don't run manual check
  names(chm)
  global_view(chm)
}