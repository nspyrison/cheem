{
  X <- chocolates[, 5:14] %>% as.data.frame()
  Y <- clas <- chocolates$Type
  colnames(X) <- gsub("\\_.*", "", colnames(X))
  nm_imc <- paste(chocolates$Name, chocolates$MFR, chocolates$Country, sep = ", ")
  r_idx <- which(nm_imc == "85% Cocoa Dark French Chocolate, Thorntons, UK")[2]
  nm_imc[r_idx] <- paste0(nm_imc[r_idx], " (2nd)")
  row.names(X) <- nm_imc
}

## Model and predict
train    <- data.matrix(X) %>% xgb.DMatrix(label = Y)
xgb_fit  <- xgboost(data = train, max.depth = 3, nrounds = 6)
xgb_pred <- predict(xgb_fit, newdata = train)

## shapviz
xgb_shap <- shapviz(xgb_fit, X_pred = train, X = X)
xgb_shap <- xgb_shap$S

## Cheem
chm <- cheem_ls(X, Y, xgb_shap, xgb_pred, clas,
                label = "Chocolates, xgb, shapviz")

## Export ----
NM <- "preprocess_chocolates.rds"
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