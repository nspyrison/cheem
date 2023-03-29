## Setup ------
{
  raw <- spinifex::penguins_na.rm
  X <- raw[, 1:4] %>% as.data.frame()
  Y <- clas <- raw$species
  colnames(X) <- c("bl", "bd", "fl", "bm")
}

## Model and predict
train    <- data.matrix(X) %>% xgb.DMatrix(label = Y)
xgb_fit  <- xgboost(data = train, max.depth = 3, nrounds = 25)
xgb_pred <- predict(xgb_fit, newdata = train)

## shapviz
xgb_shap <- shapviz(xgb_fit, X_pred = train, X = X)
xgb_shap <- xgb_shap$S

## Cheem
chm <- cheem_ls(X, Y, xgb_shap, xgb_pred, clas,
                label = "Penguins, xgb, shapviz")

## Export ----
NM <- "preprocess_penguins.rds"
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