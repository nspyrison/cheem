## Setup ------
{
  raw <- spinifex::penguins_na.rm
  X <- raw[, 1:4] %>% as.data.frame()
  Y <- clas <- raw$species
  colnames(X) <- c("bl", "bd", "fl", "bm")
}
"Note that after treeshap when to CRAN the unify step is returning all null with a bunch of warnings:"
"Warning in eval(jsub, SDenv, parent.frame()) :
  NAs introduced by coercion"

## Model and predict
train    <- data.matrix(X) %>% xgb.DMatrix(label = Y)
xgb_fit  <- xgboost(data = train, max.depth = 3, nrounds = 5)
xgb_pred <- predict(xgb_fit, newdata = train)
xgb_pred2 <- levels(Y)[xgb_pred %>% round()] %>% factor()

## Shapviz shap
xgb_shap <- shapviz(xgb_fit, X_pred = train, X = X)
xgb_shap <- xgb_shap$S

## Cheem
chm <- cheem_ls(X, Y, xgb_shap, xgb_pred2, clas,
                label = "Penguins, xgb, shapviz")

## Export ----
NM <- "preprocess_penguins.rds"
if(chm$decode_df$is_misclassified %>% sum == 0)
  stop(paste0(NM, ": No missclassified points in model."))
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



## Deprecated xgb model ------


# ## Model and predict
# train    <- data.matrix(X) %>% xgb.DMatrix(label = Y)
# xgb_fit  <- xgboost(data = train, max.depth = 3, nrounds = 25)
# xgb_pred <- predict(xgb_fit, newdata = train)
# 
# ## shapviz
# xgb_shap <- shapviz(xgb_fit, X_pred = train, X = X)
# xgb_shap <- xgb_shap$S
# 
# ## Cheem
# chm <- cheem_ls(X, Y, xgb_shap, xgb_pred, clas,
#                 label = "Penguins, xgb, shapviz")
# 
# ## Export -
# NM <- "preprocess_penguins.rds"
# if(chm$decode_df$is_misclassified %>% sum == 0)
#   stop(paste0(NM, ": No missclassified points in model."))
# saveRDS(chm, file = paste0("./inst/shiny_apps/cheem/data/", NM))
# cat("Saved", NM, "\n")

# ##  Model and predict -- random forest, out of use
# rf_fit <- default_rf(X, Y)
# rf_fit <- randomForest::randomForest(
#   X, Y, ntree = 125,
#   mtry = ifelse(is_discrete(Y), sqrt(ncol(X)), ncol(X) / 3),
#   nodesize = max(ifelse(is_discrete(Y), 1, 5), nrow(X) / 500))
# rf_pred <- predict(rf_fit)
# rf_shap <- treeshap::treeshap(
#   treeshap::unify(rf_fit, X), 
#   X, F, F) #%>% suppressWarnings()
# rf_shap <- rf_shap$shaps

# ## Lime example --
# library(MASS)
# library(lime)
# data(biopsy)
# 
# # First we'll clean up the data a bit
# biopsy$ID <- NULL
# biopsy <- na.omit(biopsy)
# names(biopsy) <- c('clump thickness', 'uniformity of cell size', 
#                    'uniformity of cell shape', 'marginal adhesion',
#                    'single epithelial cell size', 'bare nuclei', 
#                    'bland chromatin', 'normal nucleoli', 'mitoses',
#                    'class')
# 
# # Now we'll fit a linear discriminant model on all but 4 cases
# set.seed(4)
# test_set <- sample(seq_len(nrow(biopsy)), 4)
# prediction <- biopsy$class
# biopsy$class <- NULL
# model <- lda(biopsy[-test_set, ], prediction[-test_set])
# 
# pred <- predict(model)
# sum(prediction != pred$class)
# explainer <- lime(biopsy[-test_set,], model, bin_continuous = TRUE, quantile_bins = FALSE)
# 
# chm <- cheem_ls(X, Y, xgb_shap, xgb_pred, clas,
#                 label = "Penguins, xgb, shapviz")