## Data simulation ------
{
  set.seed(20211105)
  .n <- 50  ## obs per class
  .m <- 3.5 ## mean difference between class
  ############## clas a:         clas b:         clas c:
  sim_EEE_p4 <- data.frame(
    x1 = c(rnorm(.n,0),    rnorm(.n,.m),   rnorm(.n,.m/2)), ## Separates A from B
    x2 = c(rnorm(.n,0),    rnorm(.n,0),    rnorm(.n,sqrt(.m^2 + (.m/2)^2))), ## Separates C from A&B
    x3 = c(rnorm(.n,0,.2), rnorm(.n,0,.2), rnorm(.n,0,.2)), ## noise, half sd
    x4 = c(rnorm(.n,0,.2), rnorm(.n,0,.2), rnorm(.n,0,.2))) ## noise, half sd
  attr(sim_EEE_p4, "cluster") <- factor(rep(LETTERS[1:3], each = .n))
  
  ## Visualize
  if(F){
    str(sim_EEE_p4)
    require(ggplot2)
    ggplot(sim_EEE_p4, aes(x1, x2, color = clas, shape = clas)) + geom_point()
  }
  
  X <- sim_EEE_p4
  Y <- clas <- attr(sim_EEE_p4, "cluster")
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
                label = "Toy classification, xgb, shapviz")

## Export ----
NM <- "preprocess_toy_classification.rds"
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
