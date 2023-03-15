library(shapviz)
library(ggplot2)
library(xgboost)

set.seed(3653)

x <- c("carat", "cut", "color", "clarity")
dtrain <- xgb.DMatrix(data.matrix(diamonds[x]), label = diamonds$price)

fit <- xgb.train(
  params = list(learning_rate = 0.1, objective = "reg:squarederror"), 
  data = dtrain,
  nrounds = 65L
)

dia_small <- diamonds[sample(nrow(diamonds), 2000L), ]

shp <- shapviz(fit, X_pred = data.matrix(dia_small[x]), X = dia_small)
str(shp)
shp$S


########## -

dat  <- amesHousing2018_NorthAmes
X    <- dat[, 1:9]
Y    <- dat$SalePrice
clas <- dat$SubclassMS

## Model, treeSHAP, cheem list, visualize
rf_fit  <- default_rf(X, Y)
#shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
shp <- shapviz(rf_fit, X_pred = predict(rf_fit), X = X)

rf_fit

