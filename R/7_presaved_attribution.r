## penguin_xgb_: pred, shap -----
#' Penguins xgb model predictions and shap values
#' 
#' Predictions and shapviz attribution of an xgb model of Penguin data 
#' classifying penguin species.
#' 
#' @format `penguin_xgb_pred` is a n=333 length vector of the prediction of an
#' xgb model predicting the number of the factor level of the species of penguin.
#' `penguin_xgb_shap` is a (333 x 4) data frame of the shapviz SHAP attribution of
#' the xgb model for each observation.
#' 
#' __Replicating__
#' ```
#' library(cheem)
#' library(xgboost)
#' library(shapviz)
#' set.seed(135)
#' 
#' ## Classification setup
#' X    <- spinifex::penguins_na.rm[, 1:4]
#' Y    <- spinifex::penguins_na.rm$species
#' clas <- spinifex::penguins_na.rm$species
#' 
#' ## Model and predict
#' peng_train    <- data.matrix(X) %>%
#'   xgb.DMatrix(label = Y)
#' peng_xgb_fit  <- xgboost(data = peng_train, max.depth = 3, nrounds = 5)
#' penguin_xgb_pred <- predict(peng_xgb_fit, newdata = peng_train)
#' 
#' ## shapviz
#' penguin_xgb_shap <- shapviz(peng_xgb_fit, X_pred = peng_train, X = X)
#' penguin_xgb_shap <- penguin_xgb_shap$S
#' 
#' if(F){ ## Don't accidentally save
#'   save(penguin_xgb_pred, file = "./data/penguin_xgb_pred.rda")
#'   save(penguin_xgb_shap, file = "./data/penguin_xgb_shap.rda")
#'   #usethis::use_data(penguin_xgb_pred)
#'   #usethis::use_data(penguin_xgb_shap)
#' }
#' ```
#' @keywords datasets
#' @examples
#' library(cheem)
#' 
#' ## Classification setup
#' X    <- spinifex::penguins_na.rm[, 1:4]
#' Y    <- spinifex::penguins_na.rm$species
#' clas <- spinifex::penguins_na.rm$species
#' 
#' ## Precomputed predictions and shap attribtion
#' str(penguin_xgb_pred)
#' str(penguin_xgb_shap)
#' 
#' ## Cheem
#' peng_chm <- cheem_ls(X, Y, penguin_xgb_shap, penguin_xgb_pred, clas,
#'                      label = "Penguins, xgb, shapviz")
#' 
#' ## Save for use with shiny app (expects an rds file)
#' if(FALSE){ ## Don't accidentally save.
#'   saveRDS(peng_chm, "./chm_peng_xgb_shapviz.rds")
#'   run_app() ## Select the saved rds file from the data dropdown.
#' }
#' 
#' ## Cheem visuals
#' if(interactive()){
#'   prim <- 1
#'   comp <- 2
#'   global_view(peng_chm, primary_obs = prim, comparison_obs = comp)
#'   bas <- sug_basis(penguin_xgb_shap, prim, comp)
#'   mv  <- sug_manip_var(penguin_xgb_shap, primary_obs = prim, comp)
#'   ggt <- radial_cheem_tour(peng_chm, basis = bas, manip_var = mv)
#'   animate_plotly(ggt)
#' }
"penguin_xgb_pred"

#' @rdname penguin_xgb_pred
"penguin_xgb_shap"


## chocolate_svm_: pred, shap -----
#' Chocolate svm model predictions and shap values
#' 
#' Predictions and DALEX shap attribution of an svm model of Chocolate data 
#' classifying type of chocolate (light/dark).
#' 
#' @format `chocolate_svm_pred` is a n=88 length vector of the prediction of an
#' svm model predicting the number of the factor level of the species of penguin.
#' `chocolate_svm_shap` is a (88 x 10) data frame of the DALEX SHAP attribution 
#' of the svm model for each observation.
#' 
#' __Replicating__
#' ```
#' library(cheem)
#' library(e1071)
#' library(DALEX)
#' set.seed(135)
#' 
#' ## Classification setup
#' X    <- chocolates[, 5:14]
#' Y    <- chocolates$Type
#' clas <- chocolates$Type
#' 
#' ## Model and predict
#' choc_svm_fit <- svm(
#'   formula = Y ~ ., data = data.frame(Y, X),
#'   type = 'C-classification', kernel = 'linear', probability = TRUE)
#' chocolates_svm_pred <- predict(choc_svm_fit, data.frame(Y, X))
#' 
#' ## SHAP via DALEX, versatile but slow
#' choc_svm_exp <- explain(choc_svm_fit, data = X, y = Y,
#'                         label = "Chocolates, svm")
#' ## Note that cheem expects a full [n, p] attribution space
#' ## Shap takes about ~30-40 sec for me
#' chocolates_svm_shap <- matrix(NA, nrow(X), ncol(X)) ## init a df of the same structure
#' sapply(1:nrow(X), function(i){
#'   pps <- predict_parts_shap(choc_svm_exp, new_observation = X[i, ])
#'   ## Keep just the [n, p] local explanations
#'   chocolates_svm_shap[i, ] <<- tapply(
#'     pps$contribution, pps$variable, mean, na.rm = TRUE) %>% as.vector()
#' })
#' chocolates_svm_shap <- as.data.frame(chocolates_svm_shap)
#' 
#' if(F){ ## Don't accidentally save
#'   save(chocolates_svm_pred, file = "./data/chocolates_svm_pred.rda")
#'   save(chocolates_svm_shap, file = "./data/chocolates_svm_shap.rda")
#'   #usethis::use_data(chocolates_svm_pred)
#'   #usethis::use_data(chocolates_svm_shap)
#' }
#' ```
#' @keywords datasets
#' @examples
#' library(cheem)
#' 
#' ## Classification setup
#' X    <- chocolates[, 5:14]
#' Y    <- chocolates$Type
#' clas <- chocolates$Type
#' 
#' ## Precomputed predictions and shap attribution
#' str(chocolates_svm_pred)
#' str(chocolates_svm_shap)
#' 
#' ## Cheem
#' choc_chm <- cheem_ls(X, Y, chocolates_svm_shap,
#'                      chocolates_svm_pred, clas,
#'                      label = "Chocolates, SVM, shap")
#' 
#' ## Save for use with shiny app (expects an rds file)
#' if(FALSE){ ## Don't accidentally save.
#'   saveRDS(choc_chm, "./cmh_chocolates_svm_shap.rds")
#'   run_app() ## Select the saved rds file from the data dropdown.
#' }
#' 
#' ## Cheem visuals
#' if(interactive()){
#'   prim <- 1
#'   comp <- 2
#'   global_view(choc_chm, primary_obs = prim, comparison_obs = comp)
#'   bas <- sug_basis(chocolates_svm_shap, prim, comp)
#'   mv  <- sug_manip_var(chocolates_svm_shap, primary_obs = prim, comp)
#'   ggt <- radial_cheem_tour(choc_chm, basis = bas, manip_var = mv)
#'   animate_plotly(ggt)
#' }
"chocolates_svm_pred"

#' @rdname chocolates_svm_pred
"chocolates_svm_shap"


## ames_rf_: pred, shap -----
#' Ames random forest model predictions and shap values
#' 
#' Predictions and treeshap attribution of a random forest model of North Ames 
#' house sales data regressing Sales Price from house and lot variables.
#' 
#' @format `ames_rf_pred` is a n=338 length vector of the prediction of an
#' random forest model predicting the numeric House Sales in North Ames.
#' `ames_rf_shap` is a (338 x 9) data frame of the treeshap SHAP attribution of
#' the random forest model for each observation.
#' 
#' __Replicating__
#' ```
#' library(cheem)
#' library(randomForest)
#' library(treeshap)
#' set.seed(135)
#' 
#' ## Regression setup
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Model and treeSHAP
#' ames_rf_fit <- randomForest::randomForest(
#'   X, Y, ntree = 125,
#'   mtry = ifelse(is_discrete(Y), sqrt(ncol(X)), ncol(X) / 3),
#'   nodesize = max(ifelse(is_discrete(Y), 1, 5), nrow(X) / 500))
#' ames_rf_pred <- predict(ames_rf_fit, X)
#' ames_rf_shap <- treeshap::treeshap(
#'   treeshap::randomForest.unify(ames_rf_fit, X), X, FALSE, FALSE)
#' ames_rf_shap <- ames_rf_shap$shaps
#' 
#' if(F){ ## Don't accidentally save
#'   save(ames_rf_pred, file = "./data/ames_rf_pred.rda")
#'   save(ames_rf_shap, file = "./data/ames_rf_shap.rda")
#'   #usethis::use_data(ames_rf_pred)
#'   #usethis::use_data(ames_rf_shap)
#' }
#' ```
#' @keywords datasets
#' @examples
#' library(cheem)
#' 
#' ## Regression setup
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Precomputed predictions and shap attribution
#' str(ames_rf_pred)
#' str(ames_rf_shap)
#' 
#' ## Cheem
#' ames_chm <- cheem_ls(X, Y, ames_rf_shap, ames_rf_pred, clas,
#'                      label = "Ames, random forest, treeshap")
#' 
#' ## Save for use with shiny app (expects an rds file)
#' if(FALSE){ ## Don't accidentally save.
#'   saveRDS(ames_chm, "./chm_ames_rf_tshap.rds")
#'   run_app() ## Select the saved rds file from the data dropdown.
#' }
#' 
#' ## Cheem visuals
#' if(interactive()){
#'   prim <- 1
#'   comp <- 2
#'   global_view(ames_chm, primary_obs = prim, comparison_obs = comp)
#'   bas <- sug_basis(ames_rf_shap, prim, comp)
#'   mv  <- sug_manip_var(ames_rf_shap, primary_obs = prim, comp)
#'   ggt <- radial_cheem_tour(ames_chm, basis = bas, manip_var = mv)
#'   animate_plotly(ggt)
#' }
"ames_rf_pred"

#' @rdname ames_rf_pred
"ames_rf_shap"