### Setup ----
{
  library("cheem")
  library("testthat")
  library("spinifex")
  
  r_idx <- 1L:100L
  ## Classification:
  sub <- wine[r_idx, ]
  c_X <- sub[, 2:5]
  c_clas <- sub$Type
  c_Y <- as.integer(c_clas)
  ## Regression:
  sub <- amesHousing2018_NorthAmes[r_idx, ]
  r_X <- sub[, 1:5]
  r_clas <- sub$SubclassMS[r_idx]
  r_Y <- log(sub$SalePrice[r_idx])
}


### default_rf -----
c_rf <- default_rf(c_X, c_Y, verbose = FALSE)
r_rf <- default_rf(r_X, r_Y, verbose = FALSE)

test_that("default_rf", {
  expect_equal(class(c_rf), c("randomForest.formula", "randomForest"))
  expect_equal(class(r_rf), c("randomForest.formula", "randomForest"))
})


### treeshap supported models -----
## Create, unify, and apply through global view for each of:

#### randomForest -----
fit <- randomForest::randomForest(r_X, r_Y, ntree = 10)
r_attr_df <- attr_df_treeshap(fit, r_X, noisy = FALSE, verbose = FALSE)
this_ls <- cheem_ls(r_X, r_Y, class = r_clas, model = fit, attr_df = r_attr_df)
global_view(this_ls)

#### ranger -----
fit <- ranger::ranger(r_Y ~ ., data.frame(r_X, r_Y), num.trees = 10)
r_attr_df <- attr_df_treeshap(fit, r_X, noisy = FALSE, verbose = FALSE)
this_ls <- cheem_ls(r_X, r_Y, class = r_clas, model = fit, attr_df = r_attr_df)
## issue with ranger:
# Error in predict.ranger(model) : 
#   Error: Argument 'data' is required for non-quantile prediction.
##global_view(this_ls)

#### gbm -----
fit <- gbm::gbm(r_Y ~ ., data = data.frame(r_X, r_Y), n.trees = 10)
r_attr_df <- attr_df_treeshap(fit, r_X, noisy = FALSE, verbose = FALSE)
this_ls <- cheem_ls(r_X, r_Y, class = r_clas, model = fit, attr_df = r_attr_df)
## error for gbm:
# warning: eig_sym(): given matrix is not symmetric
# 
# error: Col::tail(): size out of bounds
# Error in dt_pca(X, myndim, mycor) : Col::tail(): size out of bounds
## global_view(this_ls)


#### xgboost -----
fit <- xgboost::xgboost(as.matrix(r_X), r_Y, nrounds = 10,
                        params = list(objective = "reg:squarederror"))
r_attr_df <- attr_df_treeshap(fit, data.frame(r_X, r_Y), noisy = FALSE, verbose = FALSE)
this_ls <- cheem_ls(r_X, r_Y, class = r_clas, model = fit, attr_df = r_attr_df)
## error for xgboost:
# warning: eig_sym(): given matrix is not symmetric
# 
# error: Col::tail(): size out of bounds
# Error in dt_pca(X, myndim, mycor) : Col::tail(): size out of bounds
##global_view(this_ls)

#### lightgbm -----
lgbm_params <- list(objective = "regression", num_leaves = 10L)
fit <- lightgbm::lightgbm(as.matrix(r_X), r_Y, params = params, nrounds = 2)
r_attr_df <- attr_df_treeshap(fit, r_X, noisy = FALSE, verbose = FALSE)

#### NO catboost ----


### attr_df_treeshap ----
## doesn't work when rows are few:  Error in 1:nrow(tree) : argument of length 0
c_attr_df <- attr_df_treeshap(c_rf, c_X, noisy = FALSE, verbose = FALSE)
r_attr_df <- attr_df_treeshap(r_rf, r_X, noisy = FALSE, verbose = FALSE)

test_that("attr_df_treeshap", {
  expect_equal(class(c_attr_df), c("data.frame", "treeshap"))
  expect_equal(class(r_attr_df), c("data.frame", "treeshap"))
})

### model_performance_df ----
c_mp <- model_performance_df(c_rf)
r_mp <- model_performance_df(r_rf)

test_that("model_performance_df", {
  expect_equal(class(c_mp), "data.frame")
  expect_equal(class(r_mp), "data.frame")
})

### global_view_df_1layer ----
c_gv1 <- cheem:::global_view_df_1layer(c_X, c_Y, c_clas)
r_gv1 <- cheem:::global_view_df_1layer(r_X, r_Y, r_clas)

test_that(":::global_view_df_1layer", {
  expect_equal(class(c_mp), "data.frame")
  expect_equal(class(r_mp), "data.frame")
})


### cheem_ls -----
c_gv1 <- cheem_ls(c_X, c_Y, c_clas, c_rf, c_attr_df, verbose = FALSE)
r_gv1 <- cheem_ls(r_X, r_Y, r_clas, r_rf, r_attr_df, verbose = FALSE)

test_that("cheem_ls", {
  expect_equal(class(c_gv1), "list")
  expect_equal(class(r_gv1), "list")
})
