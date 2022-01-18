### Setup ----
{
  library("cheem")
  library("testthat")
  library("spinifex")
  
  set.seed(2022) ## Incase models use some sampling
  r_idx  <- 1L:100L
  ## Classification:
  sub    <- wine[r_idx, ]
  c_X    <- sub[, 2:5]
  c_clas <- sub$Type
  c_Y    <- as.integer(c_clas)
  ## Regression:
  sub    <- amesHousing2018_NorthAmes[r_idx, ]
  r_X    <- sub[, 1:5]
  r_clas <- sub$SubclassMS[r_idx]
  r_Y    <- log(sub$SalePrice[r_idx])
}


### treeshap supported models -----
## Create, unify, and apply through global view for each of:

#### randomForest -----
r_fit     <- randomForest::randomForest(r_X, r_Y, ntree = 25)
r_attr_df <- attr_df_treeshap(r_fit, r_X, noisy = FALSE, verbose = FALSE)
r_this_ls <- cheem_ls(r_X, r_Y, class = r_clas, model = r_fit, attr_df = r_attr_df)
r_gv      <- global_view(r_this_ls)
suppressWarnings(
  c_fit     <- randomForest::randomForest(c_X, c_Y, ntree = 25)
)## Warning expected
c_attr_df <- attr_df_treeshap(c_fit, c_X, noisy = FALSE, verbose = FALSE)
c_this_ls <- cheem_ls(c_X, c_Y, class = c_clas, model = c_fit, attr_df = c_attr_df)
c_gv      <- global_view(c_this_ls)
test_that("randomForest thru global_view", {
  expect_equal(class(r_gv), c("plotly", "htmlwidget"))
  expect_equal(class(c_gv), c("plotly", "htmlwidget"))
})

#### ranger -----
r_fit     <- ranger::ranger(r_Y ~ ., data.frame(r_X, r_Y), num.trees = 25)
r_attr_df <- attr_df_treeshap(r_fit, r_X, noisy = FALSE, verbose = FALSE)
r_this_ls <- cheem_ls(r_X, r_Y, class = r_clas, model = r_fit, attr_df = r_attr_df)
r_gv      <- global_view(r_this_ls)
c_fit     <- ranger::ranger(c_Y ~ ., data.frame(c_X, c_Y), num.trees = 25)
c_attr_df <- attr_df_treeshap(c_fit, c_X, noisy = FALSE, verbose = FALSE)
c_this_ls <- cheem_ls(c_X, c_Y, class = c_clas, model = c_fit, attr_df = c_attr_df)
c_gv      <- global_view(c_this_ls)
test_that("ranger thru global_view", {
  expect_equal(class(r_gv), c("plotly", "htmlwidget"))
  expect_equal(class(c_gv), c("plotly", "htmlwidget"))
})


#### gbm -----
r_fit     <- gbm::gbm(r_Y ~ ., "gaussian", data.frame(r_X, r_Y), n.trees = 25)
r_attr_df <- attr_df_treeshap(r_fit, r_X, noisy = FALSE, verbose = FALSE)
r_this_ls <- cheem_ls(r_X, r_Y, class = r_clas, model = r_fit, attr_df = r_attr_df)
r_gv      <- global_view(r_this_ls)
c_fit     <- gbm::gbm(c_Y ~ ., "tdist", data.frame(c_X, c_Y), n.trees = 50) ## 25 not enough for 
c_attr_df <- attr_df_treeshap(c_fit, c_X, noisy = FALSE, verbose = FALSE)
c_this_ls <- cheem_ls(c_X, c_Y, class = c_clas, model = c_fit, attr_df = c_attr_df)
c_gv      <- global_view(c_this_ls)
test_that("gbm thru global_view", {
  expect_equal(class(r_gv), c("plotly", "htmlwidget"))
  expect_equal(class(c_gv), c("plotly", "htmlwidget"))
})

#### xgboost -----
r_fit     <- xgboost::xgboost(as.matrix(r_X), r_Y, nrounds = 25, verbose = 0,
                              params = list(objective = "reg:squarederror"))
r_attr_df <- attr_df_treeshap(r_fit, as.matrix(r_X), noisy = FALSE, verbose = FALSE)
r_this_ls <- cheem_ls(r_X, r_Y, class = r_clas, model = r_fit, attr_df = r_attr_df)
r_gv      <- global_view(r_this_ls)
c_fit     <- xgboost::xgboost(as.matrix(c_X), c_Y, nrounds = 25, verbose = 0,
                              params = list(objective = "reg:squarederror"))
c_attr_df <- attr_df_treeshap(c_fit, c_X, noisy = FALSE, verbose = FALSE)
c_this_ls <- cheem_ls(c_X, c_Y, class = c_clas, model = c_fit, attr_df = c_attr_df)
c_gv      <- global_view(c_this_ls)
test_that("xgboost thru global_view", {
  expect_equal(class(r_gv), c("plotly", "htmlwidget"))
  expect_equal(class(c_gv), c("plotly", "htmlwidget"))
})

#### lightgbm -----
# lgbm_params <- list(objective = "regression", num_leaves = 25)
# r_fit     <- lightgbm::lightgbm(as.matrix(r_X), r_Y, params = lgbm_params, nrounds = 2)
# print("Continue here, with lightgbm models.")
# # debugonce(cheem::unify_predict)
# r_attr_df <- attr_df_treeshap(r_fit, r_X, noisy = FALSE, verbose = FALSE)
# r_this_ls <- cheem_ls(r_X, r_Y, class = r_clas, model = r_fit, attr_df = r_attr_df)
# # warning: eig_sym(): given matrix is not symmetric
# # 
# # error: Col::tail(): size out of bounds
# # Error in dt_pca(X, myndim, mycor) : Col::tail(): size out of bounds
# ### Inceasing num_leaves, nrounds, or changing objective not helping.
# r_gv      <- global_view(r_this_ls)
# c_fit     <- lightgbm::lightgbm(as.matrix(c_X), c_Y, params = lgbm_params, nrounds = 2)
# c_attr_df <- attr_df_treeshap(c_fit, c_X, noisy = FALSE, verbose = FALSE)
# c_this_ls <- cheem_ls(c_X, c_Y, class = c_clas, model = c_fit, attr_df = c_attr_df)
# # warning: eig_sym(): given matrix is not symmetric
# # 
# # error: Col::tail(): size out of bounds
# # Error in dt_pca(X, myndim, mycor) : Col::tail(): size out of bounds
# ### Inceasing num_leaves, nrounds, or changing objective not helping.
# c_gv      <- global_view(c_this_ls)
# test_that("xgboost thru global_view", {
#   expect_equal(class(r_gv), c("plotly", "htmlwidget"))
#   expect_equal(class(c_gv), c("plotly", "htmlwidget"))
# })

#### NO catboost ----


### default_rf -----
r_rf <- default_rf(r_X, r_Y, verbose = FALSE)
c_rf <- default_rf(c_X, c_Y, verbose = FALSE)

test_that("default_rf", {
  expect_true("randomForest" %in% class(r_rf))
  expect_true("randomForest" %in% class(c_rf))
})

### attr_df_treeshap ----
## doesn't work when rows are few:  Error in 1:nrow(tree) : argument of length 0
r_attr_df <- attr_df_treeshap(r_rf, r_X, noisy = FALSE, verbose = FALSE)
c_attr_df <- attr_df_treeshap(c_rf, c_X, noisy = FALSE, verbose = FALSE)


test_that("attr_df_treeshap", {
  expect_equal(class(r_attr_df), c("data.frame", "treeshap"))
  expect_equal(class(c_attr_df), c("data.frame", "treeshap"))
})

### model_performance_df ----
r_mp <- model_performance_df(r_rf)
c_mp <- model_performance_df(c_rf)

test_that("model_performance_df", {
  expect_equal(class(r_mp), "data.frame")
  expect_equal(class(c_mp), "data.frame")
})

### global_view_df_1layer ----
r_gv1 <- cheem:::global_view_df_1layer(r_X, r_Y, r_clas)
c_gv1 <- cheem:::global_view_df_1layer(c_X, c_Y, c_clas)

test_that(":::global_view_df_1layer", {
  expect_equal(class(r_mp), "data.frame")
  expect_equal(class(c_mp), "data.frame")
})


### cheem_ls -----
r_gv1 <- cheem_ls(r_X, r_Y, r_clas, r_rf, r_attr_df, verbose = FALSE)
c_gv1 <- cheem_ls(c_X, c_Y, c_clas, c_rf, c_attr_df, verbose = FALSE)

test_that("cheem_ls", {
  expect_equal(class(r_gv1), "list")
  expect_equal(class(c_gv1), "list")
})
