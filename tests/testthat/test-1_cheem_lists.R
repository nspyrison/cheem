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
  sub <- amesHousing2018_thin[r_idx, ]
  r_X <- sub[, 1:5]
  r_clas <- sub$ZoneMS[r_idx]
  r_Y <- log(sub$SalePrice[r_idx])
}

### default_rf -----
c_rf <- default_rf(c_X, c_Y)
r_rf <- default_rf(r_X, r_Y)

test_that("default_rf", {
  expect_equal(class(c_rf), c("randomForest.formula", "randomForest"))
  expect_equal(class(r_rf), c("randomForest.formula", "randomForest"))
})

### attr_df_treeshap ----
## doesn't work when rows are few:  Error in 1:nrow(tree) : argument of length 0
c_attr_df <- attr_df_treeshap(c_rf, c_X, noisy = FALSE)
r_attr_df <- attr_df_treeshap(r_rf, r_X, noisy = FALSE)

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

### global_view_1layer ----
c_gv1 <- cheem:::global_view_1layer(c_X, c_Y, c_clas)
r_gv1 <- cheem:::global_view_1layer(r_X, r_Y, r_clas)

test_that(":::global_view_1layer", {
  expect_equal(class(c_mp), "data.frame")
  expect_equal(class(r_mp), "data.frame")
})


### cheem_ls -----
c_gv1 <- cheem_ls(c_X, c_Y, c_clas, c_rf, c_attr_df)
r_gv1 <- cheem_ls(r_X, r_Y, r_clas, r_rf, r_attr_df)

test_that("cheem_ls", {
  expect_equal(class(c_gv1), "list")
  expect_equal(class(r_gv1), "list")
})
