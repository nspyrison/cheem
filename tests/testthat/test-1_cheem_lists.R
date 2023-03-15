### Setup ----
{
  library("cheem")
  library("testthat")
  library("spinifex")
  
  set.seed(2022) ## Incase models use some sampling
  r_idx  <- 1:100
  ## Classification:
  sub    <- wine[r_idx, ]
  c_X    <- sub[, 2:5]
  c_clas <- sub$Type
  c_Y    <- as.integer(c_clas)
  ## Regression:
  sub    <- amesHousing2018_NorthAmes[r_idx, ]
  r_X    <- sub[, 1:5]
  r_clas <- sub$SubclassMS[r_idx]
  r_Y    <- sub$SalePrice[r_idx]
}


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
r_gv1 <- cheem:::global_view_df_1layer(r_X)
c_gv1 <- cheem:::global_view_df_1layer(c_X)

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
