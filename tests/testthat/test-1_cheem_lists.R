### Setup ----
{
  library("cheem")
  library("testthat")
  
  ## Classification:
  c_X    <- spinifex::penguins_na.rm[, 1:4]
  c_clas <- spinifex::penguins_na.rm$species
  c_Y    <- as.integer(c_clas)
  ## Regression:
  r_X    <- amesHousing2018_NorthAmes[, 1:9]
  r_clas <- amesHousing2018_NorthAmes$SubclassMS
  r_Y    <- amesHousing2018_NorthAmes$SalePrice
  
  r_pred <- ames_rf_pred
  c_pred <- penguin_xgb_pred
  r_attr <- ames_rf_shap
  c_attr <- penguin_xgb_shap
}


### model_performance_df ----
r_mp <- cheem:::model_performance(r_Y, r_pred)
c_mp <- cheem:::model_performance(c_Y, c_pred)

test_that(":::model_performance class", {
  expect_equal(class(r_mp), "data.frame")
  expect_equal(class(c_mp), "data.frame")
})

### global_view_df_1layer ----
r_gv1 <- cheem:::global_view_df_1layer(r_X)
c_gv1 <- cheem:::global_view_df_1layer(c_X)

test_that(":::global_view_df_1layer class", {
  expect_equal(class(r_mp), "data.frame")
  expect_equal(class(c_mp), "data.frame")
})

### cheem_ls -----
r_chm <- cheem_ls(r_X, r_Y, r_attr, r_pred, r_clas, verbose = FALSE)
c_chm <- cheem_ls(c_X, c_Y, c_attr, c_pred, c_clas, verbose = FALSE)

test_that("cheem_ls", {
  expect_equal(class(r_chm), "list")
  expect_equal(class(c_chm), "list")
})
