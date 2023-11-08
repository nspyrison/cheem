## Setup -----
{
  library("spinifex")
  library("cheem")
  library("testthat")
  
  ## Classification:
  c_X <- penguins_na.rm[, 1:4]
  c_Y <- c_clas <- penguins_na.rm$species
  ## Regression:
  r_X    <- amesHousing2018_NorthAmes[, 1:9]
  r_clas <- amesHousing2018_NorthAmes$SubclassMS
  r_Y    <- amesHousing2018_NorthAmes$SalePrice
  
  r_pred <- ames_rf_pred
  c_pred <- penguin_xgb_pred
  r_attr <- ames_rf_shap
  c_attr <- penguin_xgb_shap
  c_chee <- cheem_ls(c_X, c_Y, c_attr, c_pred, c_clas, "label", FALSE)
  r_chee <- cheem_ls(r_X, r_Y, r_attr, r_pred, r_clas, "label", FALSE)
}

## basis_attr -----
c_bas_attr <- sug_basis(c_attr, 1)
r_bas_attr <- sug_basis(r_attr, 2)

test_that("basis_attr", {
  expect_equal(class(c_bas_attr), c("matrix", "array"))
  expect_equal(class(r_bas_attr), c("matrix", "array"))
})

## proto_basis1d_distribution -----
c_ggt <- ggtour(c_bas_attr, scale_sd(c_X), angle = .3) +
  proto_basis1d_distribution(
    attr_df = c_attr, group_by = c_clas, primary_obs = 1, comparison_obs = 2)
r_ggt <- ggtour(r_bas_attr, scale_sd(r_X), angle = .3) +
  proto_basis1d_distribution(
    attr_df = r_attr, group_by = r_clas, primary_obs = 1, comparison_obs = 2)

test_that("proto_basis1d_distribution", {
  expect_equal(class(c_ggt), c("gg", "ggplot"))
  expect_equal(class(r_ggt), c("gg", "ggplot"))
})

## global_view -----
c_gv <- global_view(c_chee)
r_gv <- global_view(r_chee) |> suppressWarnings()
test_that("global_view", {
  expect_equal(class(c_gv), c("plotly", "htmlwidget"))
  expect_equal(class(r_gv), c("plotly", "htmlwidget"))
})

## global_view as ggplot ----
c_gv <- global_view(c_chee, as_ggplot = TRUE)
r_gv <- global_view(r_chee, as_ggplot = TRUE)
test_that("global_view as_ggplot", {
  expect_equal(class(c_gv), c("gg", "ggplot"))
  expect_equal(class(r_gv), c("gg", "ggplot"))
})

## radial_cheem_tour -----
c_ggt <- radial_cheem_tour(c_chee, c_bas_attr, 1, 1, 2)
r_ggt <- radial_cheem_tour(r_chee, r_bas_attr, 1, 1, 2)
test_that("radial_cheem_tour", {
  expect_equal(class(c_ggt), c("gg", "ggplot"))
  expect_equal(class(r_ggt), c("gg", "ggplot"))
})

