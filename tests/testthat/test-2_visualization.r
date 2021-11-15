## Setup -----
{
  library("cheem")
  library("spinifex")
  library("testthat")
  
  r_idx <- 1L:100L
  ## Classification:
  sub    <- wine[r_idx, ]
  c_X    <- sub[, 2:5]
  c_clas <- sub$Type
  c_Y    <- as.integer(c_clas)
  c_rf   <- default_rf(c_X, c_Y)
  c_attr <- attr_df_treeshap(c_rf, x = c_X, noisy = FALSE)
  c_chee <- cheem_ls(x = c_X, y = c_Y, class = c_clas, c_rf, c_attr)
  ## Regression:
  sub    <- amesHousing2018_thin[r_idx, ]
  r_X    <- sub[, 1:5]
  r_clas <- sub$ZoneMS[r_idx]
  r_Y    <- log(sub$SalePrice[r_idx])
  r_rf   <- default_rf(r_X, r_Y)
  r_attr <- attr_df_treeshap(r_rf, x = r_X, noisy = FALSE)
  r_chee <- cheem_ls(x = r_X, y = r_Y, class = r_clas, r_rf, r_attr)
}


## basis_attr -----
c_bas_attr <- basis_attr_df(attr_df = c_attr, 1)
r_bas_attr <- basis_attr_df(attr_df = r_attr, 2)

test_that("basis_attr", {
  expect_is(c_bas_attr, "matrix")
  expect_is(r_bas_attr, "matrix")
})

## proto_basis1d_distribution -----
c_ggt <- ggtour(c_bas_attr, scale_sd(c_X), angle = .3) +
  proto_basis1d_distribution(
    attr_df = c_attr, group_by = c_clas, primary_obs = 1, comparison_obs = 2)
r_ggt <- ggtour(r_bas_attr, scale_sd(r_X), angle = .3) +
  proto_basis1d_distribution(
    attr_df = r_attr, group_by = r_clas, primary_obs = 1, comparison_obs = 2)

test_that("proto_basis1d_distribution", {
  expect_is(c_ggt, c("gg", "ggplot"))
  expect_is(r_ggt, c("gg", "ggplot"))
})

## global_view -----
?global_view

c_gv <- global_view(c_chee)
r_gv <- global_view(r_chee)
test_that("global_view", {
  expect_is(c_gv, c("plotly", "htmlwidget"))
  expect_is(r_gv, c("plotly", "htmlwidget"))
})

## radial_cheem_tour -----
c_ggt <- radial_cheem_tour(c_chee, c_bas_attr, manip_var = 1L, 1L, 2L)
r_ggt <- radial_cheem_tour(r_chee, r_bas_attr, manip_var = 1L, 1L, 2L)
test_that("radial_cheem_tour", {
  expect_is(c_ggt, c("gg", "ggplot"))
  expect_is(r_ggt, c("gg", "ggplot"))
})
