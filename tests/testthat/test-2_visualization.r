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
  sub    <- amesHousing2018_NorthAmes[r_idx, ]
  r_X    <- sub[, 1:5]
  r_clas <- sub$SubclassMS[r_idx]
  r_Y    <- sub$SalePrice[r_idx]
  r_rf   <- default_rf(r_X, r_Y, verbose = FALSE)
  r_attr <- attr_df_treeshap(r_rf, x = r_X, noisy = FALSE, verbose = FALSE)
  r_chee <- cheem_ls(x = r_X, y = r_Y, class = r_clas, r_rf, r_attr, verbose = FALSE)
  
  
  ## basis_attr -----
  c_bas_attr <- basis_attr_df(attr_df = c_attr, 1)
  r_bas_attr <- basis_attr_df(attr_df = r_attr, 2)
  
  test_that("basis_attr", {
    expect_equal(class(c_bas_attr), c("matrix", "array"))
    expect_equal(class(r_bas_attr), c("matrix", "array"))
  })
  
  
}

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
r_gv <- global_view(r_chee)
test_that("global_view", {
  expect_equal(class(c_gv), c("plotly", "htmlwidget"))
  expect_equal(class(r_gv), c("plotly", "htmlwidget"))
})

## global_view as ggplot ----
c_gvgg <- global_view(c_chee, as_ggplot = TRUE)
r_gvgg <- global_view(r_chee, as_ggplot = TRUE)
test_that("global_view as_ggplot", {
  expect_equal(class(c_gvgg), c("gg", "ggplot"))
  expect_equal(class(r_gvgg), c("gg", "ggplot"))
})

## global_view_subplots -----
c_gvsp <- global_view_subplots(c_chee)
r_gvsp <- global_view_subplots(r_chee)
test_that("global_view_subplots", {
  expect_equal(class(c_gvsp), c("plotly", "htmlwidget"))
  expect_equal(class(r_gvsp), c("plotly", "htmlwidget"))
})


## radial_cheem_tour -----
c_ggt <- radial_cheem_tour(c_chee, c_bas_attr, manip_var = 1L, 1L, 2L)
r_ggt <- radial_cheem_tour(r_chee, r_bas_attr, manip_var = 1L, 1L, 2L)
test_that("radial_cheem_tour", {
  expect_equal(class(c_ggt), c("gg", "ggplot"))
  expect_equal(class(r_ggt), c("gg", "ggplot"))
})

## radial_cheem_tour_subplots -----
c_ggtsp <- radial_cheem_tour_subplots(c_chee, c_bas_attr, manip_var = 1L, 1L, 2L)
## Plotly uses gather_ in regression case: offending line in radial_cheem_tour_subplots:
# p3 <- plotly::ggplotly(g3) %>% plotly::layout(yaxis = list(title = "residual"))
suppressWarnings(
  r_ggtsp <- radial_cheem_tour_subplots(r_chee, r_bas_attr, manip_var = 1L, 1L, 2L)
)

test_that("radial_cheem_tour_subplots", {
  expect_equal(class(c_ggtsp), c("plotly", "htmlwidget"))
  expect_equal(class(r_ggtsp),  c("plotly", "htmlwidget"))
})
