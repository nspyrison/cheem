# radial_cheem_tour -----


# ## devtools::check() claims:
# Examples with CPU (user + system) or elapsed time > 5s
# user system elapsed
# radial_cheem_tour 5.69   0.13    5.79
# ## Testing locally:
# Without don't run: 3.41 sec elapsed
# With don't run: 10.14 sec elapsed
# ## Testing locally: 23/02/2022
# Without don't run: 3.51 sec elapsed
# With don't run: 7.06 sec elapsed
###

# may want to check lineprof.

## tictoc -----

{
  tictoc::tic("Without don't run")
  library(cheem)
  library(spinifex)
  ## Classification:
  X    <- penguins_na.rm[, 1:4]
  clas <- penguins_na.rm$species
  Y    <- as.integer(clas)
  rf_fit  <- default_rf(X, Y)
  ## Long runtime for full datasets or complex models:
  shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
  this_ls <- cheem_ls(X, Y, class = clas,
                      model = rf_fit,
                      attr_df = shap_df)
  bas <- basis_attr_df(shap_df, rownum = 1)
  ggt <- radial_cheem_tour(this_ls, basis = bas, manip_var = 1,
                           primary_obs = 1, comparison_obs = 2)
  tictoc::toc()
}

{
  tictoc::tic("With don't run")
  library(cheem)
  library(spinifex)
  ## Classification:
  X    <- penguins_na.rm[, 1:4]
  clas <- penguins_na.rm$species
  Y    <- as.integer(clas)
  rf_fit  <- default_rf(X, Y)
  ## Long runtime for full datasets or complex models:
  shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
  this_ls <- cheem_ls(X, Y, class = clas,
                      model = rf_fit,
                      attr_df = shap_df)
  bas <- basis_attr_df(shap_df, rownum = 1)
  ggt <- radial_cheem_tour(this_ls, basis = bas, manip_var = 1,
                           primary_obs = 1, comparison_obs = 2)
  
  animate_plotly(ggt)
  if(FALSE) ## or animate with gganimate
    animate_gganimate(ggt) #, render = gganimate::av_renderer())
  ## Regression:
  dat  <- amesHousing2018_NorthAmes
  X    <- dat[, 1:9]
  Y    <- log(dat$SalePrice)
  clas <- dat$SubclassMS
  rf_fit  <- default_rf(X, Y)
  ## Long runtime for full datasets or complex models:
  shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
  this_ls <- cheem_ls(X, Y, class = clas,
                      model = rf_fit,
                      attr_df = shap_df)
  bas <- basis_attr_df(shap_df, rownum = 1)
  ggt <- radial_cheem_tour(this_ls, basis = bas, manip_var = 1)
  animate_plotly(ggt)
  if(FALSE) ## or animate with gganimate
    animate_gganimate(ggt, render = gganimate::av_renderer())
  tictoc::toc()
}


## Lineprof ----
require(lineprof)

lp <- lineprof({
  #"Without don't run"
  library(cheem)
  library(spinifex)
  ## Classification:
  X    <- penguins_na.rm[, 1:4]
  clas <- penguins_na.rm$species
  Y    <- as.integer(clas)
  rf_fit  <- default_rf(X, Y)
  ## Long runtime for full datasets or complex models:
  shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
  this_ls <- cheem_ls(X, Y, class = clas,
                      model = rf_fit,
                      attr_df = shap_df)
  bas <- basis_attr_df(shap_df, rownum = 1)
  ggt <- radial_cheem_tour(this_ls, basis = bas, manip_var = 1,
                           primary_obs = 1, comparison_obs = 2)
})
shine(lp)


# amesHousing2018 -----
{
  tictoc::tic("amesHousing2018")
  library(cheem)
  ## Regression setup:
  sub  <- amesHousing2018_NorthAmes[1:100, ]
  X    <- sub[, 1:9]
  Y    <- log(sub$SalePrice)
  clas <- sub$SubclassMS
  ## Model, treeSHAP explanation, cheem list:
  rf_fit  <- default_rf(X, Y)
  ## Treeshap/attr_df_treeshap returning NaN df.
  #debugonce(treeshap)
  shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
  this_ls <- cheem_ls(X, Y, class = clas,
                      model = rf_fit,
                      attr_df = shap_df)
  ## Visualize:
  global_view(this_ls)
  tictoc::toc() 
}
