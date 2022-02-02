require(cheem)
require(microbenchmark)

?global_view

## Regression:
dat <- amesHousing2018_NorthAmes
X <- dat[, 1:9]
Y <- log(dat$SalePrice)
clas <- dat$SubclassMS

rf_fit <- default_rf(X, Y)
## Long runtime for full datasets:
shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
this_ls <- cheem_ls(X, Y, class = clas,
                    model = rf_fit,
                    attr_df = shap_df)

(mbm1 <- microbenchmark::microbenchmark(
  times = 10, 
  global_view_orig = global_view(this_ls),
  global_view_orig.print = print(global_view(this_ls)),
  global_view_subplots_orig = global_view_subplots(this_ls),
  global_view_subplots_orig.print = print(global_view_subplots(this_ls))
))
