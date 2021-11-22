## Dependencies ------
## Local files
require(cheem)
s <- function(sec = .01)Sys.sleep(sec)

#### A simplified version
set.seed(20211105)
X <- data.frame(x1 = runif(200, 0, 4 * pi),
                x2 = runif(200, 0, 4 * pi),
                x3 = runif(200, 0, 4 * pi),
                x4 = runif(200, 0, 4 * pi),
                x5 = runif(200, 0, 4 * pi))
Y <- sin(X$x1) + sin(X$x2) + .1 * X$x3 + .1 * X$x4 + .1 * X$x5 + rnorm(200, sd = .05)

rf_fit  <- default_rf(X, Y); s();
shap_df <- attr_df_treeshap(rf_fit, X); s();
this_ls <- cheem_ls(X, Y,
                    model = rf_fit,
                    attr_df = shap_df)
if(F)
  global_view(this_ls)

## EXPORT OBJECTS ----
setwd("~/R/cheem")
saveRDS(this_ls,
        file = "./inst/shiny_apps/cheem_initial/data/9preprocess_toy_regression_trig.rds")
cat("Saved.\n")
if(F) ## Not run, load cheem_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/9preprocess_toy_regression_trig.rds")

