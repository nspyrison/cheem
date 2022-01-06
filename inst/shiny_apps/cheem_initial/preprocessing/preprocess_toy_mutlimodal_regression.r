## Dependencies ------
## Local files
require(cheem)
s <- function(sec = .01)Sys.sleep(sec)

#### A simplified version
set.seed(20211105)
n_obs <- 200
X <- data.frame(x1 = runif(n_obs, 0, 4 * pi),
                x2 = runif(n_obs, 0, 4 * pi),
                x3 = runif(n_obs, 0, 1),
                x4 = runif(n_obs, 0, 1),
                x5 = runif(n_obs, 0, 1))
Y <- sin(X$x1) + sin(X$x2) + .1 * X$x3 + .1 * X$x4 + .1 * X$x5 + rnorm(n_obs, sd = .05)


if(F){ ## Visualize
  library(ggplot2)
  df <- data.frame(X, Y)
  ggplot(df, aes(x1, x2, color = Y)) + geom_point()
}


rf_fit  <- default_rf(X, Y); s();
shap_df <- attr_df_treeshap(rf_fit, X, verbose = TRUE); s();
this_ls <- cheem_ls(X, Y,
                    model = rf_fit,
                    attr_df = shap_df)
if(F)
  global_view(this_ls)

## EXPORT OBJECTS ----
setwd("~/R/cheem")
saveRDS(this_ls,
        file = "./inst/shiny_apps/cheem_initial/data/preprocess_toy_trig_regression.rds")
cat("Saved.\n")
if(F) ## Not run, load cheem_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/preprocess_toy_trig_regression.rds")

