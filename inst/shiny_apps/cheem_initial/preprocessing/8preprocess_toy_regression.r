## Dependencies ------
## Local files
require(cheem)

#### A simplified version
set.seed(20211105)
X <- data.frame(x1 = runif(200, 0, 5),
                x2 = runif(200, 0, 5),
                x3 = runif(200, 0, 5),
                x4 = runif(200, 0, 5),
                x5 = runif(200, 0, 5))
Y <- X$x1 + X$x2 + (X$x1 * X$x2) + .1 * X$x3 + .1 * X$x4 + .1 * X$x5 + rnorm(200)



rf_fit  <- default_rf(X, Y)
pred <- predict(rf_fit)
obs <- Y
df <- data.frame(obs=obs, pred=pred, rn= 1:200)
ggplot(df, aes(pred, obs, label= rn)) + geom_point()  #+ geom_smooth()
ggplotly()
  
shap_df <- attr_df_treeshap(rf_fit, X)
this_ls <- cheem_ls(X, Y,
                    model = rf_fit,
                    attr_df = shap_df)
#global_view(this_ls)

## EXPORT OBJECTS ----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  saveRDS(this_ls,
          file = "./inst/shiny_apps/cheem_initial/data/8preprocess_toy_regression.rds")
}
if(F) ## Not run, load cheem_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/8preprocess_toy_regression.rds")

