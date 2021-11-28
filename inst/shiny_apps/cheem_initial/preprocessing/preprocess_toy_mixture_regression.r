{
  require(cheem)
  s <- function(sec = 1)Sys.sleep(sec)
  
  set.seed(20211105)
  n_obs <- 200
  X <- tibble::tibble(
    x1 = runif(n_obs, 0, 4 * pi),
    x2 = runif(n_obs, 0, 4 * pi),
    x3 = runif(n_obs, 0, 3),
    x4 = runif(n_obs, 0, 3),
    x5 = runif(n_obs, 0, 1))
  y1 <- 2 + sin(X$x1) + sin(X$x2) + X$x3/30 + X$x4/30 + .1 * X$x5 + rnorm(n_obs, sd = .1)
  y2 <- X$x1/(40*pi) + X$x2/(40*pi) + (X$x3 * X$x4) + .1 * X$x5 + rnorm(n_obs, sd = .1)
  id <- 1:n_obs
  Y <-  NULL
  source <- NULL
  .m <- sapply(1:n_obs, function(i){
    Y[i] <<- max(y1[i], y2[i])
    source[i] <<- if(y1[i] == max(y1[i], y2[i])) "y1" else "y2"
  })
  table(source)
}

if(F){ ## Visualize
  library(ggplot2)
  df <- tibble::tibble(X, y1, y2, Y)
  ## trig
  ggplot(df, aes(x1, x2, color = y1)) + geom_point()
  ## quad
  ggplot(df, aes(x3, x4, color = y2)) + geom_point()
  
}

## Cheem functions
rf_fit  <- default_rf(X, Y); s();
shap_df <- attr_df_treeshap(rf_fit, X); s();
this_ls <- cheem_ls(X, Y,
                    model = rf_fit,
                    attr_df = shap_df)
if(F)
  global_view(this_ls)

## EXPORT OBJECTS ----
saveRDS(this_ls,
        file = "~/R/cheem/inst/shiny_apps/cheem_initial/data/preprocess_toy_mixture_regression.rds")
cat("Saved.\n")
if(F) ## Not run, load cheem_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/preprocess_toy_mixture_regression.rds")

