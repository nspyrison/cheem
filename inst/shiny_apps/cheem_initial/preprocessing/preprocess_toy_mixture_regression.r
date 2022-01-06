{
  require(cheem)
  s <- function(sec = 1)Sys.sleep(sec)
  
  set.seed(20211105)
  n_obs <- 240
  X <- tibble::tibble(
    x1 = runif(n_obs, 0, 5),
    x2 = runif(n_obs, 0, 5),
    x3 = runif(n_obs, 0, 5),
    x4 = runif(n_obs, 0, 5),
    x5 = runif(n_obs, 0, 5))
  n_thrid <- n_obs / 3
  idx1 <- 0 * n_thrid + 1:n_thrid
  idx2 <- 1 * n_thrid + 1:n_thrid
  idx3 <- 2 * n_thrid + 1:n_thrid
  Y <- c(
    c(X$x1^2 + (X$x2 + X$x3 + X$x4 + X$x5) / 10)[idx1] + rnorm(n_thrid),
    c(X$x2^2 + (X$x1 + X$x3 + X$x4 + X$x5) / 10)[idx2] + rnorm(n_thrid),
    c(X$x3^2 + (X$x1 + X$x2 + X$x4 + X$x5) / 10)[idx3] + rnorm(n_thrid)
  )
  
  ### First attempt was ~1/2 sin in x1, x2, 1/2 exponential
  # X <- tibble::tibble(
  #   x1 = runif(n_obs, 0, 4 * pi),
  #   x2 = runif(n_obs, 0, 4 * pi),
  #   x3 = runif(n_obs, 0, 3),
  #   x4 = runif(n_obs, 0, 3),
  #   x5 = runif(n_obs, 0, 1))
  # y1 <- 2 + sin(X$x1) + sin(X$x2) + X$x3/30 + X$x4/30 + .1 * X$x5 + rnorm(n_obs, sd = .1)
  # y2 <- X$x1/(40*pi) + X$x2/(40*pi) + (X$x3 * X$x4) + .1 * X$x5 + rnorm(n_obs, sd = .1)
  # id <- 1:n_obs
  # Y <-  NULL
  # source <- NULL
  # .m <- sapply(1:n_obs, function(i){
  #   Y[i] <<- max(y1[i], y2[i])
  #   source[i] <<- if(y1[i] == max(y1[i], y2[i])) "y1" else "y2"
  # })
  # table(source)
}

if(F){ ## Visualize
  library(ggplot2)
  skimr::skim(df)
  
  df <- tibble::tibble(X, Y) %>% spinifex::scale_sd()
  gt_path <- save_history(df, grand_tour(), max_bases = 10, rescale = FALSE)
  
  ggt <- ggtour(gt_path) + proto_default(aes_args = list(color = Y))
  animate_plotly(ggt)
}

## Cheem functions
rf_fit  <- default_rf(X, Y); s();
shap_df <- attr_df_treeshap(rf_fit, X, verbose = TRUE); s();
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

