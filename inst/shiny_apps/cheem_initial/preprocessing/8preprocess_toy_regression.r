## Dependencies ------
## Local files
require("cheem")


#### A simplified version
set.seed(20211105)
X <- data.frame(x1 = rnorm(200, 5, 1),
                x2 = rnorm(200, 5, 1),
                x3 = rnorm(200, 5, 1),
                x4 = rnorm(200, 5, 1))
Y <- X$x1 + X$x2 + (X$x1 * X$x2) + .1 * X$x3 + .1 * X$x4

## cheem_ls -----
cheem_ls <- cheem_ls(
  x = X, y = Y, basis_type = "pca", class = NULL)
names(cheem_ls)


## EXPORT OBJECTS ----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  saveRDS(cheem_ls,
          file = "./inst/shiny_apps/cheem_initial/data/8preprocess_toy_regression.rds")
}
if(F) ## Not run, load cheem_ls
  cheem_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/8preprocess_toy_regression.rds")

