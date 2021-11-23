## Dependencies ------
{
  require(cheem)
  s <- function(sec = .01)Sys.sleep(sec)
  
  ## Data simulation ------
  set.seed(20211105)
  .n <- 50  ## obs per class
  .m <- 3.5 ## mean difference between class
  ############## clas a:         clas b:         clas c:
  sim_EEE_p4 <- data.frame(
    x1 = c(rnorm(.n,0),    rnorm(.n,.m),   rnorm(.n,.m/2)), ## Separates A from B
    x2 = c(rnorm(.n,0),    rnorm(.n,0),    rnorm(.n,sqrt(.m^2 + (.m/2)^2))), ## Separates C from A&B
    x3 = c(rnorm(.n,0,.2), rnorm(.n,0,.2), rnorm(.n,0,.2)), ## noise, half sd
    x4 = c(rnorm(.n,0,.2), rnorm(.n,0,.2), rnorm(.n,0,.2))) ## noise, half sd
  attr(sim_EEE_p4, "cluster") <- factor(rep(LETTERS[1:3], each = .n))
  
  ## Visualize
  if(F){
    str(sim_EEE_p4)
    require(ggplot2)
    ggplot(sim_EEE_p4, aes(x1, x2, color = clas, shape = clas)) + geom_point()
  }
}

## Create the data & shap layer_ls -----
#cr_simulation() ## see obj `sim_EEE_p4`
X <- sim_EEE_p4
clas <- attr(sim_EEE_p4, "cluster")
Y <- as.integer(clas)

rf_fit  <- default_rf(X, Y); s();
shap_df <- attr_df_treeshap(rf_fit, X); s();
this_ls <- cheem_ls(X, Y, class = clas,
                    model = rf_fit,
                    attr_df = shap_df)
names(this_ls)

## EXPORT OBJECTS ----
setwd("~/R/cheem")
saveRDS(this_ls,
        file = "./inst/shiny_apps/cheem_initial/data/preprocess_toy_classification.rds")
cat("Saved.\n")
if(F) ## Not run, load this_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/preprocess_toy_classification.rds")

