## Dependencies ------
{
  require(cheem)
  s <- function(sec = .01) Sys.sleep(sec)
  
  ## Data preprocess ------
  ## Data setup, spinifex::penguins_na.rm
  raw <- spinifex::penguins_na.rm
  clas <- raw$species
  lvls <- levels(clas)
  # ## Filter to closest 2 classes
  # raw <- raw[raw$species %in% lvls[1:2], ]
  # clas <- factor(raw$species, levels = lvls[1:2]) ## Manually remove 3rd lvl
  X <- raw[, 1:4] %>% as.data.frame() ## X's not scaled.
  colnames(X) <- c("bl", "bd", "fl", "bm")
  Y <- as.integer(raw$species)
}

rf_fit  <- default_rf(X, Y); s();
shap_df <- attr_df_treeshap(rf_fit, X, verbose = TRUE); s();
this_ls <- cheem_ls(X, Y, class = clas,
                    model = rf_fit,
                    attr_df = shap_df)
names(this_ls)

## EXPORT OBJECTS ----
saveRDS(this_ls,
        file = "~/R/cheem/inst/shiny_apps/cheem_initial/data/preprocess_penguins.rds")
cat("Saved.\n")
if(F) ## Not run, load this_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/preprocess_penguins.rds")

