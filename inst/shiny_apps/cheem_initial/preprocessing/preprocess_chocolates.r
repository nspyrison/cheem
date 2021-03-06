## Dependencies ------
{
  require(cheem)
  s <- function(sec = .01)Sys.sleep(sec)
  
  ## Create the data & shap layer_ls -----
  if(F){
    chocolates <- readr::read_csv('https://iml.numbat.space/data/chocolates.csv')
    saveRDS(chocolates,
            file = "~/R/cheem/inst/shiny_apps/cheem_initial/data/chocolates_raw.rds")
  }
  clas <- factor(chocolates$Type, levels = rev(unique(chocolates$Type)))
  lvls <- levels(clas)
  X <- chocolates[, 5:14] %>% as.data.frame() ## X's not scaled.
  colnames(X) <- gsub("\\_.*", "", colnames(X))
  nm_imc <- paste(chocolates$Name, chocolates$MFR, chocolates$Country, sep = ", ")
  r_idx <- which(nm_imc == "85% Cocoa Dark French Chocolate, Thorntons, UK")[2L]
  nm_imc[r_idx] <- paste0(nm_imc[r_idx], " (2nd)")
  row.names(X) <- nm_imc
  Y <- as.integer(clas)
  
  rf_fit  <- default_rf(X, Y)
  ## Long runtime for full datasets:
  shap_df <- attr_df_treeshap(rf_fit, X, verbose = TRUE, noisy = FALSE)
  this_ls <- cheem_ls(
    x = X, y = Y, class = clas,
    model = rf_fit, attr_df = shap_df)
}

## EXPORT OBJECTS ----
saveRDS(this_ls,
        file = "~/R/cheem/inst/shiny_apps/cheem_initial/data/preprocess_chocolates.rds")
cat("Saved.\n")
if(F) ## Not run, load this_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/preprocess_chocolates.rds")

