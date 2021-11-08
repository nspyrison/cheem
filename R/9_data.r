## Ames housing 2018 ----
#' Ames housing data 2018
#' 
#' Cleaned house price data for Ames, Iowa, USA from 2018. Only complete 
#' numeric observations remain.
#' @format A data frame with 2290 rows and 19 numeric variables, SalesPrice, the response variable, and 3 class variables
#' @source {Kaggle, Ames Housing Dataset} \url{https://www.kaggle.com/prevek18/ames-housing-dataset}
#' Replicating this dataset:
#' ```
#' if(F) browseURL("https://www.kaggle.com/prevek18/ames-housing-dataset")
#' ames <- readr::read_csv("./ignore/AmesHousing.csv")
#' amesHousing2018_raw <- data.frame(ames)
#' save(amesHousing2018_raw, file = "./data/amesHousing2018_raw.rda")
#' 
#' ## Complete rows and numeric variables
#' ames1 <- ames[, unlist(lapply(ames, is.numeric))]
#' ames1$Bathrooms <- ames1$`Full Bath` + ames1$`Half Bath`
#' ames1 <- ames1[, c(1:18, 38, 19:37)]
#' col_idx <- !(colnames(ames1) %in%
#'                c("Order", "Mas Vnr Area", "BsmtFin SF 1", "BsmtFin SF 2",
#'                  "Bsmt Full Bath", "Bsmt Half Bath", "Fireplaces",
#'                  "Wood Deck SF", "Open Porch SF", "Enclosed Porch", 
#'                  "3Ssn Porch", "Screen Porch", "Pool Area", "Misc Val",
#'                  "2nd Flr SF", "Low Qual Fin SF", "Full Bath", "Half Bath",
#'                  "Kitchen.AbvGr"))
#' row_idx <- !is.na(ames1$"Garage Yr Blt") &
#'   !is.na(ames1$"Lot Frontage") &
#'   !is.na(ames1$"Bsmt Unf SF") &
#'   !is.na(ames1$"Total Bsmt SF")
#' ames2 <- ames1[row_idx, col_idx]
#' 
#' ## Looking for character classes to keep:
#' ames_char <- ames[, unlist(lapply(ames, is.character))]
#' ames_clas <- data.frame(lapply(ames_char, factor))[, -1]
#' ames_clasint <- data.frame(lapply(ames_clas, as.integer))
#' col_idx_char <- which(names(ames_clas) %in% c("MS.SubClass", "MS.Zoning", "Neighborhood"))
#' classes <- ames_clas[row_idx, col_idx_char]
#' 
#' amesHousing2018 <- data.frame(ames2, classes)[-147, ] ## huge residual
#' ## save(amesHousing2018, file = "./data/amesHousing2018.rda")
#' 
#' .thin_col_idx <- names(amesHousing2018) %in% c(
#'   "Lot.Frontage", "Lot.Area", "Overall.Qual", "Year.Built", "Total.Bsmt.SF",
#'   "Gr.Liv.Area", "Bedroom.AbvGr", "TotRms.AbvGrd",
#'   "Garage.Yr.Blt", "Garage.Cars", "SalePrice", "MS.Zoning")
#' amesHousing2018_thin <- amesHousing2018[, .thin_col_idx]
#' ## save(amesHousing2018_thin, file = "./data/amesHousing2018_thin.rda")
#' ```
#' @examples
#' library(cheem)
#' ## Regression:
#' sub <- amesHousing2018_thin[1:200, ]
#' X <- sub[, 1:10]
#' Y <- log(sub$SalePrice)
#' clas <- sub$MS.Zoning
#' 
#' rf_fit <- default_rf(X, Y)
#' shap_df <- attr_df_treeshap(rf_fit, X) ## Long runtime!
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                     model = rf_fit,
#'                     attr_df = shap_df)
#'  
#' linked_global_view(this_ls)
#' 
#' ## Save for used with shiny app (expects .rds):
#' if(F) ## Don't accidentally save.
#'   saveRDS(out_ls, "./my_cheem_ls.rds")
#"amesHousing2018_raw"
"amesHousing2018"
"amesHousing2018_thin"
