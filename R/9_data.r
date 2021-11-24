## Ames housing 2018 ----
#' Ames housing data 2018
#' 
#' Cleaned house price data for Ames, Iowa, USA from 2018. Only complete 
#' numeric observations remain.
#' 
#'  \describe{
#'   \item{amesHousing2018}{Complete data.frame, n = 2291, 18 numeric variable 
#'   (including 2 temporal: MoSold, YrSold ), response variable SalePrice, 
#'   3 class factors.}
#'   \item{amesHousing2018_NorthAmes}{A simplified subsample, just North Ames 
#'   (largest neighborhood). Complete data.frame, n = 338, 9 numeric variables, response variable SalePrice, 
#'   1 class factor SubclassMS, a zoning subclass.}
#'   \item{amesHousing2018_raw}{Original data from Kaggle before sparse rows
#'   (~700) and sparse columns (60) are removed, 2930 rows of 82 
#'   variables. Included for transparency or extracting information from 
#'   sparse rows/columns.}
#' }
#' 
#' @format  A complete data.frame with 2291 rows and 18 numeric variables, SalesPrice, the response variable, and 3 class variables
#' @source {Kaggle, Ames Housing Dataset} \url{https://www.kaggle.com/prevek18/ames-housing-dataset}
#' Replicating this dataset:
#' ```
#' if(FALSE) ## Don't accidentally open the URL.
#'   browseURL("https://www.kaggle.com/prevek18/ames-housing-dataset")
#' ames <- readr::read_csv("./ignore/AmesHousing.csv")
#' amesHousing2018_raw <- data.frame(ames)
#' ## save(amesHousing2018_raw, file = "./data/amesHousing2018_raw.rda")
#' 
#' ## Complete rows and numeric variables
#' ames1 <- ames[, unlist(lapply(ames, is.numeric))]
#' ames1$Bathrooms <- ames1$`Full Bath` + ames1$`Half Bath`
#' ames1 <- ames1[, c(1:18, 38, 19:37)]
#' col_idx <- !(colnames(ames1) %in% c(
#'   "Order", "Mas Vnr Area", "BsmtFin SF 1", "BsmtFin SF 2",
#'   "Bsmt Full Bath", "Bsmt Half Bath", "Fireplaces",
#'   "Wood Deck SF", "Open Porch SF", "Enclosed Porch",
#'   "3Ssn Porch", "Screen Porch", "Pool Area", "Misc Val", "2nd Flr SF",
#'   "Low Qual Fin SF", "Full Bath", "Half Bath", "Kitchen AbvGr"))
#' row_idx <- !is.na(ames1$"Garage Yr Blt") &
#'   !is.na(ames1$"Lot Frontage") &
#'   !is.na(ames1$"Bsmt Unf SF") &
#'   !is.na(ames1$"Total Bsmt SF")
#' ames2 <- as.data.frame(ames1[row_idx, col_idx])
#' 
#' ## Looking for character classes to keep:
#' ames_char <- ames[, unlist(lapply(ames, is.character))]
#' ames_clas <- as.data.frame(lapply(ames_char, factor))[, -1]
#' ames_clasint <- data.frame(lapply(ames_clas, as.integer))
#' col_idx_char <- which(names(ames_clas) %in% c("MS.SubClass", "MS.Zoning", "Neighborhood"))
#' classes <- ames_clas[row_idx, col_idx_char]
#' 
#' amesHousing2018 <- cbind(ames2, classes)
#' names(amesHousing2018) <- c(
#'   "LotFrontage", "LotArea","OverallQual", "OverallCond", "YearBuild",
#'   "YearRemod", "BsmtUnfArea", "TotBsmtArea", "1stFlrArea", "LivingArea",
#'   "Bathrms", "Bedrms", "TotRms", "GarageYrBlt", "GarageCars", "GarageArea",
#'   "MoSold", "YrSold", "SalePrice", "SubclassMS", "ZoneMS", "Neighborhd")
#' ## save(amesHousing2018, file = "./data/amesHousing2018.rda")
#' 
#' .thin_col_idx <- names(amesHousing2018) %in% c(
#'   "LotArea", "OverallQual", "YearBuild",
#'   "LivingArea", "Bathrms", "Bedrms", "TotRms",
#'   "GarageYrBlt", "GarageArea", "SalePrice", "SubclassMS")
#' amesHousing2018_thin <- amesHousing2018[, .thin_col_idx]
#' 
#' ## subset to north ames, and only 5 largest subclasses
#' r_idx <- amesHousing2018$Neighborhd == "NAmes" &
#'   amesHousing2018$SubclassMS %in% c("020", "050", "080", "090", "060")
#' amesHousing2018_NorthAmes <- amesHousing2018_thin[r_idx, ]
#' amesHousing2018_NorthAmes$SubclassMS <- factor(
#'   amesHousing2018_NorthAmes$SubclassMS, levels = unique(amesHousing2018_NorthAmes$SubclassMS))
#' ## save(amesHousing2018_NorthAmes, file = "./data/amesHousing2018_NorthAmes.rda")
#' ```
#' @examples
#' library(cheem)
#' ## Regression:
#' sub <- amesHousing2018_NorthAmes[1:200, ]
#' X <- sub[, 1:9]
#' Y <- log(sub$SalePrice)
#' clas <- sub$SubclassMS
#' 
#' rf_fit  <- default_rf(X, Y)
#' ## Long runtime for full datasets:
#' shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
#' this_ls <- cheem_ls(X, Y, class = clas,
#'                      model = rf_fit,
#'                      attr_df = shap_df)
#'  
#' global_view(this_ls)
#' 
#' ## Save for used with shiny app (expects .rds):
#' if(FALSE) ## Don't accidentally save.
#'   saveRDS(out_ls, "./my_cheem_ls.rds")
"amesHousing2018"

#' @rdname amesHousing2018
"amesHousing2018_raw"

#' @rdname amesHousing2018
"amesHousing2018_NorthAmes"