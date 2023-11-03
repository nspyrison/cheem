## Ames housing 2018 ----
#' Ames housing data 2018
#' 
#' House sales prices from Ames, Iowa, USA between 2006 and 2010. Only complete 
#' numeric observations remain.
#' 
#' \describe{
#'   \item{amesHousing2018}{Complete data.frame, n = 2291, 18 numeric variable 
#'   (including 2 temporal: MoSold, YrSold ), response variable SalePrice, 
#'   3 class factors.}
#'   \item{amesHousing2018_NorthAmes}{A simplified subsample, just North Ames 
#'   (largest neighborhood). Complete data.frame, n = 338, 9 numeric variables, 
#'   response variable SalePrice, 1 class factor SubclassMS, a zoning subclass.}
#'   \item{amesHousing2018_raw}{Original data from Kaggle, 2930 rows of 82 
#'   variables. Sparse rows (639) and sparse/defaulted columns (64) are removed.}
#' }
#' 
#' No data dictionary is provided on Kaggle, but amesHousing2018 variables 
#' are inferred to be:
#' \itemize{
#'   \item LotFrontage, Length of the front (street facing) side of the lot 
#'   in yards (0.914m)
#'   \item LotArea, Area of the lot in square yards (0.836m^2)
#'   \item OverallQual, Overall quality (of the house?), integer in (1, 10)
#'   \item OverallCond, Overall condition (of the lot?), integer in (1, 10)
#'   \item YearBuild, The year the house was originally built
#'   \item BsmtUnfArea, Unfinished basement area, in square yards (0.836m^2)
#'   \item TotBsmtArea, Total basement area, in square yards (0.836m^2)
#'   \item 1stFlrArea,  First (ground) floor living area in square yards (0.836m^2)
#'   \item LivingArea, Total living area in square yards (0.836m^2)
#'   \item Bathrms, The number of bathrooms
#'   \item Bedrms, The number of bedrooms
#'   \item TotRms, The total number of rooms
#'   \item GarageYrBlt, The year the garage was build
#'   \item GarageCars, The number of car spaces in the garage
#'   \item GarageArea, The area of the garage in square yards (0.836m^2)
#'   \item MoSold, The number of the month of the house sale
#'   \item YrSold, The number of the year of the house sale
#'   \item SalePrice, The sale of the house in USD (as of the year of sale?)
#'   \item SubclassMS, Factor subclass of construction zone, 16 levels
#'   \item SubclassMS, Factor major class of construction zone, 7 levels
#'   \item Neighborhd, Factor neighborhood of Ames, IA, 28 levels
#' }
#' 
#' @format complete data.frame with 2291 rows and 18 numeric variables, 
#' SalesPrice, the response variable, and 3 class variables
#' @source De Cock, D. (2011). "Ames, Iowa: Alternative to the Boston Housing Data as an End of Semester Regression Project," \emph{Journal of Statistics Education},  Volume 19, Number 3.
#' \url{http://jse.amstat.org/v19n3/decock/DataDocumentation.txt}
#' \url{http://jse.amstat.org/v19n3/decock.pdf}
#' @source {Kaggle, Ames Housing Dataset} 
#' \url{https://www.kaggle.com/prevek18/ames-housing-dataset}
#' 
#' __Replicating this dataset:__
#' ```
#' if(FALSE) ## Don't accidentally open the URL.
#'   browseURL("https://www.kaggle.com/prevek18/ames-housing-dataset")
#' ames <- readr::read_csv("./buildignore/AmesHousing.csv")
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
#' col_idx_char <- which(names(ames_clas) %in%
#'                         c("MS.SubClass", "MS.Zoning", "Neighborhood"))
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
#'   amesHousing2018_NorthAmes$SubclassMS,
#'   unique(amesHousing2018_NorthAmes$SubclassMS))
#' if(F){ ## Don't accidentally save
#'   save(amesHousing2018_NorthAmes, file = "./data/amesHousing2018_NorthAmes.rda")
#' ```
#' @keywords datasets
#' @examples
#' library(cheem)
#' 
#' ## Regression setup:
#' dat  <- amesHousing2018_NorthAmes
#' X    <- dat[, 1:9]
#' Y    <- dat$SalePrice
#' clas <- dat$SubclassMS
#' 
#' ## Cheem list
#' ames_rf_chm <- cheem_ls(X, Y, ames_rf_shap, ames_rf_pred, clas,
#'                         label = "North Ames, RF, SHAP")
#' ## Cheem visuals
#' if(interactive()){
#'   prim <- 1
#'   comp <- 2
#'   global_view(ames_rf_chm, primary_obs = prim, comparison_obs = comp)
#'   bas <- sug_basis(ames_rf_chm, prim, comp)
#'   mv  <- sug_manip_var(ames_rf_chm, primary_obs = prim, comp)
#'   ggt <- radial_cheem_tour(ames_rf_chm, basis = bas, manip_var = mv)
#'   animate_plotly(ggt)
#' }
#' 
#' ## Save for use with shiny app (expects an rds file)
#' if(FALSE){ ## Don't accidentally save.
#'   saveRDS(ames_rf_chm, "./NAmes_rf_tshap.rds")
#'   run_app() ## Select the saved rds file from the data drop down.
#' }
"amesHousing2018"

#' @rdname amesHousing2018
"amesHousing2018_raw"

#' @rdname amesHousing2018
"amesHousing2018_NorthAmes"


## Chocolates -----
#' Chocolates dataset
#' 
#' The chocolates data was compiled by students at Iowa State University of 
#' STAT503 (circa 2015) taught by Dianne Cook. Nutrition label information
#' on the chocolates as listed on manufacturer websites. 
#' All numbers were normalized to be equivalent to a 100g serving. 
#' Units of measurement are listed in the variable name.
#' 
#' @format  A complete data.frame with 88 observations and 10 numeric variables,
#' name of the chocolate, manufacturer, country, and type of the chocolate.
#' \itemize{
#'   \item Name, the name of the chocolate
#'   \item MFR, chocolate manufacturer
#'   \item Country, the country the manufacturer is incorporated.
#'   \item Type, the type of chocolate according to the website, either 'Dark' 
#'   or 'Milk"
#'   \item Calories, the number of calories per 100 grams
#'   \item CalFat, calories from fat per 100 grams
#'   \item TotFat_g, grams of total fat per 100 grams
#'   \item SatFat_g, grams of saturated fat per 100 grams
#'   \item Chol_mg, milligrams of cholesterol per 100 grams
#'   \item Na_mg, milligrams of sodium (salt) per 100 grams
#'   \item Carbs_g, grams of carbohydrates per 100 grams
#'   \item Fiber_g, grams of fiber per 100 grams
#'   \item Sugars_g, grams of sugar per 100 grams
#'   \item Protein_g, grams of sugar per 100 grams
#' }
#' @source {Monash University, Introduction to Machine Learning course} \url{https://iml.numbat.space/}
#' 
#' __Replicating this dataset:__
#' ```
#' if(FALSE) ## Don't accidentally open the URL.
#'   browseURL("https://iml.numbat.space/")
#' ## Accessed Jan 2022
#' chocolates <- readr::read_csv("https://iml.numbat.space/data/chocolates.csv")
#' chocolates <- data.frame(chocolates)
#' chocolates[, 2] <- factor(chocolates[, 2])
#' chocolates[, 3] <- factor(chocolates[, 3])
#' chocolates[, 4] <- factor(chocolates[, 4])
#' if(F){ ## Don't accidentally save
#'   save(chocolates, file = "./data/chocolates.rda")
#' ```
#' @examples
#' library(cheem)
#' 
#' ## Classification setup
#' X    <- chocolates[, 5:14]
#' Y    <- chocolates$Type
#' clas <- chocolates$Type
#' 
#' ## Cheem
#' choc_chm <- cheem_ls(X, Y, chocolates_svm_shap, chocolates_svm_pred, clas,
#'                      label = "Chocolates, LM, shap")
#' 
#' ## Save for use with shiny app (expects an rds file)
#' if(FALSE){ ## Don't accidentally save.
#'   saveRDS(choc_chm, "./chocolates_svm_shap.rds")
#'   run_app() ## Select the saved rds file from the data dropdown.
#' }
#' 
#' ## Cheem visuals
#' if(interactive()){
#'   prim <- 1
#'   comp <- 2
#'   global_view(peng_chm, primary_obs = prim, comparison_obs = comp)
#'   bas <- sug_basis(peng_xgb_shap, prim, comp)
#'   mv  <- sug_manip_var(peng_xgb_shap, primary_obs = prim, comp)
#'   ggt <- radial_cheem_tour(peng_chm, basis = bas, manip_var = mv)
#'   animate_plotly(ggt)
#' }
"chocolates"