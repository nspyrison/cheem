# Build notes: -----
## with the addition of Rcpp content the workflow is a little more complex
## I think best practice is something like:

message(paste0("Build started at ", Sys.time()))
Rcpp::compileAttributes()
## Create "packagename_init.c"; should be okay with Rcpp::compileAttributes()
Kmisc::registerFunctions(prefix = "")
devtools::document()
## Needed for rebuild after adding Rcpp treeshap content
rstudioapi::restartSession()
devtools::install()
beepr::beep(1)
devtools::check()
beepr::beep(2)


## Also consider
devtools::build_site()

## This will create a file "packagename_init.c":
#devtools::install_github("kevinushey/Kmisc")
Kmisc::registerFunctions(prefix="")
## but idk if this is good to use with Rcpp::compileAttributes()

## check the registered routines with:
getDLLRegisteredRoutines("cheem")
