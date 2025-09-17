# Build notes: -----
## with the addition of Rcpp content the workflow is a little more complex
## I think best practice is something like:

## OUTDATED FROM CPP USEAGE:
# message(paste0("Build started at ", Sys.time()))
# Rcpp::compileAttributes()
# ## Create "packagename_init.c"; should be okay with Rcpp::compileAttributes()
# Kmisc::registerFunctions(prefix = "")
# devtools::document()
# ## Needed for rebuild after adding Rcpp treeshap content
# rstudioapi::restartSession()
# message("Not sure this replaces exisiting package, use Build tab > Install and Restart")
# #devtools::install()
# beepr::beep(1)
# 
# message("I have been running into hung session from check, goodluck below.")
# ## Stopping from stalled console run gives the dreaded, ambiguous: 
# #Error in process_get_error_connection(self, private) : 
# #stderr is not a pipe.
# #devtools::check_rhub()
# devtools::check()
# beepr::beep(2)
# message("Note that tests seem to be working")
# devtools::test()


## Also consider:
if(F){
  devtools::build_site()
  
  ## This will create a file "packagename_init.c":
  #devtools::install_github("kevinushey/Kmisc")
  Kmisc::registerFunctions(prefix="")
  ## but idk if this is good to use with Rcpp::compileAttributes()
  
  ## check the registered routines with:
  getDLLRegisteredRoutines("cheem")
}

## Development build and check reminder
if(F){
  devtools::document()
  devtools::build()
  devtools::check()
  
  rhub::rhub_check()
  
  devtools::submit_cran()
}