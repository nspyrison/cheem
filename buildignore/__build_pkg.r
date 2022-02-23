# Build notes: -----
## with the addition of Rcpp content the workflow is a little more complex
## I think best practice is something like:

message(paste0("Build started at ", Sys.time()))
Rcpp::compileAttributes()
devtools::document()
## needed for rebuild after adding Rcpp treeshap content
rstudioapi::restartSession()
devtools::build()
beepr::beep(1)
devtools::check()
beepr::beep(2)


## Also consider
devtools::build_site()
