#' Trees of {cheem}
#'
#' Consider an arbitrary black-box model. Local explanations are a class of 
#' point-measures of the variable importance to the model for one location in 
#' the explanatory variables. `Cheem` extracts the local attribution of all 
#' observations and first creates a 2D approximated space comparing data- and
#' attribution-spaces. After a primary and comparison observation have been 
#' selected the attribution of the primary is used as the 1D basis for a manual 
#' tour. This tour changes the contribution from the variable that deviates the
#' most from its expected value. By viewing the positions of the  primary and 
#' comparison points, one can interrogate the explanation identify improperly 
#' weighted variable especially in the case of of a misclassified point as 
#' compared with a correctly classified neighbor.
#' 
#' 
#' GitHub: \url{https://github.com/nspyrison/cheem}
#' 
#' @name cheem
#' @docType package
#' @seealso [nested_local_attr_layers()] [run_app()]
NULL

## Print message -----
#### prints upon first attaching the package
.onAttach <- function(...){
  packageStartupMessage("--------------------------------------------------------")
  packageStartupMessage("cheem --- version ", utils::packageVersion("cheem"))
  packageStartupMessage("Please share bugs, suggestions, and feature requests at:")
  packageStartupMessage("https://github.com/nspyrison/cheem/issues/")
  packageStartupMessage("--------------------------------------------------------")
}

## Exports ------
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
NULL ## Required for roxygen2 to work, do not delete
