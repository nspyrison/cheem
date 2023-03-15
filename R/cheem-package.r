#' cheem
#'
#' Consider an arbitrary black-box model. Local explanations are a class of 
#' point-measures of the variable importance to the model for one location in 
#' the explanatory variables. The package `cheem` extracts the local attribution 
#' of all observations and first creates a 2D approximated space comparing 
#' data- and attribution-spaces. After a primary and comparison observation have
#' been selected the attribution of the primary is used as the 1D basis for a 
#' manual tour. This tour changes the contribution from the variable that 
#' deviates the most from its expected value. By viewing the positions of the 
#' primary and comparison points, the analyst can scrutinize the explanation 
#' identify variable attribution leading to a misclassification or large 
#' residual.
#' 
#' 
#' GitHub: \url{https://github.com/nspyrison/cheem}
#' 
#' @name cheem
#' @docType package
#' @seealso [cheem_ls()] or [run_app()] for help getting started
NULL


## Print message -----
#### prints upon first attaching the package
.onAttach <- function(...){
  packageStartupMessage("--------------------------------------------------------")
  packageStartupMessage("cheem --- version ", utils::packageVersion("cheem"))
  packageStartupMessage("Please share bugs, suggestions, and feature requests at:")
  packageStartupMessage("https://github.com/nspyrison/cheem/issues/")
  packageStartupMessage("--------------------------------------------------------")
  conflicted::conflict_prefer("run_app", "cheem", quiet = TRUE)
}


## Exports ------
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
NULL ## Required for roxygen2 to work, do not delete

## globals.R -----
globalVariables(c(
  "Feature", "split_index", "tree_index", ".", "node_parent",
  "default_left", "decision_type", "position", "cumulative",
  "prev", "text", "contribution", "var_value", "shap_value",
  "reorder", "variable", "importance", "Tree", "Missing",
  "Node", "Cover", "Yes", "No", "Prediction", "Decision.type"
  ))