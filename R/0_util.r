
## Small utility functions -----
#' Check if a vector is discrete
#' 
#' Whether or not a vector is a discrete variable, returns a logical.
#' Typically used on the Y variable of a model.
#' 
#' @param x A vector to check the discreteness of.
#' @return Logical, whether or not `x` is a discrete variable,
#' @export
#' @examples
#' is_discrete(mtcars$mpg)
#' is_discrete(mtcars$cyl) ## Numeric column, labeled as discrete, because less than 9 unique values
#' is_discrete(letters)
is_discrete <- function(x){
  is.factor(x) || is.character(x) || is.logical(x) ||
    (length(unique(x)) < 9L & is.numeric(x))
}

#' The type of model for a given Y variable.
#' 
#' Whether the Y is a "classification", "regression" or ill-defined problem.
#' Returns a character: "classification", "regression", or an error for strange 
#' classes. Minor redundancy with is_discrete, though explicit, and does check 
#' for strange classes.
#' @param y Response variable to be modeled
#' @return Character in c("classification", "regression")
#' @export
#' @examples
#' problem_type(mtcars$mpg)
#' problem_type(mtcars$cyl) ## Numeric column, labeled as discrete, because less than 9 unique values
#' problem_type(letters)
#TODO May want be deprecated in favor of is.discrete, this may be slightly safer if outside of the checks of is_discrete
problem_type <- function(y){
  if(is_discrete(y) == TRUE) return("classification")
  if(is.numeric(y) == TRUE)  return("regression")
  stop("y was expected as a with less than 9 unique values, or continuous numeric; indicating a classification or regression problem respectivly.")
}


#' Check if a vector contains non-numeric character
#' 
#' Returns a logical, whether or not a vector contains any non-numeric 
#' characters. Typically used to test if row names hold non-index information.
#' @param x A vector to be tested for existence of non-numeric characters.
#' @return Character in c("classification", "regression")
#' @export
#' @examples
#' does_contain_nonnumeric(mtcars$mpg)
#' does_contain_nonnumeric(rownames(mtcars)) ## Meaningful info to use in tooltip
#' does_contain_nonnumeric(rownames(cars)) ## Assume no meaningful info to use in tooltip
does_contain_nonnumeric <- function(x){
  suppressWarnings(any(is.na(as.numeric(as.character(x)))))
}

#' Draw new samples from the supplied data given its mean and covariances.
#' 
#' Creates new observation of the data given its specific means and shapes.
#' typically applied to a cluster subset of data. _ie_ draw from cluster 'a', 
#' then assign to cluster 'b'.
#' Returns a data frame with column names of the original data.
#' @param data A data.frame or matrix to sample from.
#' @param n_obs Number of new observations to draw. Defaults to 1.
#' @param var_coeff Variance coefficient, closer to 0 make points near the 
#' median, above 1 makes more points further away from the median. 
#' Defaults to 1.
#' @return Character in c("classification", "regression")
#' @export
#' @examples
rnorm_from <- function(data, n_obs = 1, var_coeff = 1){
  .mns <- apply(data, 2L, median)
  .cov <- cov(data, method = "pearson")
  diag(.cov) <- var_coeff * diag(.cov) ## Decrease univariate variance if needed.
  ## person numeric, not spearman ranked/ordinal
  
  ## Sample
  ret <- mvtnorm::rmvnorm(n = n_obs,
                          mean = .mns,
                          sigma = .std_cov)
  return(as.data.frame(data))
}
