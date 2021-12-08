## Utility functions -----

#' Check if a vector is discrete
#' 
#' Whether or not a vector is a discrete variable, returns a logical.
#' Typically used on the Y variable of a model.
#' 
#' @param x A vector to check the discreteness of.
#' @param na.rm Whether or not to remove NA values before testing discretness.
#' Defaults to TRUE.
#' @return Logical, whether or not `x` is a discrete variable.
#' @export
#' @family cheem utility
#' @examples
#' is_discrete(mtcars$mpg) ## Numeric column, too many levels for discretness
#' is_discrete(mtcars$cyl) ## Numeric column, labeled as discrete, because less than 25 unique values
#' is_discrete(letters)    ## Characters and factors labeled discrete.
is_discrete <- function(x, na.rm = TRUE){
  x <- x[is.na(x) == FALSE] ## Remove NAs
  is.factor(x) || is.character(x) || is.logical(x) ||
    (length(unique(x)) < 25L & is.numeric(x))
}

#' Check if a vector diverges a value
#' 
#' Whether or not a vector is a diverges a value, returns a logical.
#' Used to help default a scale_color for ggplot2.
#' 
#' @param x A vector to check the divergence of.
#' @param mid_pt A single number checking divergence from. Defaults to 0.
#' @return Logical, whether or not `x` is a diverges `mid_pt`.
#' @export
#' @family cheem utility
#' @examples
#' is_diverging(-10:10)
#' is_diverging(-10:-5)
#' is_diverging(mtcars$mpg, 25)
#' is_diverging(mtcars$mpg, 40)
is_diverging <- function(x, mid_pt = 0){
  x <- x[is.na(x) == FALSE] ## Remove NAs
  max(x) > mid_pt & min(x) < mid_pt
}

#' Suggest a color and fill scale.
#' 
#' Whether or not a vector is a diverges a value, returns a logical.
#' Used to help default a scale_color for ggplot2.
#' 
#' @param x A vector to to scale the color to.
#' @param mid_pt A single number checking divergence from. Defaults to 0.
#' @param limits A vector of the min and max values for the scale. 
#' Useful for setting an absolute range, such as (-1, 1) for attribution/y 
#' correlation of each point. Points outside of limits show as default grey.
#' Defaults to NULL; the range of x.
#' @param ... Optional other arguments passed to ggplot2::continuous_scale 
#' or ggplot2::discrete_scale.
#' @return A list containing a scale_color, and scale_fill; 
#' the suggested color/fill scale for a ggplot. 
#' @export
#' @aliases colour_scale_of
#' @family cheem utility
#' @examples
#' library(cheem)
#' library(ggplot2)
#' g <- ggplot(mtcars, aes(disp, mpg))
#' 
#' ## Discrete
#' g + geom_point(aes(color = factor(vs))) +
#'   color_scale_of(mtcars$vs) 
#' ## Sequential increasing
#' g + geom_point(aes(color = mpg)) +
#'   color_scale_of(mtcars$mpg)
#' ## Dummy sequential decr
#' g + geom_point(aes(color = -1 *mpg)) +
#'   color_scale_of(-1 * mtcars$mpg)
#' ## Dummy diverging
#' g + geom_point(aes(color = mpg - median(mpg))) +
#'   color_scale_of(mtcars$mpg - median(mtcars$mpg))
#' ## Dummy limits
#' g + geom_point(aes(color = mpg - median(mpg))) +
#'   color_scale_of(mtcars$mpg - median(mtcars$mpg), limits = c(-5, 5))
color_scale_of <- function(x, mid_pt = 0, limits = NULL, ...){
  x <- x[is.na(x) == FALSE] ## Remove NAs
  b <- "blue3" #scales::muted("blue")
  g <- "grey80"
  r <- "red3" #scales::muted("red")
  if(is_discrete(x)){
    ret <- list(ggplot2::scale_color_brewer(palette = "Dark2", ...),
                ggplot2::scale_fill_brewer( palette = "Dark2", ...))
  }else if(is_diverging(x, mid_pt)){
    ## Diverging
    ret <- list(
      ggplot2::scale_color_gradient2(
        low = b, mid = g, high = r, limits = limits, ...),
      ggplot2::scale_fill_gradient2( 
        low = b, mid = g, high = r, limits = limits, ...))
  }else if(all(x >= mid_pt)){
    ## Sequential above mid_pt
    ret <-  ret <- list(
      ggplot2::scale_color_gradient(low = g, high = r, limits = limits, ...),
      ggplot2::scale_fill_gradient( low = g, high = r, limits = limits, ...))
  }else{
    ## Sequential below mid_pt
    ret <- list(
      ggplot2::scale_color_gradient(low = b, high = g, limits = limits, ...),
      ggplot2::scale_fill_gradient( low = b, high = g, limits = limits, ...))
  }
  ret
}

#' The type of model for a given Y variable
#' 
#' Whether the Y is a "classification", "regression" or ill-defined problem.
#' Returns a character: "classification", "regression", or an error for strange 
#' classes. Minor redundancy with is_discrete, though explicit. Could be useful
#' for `DALEX::explain(type)` as it also expects "classification" or 
#' "regression".
#' @param y Response variable to be modeled
#' @return Character in c("classification", "regression")
#' @export
#' @family cheem utility
#' @examples
#' problem_type(mtcars$mpg)
#' problem_type(mtcars$cyl) ## Numeric column, labeled as discrete, because less than 25 unique values
#' problem_type(letters)
problem_type <- function(y){
  if(is_discrete(y) == TRUE) return("classification")
  if(is.numeric(y) == TRUE)  return("regression")
  stop("y was expected as a with less than 25 unique values, or continuous numeric; indicating a classification or regression problem respectivly.")
}


#' Check if a vector contains non-numeric character
#' 
#' Returns a logical, whether or not a vector contains any non-numeric 
#' characters. Typically used to test if row names hold non-index information.
#' 
#' @param x A vector to be tested for existence of non-numeric characters.
#' @return Character in c("classification", "regression")
#' @export
#' @family cheem utility
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
#' 
#' @param data A data.frame or matrix to sample from.
#' @param n_obs Number of new observations to draw. Defaults to 1.
#' @param var_coeff Variance coefficient, closer to 0 make points near the 
#' median, above 1 makes more points further away from the median. 
#' Defaults to 1.
#' @return A data.frame with column names of the original data.
#' @export
#' @family cheem utility
#' @examples
#' sub <- mtcars[mtcars$cyl == 6, ]
#' ## Draw 3 new observations in the shape of 6 cylinder vehicles, with reduced variance.
#' rnorm_from(data = sub, n_obs = 3, var_coeff = .5)
rnorm_from <- function(data, n_obs = 1, var_coeff = 1){
  .mns <- apply(data, 2L, stats::median)
  .cov <- stats::cov(data, method = "pearson")
  diag(.cov) <- var_coeff * diag(.cov) ## Decrease univariate variance if needed.
  ## person numeric, not spearman ranked/ordinal
  
  ## Check if this sigma is a positive definite matrix.
  if(lqmm::is.positive.definite(.cov) == FALSE){
    .cov <- lqmm::make.positive.definite(.cov)
  }
  
  ## Sample
  ret <- mvtnorm::rmvnorm(n = n_obs,
                          mean = .mns,
                          sigma =  var_coeff * .cov)
  as.data.frame(ret)
}

#' Linear function to help set alpha opacity
#' 
#' Suggests a alpha opacity to plot with as a function of the number of 
#' observation.
#' 
#' @param n Number of observations to plot.
#' @param appox_max_n The number of observation to reach floor opacity.
#' @param ceiling The highest number returned. Defaults to 1.
#' @param floor The lowest number returned. Defaults to 0.2.
#' @return A scalar numeric, suggested value to set alpha opacity.
#' @export
#' @family cheem utility
#' @examples
#' ## Suggest an opacity to use in plotting:
#' (my_alpha <- linear_tform(nrow(spinifex::penguins)))
#'
#' ## Visualize
#' x <- 1:2000
#' plot(x, sapply(x, linear_tform), col = 'blue')
linear_tform = function(
  n, appox_max_n = 5000L, ceiling = 1, floor = .2
){
  vec <- 1L - min(n / appox_max_n, 1L)
  ceiling * (floor + (1L - floor) * vec)
}

#' Logistic function to help set alpha opacity
#' 
#' Suggests a alpha opacity to plot with as a function of the number of 
#' observation.
#' 
#' @param n Number of observations to plot.
#' @param mid_pt Inflection point that the logistic curve. Defaults to 600.
#' @param k_attenuation The steepness of the transition, larger is a sharper
#' transition. Quite sensitive and defaults to 5.
#' @param ceiling The highest number returned. Defaults to 1.
#' @param floor The lowest number returned. Defaults to 0.3.
#' @return A scalar numeric, suggested value to set alpha opacity.
#' @export
#' @family cheem utility
#' @examples
#' ## Suggest an opacity to use in plotting:
#' (my_alpha <- logistic_tform(nrow(spinifex::penguins)))
#'
#' ## Visualize
#' x <- 1:2000
#' plot(x, logistic_tform(x), col = 'blue')
logistic_tform = function(
  n, mid_pt = 600, k_attenuation = 5, ceiling = 1, floor = .3
){
  vec <- 1L / (1L + exp(k_attenuation / 1000L * (n - mid_pt)))
  ceiling * (floor + (1L - floor) * vec)
}


#' Assure a full length logical index
#' 
#' Suggests a alpha opacity to plot with as a function of the number of 
#' observation.
#' 
#' @param index A vector, typically a numeric row index of the data to
#' coerce to a logical index.
#' @param n Single numeric, the number of rows of the data use as a replicate
#' return length.
#' @return A logical index of length `n`.
#' @export
#' @family cheem utility
#' @examples
#' ## Coerce a numeric index to logical:
#' as_logical_index(c(1, 4:10, 15), nrow(mtcars))
as_logical_index <- function(index, n){
  if(is.logical(index) & length(index) != n)
    stop("as_logical_index: `index` was logical, but not of length `n`.")
  if(is.numeric(index) == TRUE){
    rep_f <- rep(FALSE, n)
    rep_f[index] <- TRUE
    index <- rep_f
  }
  if(identical(index, TRUE)) index <- rep(TRUE, n)
  index
}

