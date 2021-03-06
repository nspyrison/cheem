% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_util.r
\name{problem_type}
\alias{problem_type}
\title{The type of model for a given Y variable}
\usage{
problem_type(y)
}
\arguments{
\item{y}{Response variable to be modeled}
}
\value{
Character either c("classification", "regression") specifying the
assumed model task based on the discreteness of y.
}
\description{
Whether the Y is a "classification", "regression" or ill-defined problem.
Returns a character: "classification", "regression", or an error for strange
classes. Minor redundancy with is_discrete, though explicit. Could be useful
for \code{DALEX::explain(type)} as it also expects "classification" or
"regression".
}
\examples{
library(cheem)

problem_type(mtcars$mpg)
problem_type(mtcars$cyl) ## Numeric column, labeled as discrete, because less than 25 unique values
problem_type(letters)
}
\seealso{
Other cheem utility: 
\code{\link{as_logical_index}()},
\code{\link{basis_attr_df}()},
\code{\link{color_scale_of}()},
\code{\link{does_contain_nonnumeric}()},
\code{\link{is_discrete}()},
\code{\link{is_diverging}()},
\code{\link{linear_tform}()},
\code{\link{logistic_tform}()},
\code{\link{manip_var_of_attr_df}()},
\code{\link{rnorm_from}()}
}
\concept{cheem utility}
