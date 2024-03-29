% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_util.r
\name{is_diverging}
\alias{is_diverging}
\title{Check if a vector diverges a value}
\usage{
is_diverging(x, mid_pt = 0)
}
\arguments{
\item{x}{A vector to check the divergence of.}

\item{mid_pt}{A single number checking divergence from. Defaults to 0.}
}
\value{
Logical, whether or not \code{x} is a diverges \code{mid_pt}.
}
\description{
Whether or not a vector is a diverges a value, returns a logical.
Used to help default a scale_color for ggplot2.
}
\examples{
library(cheem)

is_diverging(-10:10)
is_diverging(-10:-5)
is_diverging(mtcars$mpg, 25)
is_diverging(mtcars$mpg, 40)
}
\seealso{
Other cheem utility: 
\code{\link{as_logical_index}()},
\code{\link{color_scale_of}()},
\code{\link{contains_nonnumeric}()},
\code{\link{is_discrete}()},
\code{\link{linear_tform}()},
\code{\link{logistic_tform}()},
\code{\link{problem_type}()},
\code{\link{rnorm_from}()},
\code{\link{sug_basis}()},
\code{\link{sug_manip_var}()}
}
\concept{cheem utility}
