% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/not_in.R
\name{\%ni\%}
\alias{\%ni\%}
\title{Not in}
\usage{
x \%ni\% table
}
\arguments{
\item{x}{vector or NULL: the values to be matched. Long vectors are supported.}

\item{table}{vector or NULL: the values to be matched against. Long vectors are not supported.}
}
\value{
A logical vector, indicating if a match was located for each element of x: thus the values are TRUE or FALSE and never NA.
}
\description{
Not in
}
\examples{
"foo" \%ni\% letters
}
