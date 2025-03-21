% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_rev.R
\name{drop_variable}
\alias{drop_variable}
\title{Conditionally drop variables without an error}
\usage{
drop_variable(df, var_name)
}
\arguments{
\item{df}{data}

\item{var_name}{a character name of a variable}
}
\value{
data
}
\description{
Conditionally drop variables without an error
}
\examples{
df <- data.frame(A = 1:5, B = letters[1:5], C = rnorm(5))
drop_variable(df, "C")
drop_variable(df, "D")
}
