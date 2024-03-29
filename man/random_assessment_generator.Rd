% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_data.R
\name{random_assessment_generator}
\alias{random_assessment_generator}
\title{Generate random NF-like data}
\usage{
random_assessment_generator(date_df = NULL, num_dates = NULL, ...)
}
\arguments{
\item{date_df}{A data.frame if provided. Defaults to NULL, and then is rewritten.}

\item{num_dates}{Optional. Number of assessments to produce.}

\item{...}{Additional parameters}
}
\value{
You have to find out
}
\description{
Generate random NF-like data
}
\examples{
random_assessment_generator()
random_assessment_generator(Sys.Date())
random_assessment_generator(date_df = tibble("date" = c(Sys.Date(), Sys.Date() +1)))
random_assessment_generator(num_dates = 3)
}
