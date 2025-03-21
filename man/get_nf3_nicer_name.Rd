% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaningFuncs.R
\name{get_nf3_nicer_name}
\alias{get_nf3_nicer_name}
\title{Swap the short name of a scale for the nicer name of a scale in NF 3.x}
\usage{
get_nf3_nicer_name(simplename, version = "3")
}
\arguments{
\item{simplename}{A character matching a value in \code{scale_names}.}

\item{version}{A character string defining version to use. Defaults to
\code{3} for NF 3.x.}
}
\value{
A character from \code{nicer.nf2.names}.
If no match found, returns \code{simplename}.
}
\description{
Swap the short name of a scale for the nicer name of a scale in NF 3.x
}
\examples{
get_nf3_nicer_name("sad")
}
