% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ScoringFuncs.R
\name{score_all_NORSE2}
\alias{score_all_NORSE2}
\title{Scoring All NORSE scales without losing subthreshold scores to NA}
\usage{
score_all_NORSE2(dat, process_vars = TRUE)
}
\arguments{
\item{dat}{a data_frame.}

\item{process_vars}{Logical. Should process variables (Alliance and
Expressed Needs) be computed. Defaults to TRUE.}
}
\value{
a data_frame with new subscale values
}
\description{
\code{score_all_NORSE2} provides scores for ALL NORSE subscales without
trashing missing values when trigger not met.
}
\details{
Provides the following scale scores:
\itemize{
\item Cognitive problems: \code{cog}
\item Control: \code{control}
\item etc.
}
}
\examples{
test_out <- score_all_NORSE2(data2017_18)
}
