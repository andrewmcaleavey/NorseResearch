% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ScoringFuncs.R
\name{score_all_NORSE2_ou}
\alias{score_all_NORSE2_ou}
\title{Scoring All NORSE scales OVER-UNDER}
\usage{
score_all_NORSE2_ou(dat)
}
\arguments{
\item{dat}{a data_frame.}
}
\value{
a data_frame with new subscale values in subscales
with names ending in "_ou".
}
\description{
\code{score_all_NORSE2_ou} provides scores for ALL NORSE subscales
in the Over-Under style.
}
\details{
Provides the following scale scores:
\itemize{
\item Cognitive problems: \code{cog_ou}
\item Control: \code{control_ou}
\item etc.
}
}
\examples{
test_out <- score_all_NORSE2_ou(data2017_18)

}
