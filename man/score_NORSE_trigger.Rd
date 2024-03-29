% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ScoringFuncs.R
\name{score_NORSE_trigger}
\alias{score_NORSE_trigger}
\title{Scoring NORSE scales without losing subthreshold scores to NA}
\usage{
score_NORSE_trigger(
  dat,
  vars = names(dat),
  trigger = lookup_trigger_among(vars)
)
}
\arguments{
\item{dat}{a data_frame.}

\item{vars}{variables that make the scale, defaults to all in \code{dat}.
Can use with e.g., "somAnx.names".}

\item{trigger}{the trigger item. Default uses \code{lookup_trigger_among} with
\code{vars} but this can be overwritten. This will use the NF v2.1 Triggers
by default.

Inconsequential in present implementation.}
}
\description{
\code{score_NORSE_trigger} provides scores for NORSE subscales without
trashing missing values when trigger not met
}
\examples{
test_data <- data.frame(Q1 = c(1, 2, 3, 5, 3, 2, NA, NA),
                        Q2 = c(3, 4, 2, 5, 2, NA, 1, NA),
                        Q3 = c(4, 3, 5, 2, NA, NA, NA, NA))

score_NORSE_trigger(dat = test_data, vars = c("Q1", "Q2", "Q3"))

# NOTE use of (.) in pipes
test_data \%>\% mutate(subscale_score = score_NORSE_trigger(.))
test_data \%>\% mutate(subscale_score = score_NORSE_trigger(., trigger = "Q3"))

# Can score other versions of NF using options directly in trigger parameter:
score_NORSE_trigger(dat = test_data, vars =)
}
