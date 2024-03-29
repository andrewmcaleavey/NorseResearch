% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{nf2.1.logic}
\alias{nf2.1.logic}
\title{NF v. 2.1 Logic}
\format{
A data frame.
\describe{
\itemize{
\item{\code{simple_scale}} {Character. Scale name for programming.}
\item{\code{scale_e}} {Character. Scale name for presentation.}
\item{\code{scale_n}} {Character. Scale name in Nynorsk.}
\item{\code{trigger_item}} {Character. Item name (e.g., "Q145") of the trigger for this scale.}
\item{\code{trigger_val}} {Numeric. Value of the opening threshold. Item response
must be greater than this to open the scale.}
\item{\code{closing_threshold}} {Numeric. Value of the closing threshold.
Moving average of past 3 administrations must be below this value to close
the scale.}
}
}
}
\source{
Based on 'NORSE Measure Master Document.xlsx', and more proximally 'NF2.1.logic.xlsx'.
}
\usage{
View(nf2.1.logic)
}
\description{
A table to summarize opening and closing logic in NF 2.1
Added 7 September 2020.
}
\details{
Not generally accessed directly. Use lookup functions like \code{\link{lookup_item}}.
Used in scoring.
}
\keyword{datasets}
