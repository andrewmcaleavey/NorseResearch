% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{nf2.1.item.descriptions}
\alias{nf2.1.item.descriptions}
\title{Item information, NF 2.1}
\format{
a data frame
\describe{
\itemize{
\item{\code{assess_order}} {Numeric. Order in assessment.}
\item{\code{item}} {Character. Item name (e.g., "Q14").}
\item{\code{reverse}} {Logical. Is item reverse scored?}
\item{\code{trigger}} {Logical. Is the item used as an opening trigger?}
\item{\code{item_text_e}} {Character. Text of the item in English.}
\item{\code{simple_scale}} {Character. Scale name for programming.}
\item{\code{scale_e}} {Character. Scale name for presentation.}
\item{\code{domain_e}} {Character. Domain name in English.}
\item{\code{item_text_n}} {Character. Text of the item in Nynorsk.}
\item{\code{scale_n}} {Character. Scale name in Nynorsk.}
\item{\code{domain_n}} {Character. Domain name in Nynorsk.}
\item{\code{item_text_b}} {Character. Text of the item in Bokmål.}
\item{\code{scale_b}} {Character. Scale name in Bokmål.}
\item{\code{domain_b}} {Character. Domain name in Bokmål.}
}
}
}
\source{
Based on 'NORSE Measure Master Document.xlsx', and more proximally 'NF2.1.items.xlsx'.
}
\usage{
View(nf2.1.item.descriptions)
}
\description{
A summary of information about the NF items, updated September 2020
}
\details{
Not generally accessed directly. Use lookup functions like \code{\link{lookup_item}}.
}
\keyword{datasets}
