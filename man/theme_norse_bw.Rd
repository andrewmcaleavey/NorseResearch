% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotThemes.R
\name{theme_norse_bw}
\alias{theme_norse_bw}
\title{A ggplot2 theme with no gridlines}
\usage{
theme_norse_bw()
}
\value{
a ggplot object
}
\description{
A ggplot2 theme with no gridlines
}
\examples{
ggplot(aes(x = 1:5, y = 2:6)) + geom_line() + theme_norse_bw()
}
