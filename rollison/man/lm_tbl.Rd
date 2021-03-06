% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{lm_tbl}
\alias{lm_tbl}
\title{Present lm() summary output as tibble}
\usage{
lm_tbl(model)
}
\arguments{
\item{model}{A model resulting from fitting a linear model with lm()}
}
\value{
A tibble with the regression output coefficients, Std. Error, t-value, and p-value.
}
\description{
This function formats lm() regression output as a tibble
}
