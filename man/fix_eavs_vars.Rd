% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_eavs.R
\name{fix_eavs_vars}
\alias{fix_eavs_vars}
\title{fix_eavs_vars()}
\usage{
fix_eavs_vars(frame)
}
\arguments{
\item{frame}{EAVS data.frame output from read_eavs(year). Requires 'year' var.}
}
\value{
a data frame
}
\description{
Basic standardization of EAVS variable names across years. Capitalization, exact matches are fixed here. Arbitrary variables that don't correspond to questions are removed.
}
