% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_methods.R
\name{summary.cofad_wi}
\alias{summary.cofad_wi}
\title{Summary of within subject design contrast analysis}
\usage{
\method{summary}{cofad_wi}(object, ci = 0.95, ...)
}
\arguments{
\item{object}{output of calc_contrast}

\item{ci}{confidence intervall for composite Score (L-Values)}

\item{...}{further arguments}
}
\value{
Displays type of contrast analysis, lambdas, t-table and typical
  effect sizes. If you assign this to a variable, it will be a list with the
  elements Lambdas, tTable, Effects.
}
\description{
Summary of within subject design contrast analysis
}
