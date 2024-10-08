% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptive_statistics.R
\name{descriptive_statistics}
\alias{descriptive_statistics}
\title{Compute descriptive statistics for a data set}
\usage{
descriptive_statistics(data, ..., digits = 2, compact = TRUE)
}
\arguments{
\item{data}{the data set to summarize.}

\item{...}{additional parameters, sent to \code{tbl_summary}.}

\item{digits}{desired number of significant figures, for numeric variables.}

\item{compact}{logical specifying whether or not to compact the resulting \code{gt} table.}
}
\description{
Use \code{gtsummary::tbl_summary} to summarize variables in a data set.
For categorical variables, reports count/total and percent; for
continuous variables, reports mean, standard deviation, median, and range.
}
\examples{
mtcars2 |> dplyr::select(-model) |> descriptive_statistics()

}
