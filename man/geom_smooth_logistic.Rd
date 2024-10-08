% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_smooth_logistic.R
\name{geom_smooth_logistic}
\alias{geom_smooth_logistic}
\title{Add a smooth from a logistic regression}
\usage{
geom_smooth_logistic(mapping = NULL, data = NULL, se = TRUE, ...)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[=aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping at
the top level of the plot. You must supply mapping if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. See \code{ggplot::geom_smooth} for additional details.}

\item{se}{logical specifying if confidence interval should be shown around the smooth.}

\item{...}{other arguments passed to \code{geom_smooth}.}
}
\description{
Add a smooth to a \code{ggplot}, using a logistic model.
}
\details{
Uses \code{geom_smooth} with method \code{glm} and the \code{binomial} family, and a formula
of \code{factor(y) ~ x}.
}
