% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/5915_model.R
\name{model_glance}
\alias{model_glance}
\title{Get model summary information}
\usage{
model_glance(model, ...)
}
\arguments{
\item{model}{a linear model or generalized linear model.}

\item{...}{additional parameters, passed to \code{broom::glance}.}
}
\description{
Retrieve statistics such as R-squared, AIC, and more, as computed by \code{broom::glance}.
}
