% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/5915_proportion2.R
\name{paired_proportion_inference}
\alias{paired_proportion_inference}
\title{Paired proportion test (McNemar's)}
\usage{
paired_proportion_inference(
  formula,
  data,
  success,
  method = c("default", "wilson", "exact"),
  alternative = c("two.sided", "less", "greater"),
  correct = FALSE,
  conf.level = NA
)
}
\arguments{
\item{formula}{a formula of the form \code{y2 - y1 ~ 1} or \code{~ y2 -y1}, where \code{y1} and \code{y2} are numeric variables.
To perform test within subgroups, use \code{y2 - y1 ~ x} or \code{y2 - y1 ~ 1 | g}, or even \code{y2- y1 ~ x | g},
where \code{x} and \code{g} are factor variables.}

\item{data}{a data frame containing the values in the formula.}

\item{success}{the level of the response variable to consider a success}

\item{method}{character string specifying which method to use. One of "\code{default}", "\code{wilson}", or "\code{exact}".}

\item{alternative}{character string specifying the alternative hypothesis, must be one of "\code{two.sided}" (default), "\code{greater}" or "\code{less}".}

\item{correct}{a logical indicating whether Yates' continuity correction should be applied; used for Wilson test only.}

\item{conf.level}{if desired, confidence level of the returned confidence interval. Must be a single number between 0 and 1.}
}
\description{
Perform McNemar's test for paired proportions, by computing the proportion
of those that switch from failure to success out of all those that switch
in either direction and performing a one-sample proportion test with a null of 0.5.
}
