#' Motor Trend Car Road Tests
#'
#' A version of the Motor Trend car road tests data from the datasets package,
#' but as a tibble with categorical variables stored as factors.
#'
#' @format A tibble with 32 rows and 12 variables.
#' \describe{
#' \item{model}{Make and model of the car}
#' \item{mpg}{Miles/(US) gallon}
#' \item{cyl}{Number of cylinders (as a factor)}
#' \item{disp}{Displacement (cu.in.)}
#' \item{hp}{Gross horsepower}
#' \item{drat}{Rear axle ratio}
#' \item{wt}{Weight (1000 lbs)}
#' \item{qsec}{1/4 mile time}
#' \item{vs}{Engine (V-shaped / straight)}
#' \item{am}{Transmission (automatic / manual)}
#' \item{gear}{Number of forward gears (as a factor)}
#' \item{carb}{Number of carburetors (as a factor)}
#' }
#'
#' @source The `datasets` package; see [datasets::mtcars].
"mtcars2"

#' Scores on pre/post test (generated data)
#'
#' Generated pre/post data to demonstrate paired tests.
#' A score of 80 is considered passing.
#'
#' @format A tibble with 50 rows and 4 columns.
#' \describe{
#' \item{score1}{pre-test score}
#' \item{score2}{post-test score}
#' \item{pass1}{pre-test pass/fail status}
#' \item{pass2}{post-test pass/fail status}
#' }
"passfail"
