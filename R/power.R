#' Power calculations for two sample t tests and confidence intervals
#'
#' @param n number of observations (per group)
#' @param delta true difference between the parameter of interest and the null hypothesis
#' @param sd standard deviation (within group)
#' @param conf.level confidence level
#' @param sig.level significance level
#' @param me margin of error of the confidence interval
#' @param power power of the test, or of having the given margin of error or smaller
#' @param tol numerical tolerance used in root finding, the default providing (at least) four significant digits.
#'
#' @export
two_t_power <- function(n=NULL, delta=NULL, sd=1, sig.level=0.05, power=0.8) {
  given_sig <- !is.null(sig.level)
  given_power <- !is.null(power)
  if(!is.null(n)) if(n - round(n) < 1e-3) n <- as.integer(round(n))
  x <- is.null(n) + is.null(delta) + is.null(sd) + is.null(sig.level) + is.null(power)
  if(x!=1) {stop("Exactly one of n, delta, sd, sig.level, or power must be set to NULL. Only n and delta are NULL by default.")}
  pw <- power.t.test(n=n, delta=delta, sd=sd, sig.level=sig.level, power=power)
  #n <- ceiling(pw$n)
  n <- pw$n
  delta <- pw$delta
  sd <- pw$sd
  sig.level <- pw$sig.level
  power <- pw$power

  #if(given_sig & given_power) {
  #  me <- two_t_me(n=n, sd=sd, conf.level=1-sig.level, power=power)
  #} else {
  #  me <- me_expected <- NA
  #}
  #if(given_sig) {
  #  me_expected <- two_t_me(n=n, sd=sd, conf.level=1-sig.level, power=NA)
  #}
  tibble(n=n,
         delta = delta,
         sd=sd,
         sig_level=sig.level,
         power=power,
         #me_expected = me_expected,
         #me = me,
  )|>
    as_atest(about.vars = c("n", "delta", "sd", "sig_level","power")) |>
    set_digits(columns=c("sig_level", "power"), decimals=2)
}


#' @export
#' @rdname two_t_power
two_t_margin_error <- function(n=NULL, me=NULL, sd=1, conf.level=0.95, power=0.8,
                               tol = .Machine$double.eps^0.25) {
  # when samples of size n are taken from a normal distribution with variance sigma^2,
  # the sampling distribution of (n−1) s^2 / sigma^2
  # has a chi-square distribution with n-1 degrees of freedom.
  # that is, s^2 ~  sigma^2 * X / df
  x <- is.null(n) + is.null(me) + is.null(sd) + is.null(conf.level) + is.null(power)
  if(x!=1) {stop("Exactly one of n, me, sd, conf.level, or power must be set to NULL. Only n and me are NULL by default.")}

  if(!is.null(n)) if(n - round(n) < 1e-3) n <- as.integer(round(n))

  two_t_me <- function(n, sd=1, conf.level=0.95, power=0.8) {
    v1 <- v2 <- sd^2
    n1 <- n2 <- n
    df1 <- n - 1
    df2 <- n - 2
    vp <- (v1 * df1 + v2 * df2) / (df1 + df2)
    v <- vp/n1 + vp/n2
    df <- n1 + n2 - 2
    sig.level <- 1 - conf.level
    k <- qt(1 - sig.level / 2, df)
    r <- if(is.na(power)) 1 else qchisq(power, df = df) / df
    k * sqrt(v *  r)
  }
  if(is.null(me)) {
    me <- two_t_me(n, sd, conf.level, power)
  } else if (is.null(n)) {
    n <- uniroot(function(n) two_t_me(n=n, sd=sd, conf.level=conf.level, power=power) - me,
                 c(2, 1e+07), tol=tol, extendInt="no")$root
  } else if (is.null(conf.level)) {
    conf.level <- uniroot(function(conf.level) two_t_me(n=n, sd=sd, conf.level=conf.level, power=power) - me,
                          c(1e-07, 1-1e-07), tol=tol, extendInt="no")$root
  } else if (is.null(power)) {
    power <- uniroot(function(power) two_t_me(n=n, sd=sd, conf.level=conf.level, power=power) - me,
                     c(1e-07, 1-1e-07), tol=tol, extendInt="no")$root
  } else if (is.null(sd)) {
    sd <- uniroot(function(sd) two_t_me(n=n, sd=sd, conf.level=conf.level, power=power) - me,
                  c(1e-07, 1e07), tol=tol, extendInt="no")$root
  }

  tibble(n=n,
         me = me,
         sd=sd,
         conf_level=conf.level,
         power=power) |>
    as_atest(about.vars = c("n", "me", "sd", "conf_level","power")) |>
    set_digits(columns=c("conf_level", "power"), decimals=2)
}




