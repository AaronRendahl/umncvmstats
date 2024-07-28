# linear
# quadratic strength
# quadratic location
# std error
# std error spread
#' @importFrom stats cor
makep <- function(d) {
  rho <- with(d, cor(x, y))
  txt <- sprintf("rho = %0.2f", rho)
  ggplot(d) + aes(x, y) + geom_point() +
    scale_x_continuous(breaks=seq(0,10,by=2)) +
    scale_y_continuous(breaks=seq(0,10,by=2)) +
    #annotate("text", label=txt, x=-Inf, y=Inf, hjust=0, vjust=1) +
    stat_smooth(method="lm", se=FALSE, formula=y~x) +
    labs(title=paste("Data", attr(d, "seed")),
         subtitle=txt)
}

#' @param seed which sample data to use.
#' @param verbose logical specifying if messages about sample data should be displayed.
#' @export
#' @importFrom stats rbeta
#' @importFrom stats runif
#' @importFrom stats rnorm
#' @rdname sample_lm
sample_data <- function(seed, verbose=FALSE) {
  mess <- function(...) {
    if(verbose) message(sprintf(...))
  }
  set.seed(seed)
  b1 <- rbeta(1, shape1=1.3, shape2=1) * sample(c(-1,1), 1)
  mess("Slope of %0.1f", b1)
  s <- runif(1, 0.3, 1.5)
  mess("SD of %0.1f", s)
  sx <- 1
  b2 <- 0
  x0 <- 0
  if(runif(1) < 1/4) {
    b2 <- runif(1, 0.5, 1)
    x0 <- runif(1, -1.5, 1.5)
    mess("Curve of %0.1f, at %0.1f", b2, x0)
  }
  x <- c(runif(25,-2,2), rnorm(25))
  e <- rnorm(length(x))
  if(runif(1)<0.5) {
    aa <- runif(1)
    if(aa < 0.25) {
      sx <- runif(1, 3, 12)
      mess("SD range is %0.1f", sx)
    } else if(aa<0.5) {
      mess("light tails")
      kk <- abs(e) > 1.5
      e[kk] <- e[kk]*0.75
    } else if (aa<0.75) {
      mess("heavy tails")
      kk <- abs(e) > 1.5
      e[kk] <- e[kk]*2
    } else {
      e <- rexp(length(x))
      mess("skew tails")
    }
  }
  e0 <- e * s
  sx <- sx^((x - min(x))/diff(range(x)))/sx^0.5
  e <- e * sx
  mm <- (max(abs(x))+abs(x0))^2/4
  y <- b1*x + b2*(x-x0)^2 + e
  if(runif(1) < 1/3) {
    if(runif(1) < 1/2) {
      mess("Adding x outlier?")
      x[which.min(x)] <- x[which.min(x)] - 1
    }
    if(runif(1) < 1/2) {
      mess("Adding y outlier?")
      y[which.min(x)] <- y[which.min(x)] - 1
    }
  }
  x <- (x - min(x))
  x <- x/max(x)*9+runif(1)
  y <- (y - min(y))
  y <- y/max(y)*9+runif(1)
  out <- tibble(x=round(x, 2), y=round(y, 2), e0=e0, e=e)
  attr(out, "seed") <- seed
  out
}

#' Create a random data set for regression
#'
#' Create a random data set for regression and display the standard diagnostic plots.
#'
#' @export
#' @importFrom stats lm
#' @importFrom stats printCoefmat
sample_lm <- function(seed=sample(1e4-1, 1)) {
  d <- sample_data(seed)
  m <- lm(y ~ x, data=d)
  aa <- coef(summary(m))
  colnames(aa) <- c("estimate", "SE", "t.value", "p.value")
  printCoefmat(aa, digits=3,
               eps.Pvalue = 1e-4, signif.stars=FALSE, has.Pvalue = TRUE)
  p <- makep(d)
  plot(p | diag_plots(m))
  invisible(d)
}
