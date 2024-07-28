#' @export
#' @importFrom stats rbeta
#' @importFrom stats runif
#' @importFrom stats rnorm
#' @importFrom stats cor
guess_cor <- function(seed=NA, nonp=FALSE, skew=nonp, rank=FALSE,
                      answer=c("interactive", "show", "hide"),
                      return.data=FALSE) {
  answer <- match.arg(answer)
  ans <- ""
  while(ans!="X") {
    if(is.na(seed)) seed <- sample.int(9999, 1)
    set.seed(seed)
    x <- rnorm(50)
    s1 <- rbeta(1, shape1=1.3, shape2=1) * sample(c(-1,1), 1)
    s2 <- sample(c(0,0,0,0,1,2,-2),1)
    s <- runif(1, 0.3, 1.5)
    e <- rnorm(50, s=s)
    e <- e*(1+sample(c(0,0,5),1)*(x - min(x)))
    x0 <- sample(c(-2,-1,0,1,2),1)
    y <- s1*x + s2*(x-x0)^2 + e
    if(skew) {
      y <- y-min(y)
      y <- y/max(y)*2
      y <- 10^y
    }
    if(rank) {
      x <- rank(x)
      y <- rank(y)
    }
    d <- tibble(x=x, y=y)
    rho <- with(d, cor(x, y))
    txt <- sprintf("rho=%0.2f", rho)
    if(nonp) {
      tau.kendall <- with(d, cor(x, y, method="kendall"))
      rho.spearman <- with(d, cor(x, y, method="spearman"))
      txt <- sprintf("%s\nKendall's tau: %0.2f\nSpearman's rho: %0.2f",
                     txt, tau.kendall, rho.spearman)
    }
    extratxt <- case_when(rank ~ " (ranks)",
                          skew ~ " (with skew)",
                          TRUE ~ "")
    p1 <- ggplot(d) + aes(x, y) + geom_point() + ggtitle(paste("Sample", seed, extratxt)) +
      xlab(NULL) + ylab(NULL)
    p1.ans <- p1 + annotate("text", label=txt, x=-Inf, y=Inf, hjust=0, vjust=1)
    if(answer=="show") p1 <- p1.ans
    if(answer!="interactive") {
      break
    } else {
      plot(p1)
      cat("Sample", seed)
      cat("\nWhat is the strength, direction, linearity, and shape?\nWhat do you think the correlation is?")
      ans <- readline()
      if(ans=="X") break;
      plot(p1.ans)
      if(!nonp) {
        cat(sprintf("You guessed %s, it was %0.2f.\n", ans, rho))
      } else {
        cat(sprintf("Pearson's rho: %0.2f; Kendall's tau: %0.2f; Spearmans's rho: %0.2f\n\n",
                    rho, tau.kendall, rho.spearman))
      }
      cat("Hit enter for a random sample. [Type a number for that sample. Type X to quit.]")
      ans <- readline()
      seed <- suppressWarnings(as.integer(ans))
    }
  }
  if(answer=="interactive") {
    invisible(NULL)
  } else if(return.data) {
    invisible(d)
  } else {
    p1
  }
}

#1510
#2361
#3998
