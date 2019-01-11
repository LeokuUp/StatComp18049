#'@title SimpleChitest__question5(Advanced R)
#'@description Make a faster version of chisq.test() that only computes the chi-square test statistic
#' when the input is two numeric vectors with no missing values. You can try simplifying chisq.test()
#'or by coding from the mathematical de finition. Here we also have the function SimpleTable
#'@description This method simplifies the traditional Chi-square test. An example is given to
#'compare the computational speed of this method with that of the traditional method.
#' @param x numeric vector with no missing values
#' @param y numeric vector with no missing values
#' @return STATISTIC the chi-square test statistic
#' @return p.value   the P-value of chi-square test statistic
#' @export SimpleChitest
#' @examples
#' x1<-sample(1:10,1000,replace=TRUE)
#' x2<-sample(10:20,1000,replace=TRUE)
#' SimpleChitest(x1,x2)
#' library(microbenchmark)
#' ts <- microbenchmark(chisq=chisq.test(x1,x2),simple_chitest=SimpleChitest(x1,x2))
#' summary(ts)[,c(1,3,5,6)]

#use simple_table to do simple_chitest2
SimpleChitest <- function(x,y){

  OK <- complete.cases(x, y)
  x <- factor(x[OK])
  y <- factor(y[OK])

  x <- table(x, y)   #use simple_table

  if ((n <- sum(x)) == 0)
    stop("at least one entry of 'x' must be positive")

  nr <- as.integer(nrow(x))
  nc <- as.integer(ncol(x))
  sr <- rowSums(x)
  sc <- colSums(x)
  E <- outer(sr, sc, "*")/n

  STATISTIC <- sum((x - E)^2/E)
  PARAMETER <- (nr - 1L) * (nc - 1L)
  PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)


  structure(list(statistic = STATISTIC, p.value = PVAL), class = "htest")
}
