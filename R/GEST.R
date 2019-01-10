#'@title GEST__question 6.9(Statistical Computing with R)
#'@description The Gini ratio is applied in economics to measure inequality in income distribution.
#' Note that G can be written in terms of the order statistics x
#' @param distribution should be 'lognormal','uniform' or 'bernoulli',
#' @param m the length of random vector x
#' @param n Number of repeated calculations
#' @return g.est the static of g.est
#' @export GEST
#' @examples
#' GEST(distribution='lognormal',m=1e3,n=1e3)
#' hist(g.hat,freq = F, main = "standard lognormal" )

GEST <- function(distribution='lognormal',m=1e3,n=1e3){
  g.hat <-  numeric(m)
  for(i in 1:m){

    #chose distribution
    if(distribution == 'lognormal')
      x <- rlnorm(n)
    else if(distribution == 'uniform')
      x <- runif(n)
    else if(distribution =='bernoulli')
      x <- rbinom(n,size=1,prob = 0.1)
    else
      stop("The distribution function you entered is incorrect ")

    xorder <- sort(x)
    for(j in 1:n){
      g.hat[i] <- g.hat[i] + (2*j-n-1)*xorder[j]
    }
    g.hat[i] <- g.hat[i]/(n*n*mean(xorder))
  }

  g.est <- mean(g.hat)
  g.est

}
