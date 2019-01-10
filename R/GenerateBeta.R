#'@title GenerateBeta__question3.7(Statistical Computing with R)
#'@description Write a function to generate a random sample of size n from the Beta(a, b)
#'distribution by the acceptance-rejection method. Generate a random sampleof size 1000 from
#'the Beta(3,2) distribution. Graph the histogram of the sample with the theoretical Beta(3,2)
#' density superimposed
#' @param n Generate a random sample of size
#' @param a The first parameter of distribution function
#' @param b The second parameter of distribution function
#' @return y The random sample has been Generated
#' @export GenerateBeta
#' @examples
#' y<-GenerateBeta(1000,3,2)
#' hist(y,prob = TRUE,ylim=c(0,2))
#' x <- seq(0, 1, .01)
#' lines(x, x^2*(1-x)^1/beta(3,2))


GenerateBeta <- function(n,a,b){

  mx=(1-a)/(2-a-b)
  #max
  max=mx^(a-1)*(1-mx)^(b-1)/beta(a,b)
  c=max+1

  j<-k<-0; y <- numeric(n)
  while (k < n) {
    u <- runif(1)
    j <- j + 1
    x <- runif(1) #random variate from g
    if (x^(a-1)*(1-x)^(b-1)/beta(a,b) > c*u) {
      #we accept x
      k <- k + 1
      y[k] <- x
    }
  }

  hist(y,prob = TRUE,ylim=c(0,2))
  x <- seq(0, 1, .01)
  lines(x, x^2*(1-x)^1/beta(3,2))

  return(y)
}
