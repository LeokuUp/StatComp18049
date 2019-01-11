#'@title GenerateBeta__question3.7(Statistical Computing with R)
#'@description question3.7(Statistical Computing with R):Write a function to generate a random sample of size n from the Beta(a, b)
#'distribution by the acceptance-rejection method. Generate a random sample of size 1000 from
#'the Beta(3,2) distribution. Graph the histogram of the sample with the theoretical Beta(3,2)
#' density superimposed
#'@description Here we use Acceptance-Rejection Method to generate a random sample, and And draw the density function of beta distribution
#' and compare them.
#' @param n The sise of random sample we want to generateBeta
#' @param a The first parameter of Beta distribution function
#' @param b The second parameter of Beta distribution function
#' @return y The random sample has been Generated, which is a vector
#' @export GenerateBeta
#' @examples
#' y<-GenerateBeta(1000,3,2)
#' hist(y,prob = TRUE,ylim=c(0,2))
#' x <- seq(0, 1, .01)
#' lines(x, x^2*(1-x)/beta(3,2))


#Acceptance-Rejection Method
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
  lines(x, x^(a-1)*(1-x)^(b-1)/beta(a,b))

  return(y)
}
