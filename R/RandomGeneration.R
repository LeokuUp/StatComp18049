RandomGeneration <- function(distribution='lognormal',n=1e3){
  if(distribution == 'lognormal')
    x <- rlnorm(n)
  else if(distribution == 'uniform')
    x <- runif(n)
  else if(distribution =='bernoulli')
    x <- rbinom(n,size=1,prob = 0.1)
  else
    stop("distribution error")
  x
}
