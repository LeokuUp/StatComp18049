## ------------------------------------------------------------------------
X <- matrix(5:8, 2)
rownames(X) <- c("a", "b")
colnames(X) <- c("c", "d")
X

Y <- matrix(1:4, 2)
rownames(Y) <- c("e", "f")
colnames(Y) <- c("g", "h")
Y

cbind(X, Y)
rbind(X, Y)
t(X)


## ------------------------------------------------------------------------
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31,40)

opar <- par(no.readonly = TRUE)
par(mfrow=c(1,2))
par(lwd=2, cex=1.5)
par(cex.axis=0.75, font.axis=3)
#plot(dose, drugA, type="b", pch=19, lty=2, col="red")
#plot(dose, drugA, type="b", pch=23, lty=6, col="blue", bg="green")
par(opar)


## ------------------------------------------------------------------------
myfun <- function(S,C1, C2) {
mfrow=c(1,1)  
plot(C1, C2, type="l")
title(S)
}

c1<-c(1:10)
c2<-c(11:20)
s<-"Graph1"
myfun(s,c1,c2)

## ------------------------------------------------------------------------
set.seed(12345)
x<-sample(0:4, size = 1000, replace = TRUE,prob = c(0.1,0.2,0.2,0.2,0.3))
y=table(x)
y
z<-y/1000
z

## ------------------------------------------------------------------------
generateBeta <- function(n,a,b){
  
  mx=(1-a)/(2-a-b)
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
  
  return(y)
}

#example
y<-generateBeta(1000,3,2)
hist(y,prob = TRUE,ylim=c(0,2))
x <- seq(0, 1, .01)
#lines
lines(x, x^2*(1-x)^1/beta(3,2))


## ------------------------------------------------------------------------
set.seed(12345)
n <- 1e3; r <- 4; beta <- 2
#lambda
lambda <- rgamma(n, r, beta)
x<-rexp(n,lambda)
hist(x,breaks = 50)

## ------------------------------------------------------------------------
mypbeta<-function(a,b,x){ 
  m=20000
  u=runif(m/2,min=0,max=x)
  u1=1-u
  g=1/beta(a,b)*u^(a-1)*(1-u)^(b-1)*x
  g1=1/beta(a,b)*u1^(a-1)*(1-u1)^(b-1)*x
  theta=(mean(g)+mean(g1))/2
  sd=sd(c(g,g1))/m
  return(list("theta"=theta,"sd"=sd))
}
x=seq(0.1,0.9,by=0.1)
MC=numeric(9)
sd=numeric(9)
for(i in 1:9){
  result=mypbeta(3,3,x[i])
  MC[i]=result[[1]]
  sd[i]=result[[2]]
}
pBeta=pbeta(x,3,3)
cdf=rbind(MC,pBeta)
knitr::kable(cdf,col.names=x)
matplot(x,cbind(MC,pBeta),col=2:3,pch=1:2,xlim=c(0,1))
legend("topleft", inset=.05,legend=c("MC","pbeta"),col=2:3,pch=1:2)

## ------------------------------------------------------------------------
myrRayleigh<-function(n,sigma,antithetic=TRUE){
  u=runif(n/2)
  if(!antithetic) 
    {v=runif(n/2)}
  else
    {v=1-u}
  u=c(u,v)
  x=sqrt(-2*sigma^2*log(1-u))
  return(x)
}
n=1000
set.seed(12345)
MC1=myrRayleigh(n,2,antithetic = TRUE)
set.seed(12345)
MC2=myrRayleigh(n,2,antithetic = FALSE)
qqplot(MC1,MC2) 
y=seq(0,10)
lines(y,y,col=6)
1-var(MC1)/var(MC2)

## ------------------------------------------------------------------------
x=seq(1,5,0.1)
g=function(x)x^2/sqrt(2*pi)*exp(-(x^2)/2)
f1=function(x)1/((1-pnorm(-0.5))*sqrt(2*pi))*exp(-((x-1.5)^2)/2)
f2=function(x)x*exp(-((x^2)-1)/2)
plot(x,g(x),col=2,type="o",ylim=c(0,1),lty=2,ylab="y",pch=2)
lines(x,f1(x),col=3,type="o",lty=2,pch=2)
lines(x,f2(x),col=4,,type="o",lty=2,pch=2)
legend("topright",inset=.05,legend=c("g(x)","f1(x)","f2(x)"),lty=1,col=2:4,horiz=FALSE)

n=2000
set.seed(12345)
X=rnorm(1.5*n,mean=1.5,sd=1)
X=X[X>1]
X=X[1:n]
theta1=mean(g(X)/f1(X))
sd1=sd(g(X)/f1(X))/n
theta1
sd1
set.seed(12345)
Z=rexp(n,rate=0.5)
Y=sqrt(Z+1)
theta2=mean(g(Y)/f2(Y))
sd2=sd(g(Y)/f2(Y))/n
theta2
sd2

## ------------------------------------------------------------------------
m <- 1e3; n <- 1e3; set.seed(123)

##X is standard lognormal
g.hat <-  numeric(m)
for(i in 1:m){
  x <- rlnorm(n)
  xorder <- sort(x)
  for(j in 1:n){
    g.hat[i] <- g.hat[i] + (2*j-n-1)*xorder[j]
  }
  g.hat[i] <- g.hat[i]/(n*n*mean(xorder))
}
hist(g.hat,freq = F, main = "standard lognormal" )
g.est <- mean(g.hat)
g.est

##X is uniform distribution
g.hat <-  numeric(m)
for(i in 1:m){
  x <- runif(n)
  xorder <- sort(x)
  for(j in 1:n){
    g.hat[i] <- g.hat[i] + (2*j-n-1)*xorder[j]
  }
  g.hat[i] <- g.hat[i]/(n*n*mean(xorder))
}
hist(g.hat,freq = F, main = "uniform distribution")
g.est <- mean(g.hat)
g.est

##X is Bernoulli(0,1) distribution
g.hat <-  numeric(m)
for(i in 1:m){
  x <- rbinom(n,size=1,prob = 0.1)
  xorder <- sort(x)
  for(j in 1:n){
    g.hat[i] <- g.hat[i] + (2*j-n-1)*xorder[j]
  }
  g.hat[i] <- g.hat[i]/(n*n*mean(xorder))
}
hist(g.hat,freq = F,main = "Bernoulli(0,1)")
g.est <- mean(g.hat)
g.est



## ------------------------------------------------------------------------
m <- 1e2; n <- 1e4; set.seed(123)

##X is lognormal with unknown parameters
g.hat <-  numeric(m)
for(i in 1:m){
  x <- rlnorm(n)
  xorder <- sort(x)
  for(j in 1:n){
    g.hat[i] <- g.hat[i] + (2*j-n-1)*xorder[j]
  }
  g.hat[i] <- g.hat[i]/(n*n*mean(xorder))
}
g.est <- mean(g.hat)

CI<-c(mean(g.hat)-qt(0.975,1e4-2)*sd(g.hat),mean(g.hat)+qt(0.975,1e4-2)*sd(g.hat)) #approximate 95% confidence interval 
mean(g.hat>=CI[1]&g.hat<=CI[2]) #coverage rate


## ------------------------------------------------------------------------
library(mvtnorm)
m <- 1e3; n <- 5e2

##test1
test1 <- function(md){
  mean<-c(0,0)
  sigma<-matrix(c(1,0.1,0.1,1),nrow=2)
  p.val1 <- numeric(m)
  for(i in 1:m){
    x <- rmvnorm(n,mean,sigma)
    p.val1[i] <- cor.test(x[,1],x[,2],method = md)$p.value
  }
  print(mean(p.val1<=0.05))
}
test1("pearson")
test1("kendall")
test1("spearman")

##test2
test2 <- function(md){
  
  p.val2 <- numeric(m)
  for(i in 1:m){
    x<-rnorm(n)
    a<-rbinom(n,1,0.5)
    v<-2*a-1
    y<-data.frame(x=x,y=v*x)
    p.val2[i] <-cor.test(y[,1],y[,2],method = md)$p.value
  }
  print(mean(p.val2<=0.05))
}
test2("pearson")
test2("kendall")
test2("spearman")


## ------------------------------------------------------------------------
library(boot);  set.seed(12345)
n=15

#this data is from page-185
LAST <- c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 545, 572, 594)
GPA  <- c(339, 330, 281, 303, 344, 307, 300, 343, 336, 313, 312, 274, 276, 288, 296)
law <- data.frame(LAST,GPA)

b.cor <- function(data,i){
  d <- data[i,]
  return(cor(d$LAST,d$GPA))
} 

theta.hat <- b.cor(law,1:n)
theta.jack <- numeric(n)

for(i in 1:n){
theta.jack[i] <- b.cor(law,(1:n)[-i])
}
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(original=theta.hat,bias=bias.jack,
se=se.jack),3)

## ------------------------------------------------------------------------
library(boot)
set.seed(12345)

boot.mean <- function(x,i) mean(x[i]) #the statistic of boot

n <- 12
x <-c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487) #12 observations

ci.norm<-ci.basic<-ci.perc<-ci.bca<-c(NA,n)  # initialize

de <- boot(data=x, statistic=boot.mean, R = 1e3)
ci <- boot.ci(de,type=c("norm","basic","perc","bca"))
ci.norm[]<-ci$norm[2:3]
ci.basic[]<-ci$basic[4:5]
ci.perc[]<-ci$percent[4:5]
ci.bca[]<-ci$bca[4:5]

#by the standard normal, basic, percentile, and BCa methods
cat('norm = ', ci.norm,
'basic = ', ci.basic,
'perc = ',ci.perc,
'BCa = ',ci.bca)


## ------------------------------------------------------------------------
library("bootstrap")
m=5; n=88

getTheta <- function(data,i){
  d <- data[i,]
  cov.d  <- cov(d)
  lamba <- eigen(cov.d)$values
  theta <- max(lamba)/sum(lamba)
  
  return(theta)
  }

theta.hat <- getTheta(scor,1:n)
theta.jack <- numeric(n)

for(i in 1:n){
theta.jack[i] <- getTheta(scor,(1:n)[-i])
}
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(original=theta.hat,bias=bias.jack,
se=se.jack),3)


## ------------------------------------------------------------------------
library(DAAG) 
attach(ironslag)

n <- length(magnetic)

ke1 <- ke2 <- ke3 <- ke4 <- 0
ge1 <- ge2 <- ge3 <- ge4 <- 0
sqrte1 <- sqrte2 <- sqrte3 <- sqrte4 <- 0   #the sqrt of errors


for(k in 1:(n-1))
    
  for(g in (k+1):n){                        #leave-two-out
    
    y <- magnetic[-k][-g]
    x <- chemical[-k][-g]
    
    J1 <- lm(y ~ x)
    yhat1k <- J1$coef[1] + J1$coef[2] * chemical[k]
    yhat1g <- J1$coef[1] + J1$coef[2] * chemical[g]
    ke1 <- magnetic[k] - yhat1k
    ge1 <- magnetic[g] - yhat1g
    sqrte1 = sqrte1 +  ke1^2 + ge1^2
    
    
    J2 <- lm(y ~ x + I(x^2))
    yhat2k <- J2$coef[1] + J2$coef[2] * chemical[k] + J2$coef[3] * chemical[k]^2
    yhat2g <- J2$coef[1] + J2$coef[2] * chemical[g] + J2$coef[3] * chemical[g]^2
    ke2 <- magnetic[k] - yhat2k
    ge2 <- magnetic[g] - yhat2g
    sqrte2 = sqrte2 +  ke2^2 + ge2^2
    
    J3 <- lm(log(y) ~ x)
    logyhat3k <- J3$coef[1] + J3$coef[2] * chemical[k]
    logyhat3g <- J3$coef[1] + J3$coef[2] * chemical[g]
    yhat3k <- exp(logyhat3k)
    yhat3g <- exp(logyhat3g)
    ke3 <- magnetic[k] - yhat3k
    ge3 <- magnetic[g] - yhat3g
    sqrte3 = sqrte3 +  ke3^2 + ge3^2

    
    
    J4 <- lm(log(y) ~ log(x))
    logyhat4k <- J4$coef[1] + J4$coef[2] * log(chemical[k])
    logyhat4g <- J4$coef[1] + J4$coef[2] * log(chemical[g])
    yhat4k <- exp(logyhat4k)
    yhat4g <- exp(logyhat4g)
    ke4 <- magnetic[k] - yhat4k
    ge4 <- magnetic[g] - yhat4g
    sqrte4 = sqrte4 +  ke4^2 + ge4^2

    
    }
    
#sqrte is the total errors  
sqrte1/(n*(n-1))
sqrte2/(n*(n-1))
sqrte3/(n*(n-1))
sqrte4/(n*(n-1))




## ------------------------------------------------------------------------
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)
z <- c(x, y)

#Cramer-von Mises test
cramer.test <- function(X,Y){
  m = length(X)
  n = length(Y)
  
  FM = ecdf(X)
  GN = ecdf(Y)
  
  sumx = sum(   (FM(X)-GN(X))^2  )
  sumy = sum(   (FM(Y)-GN(Y))^2  )
  
  static = (sumx + sumy)*m*n/sqrt((m+n))
  
  return(static)
}


R <- 999
K <- 1:26
n<-length(x)
set.seed(12345)
reps <- numeric(R)

library(nortest)
c0 <- cramer.test(x,y)

for (i in 1:R) {
 k <- sample(K, size = n, replace = FALSE)
 x1 <- z[k];y1 <- z[-k]
 z1 <- c(x1,y1)
 reps[i] <- cramer.test(x1,y1)
}
 
 p <- mean(abs(c(c0, reps)) >= abs(c0))
 p


## ------------------------------------------------------------------------
library(RANN)
library(boot)
library(energy)
library(Ball)

m <- 1e3; k<-3; p<-2; mu <- 0.5; su <- 0.75; set.seed(12345)
n1 <- n2 <- 50; R<-999; n <- n1+n2; N = c(n1,n2)
n3 <- 10; n4 <-100; M <- c(n3,n4)  #Unbalanced samples

Tn <- function(z, ix, sizes,k) {
   n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
   if(is.vector(z)) z <- data.frame(z,0);
   z <- z[ix, ];
   NN <- nn2(data=z, k=k+1) # what's the first column?
   block1 <- NN$nn.idx[1:n1,-1]
   block2 <- NN$nn.idx[(n1+1):n,-1]
   i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
   (i1 + i2) / (k * n)
}

eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
  sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}

#Unequal variances and equal expectations
# p.values1 <- matrix(NA,m,3)
# for(i in 1:m){
#   x <- matrix(rnorm(n1*p),ncol=p);
#   y <- cbind(rnorm(n2),rnorm(n2,sd=su));
#   z <- rbind(x,y)
#   p.values1[i,1] <- eqdist.nn(z,N,k)$p.value
#   p.values1[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
#   p.values1[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
# }
# 
# #Unequal variances and unequal expectations
# p.values2 <- matrix(NA,m,3)
# for(i in 1:m){
#   x <- matrix(rnorm(n1*p),ncol=p);
#   y <- cbind(rnorm(n2),rnorm(n2,mean=mu,sd=su));
#   z <- rbind(x,y)
#   p.values2[i,1] <- eqdist.nn(z,N,k)$p.value
#   p.values2[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
#   p.values2[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
# }
# 
# #t distribution with 1 df (heavy-tailed distribution)
# p.values3 <- matrix(NA,m,3)
# for(i in 1:m){
#   x <- matrix(rt(n1*p,df=1),ncol=p);
#   y <- cbind(rt(n2,df=1),rt(n2,df=0.8));
#   z <- rbind(x,y)
#   p.values3[i,1] <- eqdist.nn(z,N,k)$p.value
#   p.values3[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
#   p.values3[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
# }
# 
# #Unbalanced samples (say, 1 case versus 10 controls)
# p.values4 <- matrix(NA,m,3)
# for(i in 1:m){
#   x <- matrix(rnorm(n3*p),ncol=p);
#   y <- cbind(rnorm(n4),rnorm(n4,mean = mu));
#   z <- rbind(x,y)
#   p.values4[i,1] <- eqdist.nn(z,M,k)$p.value
#   p.values4[i,2] <- eqdist.etest(z,sizes=M,R=R)$p.value
#   p.values4[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
# }
# 
# alpha <- 0.1;
# pow1 <- colMeans(p.values1<alpha)
# pow2 <- colMeans(p.values2<alpha)
# pow3 <- colMeans(p.values3<alpha)
# pow4 <- colMeans(p.values4<alpha)
# pow1
# pow2
# pow3
# pow4


## ------------------------------------------------------------------------
set.seed(123456)
m=50000
discard=1000

# cauchy density
cauchy<-function(x, thata=0, gamma=1){

  cau_y<-1/(pi*gamma*(1+((x-thata)/gamma)^2))
  return(cau_y)

}

chain<-c(0)
for(i in 1:m){
    proposal<-chain[i]+runif(1, min=-1, max=1)
    accept<-runif(1)<cauchy(proposal)/cauchy(chain[i])
    chain[i+1]<-ifelse(accept==T, proposal, chain[i])
}

#plot
y <- chain[discard:m]
hist(y, breaks="scott", main="", xlab="", freq=FALSE)

a <- ppoints(100)
QR=tan((a-0.5)*pi)
lines(QR, cauchy(QR),col='red')

b <- ppoints(1000)
Q <- quantile(y, b)
qqplot(QR, Q, main="",xlab="Rayleigh Quantiles", ylab="Sample Quantiles",col='green')


## ------------------------------------------------------------------------
kset <- c(4:25,100,500,1000)   #chose k
res <- numeric(25)
#f is the functon 
f <- function(x){
  k1 = ( x*x*(k-1)/(k-x*x) )^0.5
  k2 = ( x*x*k/(k+1-x*x) )^0.5
  y = pt(k1,df=k-1)-pt(k2,df=k)
  return(y)
}
#k = 4 : 25, 100, 500, 1000
for(i in 1:25){
  k <- kset[i]
  res[i] <- uniroot(f,c(1e-1,k^0.5-1e-1))$root  #res can't be 0
}

kr <- cbind(kset,res)
kr

## ------------------------------------------------------------------------
m <- 1e4 
discard <- 200
k<-4
x0 <- c(0.2, 0.4, 0.6, 0.8)

#Gelman-Rubin method of monitoring convergence
Gelman.Rubin <- function(psi) {
    psi <- as.matrix(psi)
    n <- ncol(psi)
    k <- nrow(psi)
    psi.means <- rowMeans(psi) 
    B <- n * var(psi.means) 
    psi.w <- apply(psi, 1, "var")
    W <- mean(psi.w) 
    v.hat <- W*(n-1)/n + (B/n) 
    r.hat <- v.hat / W #G-R statistic
    return(r.hat)
}


prob <- function(y, win) {
        if (y < 0 || y >= 1)
            return (0)
        return( 
          (1/2+y/4)^win[1] * ((1-y)/4)^win[2] * ((1-y)/4)^win[3] * (y/4)^win[4] 
          )
}


normal.chain <- function(m, w){
  
  u <- runif(m) 
  v <- runif(m, -w, w)
  x <- numeric(m)

  x[1] <- w
  win=c(125,18,20,34)

  for (i in 2:m) {
    y <- x[i-1] + v[i]
    if (u[i] <= prob(y, win) / prob(x[i-1], win))
      x[i] <- y 
    else
      x[i] <- x[i-1]
  } 
  return(x)
}

X <- matrix(0, nrow=k, ncol=m)
theta <- numeric(k)

for (i in 1:k){
      X[i, ] <- normal.chain(m, x0[i])
      theta[i] <- mean( X[i,(discard+1):m] )
}
theta
  
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
       psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))
 
#par(mfrow=c(2,2))
#for (i in 1:k)
    #plot(psi[i, (discard+1):m], type="l", xlab=i, ylab=bquote(psi))
#par(mfrow=c(1,1))

#Sequence of the Gelman-Rubin
rhat <- rep(0, m)
for (j in (discard+1):m)
    rhat[j] <- Gelman.Rubin(psi[,1:j])
#plot(rhat[(discard+1):m], type="l", xlab="", ylab="R")
#abline(h=1.1, lty=2)

## ------------------------------------------------------------------------
#the function of caushy
f <- function(x,theta,eta){
    y <- 1/(theta*pi*(1+((x-eta)/theta)^2))
}

res_zero <- integrate(f, lower=-Inf, upper=0,rel.tol=.Machine$double.eps^0.25,theta=1,eta=0)
res_inf <- integrate(f, lower=-Inf, upper=Inf,rel.tol=.Machine$double.eps^0.25,theta=1,eta=0)

res_zero
res_inf

## ------------------------------------------------------------------------
N<-100
tol <- 1e-10
Na. = 28; Nb. = 24; Noo = 41; Nab = 70

res_old <- c(0.1,0.1) #initial est. for p and q

#A function to be minimized 
fr <- function(res){
  
  p0=res_old[1]; q0=res_old[2]; r0=1-p0-q0
  p=res[1]; q=res[2]; r=1-p-q
  
  y =  2*Na.*log(p) + 2*Nb.*log(q) + 2*Noo*log(r) + Nab*log(2*p*q) + 2*r0/(p0+2*r0)*Na.*log(2*r/p) +                                     2*r0/(q0+2*r0)*Nb.*log(2*r/q) 
  
  return(-y)
} 

#A function to return the gradient for the "BFGS", "CG" and "L-BFGS-B" methods
grr <- function(res){
  
  p0=res_old[1]; q0=res_old[2]; r0=1-p0-q0
  p=res[1]; q=res[2]; r=1-p-q
  
  yp= - ( 2*Na./p - 2*Noo/r + Nab/p - 2*r0/(p0+2*r0)*Na.*(1/r+1/p) - 2*r0/(q0+2*r0)*Nb.*(1/r) )
  yq= - ( 2*Nb./q - 2*Noo/r + Nab/q - 2*r0/(p0+2*r0)*Na.*(1/r) - 2*r0/(q0+2*r0)*Nb.*(1/r+1/q) )
  
  return ( c(yp,yq) )
  
}

#p=res[1];q=res[2];r=1-p-q
for(j in 1:N){
  
   
   res <- optim(c(0.1,0.1), fr,grr, method = "L-BFGS-B", lower = c(0.01,0.01), upper = c(0.49,0.49) )$par
   

   if (sum(abs(res - res_old)/res_old) < tol) break
   
   res_old <- res
   
   print(res_old)
}




## ------------------------------------------------------------------------
library(datasets)
formulas <- list(
   mtcars$mpg ~ mtcars$disp,
   mtcars$mpg ~ I(1 / mtcars$disp),
   mtcars$mpg ~ mtcars$disp + mtcars$wt,
   mtcars$mpg ~ I(1 / mtcars$disp) + mtcars$wt
)

rsq <- function(mod){summary(mod)$r.squared}

 #lapply
lms <- lapply(formulas,lm)
lms

#loop
r2 <- numeric(4)
for (i in 1:4){
  LM <- lm(formulas[[i]])
  print(LM)
  r2[i] = rsq(LM)
}

#lapply
print("the R2 of lapply ")
unlist( lapply(lms,rsq) )
#lapply
print("the R2 of loop ")
r2


## ------------------------------------------------------------------------
library(datasets)
set.seed(12345)
bootstraps <- lapply(1:10, function(i) {
    rows <- sample(1:nrow(mtcars), rep = TRUE)
    mtcars[rows,]
})
 #lapply
lms <-lapply(1:10,function(i){
  cars <- bootstraps[i]
  cars <- cars[[1]]
  coe <- lm(cars$mpg ~ cars$disp)
})
lms


#loop
r2<-numeric(10)
for(i in 1:10){
  cars <- bootstraps[[i]]
  LM <- lm(cars$mpg ~ cars$disp)
  print(LM)
  r2[i] <- rsq(LM)
  
}

#lapply
print("the R2 of lapply ")
unlist( lapply(lms,rsq) )
#lapply
print("the R2 of loop ")
r2


## ------------------------------------------------------------------------
set.seed(12345)
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
trials[[1]]$p.value
sapply(1:100, function(i){trials[[i]]$p.value})

## ------------------------------------------------------------------------
library(foreach)
library(datasets)

formulas <- list(
   mtcars$mpg ~ mtcars$disp,
   mtcars$mpg ~ I(1 / mtcars$disp),
   mtcars$mpg ~ mtcars$disp + mtcars$wt,
   mtcars$mpg ~ I(1 / mtcars$disp) + mtcars$wt
)

foreach(i=1:4) %do%
  lm(formulas[[i]])


## ------------------------------------------------------------------------
simple_chitest <- function(x,y){
     
     OK <- complete.cases(x, y)
     x <- factor(x[OK])
     y <- factor(y[OK])
     
     x <- table(x, y)
     
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

set.seed(123456)
x1<-sample(1:10,1000,replace = T)
x2<-sample(10:20,1000,replace = T)

#chisq.test
chisq.test(x1,x2)
##simple_chitest
simple_chitest(x1,x2)

library(microbenchmark)
ts <- microbenchmark(chisq=chisq.test(x1,x2),simple_chitest=simple_chitest(x1,x2))
summary(ts)[,c(1,3,5,6)]





## ------------------------------------------------------------------------
simple_table <- function(x1,x2){
  
  f1 <- factor(x1)
  name1 <- levels(f1)
  f2 <- factor(x2)
  name2 <- levels(f2)
  
  
  dims = c(length(name1),length(name2))
  dimname = list(name1,name2)
  
  y<-array(0,dims,dimname)
  
  n1=length(f1)
  #n2=length(f2)
  #m1=length(name1)
  #m2=length(name2)
  
  for (i in 1:n1) {
    y[f1[i],f2[i]] = y[f1[i],f2[i]] + 1
  }
  y
}

#use simple_table to do simple_chitest2
simple_chitest2 <- function(x,y){
     
     OK <- complete.cases(x, y)
     x <- factor(x[OK])
     y <- factor(y[OK])
     
     x <- simple_table(x, y)
     
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


set.seed(123456)
x1<-sample(1:10,1000,replace = T)
x2<-sample(11:20,1000,replace = T)

#table
ptm <- proc.time()
table(x1,x2)
proc.time() - ptm
#simple_table
ptm <- proc.time()
simple_table(x1,x2)
proc.time() - ptm

#chisq.test
ptm <- proc.time()
chisq.test(x1,x2)
proc.time() - ptm
##simple_chitest
ptm <- proc.time()
simple_chitest2(x1,x2)
proc.time() - ptm


microbenchmark(simple_table(x1,x2))



