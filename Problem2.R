#(a) Naive Monte Carlo

j<-1
Y <- rep(0,100000)
N <- rpois(100000,5)
repeat{
  if(sum(rnorm(N[j],0,1)) > 10){
      Y[j] <- 1
  }
  j=j+1
  if(j==100001){
    break
  }
}
Probability <- sum(Y)/length(N)
Variance <- 100000*Probability*(1-Probability)


#(b)
j<-1
Y <- rep(0,100000)
N <- rpois(100000,5)
repeat{
  if(sum(rnorm(N[j],0,1)) > 10){
    Y[j] <- 1
  }
  j=j+1
  if(j==100001){
    break
  }
}
c <- -cov(Y,N)/var(N)
C_V_reduction <- var(Y+c(N-mean(N)))
Mean <- mean(Y+c(N-mean(N)))

#(c)
N <- rpois(100000,5)
K <- pnorm(10,0,sqrt(N),lower.tail = F)
Probability <- mean(K)
Variance <- var(K)

#(d)
f_N <- function(x){
  return(exp(-5)*5^x/factorial(x))
}
g_N <- function(x){
  return(exp(-10)*10^x/factorial(x))
}
N <- rpois(100000,10)
X<-rep(0,100000)
for(i in 1:length(N)){
  X[i] <- pnorm(10,0,sqrt(N[i]),lower.tail = FALSE)*f_N(N[i])/g_N(N[i])
}
mean(X)
var(X)

#(e)
f_g <- function(x){
  return(exp(-2*x+2))
}
X<-rep(0,100000)
N <- rpois(100000,5)
j<-1
repeat{
  Y <- rnorm(N[j],2,1)
  if(sum(Y)>10){
    X[j]<- prod(f_g(Y))
  }
  j<-j+1
  if(j==100001){
    break
  }
}
mean(X)
var(X)

#(f)
h_l <- function(n){
  return(exp(5)/2^n)
}
f_g <- function(x){
  return(exp(-2*x+2))
}
X<-rep(0,100000)
N <- rpois(100000,10)
j<-1
repeat{
  Y <- rnorm(N[j],2,1)
  if(sum(Y)>10){
    X[j]<- prod(f_g(Y))*h_l(N[j])
  }
  j<-j+1
  if(j==100001){
    break
  }
}
mean(X)
var(X)


