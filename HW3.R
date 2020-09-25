# Box-Muller Transformation
U1 <- runif(10000,0,1)
U2 <- runif(10000,0,1)
Normal_Distribution<- sqrt(-2*log(U1))*cos(2*pi*U2)
hist(Normal_Distribution,probability = T)
lines(seq(-4,4,length.out = 100),dnorm(seq(-4,4,length.out = 100)),col="red")

#Poisson Process
lambda <- function(t){
  return(3+4/(t+1))
}
lambda_zero<-lambda(0)
t=0;I=0
S = 0
while(t<11){
  t<-t-(1/lambda_zero)*log(runif(1,0,1))
  if (runif(1,0,1)<=lambda(t)/lambda_zero){
    I=I+1
    S = c(S,t)
  }
}
I
S
