#1
#Acceptance-Rejection (pdf)
Fx<-function(x){
  return (0.5*(x+x^2))
}
fx<-function(x){
  return (x+0.5)
}
range_f <- seq(0,1,by=0.01)

fy<-function(x){
  return((2/3)*(1+x))
}
inverse_G <- function(y){ #cdf
  return ((3*(y+1/3))^0.5-1)
}
c <-max(fx(range_f)/fy(range_f))

c
f_x <-rep(0,10000)

i = 1
repeat{
  x<-inverse_G(runif(1,0,1))
  if(runif(1,0,1)<=fx(x)/(c*fy(x))){
    f_x[i] = x
    i = i+1
    }
  if(i == 10001){break}
}
plot(ecdf(f_x)) #generated cdf one
lines(seq(0,1,length.out = 100),Fx(seq(0,1,length.out = 100)),col="red") #given cdf one

#Inverse version(know cdf then generate distribution)
Fx<-function(x){
  return (0.5*(x+x^2))
}
range_f <- seq(0,1,by=0.01)
inverse_f<-function(y){
  return((2*(y+0.125))^0.5-0.5)
}
plot(ecdf(inverse_f(runif(10000,0,1)))) #自己生的
lines(seq(0,1,length.out = 100),Fx(seq(0,1,length.out = 100)),col="red") #原本的

#Composition Version 
g1<- function(x){
  return(1)
}
g2 <- function(x){
  return(2*x)
}
G2<-function(x){
  return (x^2)
}
inverse_G2<-function(y){
  return(sqrt(y))
}

U1<-runif(10000,0,1)
vec = numeric(10000)
for(i in 1:10000){
  if(0<=U1[i] &&U1[i]<0.5){
    vec[i] = runif(1,0,1)
  }else{
    vec[i] = inverse_G2(runif(1,0,1))
  }
}
plot(ecdf(vec)) #自己生的
lines(seq(0,1,length.out = 100),Fx(seq(0,1,length.out = 100)),col="red") #原本的

#3
#Acceptance-rejection
fx<-function(x){
  return (0.25+2*x^3+1.25*x^4)
}
range_f <- seq(0,1,by=0.01)

fy<-function(x){
  return(5/31*(x+1)^4)
}

c <-max(fx(range_f)/fy(range_f))
c
inverse_G <- function(y){ #cdf
  return ((31*(y+1/31))^(1/5)-1)
}
f_x <-rep(0,10000)

i = 1
repeat{
  x<-inverse_G(runif(1,0,1))
  if(runif(1,0,1)<=fx(x)/(c*fy(x))){
    f_x[i] = x
    i = i+1
  }
  if(i == 10001){break}
}


Fx<-function(x){
  return (0.25*x+0.5*x^4+0.25*x^5)
}
plot(ecdf(f_x)) #generated cdf one
lines(seq(0,1,length.out = 100),Fx(seq(0,1,length.out = 100)),col="red") #given cdf one

#3 Composition Method
g1<- function(x){
  return(1)
}
g2 <- function(x){
  return(4*x^3)
}
g3 <- function(x){
  return(5*x^4)
}
G2<-function(x){
  return (x^4)
}
G3<-function(x){
  return (x^5)
}
inverse_G2<-function(y){
  return(y^(0.25))
}
inverse_G3<-function(y){
  return(y^(0.2))
}
U1<-runif(10000,0,1)
vec = numeric(10000)
for(i in 1:10000){
  if(0<=U1[i] &&U1[i]<0.25){
    vec[i] = runif(1,0,1)
  }else if(0.25<=U1[i] &&U1[i]<0.75){
    vec[i] = inverse_G2(runif(1,0,1))
  }else{
    vec[i] = inverse_G3(runif(1,0,1))
  }
}
plot(ecdf(vec)) #自己生的
lines(seq(0,1,length.out = 100),Fx(seq(0,1,length.out = 100)),col="red") #原本的

#2
#Acceptance-rejection
Fx<-function(x){
  return (1-exp(-x)-exp(-2*x)+exp(-3*x))
}
fx<-function(x){
  return (exp(-x)+2*exp(-2*x)-3*exp(-3*x))
}
range_f <- seq(0,1,by=0.01)

fy<-function(x){
  return(exp(-x))
}
inverse_G <- function(y){ #cdf
  return (-log(y))
}
c <-max(fx(range_f)/fy(range_f))
c

f_x <-rep(0,10000)

i = 1
repeat{
  x<-inverse_G(runif(1,0,1))
  if(runif(1,0,1)<=fx(x)/(c*fy(x))){
    f_x[i] = x
    i = i+1
  }
  if(i == 10001){break}
}

plot(ecdf(f_x)) #generated cdf one
lines(seq(0,,length.out = 100),Fx(seq(0,,length.out = 100)),col="red") #given cdf one

#2 inverse method
Fx<-function(x){
  return (1-exp(-x)-exp(-2*x)+exp(-3*x))
}
f <- function(x,a){
  return (1-exp(-x)-exp(-2*x)+exp(-3*x)-a)
}

U1<-runif(10000,0,1)
vec = numeric(10000)
for(i in 1:10000){
  root<-uniroot(f,interval = c(0,100),a = U1[i])$root
  vec[i]<-root
}
plot(ecdf(vec)) 
lines(seq(0,max(vec),length.out = 100),Fx(seq(0,max(vec),length.out = 100)),col="red")

