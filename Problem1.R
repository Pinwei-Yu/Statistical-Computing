
#1(b)
fx <- function(x1,x2,x3){
  return((gamma(9)*x1*x2^2*x3^3)/(gamma(2)*gamma(3)*gamma(4)))
}
#proposal: uniform[0,1]^3
max_value <- 0
for (i in seq(0,1,by=0.001)){
  for(j in seq(0,1,by=0.001)){
    now <- fx(i,j,(1-i-j))
    if(now > max_value){
      max_value <- now
    }
  }
}
c<-floor(max_value)+1


f_x1 <-rep(0,10000)
f_x2 <-rep(0,10000)
f_x3 <-rep(0,10000)
N <- rep(0,10000)
i = 1
repeat{
  x1<-runif(1,0,1)
  x2<-runif(1,0,1)
  x3<-(1-x1-x2)
  N[i]<-fx(x1,x2,x3)/c
  if(runif(1,0,1)<=fx(x1,x2,x3)/c){
    f_x1[i] = x1
    f_x2[i] = x2
    f_x3[i] = x3
    i = i+1
  }
  if(i == 10001){break}
}
hist(f_x1)
hist(f_x2)
hist(f_x3)
mean(f_x1)
mean(f_x2)
mean(f_x3)
K <- cbind(f_x1,f_x2,f_x3)
cov(K)
E_N <- mean(N)
Var_N <- var(N)

#1(d)

Y1<-rep(0,10000)
Y2<-rep(0,10000)
Y3<-rep(0,10000)
i<-1
repeat{
  y1<- -sum(log(runif(2,0,1)))
  Y1[i]<-y1
  y2<- -sum(log(runif(3,0,1)))
  Y2[i]<-y2
  y3<- -sum(log(runif(4,0,1)))
  Y3[i]<-y3
  i=i+1
  if(i == 10001){break}
}

M <- cbind(Y1,Y2,Y3)
dominator <- apply(M,1,sum)
X1<- Y1/dominator
X2<- Y2/dominator
X3<- Y3/dominator

hist(X1)
hist(X2)
hist(X3)

N <- cbind(X1,X2,X3)
cov(N)

