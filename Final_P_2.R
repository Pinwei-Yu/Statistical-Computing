data<-c(0.8605 ,1.2175, -0.9772 ,-0.0378 ,2.9478,-0.2710 ,0.0380, 1.1110 ,2.4136, 0.2516,
        0.3485, 0.6765 ,2.7070, 0.5617,1.0066, 2.3637 ,1.4502, 1.6041 ,1.2023 ,1.6049)
f_x <- function(x){
  return(((2*pi)^(-0.5))*exp(-((x)^2)/2))
}
h_u <- function(u,X_list){
  h <- 1
  for(i in c(1:length(data))){
    k <- f_x(X_list[i]-u)
    h<- h*k
  }
  h = h*f_x(u)
  return(h)
}

#(b)
n <- 1
N <- 100000
U<-c(rep(0,N+1))
U[1]<- mean(data)
while(n<N+1){
  v <- runif(1,0,2)
  alpha <- min(h_u(v,data)/h_u(U[n],data),1)
  K <- runif(1,0,1)
  if(K <= alpha){
    U[n+1]=v
  }else{
    U[n+1]=U[n]
  }
  n <-n+1
}

acf(U)

new_U <- rep(0,10000)
for(n in c(1:10000)){
  new_U[n] = U[10*n+1]
}

hist(new_U)
se<-sqrt(var(new_U)/length(new_U))

cat("The mean of sampled mu is :",mean(new_U))
cat("The standard error of sampled mu is :", se)
length(new_U)
#(c)

Box.test(new_U)
