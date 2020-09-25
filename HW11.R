## Setting parameters
#initial
theta <- 0.5 
x1 <-38
x2 <- 34
x3and4 <-125
N <- x1+x2+x3and4
x3 <- 125*(theta/(2+theta))
#--------
Updating_theta <- function(x1,x2,x3){
  value <-  (x2+x3)/(x1+x2+x3)
  return(value)
}
#--------
Generating_data <-function(theta,N){
  a1 = 0
  a2 = 0
  a3 = 0
  List <- c()
  for (i in c(1:N)){
    k <- runif(1,0,1)
    a_upper <- (0.5 - theta / 2)
    b_upper <- (0.5 - theta / 4)
    if (k < a_upper) 
      a1 <- a1 + 1
    else if(a_upper<k && k<b_upper)
      a2 <- a2 + 1
    else(b_upper < k && k< 0.5)
      a3 <- a3 + 1
    
  }
  List <- c(a1, a2, a3)
  return(List)
}
#---------
EM <- function(theta,x1,x2,N){
  theta_list <- c()
  for(i in c(1:10000)){
    new_x_list <- Generating_data(theta,N)
    theta_hat <- Updating_theta(new_x_list[1],new_x_list[2],new_x_list[3])
    theta_list<-c(theta_list,theta_hat)
  }
  Max_theta <- max(theta_list)  
  return(Max_theta)
}

Max_Theta_by_EM <- EM(theta,x1,x2,N)
cat("The Max Value Theta Estimated by EM :",Max_Theta_by_EM)
