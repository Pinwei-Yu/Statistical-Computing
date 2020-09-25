library(doParallel)
library(foreach)
library(doSNOW)
library(pracma)
#cpu.cores <- detectCores()
#cl <- makeCluster(4)
#registerDoParallel(cl)


set.seed(35)
Calculate_Reward <- function(initial_reward , reward_matrix, city_sequence){
  reward <- initial_reward[city_sequence[1]]
  for(i in c(1:9)){
    r <- reward_matrix[city_sequence[i],city_sequence[i+1]]
    reward <- reward + r
  }
  return(reward)
}


#B is the Markov-Chain length
Simulated_Annealing <- function(B,U_0_k,Reward_Matirx){
  X_0 <- randperm(10, 10)
  n <- 0
  anealing_reward <- c()
  cities_order <- matrix(numeric(10*B),nrow = 10 ,ncol = B)
  while(n<B){
    X_n <- X_0
    y <-randperm(10, 10)
    V_x_n <- Calculate_Reward(U_0_k,Reward_Matirx,X_n)
    V_y <- Calculate_Reward(U_0_k,Reward_Matirx,y)
    alpha<-min((1+n)^(V_y)/(1+n)^(V_x_n),1)
    if(runif(1,0,1)<alpha){
      X_n <- y
    }
    anealing_reward <- c(anealing_reward,V_y)
    cities_order[,n] <- y
    n <- n+1
  }
  Maximum_reward <- max(anealing_reward)
  Corresponding_cities_order <- cities_order[,which(anealing_reward == Maximum_reward)[1]]
  List <- c(Maximum_reward,Corresponding_cities_order)
  return(List)
}
  
#-------Main Programming-------#
#(b)
U_0_k <- runif(10,0,1)
Reward_Matirx <- matrix(0,nrow = 10, ncol = 10) #row index is the start city
for(i in c(1:10)){
  for(j in c(1:10)){
    if(i!=j){
      Reward_Matirx[i,j]=runif(1,0,1)
    }
  }
}
print(Reward_Matirx)

#(c)
Reward_order <- Simulated_Annealing(10000,U_0_k,Reward_Matirx)
cat("The Maximum Reward is:",Reward_order[1])
cat("The Optimal Cities-Visited-Order is:",Reward_order[2:11])

#(d)

#finalMatrix <- foreach(i=1:10^2, .combine = cbind, .packages = 'topicmodels') %dopar% {
  #ccc<- Simulated_Annealing(1000,U_0_k,Reward_Matirx)[1]
 # ccc
#}


#stopCluster(cl)
##----
i <- 1
R<- c(numeric(1000000))
while(i<1000001){
  R[i]<-Simulated_Annealing(1000,U_0_k,Reward_Matirx)[1]
  print(i)
  i<-i+1
  
}
R_table <- as.table(R)
#View(R_table)
#R_table
#write.csv(R,file="/Users/brenda/Documents/三下/統計計算/Final Project/Simulted+Annealing.csv",row.names = FALSE)
#----

Mean_Maximum_Reward <- mean(R)

Variance_Maximum_Reward <-var(R)

cat("The Mean of Maximum Reward is:",Mean_Maximum_Reward)
cat("The Variance of Maximum Reward is:",Variance_Maximum_Reward)
