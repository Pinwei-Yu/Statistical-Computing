#Set original statement
##At most 2 people waiting in que(not includes 1 being served)
Time<-8
Poisson_rate <- 4
Exponential_rate <-4.2

#Generate the customer occurs time
G_P_t<-0
Arrival_Time <- c()
while (G_P_t<Time){
  G_P_t<-G_P_t - log(runif(1,0,1))/Poisson_rate 
  Arrival_Time <- c(Arrival_Time, G_P_t)
}
Arrival_Time <- Arrival_Time[-length(Arrival_Time)]
A_D_Matrix <-matrix(0,5,length(Arrival_Time)) #Row1:Arrival_Time;Row2:Enter(0)/Leave(1);Row3:Service_duration;Row4:Departure_Time;Row5:Stay_in_System_Time
A_D_Matrix[1,]<-Arrival_Time
#The first 3 customer must be served
A_D_Matrix[3,1]<- -log(runif(1,0,1))/Exponential_rate
A_D_Matrix[4,1]<-A_D_Matrix[1,1]+A_D_Matrix[3,1]
for(i in 2:3){
  if(A_D_Matrix[1,i]<A_D_Matrix[4,i-1]){
    A_D_Matrix[3,i]<- -log(runif(1,0,1))/Exponential_rate
    A_D_Matrix[4,i]<-A_D_Matrix[4,i-1]+A_D_Matrix[3,i]
  }else{
    A_D_Matrix[3,i]<- -log(runif(1,0,1))/Exponential_rate
    A_D_Matrix[4,i]<-A_D_Matrix[1,i]+A_D_Matrix[3,i]
  }
}
  
Being_Served <- function(M,i){
  M[3,i]<- -log(runif(1,0,1))/Exponential_rate
  K<-intersect(which(M[2,]==0) , which(M[4,]>M[1,i])) #only select the column before i-th
  if(length(K)>0){ #stay in que
    M[4,i]<- M[4,K[length(K)]]+M[3,i]
  }else{#Being served directly
    M[4,i]<- M[1,i]+M[3,i]
    }  
  return(M)
}


#Determine leave or or when arrival
##input:Matrix,i output:renew Matrix
Leave_or_not <- function(M,i){
  K<-intersect(which(M[2,]==0) , which(M[4,]>M[1,i]))
  if(M[4,K[length(K)-2]]>M[1,i] && length(K)-2 >0){
    M[2,i]<-1
  }
  return(M)
}

#This function aims to compute the customers spending in the system.
Time_Spending <- function(M,i){
  M[5,i]<-M[4,i]-M[1,i]
  return(M)
}

#Main Programing

for(i in 4: length(Arrival_Time)){
  A_D_Matrix <- Leave_or_not(A_D_Matrix,i)
  if(A_D_Matrix[2,i]!=1){
    A_D_Matrix <- Being_Served(A_D_Matrix,i)
    A_D_Matrix <- Time_Spending(A_D_Matrix,i)
  }
}

print(A_D_Matrix)


A <- apply(A_D_Matrix,1,mean)
Average_Time <-A[5]
print(Average_Time)

Sample_Location <- which(A_D_Matrix[2,]!=1)
Sample<- A_D_Matrix[5,Sample_Location]
Theta<-c()
for(i in 1:length(Sample)){
  theta <- sum(Sample[-i])/((length(Sample))-1)
  Theta<-c(Theta,theta)
}

se_square <- ((length(Theta)-1)^2/length(Theta))*var(Theta)
bias <- (length(Sample)-1)*(mean(Theta)-mean(Sample))
MSE <- se_square + bias
print(MSE)



