#Set original statement
##At most 2 people waiting in que(not includes 1 being served)
Time_Setting<-20
Poisson_rate_Setting <- 2
Exponential_rate_Setting <-1

#-------------RAW Generating-------------#

#Generate the customer occurs time
Generate_First_Three <- function(Time, Poisson_rate, Exponential_rate){
  G_P_t<-0
  Arrival_Time <- c()
  U1s<-c()
  U2s<-c()
  while (G_P_t<Time){
    U1<-runif(1,0,1)
    U1s<-c(U1s,U1)
    G_P_t<-G_P_t - log(U1)/Poisson_rate 
    Arrival_Time <- c(Arrival_Time, G_P_t)
  }
  Arrival_Time <- Arrival_Time[-length(Arrival_Time)]
  U1s <- U1s[-length(U1s)]
  A_D_Matrix <-matrix(0,7,length(Arrival_Time)) #Row1:Arrival_Time;Row2:Enter(0)/Leave(1);Row3:Service_duration;Row4:Departure_Time;Row5:Stay_in_System_Time;Row6:U1;Row7:U2
  A_D_Matrix[1,]<-Arrival_Time 
  A_D_Matrix[6,]<-U1s 
  #The first 3 customer must be served
  U2<-runif(1,0,1)
  U2s<-c(U2s,U2)
  A_D_Matrix[3,1]<- -log(U2)/Exponential_rate
  A_D_Matrix[4,1]<-A_D_Matrix[1,1]+A_D_Matrix[3,1]
  for(i in 2:3){
    UU<-runif(1,0,1)
    U2s<-c(U2s,UU)
    if(A_D_Matrix[1,i]<A_D_Matrix[4,i-1]){
      A_D_Matrix[3,i]<- -log(UU)/Exponential_rate
      A_D_Matrix[4,i]<-A_D_Matrix[4,i-1]+A_D_Matrix[3,i]
    }else{
      A_D_Matrix[3,i]<- -log(UU)/Exponential_rate
      A_D_Matrix[4,i]<-A_D_Matrix[1,i]+A_D_Matrix[3,i]
    }
  }
  for(i in 1:3){
    A_D_Matrix[5,i]<-A_D_Matrix[4,i]-A_D_Matrix[1,i]
    A_D_Matrix[7,i]<-U2s[i]
  }
  return(A_D_Matrix)
}



Generate_First_Three2 <- function(Time,Another_U1,Another_U2, Poisson_rate, Exponential_rate){
  G_P_t<-0
  Arrival_Time <- c()
  i<-1
  while (G_P_t<Time){
    if(length(Another_U1)<i){
      U1<-runif(1,0,1)
    }else{
      U1<-Another_U1[i]
    }
    G_P_t<-G_P_t - log(U1)/Poisson_rate 
    Arrival_Time <- c(Arrival_Time, G_P_t)
    i<-i+1
  }
  Arrival_Time <- Arrival_Time[-length(Arrival_Time)]
  A_D_Matrix <-matrix(0,5,length(Arrival_Time)) #Row1:Arrival_Time;Row2:Enter(0)/Leave(1);Row3:Service_duration;Row4:Departure_Time;Row5:Stay_in_System_Time
  A_D_Matrix[1,]<-Arrival_Time 
  #The first 3 customer must be served
  U2<-Another_U2[1]
  A_D_Matrix[3,1]<- -log(U2)/Exponential_rate
  A_D_Matrix[4,1]<-A_D_Matrix[1,1]+A_D_Matrix[3,1]
  for(i in 2:3){
    UU<-Another_U2[i]
    if(A_D_Matrix[1,i]<A_D_Matrix[4,i-1]){
      A_D_Matrix[3,i]<- -log(UU)/Exponential_rate
      A_D_Matrix[4,i]<-A_D_Matrix[4,i-1]+A_D_Matrix[3,i]
    }else{
      A_D_Matrix[3,i]<- -log(UU)/Exponential_rate
      A_D_Matrix[4,i]<-A_D_Matrix[1,i]+A_D_Matrix[3,i]
    }
  }
  for(i in 1:3){
    A_D_Matrix[5,i]<-A_D_Matrix[4,i]-A_D_Matrix[1,i]
  }
  return(A_D_Matrix)
}

Being_Served <- function(M,Exponential_rate){
  UUU<-runif(1,0,1)
  M[7,i]<-UUU
  M[3,i]<- -log(1-UUU)/Exponential_rate
  K<-intersect(which(M[2,]==0) , which(M[4,]>M[1,i])) #only select the column before i-th
  if(length(K)>0){ #stay in que
    M[4,i]<- M[4,K[length(K)]]+M[3,i]
  }else{#Being served directly
    M[4,i]<- M[1,i]+M[3,i]
  }  
  return(M)
}

Being_Served2 <- function(M,Exponential_rate,Another_U2){
  if(length(Another_U2)<i || Another_U2[i]==0 ){
    UUU<-runif(1,0,1)
  }else{
    UUU<-Another_U2[i]
  }
  M[3,i]<- -log(1-UUU)/Exponential_rate
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




#(a)Raw_Simulation
#(c)Control Variate: S1+...+S10  (S is the service duration)
#(d)Control Variate: (S1+...+S10)-(I1+...+I10)  (S is the service duration;I is the arrival interval)
#(e)Using sum(E[Ti|Ni]) i=1,2,...10
Experiment_List1<-c()
Service_Duration<-c()
S_I_Difference<-c()
Conditional_Expectation <- c()
j<-1
while (j<101) {
  A_D_Matrix<-Generate_First_Three(Time_Setting,Poisson_rate_Setting,Exponential_rate_Setting)
  for(i in 4: ncol(A_D_Matrix)){
    A_D_Matrix <- Leave_or_not(A_D_Matrix,i)
    if(A_D_Matrix[2,i]!=1){
      A_D_Matrix<-Being_Served(A_D_Matrix,Exponential_rate_Setting)
      A_D_Matrix <- Time_Spending(A_D_Matrix,i)
    }
  }
  Sample_Location <- which(A_D_Matrix[2,]!=1)
  if(length(Sample_Location)<10){
    next()
  }
  j<-j+1
  Sample<- sum(A_D_Matrix[5,Sample_Location[1:10]])
  Experiment_List1<-c(Experiment_List1,Sample)
  SD<- sum(A_D_Matrix[3,Sample_Location[1:10]])
  Service_Duration<-c(Service_Duration,SD)
  SI<- SD - (A_D_Matrix[1,Sample_Location[10]] - A_D_Matrix[1,Sample_Location[1]])
  S_I_Difference <- c(S_I_Difference, SI)

  if(A_D_Matrix[1,Sample_Location[2]] < A_D_Matrix[4,Sample_Location[1]]){
    N_0 <- 1 #The first customer doesn't need to wait.
    N_1 <- 1
  }else{
    N_0 <- 2
    N_1 <- 0
  }
  N_2 <- 0
  for(i in 3:10){
    if(A_D_Matrix[1,Sample_Location[i]] < A_D_Matrix[4,Sample_Location[i-2]]){
      N_2 <- N_2+1
    }else if(A_D_Matrix[1,Sample_Location[i]] < A_D_Matrix[4,Sample_Location[i-1]]) {
      N_1 <- N_1+1
    }else{
      N_0 <- N_0+1
    }
  }
  CE<-(1*N_0+2*N_1+3*N_2)
  Conditional_Expectation<- c(Conditional_Expectation,CE)
}
C1<- -cov(Experiment_List1,Service_Duration)/var(Service_Duration)
Adjustment1<- mean(Experiment_List1+ C1*(Service_Duration-mean(Service_Duration)))
CV1<- var(Experiment_List1) - (cov(Experiment_List1,Service_Duration))^2/var(Service_Duration)
C2<- -cov(Experiment_List1,S_I_Difference)/var(S_I_Difference)
Adjustment2<- mean(Experiment_List1+ C2*(S_I_Difference-mean(S_I_Difference)))
CV2<- var(Experiment_List1) - (cov(Experiment_List1,S_I_Difference))^2/var(S_I_Difference)
#Check whether it's the unbiased estimator
mean(Experiment_List1)
print(Adjustment1)
print(Adjustment2)
mean(Conditional_Expectation)


cat('(a)The Variance of Raw Simulation: ',var(Experiment_List1))
cat('(c)The Variance Improved by Using Service Time as Control Variate: ',CV1)
cat('(d)The Variance Improved by Using Service_Arrival Difference as Control Variate: ',CV2)
cat('(e)The Variance Improved by Using sum(E[Ti|Ni]) i=1,2,...10: ',var(Conditional_Expectation))
#(b)Antithetic Generating
Experiment_List3<-c()
Experiment_List4<-c()
j<-1
while(j<51){
  A_D_Matrix<-Generate_First_Three(Time_Setting,Poisson_rate_Setting,Exponential_rate_Setting)
  for(i in 4: ncol(A_D_Matrix)){
    A_D_Matrix <- Leave_or_not(A_D_Matrix,i)
    if(A_D_Matrix[2,i]!=1){
      A_D_Matrix<-Being_Served(A_D_Matrix,Exponential_rate_Setting)
      A_D_Matrix <- Time_Spending(A_D_Matrix,i)
    }
  }
  
  Sample_Location <- which(A_D_Matrix[2,]!=1)
  if(length(Sample_Location)<10){
    next()
  }
  j<-j+1
  Sample<- sum(A_D_Matrix[5,Sample_Location[1:10]])
  Experiment_List3<-c(Experiment_List3,Sample)
  
  Another_U1<-A_D_Matrix[6,]
  Another_U2<-A_D_Matrix[7,]
  A_D_Matrix2<-Generate_First_Three2(Time_Setting, Another_U1,Another_U2,Poisson_rate_Setting,Exponential_rate_Setting)
  for(i in 4: ncol(A_D_Matrix2)){
    A_D_Matrix2 <- Leave_or_not(A_D_Matrix2,i)
    if(A_D_Matrix2[2,i]!=1){
      A_D_Matrix2<-Being_Served2(A_D_Matrix2,Exponential_rate_Setting,Another_U2)
      A_D_Matrix2 <- Time_Spending(A_D_Matrix2,i)
    }
  }
  Sample2<- sum(A_D_Matrix2[5,Sample_Location[1:10]])
  Experiment_List4<-c(Experiment_List4,Sample2)
}

k<-length(Experiment_List4)
while(k<50){
  A_D_Matrix<-Generate_First_Three(Time_Setting,Poisson_rate_Setting,Exponential_rate_Setting)
  for(i in 4: ncol(A_D_Matrix)){
    A_D_Matrix <- Leave_or_not(A_D_Matrix,i)
    if(A_D_Matrix[2,i]!=1){
      A_D_Matrix<-Being_Served(A_D_Matrix,Exponential_rate_Setting)
      A_D_Matrix <- Time_Spending(A_D_Matrix,i)
    }
  }
  
  Sample_Location <- which(A_D_Matrix3[2,]!=1)
  if(length(Sample_Location)<10){
    next()
  }
  k<-k+1
  Sample<- sum(A_D_Matrix[5,Sample_Location[1:10]])
  Experiment_List4<-c(Experiment_List4,Sample)
}

cat('(b)The Variance by Using Antithetic Variables: ',var(c(Experiment_List1,Experiment_List2)))


