People_Size<-300
Illness_Period_Lower_Bound<-140
Illness_Period_Upper_Bound<-480
Duration <- 1800
r=50
Infection_Rate<-0.9
Location_upper_bound <- 100
Location_lower_bound <- 0
Original_Infection_Number <- 2

M <- matrix(0,4,People_Size)
X_0<-runif(People_Size,Location_lower_bound ,Location_upper_bound )
Y_0<-runif(People_Size,Location_lower_bound ,Location_upper_bound )
M[c(1,2),]<-M[c(1,2),]+rbind(X_0,Y_0)
Infection_0<-sample(1:People_Size, size = Original_Infection_Number)
M[3,Infection_0]<-1
Recover_days<-sample(Illness_Period_Lower_Bound:Illness_Period_Upper_Bound, size = Original_Infection_Number, replace = TRUE)
M[4,Infection_0]<-Recover_days
Susceptible <-rep(0,Duration)
Infectious <-rep(0,Duration)
Removed <-rep(0,Duration)
Susceptible[1] <- sum(M[3,]==0)
Infectious[1] <-sum(M[3,]==1)
Removed[1] <-sum(M[3,]==2)
Cured<-c()
M
plot(M[1,],M[2,])
#Random_Walk:(input=M)(output=newM)
Random_Walk <- function(M){
  random_walk_x <- 0.3*Location_upper_bound*cos(runif(People_Size,0,2*pi))
  random_walk_y <- 0.3*Location_upper_bound*sin(runif(People_Size,0,2*pi))
  
  #print(dim(M[c(1,2),]))
  #print(dim(rbind(random_walk_x,random_walk_y)))
  M[c(1,2),]<-M[c(1,2),]+rbind(random_walk_x,random_walk_y)

  ind_x<-union(which(M[1,]<Location_lower_bound),which(M[1,]>Location_upper_bound))
  M[1,ind_x]<-M[1,ind_x]-2*random_walk_x[ind_x]
  ind_y<-union(which(M[2,]<Location_lower_bound),which(M[2,]>Location_upper_bound))
  M[2,ind_y]<-M[2,ind_y]-2*random_walk_y[ind_y]
 
  return(M)
  
}
 
Check_Potential_People<-function(M_candidate){
  M_candidate <- M
  Spread<-which(M_candidate[3,]==1)
  D<-matrix(0,4,length(Spread))
  D[c(1,2)] <- M_candidate[c(1,2),Spread]+ matrix(r,2,length(Spread))
  D[c(3,4)] <- M_candidate[c(1,2),Spread]- matrix(r,2,length(Spread))
  
  Potential_People<-c()
  if(length(Spread)!=0){
    for(i in 1:length(Spread)){
      Potential<- Reduce(intersect,  list(which(M_candidate[1,]<D[1,i]),
                              which(M_candidate[1,]>D[3,i]),
                              which(M_candidate[2,]<D[2,i]),
                              which(M_candidate[2,]>D[4,i]),
                              which(M_candidate[3,]==0)
                              )
                  )
      
      if(length(Potential)!=0){
        delta_square = as.matrix((M_candidate[c(1,2),Potential]-M_candidate[c(1,2),Spread[i]])^2)
        target<-intersect(which(apply(delta_square,2,sum)< r^2 ),which(M_candidate[3,]==0))
        Potential_People<-c(Potential_People,target)
      }
    }
  }
  return(Potential_People)
}
  
Check_Infection_Rate<- function(Potential_People){
  #names(table(Potential_People))
  #as.numeric(table(Potential_People))
  I<-as.data.frame(table(Potential_People))
  p<-matrix(0,2,nrow(I) )
  if(nrow(I)!=0) {
    p[1,]<- as.integer(as.character(I[,1])) 
    p[2,]<-1-((1-Infection_Rate)^I[,2])
  }
  return(p)
}

Doing_Infection<-function(p,M_candidate){
  random <- runif(ncol(p),0,1)
  Determine<- p[2,]>random 
  Probable_People<-p[1,which(Determine==TRUE)]
  Healthy_People<-which(M_candidate[3,]==0)
  Infection_People <- intersect(Probable_People,Healthy_People)
  M_candidate[3,Infection_People]<-1
  Ill_Days<- sample(Illness_Period_Lower_Bound:Illness_Period_Upper_Bound, size = length(Infection_People),replace = TRUE)
  M_candidate[4,Infection_People]<-Ill_Days
  return(M_candidate)
}
  
Check_Cured <- function(M) {
  remove_people<-intersect(which(M[4,]==0),which(M[3,]==1))
  Cured <- c(Cured,remove_people)
  return(Cured)
}



for(i in 1:Duration){
  M<-Random_Walk(M)
  Potential_People<-Check_Potential_People(M) #return is the position
  if(length(Potential_People)!=0){
    Infection_Rate_Matrix <- Check_Infection_Rate(Potential_People)
    M<- Doing_Infection(Infection_Rate_Matrix,M)
  }
  Cured<-Check_Cured(M) 
  if(length(Cured)!=0){
    M[3,Cured]<-2
  }
    
  
  Susceptible_Num <- sum(M[3,]==0)
  Infectious_Num  <-sum(M[3,]==1)
  Removed_Num<-sum(M[3,]==2)
  
  Susceptible[i] <- Susceptible_Num
  Infectious[i] <-Infectious_Num
  Removed[i] <-Removed_Num
  m<-which(M[4,]!=0)
  M[4,m]<-M[4,m]-1
}
Susceptible
Infectious
Removed


# Packages
library(ggplot2)
library(dplyr)

# create data
time <- c(1:Duration ) # x Axis
Gross_Amount_of_People <- c(Susceptible,Infectious,Removed)              # y Axis
group <-rep( c("Susceptible", "Infectious" ,"Removed" ) , each=Duration)        # group, one shape per group
data <- data.frame(time, Gross_Amount_of_People , group)

# stacked area chart
data$group <- factor(data$group , levels=c("Removed", "Infectious" , "Susceptible") )
Grapgh1<- ggplot(data, aes(x=time, y=Gross_Amount_of_People, fill=group)) + 
  geom_area()
