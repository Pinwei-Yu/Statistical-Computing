
Updating_n_A <- function(xi,lambda,n0){
  n_a<- (n0*xi)/(xi+(1-xi)*exp(-lambda))
  return(n_a)
}

Updating_Xi <- function(n_A,N){
  X<- n_A/N
  return(X)
}

Updating_Lambda <- function(N,na,total_children){
  L <- total_children/(N-na)
  return(L)
}

#-----main programming----#

Obasongs <- c(3062,587,284,103,33,4,2)
n_0<- Obasongs[1]
N <- sum(Obasongs)
sum_of_children <- sum(c(0,1,2,3,4,5,6)*Obasongs)

#(b)
B <- 6

Xi_List <- c(numeric(B))
Xi_List[1]<- 0.75
Lambda_List <-c(numeric(B))
Lambda_List[1] <- 0.4
n_A_List <- c(numeric(B))

t <- 1
while(t<B+1){
  n_A_List[t]<- Updating_n_A(Xi_List[t],Lambda_List[t],n_0)
  Xi_List[t+1] <- Updating_Xi(n_A_List[t],N)
  Lambda_List[t+1] <- Updating_Lambda(N,n_A_List[t],sum_of_children)
  t<-t+1
}
n_B_List <- n_0 - n_A_List

Table <- matrix(numeric(30),nrow = 6,ncol = 5)
Table[,1] <- c(0:5)
Table[,2] <- Xi_List[1:6]
Table[,3] <- Lambda_List[1:6]
Table[,4] <- n_A_List
Table[,5] <- n_B_List
print(Table)

#(c)
B <- 6

Xi_List <- c(numeric(B))
Xi_List[1]<- 0.5
Lambda_List <-c(numeric(B))
Lambda_List[1] <- 0.6
n_A_List <- c(numeric(B))

t <- 1
while(t<B+1){
  n_A_List[t]<- Updating_n_A(Xi_List[t],Lambda_List[t],n_0)
  Xi_List[t+1] <- Updating_Xi(n_A_List[t],N)
  Lambda_List[t+1] <- Updating_Lambda(N,n_A_List[t],sum_of_children)
  t<-t+1
}
n_B_List <- n_0 - n_A_List

Table <- matrix(numeric(30),nrow = 6,ncol = 5)
Table[,1] <- c(0:5)
Table[,2] <- Xi_List[1:6]
Table[,3] <- Lambda_List[1:6]
Table[,4] <- n_A_List
Table[,5] <- n_B_List
print(Table)

