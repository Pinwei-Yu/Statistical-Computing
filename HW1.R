set.seed(1)
#11
U=runif(100,0,1)
U2=(1-U^2)^0.5
cor(U,U2)
cor(U^2,U2)

#12（1）
N1=rep(0,100)
for(i in c(1:length(N1)) ){
  n<-0
  c<-0
  j<-1
  k<-runif(100,0,1)
  while(c<1){
    c<-c+k[j]
    n<-n+1
    j<-j+1
  }
  N1[i]<-n
}
Mean_of_N1<-sum(N1[1:100])/100
Mean_of_N1

#12（2）
N2=rep(0,1000)
for(i in c(1:length(N2)) ){
  n<-0
  c<-0
  j<-1
  k<-runif(10000,0,1)
  while(c<1){
    c<-c+k[j]
    n<-n+1
    j<-j+1
   }
  N2[i]<-n
}
Mean_of_N2<-sum(N2[1:1000])/1000
Mean_of_N2

#12（3）
N3=rep(0,10000)
for(i in c(1:length(N3)) ){
  n<-0
  c<-0
  j<-1
  k<-runif(10,0,1)
  while(c<1){
    c<-c+k[j]
    n<-n+1
    j<-j+1
  }
  N3[i]<-n
}
Mean_of_N3<-sum(N3[1:10000])/10000
Mean_of_N3

#12(4) The Expectation of N depends on the sample size and random seed. Approximately, the expectation of N is 2.7.

#老師講解：用cumsum，大於1直接設1，小於1設0，直接加總。（裡面loop
#外面loop用矩陣運算。