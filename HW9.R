#(a)
set.seed(35)
n=100000
X1 <- rexp(n,1)
Double_X2 <- 2*rexp(n,1)
Tripple_X3 <- 3*rexp(n,1)
Total <- cbind(X1,Double_X2,Tripple_X3)
K<-apply(Total,1,sum)
Target_a<- which(K>15)
mean(K[Target_a])

#(b)
Target_b <- which(K<1)
mean(K[Target_b])
