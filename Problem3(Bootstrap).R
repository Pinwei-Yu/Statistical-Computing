Sample <-c(0.0839, 0.0205 ,0.3045, 0.7816 ,0.0003, 0.0095 ,0.4612, 0.9996, 0.9786 ,0.7580, 0.0002, 0.7310 ,0.0777 ,0.4483, 0.4449 ,0.7943 ,0.1447 ,0.0431, 0.8621 ,0.3273)
length(Sample)
mean(Sample)
median(Sample)

#Jacknife
Theta<-c()
for(i in 1:length(Sample)){
  theta <- sum(Sample[-i])/((length(Sample))-1)
  Theta<-c(Theta,theta)
}
hist(Theta)
se_square <- ((length(Theta)-1)^2/length(Theta))*var(Theta)

bias <- (length(Sample)-1)*(mean(Theta)-mean(Sample))
MSE <- se_square + bias
lower_bound <- median(Sample)-1.96*sqrt(se_square)
upper_bound <- median(Sample)+1.96*sqrt(se_square)
cat('[Jacksknife] 95%-confidence interval: [',lower_bound,',',upper_bound,']')

#Bootstrap
B = 10000
bootstrap_data = numeric(B)
for(i in c(1:B)){
  bootstrap_data[i] = mean(sample(Sample,length(Sample),replace = TRUE))
}
se_B_square = var(bootstrap_data) 
lower_bound <- median(Sample)-1.96*sqrt(se_B_square)
upper_bound <- median(Sample)+1.96*sqrt(se_B_square)
cat('[Bootstrap] 95%-confidence interval: [',lower_bound,',',upper_bound,']')
hist(bootstrap_data)
#Parametric Bootstrap
M<-mean(Sample)
V<-var(Sample)
a <- M^2*(1-M)/V - M
b <- (1-M)*a/M

j<-1
N <- 10000
alpha <- rep(0,N)
beta <- rep(0,N)
repeat{
  K <- rbeta(10000,a,b)
  MM <- mean(K)
  VV <- var(K)
  al <- MM^2*(1-MM)/VV - MM
  be <- (1-MM)*al/MM
  alpha[j]<-al
  beta[j]<-be
  j <- j+1
  if(j==10001){
    break
  }
}
hist(alpha)
hist(beta)
MSE_Alpha_hat <- sum((alpha-a)^2)/N
MSE_Beta_hat <- sum((beta-b)^2)/N
lower_bound <- median(Sample)-1.96*sqrt(MSE_Alpha_hat)
upper_bound <- median(Sample)+1.96*sqrt(MSE_Beta_hat)
cat('[Parametric Bootstrap] 95%-confidence interval: [',lower_bound,',',upper_bound,']')

