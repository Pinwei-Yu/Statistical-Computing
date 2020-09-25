data <- sort(c(6,7,3,4,7,3,7,2,6,3,7,8,2,1,3,5,8,7))
set.seed(35)
#(a) Compute the corresponding Kolmogorov-Smirnov statistics.
Table_data <- table(data)
Names <- names(Table_data)
L <- length(Names)
p <-mean(data)/8
D <-c()
for(i in c(1:L)){
  e <- length(which(data <=Names[i]))/length(data)
  t <- pbinom(i,8,p)
  d <- abs(e-t)
  D<-c(D,d)
  
}
Kolmogorov_Smirnov_statistics <- max(D)
cat("The corresponding Kolmogorov-Smirnov statistics is :",Kolmogorov_Smirnov_statistics)

#(c)?ˆ°åº•åœ¨å¹³è?Œå?‡å?™è£¡?¢??‰å¹¾?€‹å€¼è?åŽ»è¨ˆç?—pvalueï¼Ÿï?Ÿï?Ÿï?Ÿï??
B = 10000
Kolmogorov_Smirnov_statistics_bootstrap <- c()
for(i in c(1:B)){
  # data_bootstrap <- sample(data,length(data),replace = TRUE)
  data_bootstrap <- rbinom(length(data),8,mean(data)/8)
  p_bootstrap <- mean(data_bootstrap)/8
  Table_data_bootstrap <- table(data_bootstrap)
  Names_bootstrap <- as.numeric(names(Table_data_bootstrap))
  L_bootstrap <- length(Names_bootstrap)
  D <-c()
  for(i in c(1:L_bootstrap)){
    e <- length(which(data_bootstrap <= Names_bootstrap[i]))/length(data_bootstrap)
    t <- pbinom(Names_bootstrap[i],8,p_bootstrap)
    d <- abs(e-t)
    D<-c(D,d)
  }
  K_S_s <- max(D)
  Kolmogorov_Smirnov_statistics_bootstrap <- c(Kolmogorov_Smirnov_statistics_bootstrap,K_S_s)
}
sum(Kolmogorov_Smirnov_statistics_bootstrap > Kolmogorov_Smirnov_statistics) / B


