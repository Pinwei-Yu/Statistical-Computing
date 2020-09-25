Gibbs = function(r.trans,nsimu,x0){
  x = matrix(0,nrow = length(x0), ncol = nsimu)
  xc = x0
  for(i in 1:nsimu){
    xp = r.trans(xc)
    xc = xp
    x[,i] = xc
  }
  return(x)
}
#Set alpha=beta=lambda=5
trans2 = function(xsamp){ #xsamp = (i,y,n)
  i = xsamp[1]
  y = xsamp[2]
  n = xsamp[3]
  alpha = 5
  beta = 5
  lambda = 5
  i = rbinom(1, n, y) # n>=i 
  y = rbeta(1, i+alpha, n-i+beta)
  n = rpois(1,(1-y)*lambda)+i
  return(c(i, y, n))
}

set.seed(35)
nsimu = 10^4
x0 = c(1,0.5,2)
result = Gibbs(trans2, nsimu, x0)
#plot(1:10000, result[2,1:10000])

acf(result[1,],lag.max = 20,main = "acf of first element")

X = result[1,seq(from=1000,to=nsimu,by=20)]
Y = result[2,seq(from=1000,to=nsimu,by=20)]
N = result[3,seq(from=1000,to=nsimu,by=20)]
mean(X)
mean(Y)
mean(N)

