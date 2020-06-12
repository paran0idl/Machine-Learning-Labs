set.seed(1234567890)
max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
N=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(n in 1:N) {
  k <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[n,d] <- rbinom(1,1,true_mu[k,d])
  }
}
K=3 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations
# Random initialization of the paramters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}

dim(x)
dim(mu)
for(it in 1:max_it) {
  # plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  # points(mu[2,], type="o", col="red")
  # points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.5)
  # E-step: Computation of the fractional component assignments
  v_de=c()
  for(n in 1:N){
    numerator=c()
    for(k in 1:K){
      numerator=c(numerator,pi[k]*prod(mu[k,]^x[n,]*(1-mu[k,])^(1-x[n,])))
    }
    denominator=sum(numerator)
    v_de=c(v_de,denominator)
    z[n,]=numerator/denominator
  }
  #Log likelihood computation.
  llik[it]=sum(log(v_de))
  # sum=0
  # for(n in 1:N){
  #   for(k in 1:K){
  #     sum=sum+(log(pi[k])+sum(x[n,]*log(mu[k,])+(1-x[n,])*(log(1-mu[k,]))))
  #     #sum=sum+z[n,k]*(log(pi[k])+sum(x[n,]*log(mu[k,])+(1-x[n,])*log(1-mu[k,])))
  #     }
  # }
  # llik[it]=sum
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()
  # Stop if the lok likelihood has not changed significantly
  if(it>1){
    if(abs(llik[it]-llik[it-1])<min_change){
      break
    }
  }
  #M-step: ML parameter estimation from the data and fractional component assignments
  pi=apply(z,2,mean)
  for(k in 1:K){
    mu[k,]=0
    for (n in 1:N) {
      mu[k,] = mu[k,] + x[n,] * z[n,k]
    }
    mu[k,] = mu[k,] / sum(z[,k])
  }
}

# pi
# mu
plot(llik[1:it], type="o")
