em <- function (x,
                K = 3,
                min_change = 0.0001, # min change in log likelihood between two consecutive EM iterations
                max_it = 1 # max number of EM iterations
) {
  
  z <- matrix(nrow = nrow(x), ncol=K) # fractional component assignments
  pi <- runif(K,0.49,0.51)
  pi
  pi <- pi / sum(pi)
  
  mu <- matrix(runif(K * ncol(x), 0.49, 0.51), nrow = K) # conditional distributions
  print(mu)
  cat('Initial parameters: \n')
  print(list(pi = pi, mu = mu))
  
  llik <- vector(length = max_it) # log likelihood of the EM iterations
  print(mu[1,]^x[1,])
  print((1-mu[1,])^(1-x[1,]))
  print(mu[1,]^x[1,]*(1-mu[1,])^(1-x[1,]))
  for(it in 1:max_it) {
    plot(mu[1,], type="o", col="blue", ylim=c(0,1))
    points(mu[2,], type="o", col="red")
    points(mu[3,], type="o", col="green")
    
    ## E-step: Computation of the fractional component assignments
    ## Observation joint probability p(x_i, k_i) for each observation
    ## i. This p can accept K many possible values of k_i. So there are
    ## this matrix: Each columns are a possibility of k_i, each rows are
    ## observation. Hence each rows fully defines p(x_i, k_i) for any
    ## k_i. Summing each rows would marginalise about k, hence gives an
    ## estimated likelihood of observing the ith observation, that is,
    ## p(x_i).
    
    obs.kx <- outer(1:nrow(x), 1:K, FUN = Vectorize(function (obs, k) {
      pi[k] * prod(mu[k,]^x[obs,] * (1 - mu[k,])^(1 - x[obs,]))
      # print(k)
      # print(obs)
      # print(prod(mu[k,]^x[obs,]))
    }))
    ## Observation likelihood
    obs.x <- rowSums(obs.kx)
    ## p(k|x_i) for each K many possibilities of k, and each observation
    ## using the Bayes theorem
    k.cond.xi <- diag(1 / obs.x) %*% obs.kx
    ## Log likelihood computation.
    llik[it] <- sum(log(obs.x))
    
    cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
    flush.console()
    
    ## Stop if the lok likelihood has not changed significantly
    change <- abs(llik[it - 1] - llik[it])
    if (it == max_it || (length(change) != 0 && change < min_change))
      break
    
    ## M-step: ML parameter estimation from the data and fractional
    ##         component assignments
    pi <- apply( k.cond.xi, 2, mean )
    ## Just matrix version of the formula on the textbook
    mu <- t(k.cond.xi %*% diag(1 / colSums(k.cond.xi))) %*% x
  }
  
  cat('End results: \n')
  print(list(pi = pi, mu = mu))
  
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  
  ##        plot(llik[1:it], type="o")
  
}

gen_data <- function () {
  N <- 1000 # number of training points
  D <- 10 # number of dimensions
  
  within( list(), {
    true_pi <- c(1/3, 1/3, 1/3) # true mixing coefficients
    
    true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
    true_mu[1,] <- c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
    true_mu[2,] <- c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
    true_mu[3,] <- c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
    
    plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
    points(true_mu[2,], type="o", col="red")
    points(true_mu[3,], type="o", col="green")
    
    ## Producing the training data
    x <- matrix(nrow = N, ncol = D) # training data
    for(n in 1:N) {
      for(d in 1:D) {
        x[n,d] <- rbinom(1, 1, true_mu[sample(1:3,1,prob=true_pi), d])
      }
    }
    rm(n, d)
  })
}



set.seed(1234567890)
d <- gen_data()
em(d$x)