library(nloptr)
library(stargazer)
set.seed(100)
N <- 100000
K <- 10
sigma <- 0.5
X <- matrix(rnorm(N*K,mean=0,sd=sigma), N, K)
X[,1] <- 1
eps <- rnorm(N,mean=0,sd=sigma)
beta.true <- c(1.5,-1,-.25,.75,3.5,-2,.5,1,1.25,2)
y <- X%*%beta + eps
beta.matrix <- solve(t(X)%*%X)%*%(t(X)%*%y)
#GD
alpha <- 0.0000003
gradient <- function(beta,y,X) {
  return(as.vector(-2*t(X)%*%(y-X%*%beta)))
}
iter <- 1
beta0 <- 0*beta.matrix
beta <- runif(dim(X)[2])
while(norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,y,X)
  if(iter%%10000==0){
    print(beta)
  }
  iter <- iter+1
}
beta.GD <- beta
#LBFGS
objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}
beta0 <- runif(dim(X)[2])
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
print(result)
beta.LBFGS <- result$solution
#NM
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)
result1 <- nloptr( x0=beta0,eval_f=objfun,opts=options,y=y,X=X)
beta.NM <- result1$solution
#MLS
objfun  <- function(theta,y,X) {
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) )
  return (loglike)
}
gradient <- function(theta,y,X) {
  grad <- as.vector(rep(0,length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(y - X%*%beta)/(sig^2)
  grad[length(theta)] <- dim(X)[1]/sig-crossprod(y-X%*%beta)/(sig^3)
  return(grad) }
beta0 <- runif(dim(X)[2]+1)
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
result2 <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
beta.MLS <- result2$solution
#Regression
beta.reg <- lm(y ~ X -1)
#All Results
stargazer(beta.GD)
stargazer(beta.LBFGS)
stargazer(beta.MLS)
stargazer(beta.NM)
stargazer(beta.reg)
