#Using nloptr package with R
#Comparing optimization techniques
#Worked in group

library(nloptr)
library(stargazer)
set.seed(100)
print("Generating Matrix...")
N=100000
K=10
X<-matrix(rnorm(N*K,mean=0,sd=1), N, K)
df<-as.data.frame(X)
X[,1] <- 1
eps = rnorm(N, mean = 0, sd = 0.5)
head(eps)
beta <- c(1.5, -1, 0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y <- X %*%  beta + eps
head(Y)

print("#5")
beta_estimates <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta_estimates)
print(beta)
print("The beta_estimates were very close to the given betas in the problem")

print("#6")
set.seed(100)

alpha <- 0.0000003
maxiter <- 500000
objfun <- function(beta,Y,X) {return(sum((Y-X%*%beta)^2))}
gradient <- function(beta,Y,X) {return(as.vector(-2*t(X)%*%(Y-X%*%beta)))}
beta <- runif(dim(X)[2])
beta.All <- matrix("numeric",length(beta),maxiter)

iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

print(iter)
print(paste("The minimum of f(beta,Y,X) is ", beta, sep = ""))

print("#7")
print("Starting L-BFGS")

beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

result_NLOPT_LD_LBFGS <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result_NLOPT_LD_LBFGS)

#Nelder Mead
print("Start Nelder Mead")

xstart <- 5

options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)


result_Nelder_Mead <- nloptr(x0=xstart,eval_f=objfun,opts=options)
print(result_Nelder_Mead)

print("#8")
objfun <- function(theta,Y,X) {
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) )
  return (loglike)
}
theta0 <- runif(dim(X)[2]+1)

options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

result_MLE_Nelder_Mead <- nloptr( x0=theta0,eval_f=objfun,opts=options,Y=Y,X=X)
print(result_MLE_Nelder_Mead)

print("#9")
Q9 <- lm(Y ~ X -1)
print(summary(Q9))
stargazer(Q9)
