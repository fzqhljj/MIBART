library(Rcpp)
source("C:/Users/jj-li/Desktop/bcf_1.3.1/bcf/R/bcf.R")
# data generating process
p = 3 #two control variables and one moderator
n = 250
#
set.seed(1)

x = matrix(rnorm(n*p), nrow=n)

# create targeted selection
q = -1*(x[,1]>(x[,2])) + 1*(x[,1]<(x[,2]))

# generate treatment variable
pi = pnorm(q)
z = rbinom(n,1,pi)

# tau is the true (homogeneous) treatment effect
tau = (0.5*(x[,3] > -3/4) + 0.25*(x[,3] > 0) + 0.25*(x[,3]>3/4))

# generate the response using q, tau and z
mu = (q + tau*z)

# set the noise level relative to the expected mean function of Y
sigma = diff(range(q + tau*pi))/8

# draw the response variable with additive error
y = mu + sigma*rnorm(n)

# If you didn't know pi, you would estimate it here
pihat = pnorm(q)

bcf_fit = bcf(y, z, x, x, pihat, nburn=2000, nsim=2000,include_pi = "control")

# Get posterior of treatment effects
tau_post = bcf_fit$tau
mu_post  = bcf_fit$mu
tauhat = colMeans(tau_post)
muhat = colMeans(mu_post)
plot(tau, tauhat); abline(0,1)
plot(mu, muhat); abline(0,1)

