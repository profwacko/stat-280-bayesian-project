#library(devtools)
#install_github("theodds/SoftBART")

library(SoftBart)

## Functions used to generate fake data
set.seed(42)
f_fried <- function(x) 10 * sin(pi * x[,1] * x[,2]) + 20 * (x[,3] - 0.5)^2 + 
  10 * x[,4] + 5 * x[,5]

our_function <- function(x) sin(x[,1])*exp(x[,2])*10 + cos(2*x[,3]) - 20*x[,4]^2 + 3*x[,5]^3 - 15*x[,6]^(1/3) 


gen_data_1 <- function(n_train, n_test, P, sigma) {
  X <- matrix(runif(n_train * P), nrow = n_train)
  mu <- f_fried(X)
  X_test <- matrix(runif(n_test * P), nrow = n_test)
  mu_test <- f_fried(X_test)
  Y <- mu + sigma * rnorm(n_train)
  Y_test <- mu_test + sigma * rnorm(n_test)
  
  return(list(X = X, Y = Y, mu = mu, X_test = X_test, Y_test = Y_test, mu_test = mu_test))
}

gen_data_2 <- function(n_train, n_test, P, sigma) {
  X <- matrix(runif(n_train * P,-1000,1000), nrow = n_train)
  mu <- our_function(X)
  X_test <- matrix(runif(n_test * P,-1000,1000), nrow = n_test)
  mu_test <- our_function(X_test)
  Y <- mu + sigma * rnorm(n_train)
  Y_test <- mu_test + sigma * rnorm(n_test)
  
  return(list(X = X, Y = Y, mu = mu, X_test = X_test, Y_test = Y_test, mu_test = mu_test))
}



## Simulate normal datasets
sim_data_1 <- gen_data_1(250, 100, 5, 1)
sim_data_2 <- gen_data_2(250, 100, 5, 1)

## Fit the models
softbart_fit_1 <- softbart(X = sim_data_1$X, Y = sim_data_1$Y, X_test = sim_data_1$X_test, 
                hypers = Hypers(sim_data_1$X, sim_data_1$Y, num_tree = 50, temperature = 1),
                opts = Opts(num_burn = 5000, num_save = 5000, update_tau = TRUE))

plot(softbart_fit_1)

## Look at posterior model inclusion probabilities for each predictor. 

posterior_probs <- function(fit) colMeans(fit$var_counts > 0)
plot(posterior_probs(softbart_fit_1), 
     col = ifelse(posterior_probs(softbart_fit_1) > 0.5, muted("blue"), muted("green")), 
     pch = 20)

rmse <- function(x,y) sqrt(mean((x-y)^2))

rmse(softbart_fit_1$y_hat_test_mean, sim_data_1$mu_test)
rmse(softbart_fit_1$y_hat_train_mean, sim_data_1$mu)

## Fit the model
softbart_fit_2 <- softbart(X = sim_data_2$X, Y = sim_data_2$Y, X_test = sim_data_2$X_test, 
                           hypers = Hypers(sim_data_2$X, sim_data_2$Y, num_tree = 50, temperature = 1),
                           opts = Opts(num_burn = 5000, num_save = 5000, update_tau = TRUE))

## Plot the fit (note: interval estimates are not prediction intervals, 
## so they do not cover the predictions at the nominal rate)
plot(softbart_fit_2)
softbart_fit_1$y_hat_test_mean
## Look at posterior model inclusion probabilities for each predictor. 

plot(posterior_probs(softbart_fit_2), 
     col = ifelse(posterior_probs(fit) > 0.5, muted("blue"), muted("green")), 
     pch = 20)

rmse <- function(x,y) sqrt(mean((x-y)^2))

rmse(softbart_fit_2$y_hat_test_mean, sim_data_2$mu_test)
rmse(softbart_fit_2$y_hat_train_mean, sim_data_2$mu)