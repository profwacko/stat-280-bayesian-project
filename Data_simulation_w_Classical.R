#library(devtools)
#install_github("theodds/SoftBART")

library(caret)
library(tidyverse)

## Functions used to generate fake data
set.seed(42)
f_fried <- function(x) 10 * sin(pi * x[,1] * x[,2]) + 20 * (x[,3] - 0.5)^2 + 
  10 * x[,4] + 5 * x[,5]

our_function <- function(x) sin(x[,1])*exp(x[,2])*10 + cos(2*x[,3]) - 20*x[,4]^2 + 3*x[,5]^3 - 15*x[,6]^(1/3) 

high_dimensional_func <- function(x) 10 * sin(pi * apply(x[,1:200], 1, prod)) + 20 * (apply(x[,201:300],1,sum) - 5)^2 + 
  10 * apply(x[,301:400],1,sum) + 5 * apply(x[,401:500],1,sum)

gen_data <- function(n_train, n_test, func, P, sigma, a, b) {
  X <- matrix(runif(n_train * P, a, b), nrow = n_train)
  mu <- func(X)
  X_test <- matrix(runif(n_test * P, a, b), nrow = n_test)
  mu_test <- func(X_test)
  Y <- mu + sigma * rnorm(n_train)
  Y_test <- mu_test + sigma * rnorm(n_test)
  
  return(list(X = X, Y = Y, mu = mu, X_test = X_test, Y_test = Y_test, mu_test = mu_test))
}


## Simulate datasets
set.seed(42)
#all 5 predictors are important
sim_data_usual <- gen_data(250, 100, f_fried, 5, 1, 0, 1)

set.seed(42)
#all 500 predictors do have a relationship to y, and p > n
sim_data_high_dim <- gen_data(250, 100, high_dimensional_func, 500, 1, 0, 1)

set.seed(42)
# only 5 predictors are important out of 20
sim_data_sparse <- gen_data(250, 100, f_fried, 20, 1, 0, 1)

set.seed(42)
# only 5 predictors are important out of 500, and p > n
sim_data_high_dim_sparse <- gen_data(250, 100, f_fried, 500, 1, 0, 1)


###########################


dat <- list(sim_data_usual,
            sim_data_high_dim,
            sim_data_sparse,
            sim_data_high_dim_sparse)

caret_mod <- function(d){
  x <- dat[[d]]$X %>% magrittr::set_colnames(paste("X", 1:ncol(dat[[d]]$X), sep = ""))
  y <- dat[[d]]$Y
  
  
  start_time <- Sys.time()
  
  models <- list(train(x = x, y = y, method = 'glm'),
                 train(x = x, y = y, method = 'glmnet'),
                 train(x = x, y = y, method = 'xgbTree'),
                 train(x = x, y = y, method = 'rf', importance = T))
  
  varimp <- sapply(1:4,
                   function(x){
                    temp <- varImp(models[[x]])$importance
                    return(temp[order(row.names(temp)), ])
                    }
                   ) %>% magrittr::set_colnames(c("glm", "glmnet", "xgbTree", "rf"))
  
  x_test <- dat[[d]]$X_test %>% magrittr::set_colnames(paste("X", 1:ncol(dat[[d]]$X_test), sep = ""))
  y_test <- dat[[d]]$Y_test
  
  predicted <- residuals <- data.frame(V1 = rep(NA,nrow(dat[[d]]$X_test)))
  RMSE <- vector()
  
  for(i in 1:4) {
    predicted[,i] = predict(models[[i]], x_test)
    residuals[,i] = y_test - predicted[,i]
    RMSE[i] = sqrt(mean(residuals[, i]^2))
  }
  Sys.time() - start_time
  return(list(RMSE,varimp,models,Sys.time() - start_time))
  }

classical <- lapply(1:4,caret_mod)
