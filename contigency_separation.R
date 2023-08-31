library(brglm2)
library(logistf)
library(glmnet)
options(warn = 2)  # 设置警告级别为 "warnings"

# Set a random seed for reproducibility
set.seed(as.numeric(Sys.time()))  # Change the seed generation method


# Define parameters
num_y <- 32
num_x <- 5
num_samples <- 250
num_simulations <- 100
num_combinations<-32

num_separation <- 0 

# Simulated data parameters
num_samples <- 100

for (i in 1:num_simulations) {
     warning_message <- NULL
     
     warning_message <- NULL
     set.seed(as.numeric(Sys.time()))
     # Generate all possible combinations of independent variables
     # x_combinations <- expand.grid(rep(list(0:1), num_x))
     
     # Generate all possible combinations of independent variables
     x_combinations <- matrix(NA, nrow = num_combinations, ncol = num_x)
     
     for (i in 0:(num_combinations - 1)) {
          binary_rep <- as.integer(intToBits(i))[1:num_x]
          binary_rep[binary_rep == 0] <- -1
          x_combinations[i + 1,] <- binary_rep
     }
     
     x <- x_combinations
     colnames(x) <- c("x1", "x2", "x3", "x4", "x5")
     
     # Generate random beta coefficients
     beta <- rnorm(21)
     x <- cbind(1, x, 
                x[, 3] * x[, 4] * x[, 5],
                x[, 2] * x[, 3] * x[, 5],
                x[, 2] * x[, 3] * x[, 4],
                x[, 1] * x[, 3] * x[, 5],
                x[, 1] * x[, 2] * x[, 3],
                
                x[, 1] * x[, 2],
                x[, 1] * x[, 3],
                x[, 1] * x[, 4],
                x[, 1] * x[, 5],
                x[, 2] * x[, 3],
                x[, 2] * x[, 4],
                x[, 2] * x[, 5],
                x[, 3] * x[, 4],
                x[, 3] * x[, 5],
                x[, 4] * x[, 5])
     
     beta <- rnorm(21)
     beta[2] <- 0
     
     # Generate a 32-row matrix with the same beta vector in each row
     beta_matrix <- matrix(rep(beta, num_y), nrow = num_y, byrow = TRUE)
     
     x <- data.frame((x))
     
     result_matrix <- x * beta_matrix
     
     # Calculate the sum of each row to create a new matrix
     sums_matrix <- matrix(rowSums(result_matrix), ncol = 1)
     
     # Calculate the observation probabilities for each scenario
     observation_probabilities <- exp(sums_matrix)
     
     # Initialize an empty vector to store the generated y values
     y <- numeric(0)
     
     # Generate y values for each scenario
     for (j in 1:num_y) {
          all_y <- rpois(1, observation_probabilities[j])
          y <- c(y, all_y)
     }
     
     x<-x[-1]
     
     # Create the simulated dataset
     simulated_data <- data.frame(y, x)
     
     # ML analysis
     log <- tryCatch(
          glm(y ~ ., data = simulated_data, family = poisson(link = "log")),
          error = function(e) e
     )
     
     # Check for separation
     if (inherits(log, "error") && grepl("glm.fit:拟合速率算出来是数值零", log$message)) {
          num_separation <- num_separation + 1
          cat("Separation detected in simulation", i, "\n")
     }
     else {
          #Firth logistic regression
          firth_log <- glm(y ~ .,
                           data = simulated_data, family = poisson(link = "log"),
                           method = "brglmFit")

          summary(firth_log)

          # # Lasso for Poisson model
          # # Prepare the data for Lasso regression
          # x_pena <- model.matrix(y ~ .,
          #                        simulated_data)[,-1]
          # y_pena <- simulated_data$y
          # 
          # set.seed(10)
          # 
          # # Train the data
          # train = sample(1:nrow(x_pena), nrow(x_pena)/2)
          # x_train = x_pena[train, ]
          # y_train = y_pena[train]
          # 
          # # Obtain the optimal lambda for the Poisson model
          # cv_output <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 3, family = "poisson")
          # 
          # # Apply the best lambda to fit the data
          # best_lam <- cv_output$lambda.min
          # lasso.log <- glmnet(x_train, y_train, alpha = 1, lambda = best_lam, family = "poisson")
          # 
          # # Obtain the coefficients of the logistic model with Lasso for the optimal lambda
          # lasso_coefficients <- coef(lasso.log, s = best_lam)
          # coef(lasso.log)
          # 
          # ## Ridge Poisson regression
          # set.seed(10)
          # train = sample(1:nrow(x_pena), nrow(x_pena)/2)
          # x_test = (-train)
          # y_test = y_pena[x_test]
          # 
          # cv_output <- cv.glmnet(x_pena[train,], y_pena[train],
          #                        alpha = 0,
          #                        nfolds = 3, family = poisson(link = "log"))
          # 
          # best_lam <- cv_output$lambda.min
          # 
          # ridge.log <- glmnet(x_pena[train,], y_pena[train], alpha = 0,
          #                     lambda = best_lam, family = poisson(link = "log"))
          # 
          # coef(ridge.log)

          # #obtain the true beta
          # separation_beta <- c(intercept_logist,separation_beta)
          # 
          # #define a function to calculate RMSE
          # rmse <- function(estimates) {
          #      sqrt(sum((estimates - separation_beta)^2) / length(separation_beta))
          # }
          # 
          # # Calculate RMSE for each method
          # rmse_ml <- rmse(coef(log))
          # rmse_firth <- rmse(coef(firth_log))
          # rmse_lasso <- rmse(lasso_coefficients)
          # rmse_ridge <- rmse(coef(ridge.log))
          # 
          # rmse_results[i, ] <- c(rmse_ml, rmse_firth, rmse_lasso, rmse_ridge)
          # 
          # # Store beta coefficients from each method
          # ml_betas <- coef(log)[-1]
          # firth_betas <- coef(firth_log)[-1]
          # lasso_betas <- coef(lasso.log, s = best_lam)[-1]
          # ridge_betas <- coef(ridge.log, s = best_lam)[-1]
          # 
          # 
          # # Calculate log(y_pred) = X_test_with_intercept * estimated_betas
          # log_y_pred_ml <- exp(X%*% ml_betas+matrix(coef(log)[1], nrow = 750, ncol = 1))
          # log_y_pred_firth <- exp(X%*% firth_betas+matrix(coef(firth_log)[1], nrow = 750, ncol = 1))
          # log_y_pred_lasso <- exp(X %*% lasso_betas+matrix(coef(lasso.log)[1], nrow = 750, ncol = 1))
          # log_y_pred_ridge <- exp(X%*% ridge_betas+matrix(coef(ridge.log)[1], nrow = 750, ncol = 1))
          # 
          # 
          # # Calculate RMSE for each set of predictions
          # rmse_ml_pred <- sqrt(mean((log_y_pred_ml - exp(log_y))^2))
          # rmse_firth_pred <- sqrt(mean((log_y_pred_firth-exp(log_y))^2))
          # rmse_lasso_pred <- sqrt(mean((log_y_pred_lasso - exp(log_y))^2))
          # rmse_ridge_pred <- sqrt(mean((log_y_pred_ridge-exp(log_y))^2))
          # 
          # 
          # 
          # rmse_pred[i, ]<- c(rmse_ml_pred, rmse_firth_pred, rmse_lasso_pred, rmse_ridge_pred)
          # 
     }
}




