library(brglm2)
library(logistf)
library(glmnet)

# Set random seed for reproducibility



# Define parameters
num_y <- 32
num_x <- 5
num_samples <- 500
num_simulations <- 50
num_seoaration<-0
num_combinations<-32

# Create a matrix to store RMSE values for each method and each simulation
diff_sum_of_squares_results <- matrix(0, nrow = num_simulations, ncol = 4)

# Perform the simulations
for (sim in 1:num_simulations) {
     
     warning_message <- NULL
     set.seed(as.numeric(Sys.time()))
     # Generate all possible combinations of independent variables
     # x_combinations <- expand.grid(rep(list(0:1), num_x))
     
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
     for (i in 1:num_y) {
          all_y <- rpois(1, observation_probabilities[i])
          y <- c(y, all_y)
     }
     
     x<-x[-1]
     
     # Create the simulated dataset
     simulated_data <- data.frame(y, x)
     
     # ML analysis
     # ML analysis
     tryCatch({
          log <- glm(y ~ .,
                     data = simulated_data, family = poisson(link = "log"))
     }, warning = function(w) {
          print("Warning: glm.fit:拟合速率算出来是数值零")
     })
     
     
     # Check for separation
     if ("glm.fit:拟合速率算出来是数值零" %in% warnings()) {
          num_separation <- num_separation + 1
     }
     
     log <- glm(y ~ .,
                data = simulated_data, family = poisson(link = "log"))
     
     
     # Firth logistic regression
     firth_log <- glm(y ~ .,
                      data = simulated_data, family = poisson(link = "log"),
                      method = "brglmFit") 
     
     summary(firth_log)
     
     # Lasso for Poisson model
     # Prepare the data for Lasso regression
     x_pena <- model.matrix(y ~ ., 
                            simulated_data)[,-1]
     y_pena <- simulated_data$y
     
     set.seed(10)
     
     # Train the data
     train <- sample(1:nrow(x_pena), nrow(x_pena)/2)
     x_train <- x_pena[train, ]
     y_train <- y_pena[train]
     
     # Obtain the optimal lambda for the Poisson model
     cv_output <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 3, family = "poisson")
     
     # Apply the best lambda to fit the data
     best_lam <- cv_output$lambda.min
     lasso.log <- glmnet(x_train, y_train, alpha = 1, lambda = best_lam, family = "poisson")
     
     # Obtain the coefficients of the logistic model with Lasso for the optimal lambda
     lasso_coefficients <- coef(lasso.log, s = best_lam)
     coef(lasso.log)
     
     ## Ridge Poisson regression
     set.seed(10)
     train <- sample(1:nrow(x_pena), nrow(x_pena)/2)
     x_test <- (-train)
     y_test <- y_pena[x_test]
     
     cv_output <- cv.glmnet(x_pena[train,], y_pena[train],
                            alpha = 0, 
                            nfolds = 3, family = poisson(link = "log"))
     
     best_lam <- cv_output$lambda.min
     
     ridge.log <- glmnet(x_pena[train,], y_pena[train], alpha = 0, 
                         lambda = best_lam, family = poisson(link = "log"))
     
     coef(ridge.log)
     
     # Convert beta to binary variable
     beta <- ifelse(beta == 0, 0, 1)
     
     # Store beta coefficients from each method
     ml_betas <- coef(log)
     ml_betas_binary <- ifelse(ml_betas == 0, 0, 1)
     
     firth_betas <- coef(firth_log)
     firth_betas_binary <- ifelse(firth_betas == 0, 0, 1)
     
     lasso_betas <- coef(lasso.log, s = best_lam)
     lasso_betas_binary <- ifelse(lasso_betas == 0, 0, 1)
     
     ridge_betas <- coef(ridge.log, s = best_lam)
     ridge_betas_binary <- ifelse(ridge_betas == 0, 0, 1)
     
     # Calculate difference vectors
     ml_diff <- beta - ml_betas_binary
     firth_diff <- beta - firth_betas_binary
     lasso_diff <- beta - lasso_betas_binary
     ridge_diff <- beta - ridge_betas_binary
     
     # Calculate squared difference vectors
     ml_diff_squared <- ml_diff^2
     firth_diff_squared <- firth_diff^2
     lasso_diff_squared <- lasso_diff^2
     ridge_diff_squared <- ridge_diff^2
     
     # Calculate sum of squared differences
     ml_diff_sum_of_squares <- sqrt(sum(ml_diff_squared)/21)
     firth_diff_sum_of_squares <- sqrt(sum(firth_diff_squared)/21)
     lasso_diff_sum_of_squares <- sqrt(sum(lasso_diff_squared)/21)
     ridge_diff_sum_of_squares <- sqrt(sum(ridge_diff_squared)/21)
     
     # Store sum of squared differences
     diff_sum_of_squares_results[sim, ] <- c(ml_diff_sum_of_squares, firth_diff_sum_of_squares,
                                           lasso_diff_sum_of_squares, ridge_diff_sum_of_squares)
}

# Calculate average sum of squared differences for each method
avg_diff_sum_of_squares <- colMeans(diff_sum_of_squares_results)
