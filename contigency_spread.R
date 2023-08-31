library(brglm2)
library(logistf)
library(glmnet)


# Define parameters
num_y <- 32
num_x <- 5
num_samples <- 500
num_simulations <- 100
num_combinations<-32
set.seed(as.numeric(Sys.time()))

# Create matrices to store beta coefficients for each method
ml_betas_all <- matrix(NA, nrow = num_simulations, ncol = 21)
firth_betas_all <- matrix(NA, nrow = num_simulations, ncol = 21)
lasso_betas_all <- matrix(0, nrow = num_simulations, ncol = 21)
ridge_betas_all <- matrix(0, nrow = num_simulations, ncol = 21)

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
     #Firth logistic regression
     firth_log <- glm(y ~ .,
                      data = simulated_data, family = poisson(link = "log"),
                      method = "brglmFit") 

     
     # Lasso for Poisson model
     # Prepare the data for Lasso regression
     x_pena <- model.matrix(y ~ ., 
                            simulated_data)[,-1]
     y_pena <- simulated_data$y
     
     set.seed(10)
     
     # Train the data
     train = sample(1:nrow(x_pena), nrow(x_pena)/2)
     x_train = x_pena[train, ]
     y_train = y_pena[train]
     
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
     train = sample(1:nrow(x_pena), nrow(x_pena)/2)
     x_test = (-train)
     y_test = y_pena[x_test]
     
     cv_output <- cv.glmnet(x_pena[train,], y_pena[train],
                            alpha = 0, 
                            nfolds = 3, family = poisson(link = "log"))
     
     best_lam <- cv_output$lambda.min
     
     ridge.log <- glmnet(x_pena[train,], y_pena[train], alpha = 0, 
                         lambda = best_lam, family = poisson(link = "log"))
     
     coef(ridge.log)
     
     # Store beta coefficients from each method
     ml_betas <- coef(log)
     firth_betas <- coef(firth_log)
     lasso_betas <- coef(lasso.log, s = best_lam)
     ridge_betas <- coef(ridge.log, s = best_lam)
     
     # Store beta coefficients from each method
     ml_betas_all[sim, ] <- coef(log)
     firth_betas_all[sim, ] <- coef(firth_log)
     
     # Extract lasso beta values, filling in 0 for non-existent coefficients
     lasso_beta_values <- as.vector(coef(lasso.log, s = best_lam))
     lasso_betas_all[sim, 1:length(lasso_beta_values)] <- lasso_beta_values
     
     # Extract ridge beta values, filling in 0 for non-existent coefficients
     ridge_beta_values <- as.vector(coef(ridge.log, s = best_lam))
     ridge_betas_all[sim, 1:length(ridge_beta_values)] <- ridge_beta_values
}

# Calculate the standard deviation of betas for each method
ml_std <- apply(ml_betas_all, 2, sd)
firth_std <- apply(firth_betas_all, 2, sd)
lasso_std <- apply(lasso_betas_all, 2, sd)
ridge_std <- apply(ridge_betas_all, 2, sd)

# Calculate the average standard deviation for each method
ml_avg_std <- mean(ml_std) / 21
firth_avg_std <- mean(firth_std) / 21
lasso_avg_std <- mean(lasso_std) / 21
ridge_avg_std <- mean(ridge_std) / 21

# Print the results
print(paste("ML Average Standard Deviation:", ml_avg_std))
print(paste("Firth Average Standard Deviation:", firth_avg_std))
print(paste("Lasso Average Standard Deviation:", lasso_avg_std))
print(paste("Ridge Average Standard Deviation:", ridge_avg_std))


