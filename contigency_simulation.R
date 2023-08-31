library(brglm2)
library(logistf)
library(glmnet)

# Set random seed for reproducibility

 

# Define parameters
num_y <- 32
num_x <- 5
num_samples <- 500
num_simulations <- 10
num_combinations<-32

# Create a matrix to store RMSE values for each method and each simulation
rmse_results <- matrix(NA, nrow = num_simulations, ncol = 4)
rmse_pred <- matrix(NA, nrow = num_simulations, ncol = 4)

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

summary(firth_log)

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
cv_output <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10, family = "poisson")

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
                       nfolds = 10, family = poisson(link = "log"))

best_lam <- cv_output$lambda.min

ridge.log <- glmnet(x_pena[train,], y_pena[train], alpha = 0, 
                    lambda = best_lam, family = poisson(link = "log"))

coef(ridge.log)


#define a function to calculate RMSE
rmse <- function(estimates) {
     sqrt(sum((estimates - beta)^2) / length(beta))
}

# Calculate RMSE for each method
rmse_ml <- rmse(coef(log))
rmse_firth <- rmse(coef(firth_log))
rmse_lasso <- rmse(lasso_coefficients)
rmse_ridge <- rmse(coef(ridge.log))

# Store RMSE values in the matrix
rmse_results[sim, 1] <- rmse(coef(log))
rmse_results[sim, 2] <- rmse(coef(firth_log))
rmse_results[sim, 3] <- rmse(lasso_coefficients)
rmse_results[sim, 4] <- rmse(coef(ridge.log))

# Store beta coefficients from each method
ml_betas <- coef(log)[-1]
firth_betas <- coef(firth_log)[-1]
lasso_betas <- coef(lasso.log, s = best_lam)[-1]
ridge_betas <- coef(ridge.log, s = best_lam)[-1]

# Store beta coefficients from each method
ml_betas <- coef(log)
firth_betas <- coef(firth_log)
lasso_betas <- coef(lasso.log, s = best_lam)
ridge_betas <- coef(ridge.log, s = best_lam)


ml_betas_matrix <- matrix(ml_betas, ncol = 1)
firth_betas_matrix <- matrix(firth_betas, ncol = 1)
lasso_betas_matrix <- matrix(lasso_betas, ncol = 1)
ridge_betas_matrix <- matrix(ridge_betas, ncol = 1)

# Calculate X_test_with_intercept
X_test_with_intercept <- cbind(1, x)
X_test_with_intercept <- as.matrix(X_test_with_intercept)

# Calculate y_pred for each method
log_y_pred_ml <- exp(X_test_with_intercept %*% ml_betas_matrix)

log_y_pred_firth <- exp(X_test_with_intercept %*% firth_betas_matrix)
log_y_pred_lasso <- exp(X_test_with_intercept %*% lasso_betas_matrix)
log_y_pred_ridge <- exp(X_test_with_intercept %*% ridge_betas_matrix)




# Calculate RMSE for each set of predictions
rmse_ml_pred <- sqrt(mean((log_y_pred_ml - exp(sums_matrix))^2))
rmse_firth_pred <- sqrt(mean((log_y_pred_firth-exp(sums_matrix))^2))
rmse_lasso_pred <- sqrt(mean((log_y_pred_lasso - exp(sums_matrix))^2))
rmse_ridge_pred <- sqrt(mean((log_y_pred_ridge-exp(sums_matrix))^2))




rmse_pred[sim, ]<- c(rmse_ml_pred, rmse_firth_pred, rmse_lasso_pred, rmse_ridge_pred)

}


# Calculate the mean of each column in rmse_results
rmse_means <- apply(rmse_results, 2, mean, na.rm = TRUE)
print(rmse_means)


