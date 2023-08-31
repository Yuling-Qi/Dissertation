# Load necessary libraries
library(faux)
library(brglm2)
library(actuar)
library(MASS)
library(logistf)
library(glmnet)

# Define a function to simulate a Poisson regression model for k=5
sim_poisson <- function(n, beta) {
     
     # Generate random numbers for binary, ordinal, and continuous variables
     binary_dt <- rnorm_multi(n, varnames = c("z1", "z2", "z3"), r = c(-0.2, 0.3, 0.5),
                              mu = 0, sd = 1, empirical = FALSE)
     ordinal_dt <- rnorm_multi(n, varnames = c("z4", "z5", "z6"), r = c(-0.5, 0.2, 0.3), 
                               mu = 0, sd = 1, empirical = FALSE)
     continuous_dt <- rnorm_multi(n, varnames = c("z1", "z5", "z7"), r = c(-0.25, 0.35, 0.55), 
                                  mu = 0, sd = 1, empirical = FALSE)
     
     # Combine random variables into a dataset
     rv_dt <- as.matrix(cbind(binary_dt, ordinal_dt, continuous_dt))
     
     # Order the variables
     rv_order <- c("z1", "z2", "z3", "z4", "z5", "z6", "z7")
     rv_dt <- rv_dt[, rv_order]
     
     # Process binary variables
     rv_dt[, c("z1", "z2", "z3")] <- ifelse(rv_dt[, c("z1", "z2", "z3")] > c(3, 1.5, 1), 1, 0)
     
     # Process ordinal variables
     z4 <- as.integer(rv_dt[,"z4"] > 1.6) + as.integer(rv_dt[,"z4"] > 2)
     z5 <- as.integer(rv_dt[,"z5"] > 1.2) + as.integer(rv_dt[,"z5"] > 1.6)
     
     # Apply the floor function to ordinal variables
     z6 <- floor(10 * rv_dt[,"z6"]) + 40  
     z7 <- floor(10 * rv_dt[,"z7"]) + 100
     
     # Update the dataset with processed variables
     rv_dt[,'z4'] <- z4
     rv_dt[,'z5'] <- z5
     rv_dt[,'z6'] <- z6
     rv_dt[,'z7'] <- z7
     colnames(rv_dt) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
     
     # Define offset variable
     offset_pois <- rztpois(n, 1.6)
     
     # Construct regression model with coefficients 
     z <- log(0.1) + beta * rv_dt[,"x1"] + 0.75 * rv_dt[,"x2"] - 0.65 * rv_dt[,"x3"]
     + 0.82 * rv_dt[,"x4"] + 0.36 * rv_dt[,"x5"] 
     
     # Generate response variable y
     poisson_y <- rpois(n, exp(z) * offset_pois)
     
     # Combine variables for the regression model
     poisson_rv <- cbind(poisson_y, rv_dt)
     
}

# Define a function to calculate Firth-type logistic regression with intercept correction
flic <- function(x, y) {
     
     # Fit the initial Poisson regression model using the brglmFit method
     temp.fit1 <- glm(y ~ x, family = poisson(link = "log"), method = "brglmFit")
     
     # Calculate the linear predictors of the initial model and remove the intercept
     temp.linear.predictor <- temp.fit1$linear.predictors - temp.fit1$coef[1]
     
     # Fit a Poisson regression model with an offset as linear predictors 
     temp.fit2 <- glm(y ~ 1, family = poisson(link = "log"), offset = temp.linear.predictor)
     
     # Get the new intercept from the intercept-corrected Poisson model
     new.intercept <- temp.fit2$coef
     
     # Prepare a list to store the results
     res <- list()
     
     # Combine new intercept with the original coefficients (excluding the original intercept)
     res$coefficients <- c(new.intercept, temp.fit1$coef[-1])
     
     # Store the fitted values from the intercept-corrected model
     res$fitted <- temp.fit2$fitted
     
     # Store the linear predictors from the intercept-corrected model
     res$linear.predictions <- temp.fit2$linear.predictors
     
     # Return the list of results
     return(res)
}


# Define number of replications and observations
rep <- 1000
n <- 400

# Define beta values
betas <- c(-log(9), -log(7), -log(5), -log(3), 0, log(3), log(5), log(7), log(9))

# Function to calculate metrics using Firth-type regression
calculate_metrics_k5_firth <- function(n, betas) {
     results <- vector("list", length(betas))
     
     for (i in seq_along(betas)) {
          beta <- betas[i]
          sim.poisson.dt <- lapply(1:rep, function(j) sim_poisson(n, beta))
          name <- c("poisson_y", "x1", "x2","x3", "x4","x5")
          sim.poisson.dt <- lapply(sim.poisson.dt, function(data) data[, name])
          
          fit <- lapply(sim.poisson.dt, function(data) {
               # Use the flic function for Firth-type logistic regression
               flic(as.matrix(data[, -1]), data[, 1])
          })
          
          pred <- lapply(fit, function(fit) fit$fitted)
          diff <- mapply(function(pred, data) pred - data[, "poisson_y"], pred, sim.poisson.dt)
          RMSE_prediction <- sqrt(sum(unlist(lapply(diff, function(diff) sum(diff^2) / n))) / rep)
          
          results[[i]] <- list(RMSE_prediction = RMSE_prediction)
     }
     
     return(results)
}

# Calculate metrics for different beta values using Firth-type regression
results_k5_firth <- calculate_metrics_k5_firth(n, betas)

# Extract specific values from the results and combine them into a comma-separated string
RMSE_prediction_values_firth <- paste(sapply(results_k5_firth, function(result) result$RMSE_prediction), 
                                      collapse = ",")

# Print the comma-separated results for Firth-type regression
cat("RMSE of Predictions (Firth-type):", RMSE_prediction_values_firth, "\n")

