library(actuar)
library(faux)

#poisson regression model for simulation, k=7
#x1, x2, x3 are binary variables
#x4, x5 are ordinal variables
#x6, x7 are continuous variables
sim_poisson<-function(n,beta){ 
     
     #generate the random numbers
     
     binary_dt <- rnorm_multi(n, varnames = c("z1", "z2", "z3"), r = c(-0.2, 0.3, 0.5),
                              mu = 0, sd = 1, empirical = FALSE)
     ordinal_dt <- rnorm_multi(n, varnames = c("z4", "z5", "z6"), r = c(-0.5, 0.2, 0.3), 
                               mu = 0, sd = 1, empirical = FALSE)
     continuous_dt <- rnorm_multi(n, varnames = c("z1", "z5", "z7"), r = c(-0.25, 0.35, 0.55), 
                                  mu = 0, sd = 1, empirical = FALSE)
     
     #obtain dataset of the random variables z
     rv_dt<-as.matrix(cbind(binary_dt,ordinal_dt, continuous_dt))
     
     #order the correct name and random variables z 
     rv_order <- c("z1","z2","z3","z4","z5","z6","z7")
     rv_dt<-rv_dt[,rv_order]
     
     
     # Process binary variables
     rv_dt[, c("z1", "z2", "z3")] <- ifelse(rv_dt[, c("z1", "z2", "z3")] > c(3, 1.5, 1), 1, 0)
     
     
     #ordinal variables, all the ordinal variables has three levels(0,1,2)
     # Process ordinal variables
     z4 <- as.integer(rv_dt[,"z4"] > 1.6) + as.integer(rv_dt[,"z4"] > 2)
     z5 <- as.integer(rv_dt[,"z5"] > 1.2) + as.integer(rv_dt[,"z5"] > 1.6)
     
     
     # Apply the floor function to eliminate non-integer part
     #the mean of x6 is 39.5
     z6 <- floor(10 * rv_dt[,"z6"]) + 40  
     #the mean of x7 is 99.5
     z7 <- floor(10 * rv_dt[,"z7"]) + 100
     
     #obtain the value of z in the dataset rv_dt 
     rv_dt[,'z4']<-z4
     rv_dt[,'z5']<-z5
     rv_dt[,'z6']<-z6
     rv_dt[,'z7']<-z7
     colnames(rv_dt)<-c("x1","x2","x3","x4","x5","x6","x7")
     
     #determine the offset variable
     offset_pois<-rztpois(n, 1.6)
     
     #construct the regression model with coefficients 
     z=log(0.1)+beta*rv_dt[,"x1"]+0.75*rv_dt[,"x2"]-0.65*rv_dt[,"x3"]
     +0.82*rv_dt[,"x4"]+0.36*rv_dt[,"x5"]-0.48*rv_dt[,"x6"]+0.27*rv_dt[,"x7"]
     
     #obtain the response variable y
     poisson_y<-rpois(n, exp(z)*offset_pois)
     
     #obtain all the variables in the regression model including x and y
     poisson_rv<-cbind(poisson_y,rv_dt)
     
}





#the number of replications
rep<-1000

#sample n, the observations of the data
n<-200

# Define the values of beta
betas <- c(-log(9), -log(7), -log(5), -log(3), 0, log(3), log(5), log(7), log(9))

# Function to calculate separation, RMSE of beta, and RMSE of predictions
calculate_metrics <- function(n, beta) {
     # Construct a list to store the simulation data with all the true coefficients
     sim.poisson.dt <- lapply(1:rep, function(i) sim_poisson(n, beta))
     
     # Construct lists to store the regression outputs and standard error of beta
     fit <- lapply(sim.poisson.dt, function(data) {
          glm(poisson_y ~ ., data = as.data.frame(data), family = poisson(link = "log"))
     })
     
     se <- lapply(fit, function(fit) {
          summary(fit)$coefficients[, 2]['x1']
     })
     
     # Obtain the number of separation
     separation <- length(which(as.numeric(se) > 100))
     
     # RMSE of beta
     # Construct a list to store the estimate of the beta
     est_beta <- sapply(fit, function(fit) summary(fit)$coefficients[, 1]['x1'])
     RMSE_beta <- sqrt(sum((est_beta - beta)^2) / rep)
     
     # RMSE of predictions
     # Predictions of the response variable
     pred <- lapply(fit, function(fit) fit$fitted)
     diff <- mapply(function(pred, data) pred - data[, "poisson_y"], pred, sim.poisson.dt)
     RMSE_prediction <- sqrt(sum(unlist(lapply(diff, function(diff) sum(diff^2) / n))) / rep)
     
     # Calculate the coverage probability
     lower <- sapply(fit, function(fit) coef(fit)["x1"] - 1.96 * summary(fit)$coefficients[, 2]['x1'])
     upper <- sapply(fit, function(fit) coef(fit)["x1"] + 1.96 * summary(fit)$coefficients[, 2]['x1'])
     count <- sum(lower <= beta & beta <= upper)
     coverage.prob <- count / rep
     
     return(list(separation = separation, RMSE_beta = RMSE_beta, RMSE_prediction = RMSE_prediction,
                 coverage.prob = coverage.prob))
}


# Lists to store the results for different values of beta
results <- vector("list", length(betas))

# Loop over each value of beta
for (i in seq_along(betas)) {
     results[[i]] <- calculate_metrics(n, betas[i])
}

# Extract specific values from the results and combine them into a comma-separated string
number_of_separation <- paste(sapply(results, function(result) result$separation), collapse = ",")
RMSE_beta_values <- paste(sapply(results, function(result) result$RMSE_beta), collapse = ",")
RMSE_prediction_values <- paste(sapply(results, function(result) result$RMSE_prediction), collapse = ",")
coverage_prob_values <- paste(sapply(results, function(result) result$coverage.prob), collapse = ",")

# Print the comma-separated results
cat("Number of Separation:", number_of_separation, "\n")
cat("RMSE of Beta:", RMSE_beta_values, "\n")
cat("RMSE of Predictions:", RMSE_prediction_values, "\n")
cat("Coverage Probability:", coverage_prob_values, "\n")
