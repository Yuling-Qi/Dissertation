library(faux)
library(brglm2)
library(actuar)
library(MASS)

#poisson regression model for simulation, k=5
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
     +0.82*rv_dt[,"x4"]+0.36*rv_dt[,"x5"]
     
     #obtain the response variable y
     poisson_y<-rpois(n, exp(z)*offset_pois)
     
     #obtain all the variables in the regression model including x and y
     poisson_rv<-cbind(poisson_y,rv_dt)
     
}





#the number of replications
rep<-1000

#sample n, the observations of the data
n<-800

# Define the values of beta
betas <- c(-log(9), -log(7), -log(5), -log(3), 0, log(3), log(5), log(7), log(9))

# Function to calculate the coverage probability
calculate_coverage <- function(fit, beta) {
     conf_intervals <- lapply(fit, function(f) confint(f, "x1"))
     lower <- sapply(conf_intervals, `[`, 1)
     upper <- sapply(conf_intervals, `[`, 2)
     
     count <- sum(lower <= beta & beta <= upper)
     coverage <- count / length(fit)
     return(coverage)
}

# Function to calculate metrics for different beta values with k=3 variables
calculate_metrics_k5 <- function(n, betas) {
     results <- vector("list", length(betas))
     
     for (i in seq_along(betas)) {
          beta <- betas[i]
          sim.poisson.dt.k5 <- lapply(1:rep, function(j) sim_poisson(n, beta))
          name <- c("poisson_y", "x1", "x2","x3", "x4","x5")
          sim.poisson.dt.k5 <- lapply(sim.poisson.dt.k5, function(data) data[, name])
          
          fit <- lapply(sim.poisson.dt.k5, function(data) {
               glm(poisson_y ~ ., data = as.data.frame(data), family = poisson(link = "log"),
                   method = "brglmFit")
          })
          
          se <- lapply(fit, function(fit) {
               summary(fit)$coefficients[, 2]['x1']
          })
          
          separation <- length(which(as.numeric(se) > 100))
          
          est_beta <- sapply(fit, function(fit) summary(fit)$coefficients[, 1]['x1'])
          RMSE_beta <- sqrt(sum((est_beta - beta)^2) / rep)
          
          pred <- lapply(fit, function(fit) fit$fitted)
          diff <- mapply(function(pred, data) pred - data[, "poisson_y"], pred, sim.poisson.dt.k5)
          RMSE_prediction <- sqrt(sum(unlist(lapply(diff, function(diff) sum(diff^2) / n))) / rep)
          
          # Calculate coverage probability
          coverage <- calculate_coverage(fit, beta)
          
          results[[i]] <- list(separation = separation, RMSE_beta = RMSE_beta,
                               RMSE_prediction = RMSE_prediction, coverage.prob = coverage)
     }
     
     return(results)
}


# Calculate metrics for different beta values with k=3 variables
results_k5 <- calculate_metrics_k5(n, betas)

# Extract specific values from the results and combine them into a comma-separated string
separation_values <- paste(sapply(results_k5, function(result) result$separation), collapse = ",")
RMSE_beta_values <- paste(sapply(results_k5, function(result) result$RMSE_beta), collapse = ",")
RMSE_prediction_values <- paste(sapply(results_k5, function(result) result$RMSE_prediction), collapse = ",")
coverage_prob_values <- paste(sapply(results_k5, function(result) result$coverage.prob), collapse = ",")

# Print the comma-separated results
cat("Number of Separation:", separation_values, "\n")
cat("RMSE of Beta:", RMSE_beta_values, "\n")
cat("RMSE of Predictions:", RMSE_prediction_values, "\n")
cat("Coverage Probability:", coverage_prob_values, "\n")
