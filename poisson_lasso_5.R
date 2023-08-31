library(actuar)
library(faux)
library(glmnet)

#lasso
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
rep<-10

#sample n, the observations of the data
n<-800

# Define the values of beta
betas <- c(-log(9), -log(7), -log(5), -log(3), 0, log(3), log(5), log(7), log(9))

# Function to fit lasso regression model and calculate RMSE
fit_lasso <- function(data, beta) {
     lasso_model <- cv.glmnet(as.matrix(data[, 2:6]), data[, "poisson_y"],
                              alpha = 1, family = poisson(link = "log"), nfolds = 10)
     
     coef.lasso <- as.numeric(coef(lasso_model, s = "lambda.min")["x1", ])
     pred <- predict(lasso_model, s = "lambda.min", type = "response", newx = as.matrix(data[, 2:6]))
     RMSE_beta <- sqrt(mean((coef.lasso - beta)^2))
     RMSE_prediction <- sqrt(mean((data[, "poisson_y"] - pred)^2))
     
     return(list(RMSE_beta = RMSE_beta, RMSE_prediction = RMSE_prediction))
}

# Fit the lasso regression model and calculate RMSE for each replication and beta
results <- vector("list", length(betas))
for (i in seq_along(betas)) {
     beta <- betas[i]
     sim.poisson.dt.k5 <- lapply(1:rep, function(j) sim_poisson(n, beta))
     name <- c("poisson_y", "x1", "x2","x3","x4", "x5")
     sim.poisson.dt.k5 <- lapply(sim.poisson.dt.k5, function(data) data[, name])
     lasso_results <- lapply(sim.poisson.dt.k5, fit_lasso, beta = beta)
     results[[i]] <- lasso_results
}

# Calculate average RMSE for beta and predictions over all replications and beta values
avg_RMSE_beta <- sapply(results, function(res_list) mean(sapply(res_list, function(res) res$RMSE_beta)))
avg_RMSE_prediction <- sapply(results, function(res_list) mean(sapply(res_list,
                                                                      function(res) res$RMSE_prediction)))

# Format the results as a comma-separated string
avg_RMSE_beta_str <- paste(avg_RMSE_beta, collapse = ",")
avg_RMSE_prediction_str <- paste(avg_RMSE_prediction, collapse = ",")

# Print the results
cat("Average RMSE (Beta): ", avg_RMSE_beta_str, "\n")
cat("Average RMSE (Prediction): ", avg_RMSE_prediction_str, "\n")

