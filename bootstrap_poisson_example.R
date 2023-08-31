library(faux)
library(brglm2)
library(actuar)
library(MASS)
library(logistf)
library(profileModel)

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
     
     # #construct the regression model with coefficients 
     # z=log(0.1)+beta*rv_dt[,"x1"]+0.75*rv_dt[,"x2"]-0.65*rv_dt[,"x3"]
     # +0.82*rv_dt[,"x4"]+0.36*rv_dt[,"x5"]-0.48*rv_dt[,"x6"]+0.27*rv_dt[,"x7"]
     # 
     # #k=5
     # z=log(0.1)+beta*rv_dt[,"x1"]+0.75*rv_dt[,"x2"]-0.65*rv_dt[,"x3"]
     # +0.82*rv_dt[,"x4"]+0.36*rv_dt[,"x5"]
     
     #k=3
     z=log(0.1)+beta*rv_dt[,"x1"]+0.75*rv_dt[,"x2"]
     +0.82*rv_dt[,"x4"]
     
     #obtain the response variable y
     poisson_y<-rpois(n, exp(z)*offset_pois)
     
     #obtain all the variables in the regression model including x and y
     poisson_rv<-cbind(poisson_y,rv_dt)
     
}

# # Set the number of observations
# 
# n <- 200
# 
# # Define the true beta value for estimate PLCI
# true_beta_1 <- log(9)
# 
# # Simulate logit_rv data
# simulated_data <- sim_poisson(n, true_beta_1)
# simulated_data <- as.data.frame(simulated_data)
# 
# 
# firth_poisson <- glm(poisson_y ~ .,
#                  data=simulated_data, family = poisson(link="log"),
#                  method = "brglmFit")
# 
# # confint(firth_poisson)
# # confint.default(firth_poisson)
# 
# 
# # Set the number of bootstrap replications
# n_bootstrap <- 100
# 
# # Initialize a vector to store bootstrap estimates of x1 coefficient
# bootstrap_coeffs_x1 <- numeric(n_bootstrap)
# 
# # Initialize a counter for coverage
# coverage_count <- 0
# # Set the values of beta for estimation
# beta_values <- c(-log(9), -log(7), -log(5), -log(3), 0, log(3), log(5), log(7), log(9))
# 
# # Initialize a vector to store coverage probabilities
# coverage_probabilities <- numeric(length(beta_values))
# 
# # Perform simulations for each beta value
# for (b in 1:length(beta_values)) {
#      true_beta <- beta_values[b]
#      coverage_count <- 0
#      
#      # Perform simulations
#      for (j in 1:100) {
#           # Simulate new data
#           simulated_data <- sim_poisson(n, true_beta)
#           simulated_data <- as.data.frame(simulated_data)
#           
#           # Initialize a vector to store bootstrap estimates of x1 coefficient
#           bootstrap_coeffs_x1 <- numeric(n_bootstrap)
#           
#           # Perform bootstrap
#           for (i in 1:n_bootstrap) {
#                sampled_data <- simulated_data[sample(nrow(simulated_data), replace = TRUE), ]
#                firth_poisson <- glm(poisson_y ~ ., data = sampled_data, family = poisson(link = "log"),
#                                     method = "brglmFit")
#                bootstrap_coeffs_x1[i] <- coef(firth_poisson)["x1"]
#           }
#           
#           # Remove missing values from bootstrap_coeffs_x1
#           bootstrap_coeffs_x1 <- bootstrap_coeffs_x1[!is.na(bootstrap_coeffs_x1)]
#           
#           # Calculate percentile-based confidence intervals for x1 coefficient
#           if (length(bootstrap_coeffs_x1) > 0) {
#                confidence_interval_x1 <- quantile(bootstrap_coeffs_x1, c(0.025, 0.975))
#                
#                # Check if true_beta is within the confidence interval
#                if (confidence_interval_x1[1] <= true_beta && true_beta <= confidence_interval_x1[2]) {
#                     coverage_count <- coverage_count + 1
#                }
#           }
#      }
#      
#      # Calculate coverage probability for the current beta value
#      coverage_probabilities[b] <- coverage_count / 100
# }
# 
# # Display the coverage probabilities for different beta values
# print(coverage_probabilities)


# Set the number of observations

n <- 800

# Define the true beta value for estimate PLCI
true_beta_1 <- 0

# Simulate logit_rv data
simulated_data <- sim_poisson(n, true_beta_1)
simulated_data <- as.data.frame(simulated_data)


firth_poisson <- glm(poisson_y ~ .,
                 data=simulated_data, family = poisson(link="log"),
                 method = "brglmFit")

# confint(firth_poisson)
# confint.default(firth_poisson)


# Set the number of bootstrap replications
n_bootstrap <- 100

# Initialize a vector to store bootstrap estimates of x1 coefficient
bootstrap_coeffs_x1 <- numeric(n_bootstrap)

# Initialize a counter for coverage
coverage_count <- 0

# Perform 1000 simulations
for (j in 1:1000) {
     # Simulate new data
     simulated_data <- sim_poisson(n, true_beta_1)
     simulated_data <- as.data.frame(simulated_data)

     # Initialize a vector to store bootstrap estimates of x1 coefficient
     bootstrap_coeffs_x1 <- numeric(n_bootstrap)

     # Perform bootstrap
     for (i in 1:n_bootstrap) {
          sampled_data <- simulated_data[sample(nrow(simulated_data), replace = TRUE), ]
          firth_poisson <- glm(poisson_y ~ ., data = sampled_data, family = poisson(link = "log"),
                               method = "brglmFit")
          bootstrap_coeffs_x1[i] <- coef(firth_poisson)["x1"]
     }

     # Remove missing values from bootstrap_coeffs_x1
     bootstrap_coeffs_x1 <- bootstrap_coeffs_x1[!is.na(bootstrap_coeffs_x1)]

     # Calculate percentile-based confidence intervals for x1 coefficient
     if (length(bootstrap_coeffs_x1) > 0) {
          confidence_interval_x1 <- quantile(bootstrap_coeffs_x1, c(0.025, 0.975))

          # Check if true_beta_1 is within the confidence interval
          if (confidence_interval_x1[1] <= true_beta_1 && true_beta_1 <= confidence_interval_x1[2]) {
               coverage_count <- coverage_count + 1
          }
     }
}

# Calculate coverage probability
coverage_probability <- coverage_count / 1000

# Display the coverage probability
print(coverage_probability)


# # Initialize a counter for coverage
# coverage_count <- 0
# 
# # Perform 1000 simulations
# for (j in 1:100) {
#      # Simulate new data
#      simulated_data <- sim_poisson(n, true_beta_1)
#      simulated_data <- as.data.frame(simulated_data)
#      
#      # Initialize a vector to store bootstrap estimates of x1 coefficient
#      bootstrap_coeffs_x1 <- numeric(n_bootstrap)
#      
#      # Perform bootstrap
#      for (i in 1:n_bootstrap) {
#           sampled_data <- simulated_data[sample(nrow(simulated_data), replace = TRUE), ]
#           firth_poisson <- glm(poisson_y ~ ., data = sampled_data, family = poisson(link = "log"),
#                                method = "brglmFit")
#           bootstrap_coeffs_x1[i] <- coef(firth_poisson)["x1"]
#      }
#      
#      # Calculate percentile-based confidence intervals for x1 coefficient
#      confidence_interval_x1 <- quantile(bootstrap_coeffs_x1, c(0.025, 0.975))
#      
#      # Check if true_beta_1 is within the confidence interval
#      if (confidence_interval_x1[1] <= true_beta_1 && true_beta_1 <= confidence_interval_x1[2]) {
#           coverage_count <- coverage_count + 1
#      }
# }
# 
# # Calculate coverage probability
# coverage_probability <- coverage_count / 100
# 
# # Display the coverage probability
# print(coverage_probability)


