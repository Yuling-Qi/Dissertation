library(actuar)
library(faux)
library(flexmix)
library(MASS)
library(Rcapture)
library(logistf)
library(faux)
library(brglm2)
library(actuar)
library(MASS)
library(actuar)
library(faux)
library(glmnet)


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


#sample n, the observations of the data
n<-200

# Define the values of beta
beta <-  -log(9)

data <- sim_poisson(n, beta)


model_ori <- glm(poisson_y~ ., data = as.data.frame(data), family=poisson(link="log"))

#summary(model_ori)



## Model with single variable      
model_test <- glm(poisson_y ~ 1, data=as.data.frame(data),family=poisson(link="log"))

# Forward selection
# selection by AIC
step(model_test,scope=list(lower~model_test,upper=model_ori), direction="forward",k=2)

# selection by BIC
step(model_test,scope=list(lower~model_test,upper=model_ori), direction="forward",k=log(length(data)))

# Backward selection using step-wise regression
# selection by AIC
step(model_ori,scope=list(lower~model_test,upper=model_ori), direction="backward",k=2)

# selection by BIC
step(model_ori,scope=list(lower~model_test,upper=model_ori), direction="backward",k=log(length(data)))

# Selection from both direction
step(model_ori,direction='both')


#new model with k=7, ml
#new model x1,x3
model_new<-glm(poisson_y ~ x5, data=as.data.frame(data),family=poisson(link="log"))

###################
#firth selection
# Fit Firth logistic regression model
model_ori_firth <- glm(poisson_y ~ ., data = as.data.frame(data), family = poisson(link = "log"),
                       method = "brglmFit")
model_test_firth<-glm(poisson_y ~ 1, data = as.data.frame(data), family = poisson(link = "log"),
                      method = "brglmFit")
model_upd_firth<-glm(poisson_y ~ x1+x2+x3, data = as.data.frame(data), family = poisson(link = "log"),
                    method = "brglmFit")

# Forward selection
# selection by AIC
step(model_test_firth,scope=list(lower~model_test_firth,upper=model_ori_firth), direction="forward",k=2)



# selection by BIC
step(model_test_firth,scope=list(lower~model_test_firth,upper=model_ori_firth), direction="forward",k=log(length(data)))

# Backward selection using step-wise regression
# selection by AIC
step(model_ori_firth,scope=list(lower~model_test_firth,upper=model_ori_firth), direction="backward",k=2)

# selection by BIC
step(model_ori_firth,scope=list(lower~model_test_firth,upper=model_ori_firth), direction="backward",k=log(length(data)))

# Selection from both direction
step(model_ori_firth,direction='both')

BIC(model_ori_firth)
BIC(model_test_firth)
BIC(model_upd_firth)



#############
#lasso selection
lasso_model <- cv.glmnet(as.matrix(data[, 2:4]), data[, "poisson_y"],
                         alpha = 1, family = poisson(link = "log"), nfolds = 3)


pred <- predict(lasso_model, s = "lambda.min", type = "response", newx = as.matrix(data[, 2:4]))
RMSE_prediction <- sqrt(mean((data[, "poisson_y"] - pred)^2))

#ridge selection
ridge_model <- cv.glmnet(as.matrix(data[, 2:4]), data[, "poisson_y"],
                         alpha = 0, family = poisson(link = "log"), nfolds = 10)
pred <- predict(ridge_model, s = "lambda.min", type = "response", newx = as.matrix(data[, 2:4]))
RMSE_prediction <- sqrt(mean((data[, "poisson_y"] - pred)^2))


# Convert matrix to a data frame
data_df <- as.data.frame(data)

# Calculate predictions of the original model
original_predictions <- predict(model_ori_firth, newdata = data_df, type = "response")

# Calculate predictions of the new model
new_predictions <- predict(model_test, newdata = data_df, type = "response")

# Extract actual observed values
actual_values <- data_df$poisson_y  # Adjust based on your data structure

# Calculate RMSE for predictions of the original model
original_rmse <- sqrt(mean((actual_values - original_predictions)^2))

# Calculate RMSE for predictions of the new model
new_rmse <- sqrt(mean((actual_values - new_predictions)^2))

# Output results
cat("Original Model RMSE:", original_rmse, "\n")
cat("New Model RMSE:", new_rmse, "\n")

summary(model_new)
BIC(model_ori)
BIC(model_test)







