# Load required libraries
library(brglm2)
library(logistf)
library(glmnet)

# Read the data from the CSV file
data.3 <- read.csv("Impdent.csv")

# ML analysis
# Fit the Poisson regression model with the offset variable
log <- glm(Hema ~ Light_vs_no + Heavy_vs_light + Diabetes + Age_decade + offset(log_Implants),
           data = data.3, family = poisson(link = "log"))
summary(log)

# firth
#log linear
# Fit the Firth regression model with the offset variable
firth_log <- glm(Hema ~ Light_vs_no + Heavy_vs_light + Diabetes + Age_decade + offset(log_Implants),
                 data=data.3, family = poisson(link="log"),
                    method = "brglmFit") 

summary(firth_log)


# Lasso for poisson model
# Prepare the data for Lasso regression
x_pena <- model.matrix(Hema ~ Light_vs_no + Heavy_vs_light + Diabetes + Age_decade + offset(log_Implants), 
                       data.3)[,-1]
y_pena <- data.3$Hema

set.seed(10)

# Train the data
train = sample(1:nrow(x_pena), nrow(x_pena)/2)
x_train = x_pena[train, ]
y_train = y_pena[train]

# Obtain the optimal lambda for the poisson model
cv_output <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 3, family = "poisson")

# Apply the best lambda to fit the data
best_lam <- cv_output$lambda.min
lasso.log <- glmnet(x_train, y_train, alpha = 1, lambda = best_lam, family = "poisson")

# Obtain the coefficients of the poisson regression model with Lasso for the optimal lambda
lasso_coefficients <- coef(lasso.log, s = best_lam)
coef(lasso.log)

## ridge poisson 

set.seed(10)
train = sample(1:nrow(x_pena), nrow(x_pena)/2)
x_test = (-train)
y_test = y_pena[x_test]

cv_output <- cv.glmnet(x_pena[train,], y_pena[train],
                       alpha = 0, 
                       nfolds = 3, family=poisson(link="log"))

best_lam <- cv_output$lambda.min

ridge.log <- glmnet(x_pena[train,], y_pena[train], alpha = 0, 
                    lambda = best_lam, family = poisson(link="log"))

coef(ridge.log)



