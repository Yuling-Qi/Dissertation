# Load necessary packages
library(dplyr)
library(ggplot2)
library(brglm2)  # For Firth's logistic regression
library(logistf) # For Firth's logistic regression
library(glmnet)  # For regularization (LASSO)
library(datasets)

# data("Titanic", package = "datasets")
# data <- as.data.frame(Titanic)
# 
# # Convert "Survived" to binary labels
# data$Survived <- ifelse(data$Survived == "Yes", 1, 0)
# 
# # Convert "Class" to ordinal variable
# data$Class <- as.numeric(factor(data$Class, levels = c("1st", "2nd", "3rd", "Crew")))
# 
# # Convert "Sex" to binary variable
# data$Sex <- ifelse(data$Sex == "Female", 1, 0)
# 
# ## data modifications
# data$Survived<-ifelse(data$Class=="1",1,data$Survived[data$Class=="2"])
# 
# 
# # Perform standard logistic regression with selected variables
# standard_model <- glm(Survived ~ Class + Sex + Age + Freq, 
#                       data = data, family = "binomial")
# 
# # Print summary of standard logistic regression model
# summary(standard_model)



# Load the iris dataset
data(iris)

library(ggplot2)

# Load the iris dataset
data(iris)

# # Create a scatter plot for Petal Length vs Petal Width
# petal_plot <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
#      geom_point() +
#      labs(title = "Scatter Plot of Petal Length vs Petal Width",
#           x = "Petal Length", y = "Petal Width", color = "Species") +
#      theme_minimal()
# 
# # Create a scatter plot for Sepal Length vs Sepal Width
# sepal_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#      geom_point() +
#      labs(title = "Scatter Plot of Sepal Length vs Sepal Width",
#           x = "Sepal Length", y = "Sepal Width", color = "Species") +
#      theme_minimal()
# 
# # Combine both plots using cowplot
# library(cowplot)
# combined_plot <- plot_grid(petal_plot, sepal_plot, nrow = 1)
# 
# # Print the combined plot
# print(combined_plot)
# 
# # Load the iris dataset
# data(iris)
# 
# # Calculate the maximum and minimum values for each variable
# max_values <- sapply(iris[, 1:4], max)
# min_values <- sapply(iris[, 1:4], min)
# 
# # Print the maximum and minimum values
# print(max_values)
# print(min_values)


# Convert Species to binary labels
iris$Species <- ifelse(iris$Species == "setosa", 1, 0)

# Perform standard logistic regression
standard_model <- glm(Species ~ Sepal.Length +Petal.Length+Petal.Width , 
                      data = iris, family = "binomial")

# Print summary of standard logistic regression model
summary(standard_model)

# # Perform logistic regression using Firth's method
# firth_logit <- logistf(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
#                        data = iris, firth = TRUE) 
# 
# # Print summary of Firth's logistic regression model
# summary(firth_logit)
# Set the control parameters for logistf
# Set the control parameters for logistf
logistf_control <- logistf.control(maxit = 10000, maxstep = 0.1)

# Set the control parameters for logistpl
pl_control <- logistpl.control(maxit = 10000)

# Perform logistic regression using Firth's method with adjusted controls
firth_logit <- logistf(Species ~ Sepal.Length  + Petal.Length + Petal.Width, 
                       data = iris, firth = TRUE, control = logistf_control, plcontrol = pl_control)

# Print summary of Firth's logistic regression model
summary(firth_logit)



# Visualize the logistic regression results
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = factor(Species))) +
     geom_point() +
     geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
     labs(title = "Logistic Regression on Iris Data", x = "Sepal Length", y = "Sepal Width",
          color = "Species") +
     theme_minimal()

#lasso for logit regression model

#obtain the train and test data 

x_pena <- model.matrix(Species~., iris)[,-5]
y_pena <- iris$Species

set.seed(10)
#train the data
train = sample(1:nrow(x_pena), nrow(x_pena)/2) 
x_test = (-train)
y_test = y_pena[x_test]


#obtain the optimal lambda for the logistic model 

cv_output <- cv.glmnet(x_pena[train,], y_pena[train],
                       alpha = 1, 
                       nfolds = 3, family="binomial")

#apply best lambda to fit the data
best_lam <- cv_output$lambda.min 

lasso.logit <- glmnet(x_pena[train,], y_pena[train], alpha = 1, 
                      lambda = best_lam, family = "binomial")
#obtain the coefficients of logistic model with lasso
coef(lasso.logit) 


#ridge for logit

x_pena <- model.matrix(Species~., iris)[,-5]
y_pena <- iris$Species

set.seed(10)
train = sample(1:nrow(x_pena), nrow(x_pena)/2)
x_test = (-train)
y_test = y_pena[x_test]

cv_output <- cv.glmnet(x_pena[train,], y_pena[train],
                       alpha = 0, 
                       nfolds = 3, family="binomial")

best_lam <- cv_output$lambda.min

ridge.logit <- glmnet(x_pena[train,], y_pena[train], alpha = 0, 
                      lambda = best_lam, family = "binomial")

coef(ridge.logit)