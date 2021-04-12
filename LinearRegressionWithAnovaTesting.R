install.packages("ggplot2")
library(ggplot2)
install.packages("lessR")#for plotting
library(lessR)
install.packages("caTools")#for spliting
library(caTools)
install.packages("lattice")
library(lattice)
install.packages("caret")# for preprocessing
library(caret)
install.packages("glmnet")
install.packages("Matrix")

data <- read.csv("FEV.csv")
head(data)
print("Summary of Dataset")
summary(data)
#As the "ID" Column doesn't making effect for analysis, so we should drop it
data = subset(data,select = -(ID))
head(data)

boxplot(data)
data <- data[-which(data$Age %in% boxplot.stats(data$Age)$out), ]
pairs(data , upper.panel = NULL)

qplot(data$Age, geom="histogram",binwidth=0.5)
#As you can see Age Column is normally distributed as you can see from bull shape curv
qplot(data$Hgt, geom="histogram",binwidth=0.5)
#As you can see Height Column is normally distributed
PieChart(Sex, hole = 0, values = "%", data = data, fill = 1:4, main = "")
#As you can see Sex categorical variable is distributed balance
PieChart(x = Smoke, hole = 0, values = "%", data = data, fill = 1:4, main = "")
#As you can see Smoke categorical variable is distributed balance
qplot(data$Hgt_m, geom="histogram",binwidth=0.5)

#Now see the effect with FEV Column
qplot(data=data,Age,FEV, geom="point")
qplot(data=data,Hgt,FEV, geom="point")
qplot(data=data,Hgt_m,FEV, geom="point")
#as you can see these graph making a polynomial shape, not straight line


#Now apply anova, analysis of variance analysis
anova <- aov(FEV~Age+ Hgt + Sex + Smoke + Hgt_m, data = data)
summary(anova)

#Check interaction between Age and Height, as FEV variance difference get higher with the age
#and height
########## Now Modeling####################

#train test split
split = sample.split(data$Age,SplitRatio=0.8)
training_data = subset(data,split==TRUE)
test_data = subset(data,split==FALSE)

#Applying modeiling
lr.age = lm(FEV~Age, data = training_data)
summary(lr.age)
r2 = as.character(round(summary(lr.age)$r.squared, 2))
adj_r2 = as.character(round(summary(lr.age)$adj.r.squared, 2))
cat("R-squared:",adj_r2) #Adjusted R-squared
plot(lr.age)
#################################
lr.height = lm(FEV~Age + Hgt, data = training_data)
summary(lr.height)
r2 = as.character(round(summary(lr.height)$r.squared, 2))
adj_r2 = as.character(round(summary(lr.height)$adj.r.squared, 2))
cat("R-squared:",adj_r2) #Adjusted R-squared
#Checking which is better
#h0:model1 is better
#h1:model2 is better
anova(lr.age,lr.height) #As you can see model1 is better

#polynomial regression
pm = lm(FEV~poly(Age,2) + poly(Hgt,2), data = training_data)
summary(pm)
r2 = as.character(round(summary(pm)$r.squared, 2))
adj_r2 = as.character(round(summary(pm)$adj.r.squared, 2))
cat("R-squared:",adj_r2) #Adjusted R-squared
#h0:model1 is better
#h1:model2 is better
anova(lr.height,pm)
#############################
pm1 = lm(FEV~poly(Age,3) + poly(Hgt,3), data = training_data)
summary(pm1)
r2 = as.character(round(summary(pm1)$r.squared, 2))
adj_r2 = as.character(round(summary(pm1)$adj.r.squared, 2))
cat("R-squared:",adj_r2) #Adjusted R-squared
#h0:model1 is better
#h1:model2 is better
anova(pm,pm1)#as the difference you can see, value of F=0.9431 so, hull hypothesis is not rejeced
###############################
lr.height_m = lm(FEV~Age + Hgt+Hgt_m, data = training_data)
summary(lr.height_m)
r2 = as.character(round(summary(lr.height_m)$r.squared, 2))
adj_r2 = as.character(round(summary(lr.height_m)$adj.r.squared, 2))
cat("R-squared:",adj_r2) #Adjusted R-squared
#Checking which is better
#h0:model1 is better
#h1:model2 is better
anova(lr.height_m,pm1) #As you can see model2 is better
###############################
pm3 = lm(FEV~poly(Age,2) + poly(Hgt,2)+poly(Hgt_m,2), data = training_data)
summary(pm3)
r2 = as.character(round(summary(pm3)$r.squared, 2))
adj_r2 = as.character(round(summary(pm3)$adj.r.squared, 2))
cat("R-squared:",adj_r2) #Adjusted R-squared
#Checking which is better
#h0:model1 is better
#h1:model2 is better
anova(pm3,lr.height_m) #As you can see model2 is better
###############################
pm4 = lm(FEV~poly(Age,3) + poly(Hgt,3)+poly(Hgt_m,3), data = training_data)
summary(pm3)
r2 = as.character(round(summary(pm3)$r.squared, 2))
adj_r2 = as.character(round(summary(pm3)$adj.r.squared, 2))
cat("R-squared:",adj_r2) #Adjusted R-squared
#Checking which is better
#h0:model1 is better
#h1:model2 is better
anova(pm3,pm4) #As you can see model1 is better because of F=0.99 which is greater than 0.05
####################################
lr = lm(FEV~., data = training_data)
summary(lr)
eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  cat("R-squared:",adj_r2) #Adjusted R-squared
  cat("   RMSE : ",as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}

predictions = predict(lr, newdata = test_data)
eval_metrics(lr, training_data, predictions, target = 'FEV')
#Checking which is better
#h0:model1 is better
#h1:model2 is better
anova(pm4,lr)#model2 is better
######################################################
#Applying Ridge regression
library(glmnet)
library(Matrix)
dummies <- dummyVars(FEV ~ ., data = data)
train_dummies = predict(dummies, newdata = training_data)
test_dummies = predict(dummies, newdata = test_data)
print(dim(train_dummies)); print(dim(test_dummies))

x = as.matrix(train_dummies)
y_train = training_data$FEV
x_test = as.matrix(test_dummies)
y_test = test_data$FEV
lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)
summary(ridge_reg)
cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test_data)
