library(caTools)
library(ggplot2)

data <- read.table("student-mat.xls",header=1,sep = ';')
sum(is.na(data))
columns = names(data)
####################visualizing the dataset ##########
#plotting school column
ggplot(data, aes(x = school)) + geom_bar(fill="blue", position="dodge")
#plotting sex column
ggplot(data, aes(x = sex)) + geom_bar(fill="blue", position="dodge")
#plotting Address column
ggplot(data, aes(x = address)) + geom_bar(fill="blue", position="dodge")
#plotting fsize column
ggplot(data, aes(x = famsize)) + geom_bar(fill="blue", position="dodge")
#plotting Mjob column
ggplot(data, aes(x = Mjob)) + geom_bar(fill="blue", position="dodge")
#plotting guardian column
ggplot(data, aes(x = guardian)) + geom_bar(fill="blue", position="dodge")
#plotting schoolsup column
ggplot(data, aes(x = schoolsup)) + geom_bar(fill="blue", position="dodge")
#As schoolsup has mostly "no" value, so we don't need in dataframe, it may cause overfitting because of not equally distributed
data = data[,-(15:16)]
#plotting famsup column
ggplot(data, aes(x = famsup)) + geom_bar(fill="blue", position="dodge")
#plotting nursery column
ggplot(data, aes(x = nursery)) + geom_bar(fill="blue", position="dodge")
#plotting romantic column
ggplot(data, aes(x = romantic)) + geom_bar(fill="blue", position="dodge")

data <- transform(
  data,
  school = as.integer(factor(school)),
  sex = as.integer(factor(sex)),
  address = as.integer(factor(address)),
  famsize = as.integer(factor(famsize)),
  Pstatus = as.integer(factor(Pstatus)),
  Medu = as.integer(factor(Medu)),
  Fedu = as.integer(factor(Fedu)),
  Mjob = as.integer(factor(Mjob)),
  Fjob = as.integer(factor(Fjob)),
  reason = as.integer(factor(reason)),
  guardian = as.integer(factor(guardian)),
  famsup = as.integer(factor(famsup)),
  paid = as.integer(factor(paid)),
  activities = as.integer(factor(activities)),
  nursery = as.integer(factor(nursery)),
  higher = as.integer(factor(higher)),
  internet = as.integer(factor(internet)),
  romantic = as.integer(factor(romantic))
)
sapply(data, class)
data = data[,-(29:30)]
data[,c(1:28)] = scale(data[,c(1:28)])

#train test split
split = sample.split(data$G3,SplitRatio=0.8)
training = subset(data,split==TRUE)
test = subset(data,split==FALSE)

#random forest
library(randomForest)
rf <- randomForest(
  G3 ~ .,
  data=training
)

actual = test[,29]
preds = predict(rf, newdata=test[,c(1:28)])
rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
print("Random Forest R square :")
print(rsq)

#Linear Regression
linearMod <- lm(G3 ~., data = training)
modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
summary(linearMod)
#############Model Evaluation###################
preds = predict(linearMod, newdata=test[,c(1:28)])
actual = test$G3
#Finding RSquare
rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
print(rsq)
print("Linear Regression R square :")
print(rsq)

##Implement with k-fold cross validataion
library(lattice)
library(DAAG)
cvResults <- suppressWarnings(CVlm(data, form.lm= G3 ~., m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."))
attr(cvResults, 'ms')  # => 210.6192 mean squared error
