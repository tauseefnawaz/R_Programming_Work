library(caTools)
library(ggplot2)

data <- read.table("bank.xls",header=1,sep = ';')
sum(is.na(data))
####################visualizing the dataset ##########
#plotting job column
ggplot(data, aes(x = job)) + geom_bar(fill="blue", position="dodge")
#plotting marital column
ggplot(data, aes(x = marital)) + geom_bar(fill="blue", position="dodge")
#plotting education column
ggplot(data, aes(x = education)) + geom_bar(fill="blue", position="dodge")
#plotting default column
ggplot(data, aes(x = default)) + geom_bar(fill="blue", position="dodge")
#As default has mostly "no" value, so we don't need in dataframe, it may cause overfitting because of not equally distributed
data = data[,-(4:5)] 
#plotting housing column
ggplot(data, aes(x = housing)) + geom_bar(fill="blue", position="dodge")
#plotting loan column
ggplot(data, aes(x = loan)) + geom_bar(fill="blue", position="dodge")
#plotting contact column
ggplot(data, aes(x = contact)) + geom_bar(fill="blue", position="dodge")
#plotting month column
ggplot(data, aes(x = month)) + geom_bar(fill="blue", position="dodge")
#plotting poutcome column
ggplot(data, aes(x = poutcome)) + geom_bar(fill="blue", position="dodge")
data = data[,-(13:14)]
#for pdays column
ggplot(data, aes(x = pdays)) + geom_histogram(fill="blue", position="dodge",binwidth=100)
data$pdays[data$pdays==-1] = mean(data$pdays)
#for balance column
data$balance[data$balance==0] = mean(data$balance)

#########################Data preprocessing###############
data <- transform(
  data,
  age = age,
  job = as.integer(factor(job)),
  marital = as.integer(factor(marital)),
  balance = balance,
  housing = as.integer(factor(housing)),
  loan = as.integer(factor(loan)),
  contact = as.integer(factor(contact)),
  day = day,
  month = as.integer(factor(month)),
  duration = duration,
  campaign = campaign,
  pdays = pdays
)
sapply(data, class)

#train test split
split = sample.split(data$y,SplitRatio=0.8)
training = subset(data,split==TRUE)
test = subset(data,split==FALSE)

#scalling the data
training[,c(1:12)] = scale(training[,c(1:12)])

#applying the model
#install.packages("randomForest")
library(randomForest)
rf <- randomForest(
  y ~ .,
  data=training
)

test[,c(1:12)] = scale(test[,c(1:12)])

pred = predict(rf, newdata=test[,c(1:12)])
cm = table(test[,13], pred)
fourfoldplot(cm, color = c("#CC6666", "#99CC99"),conf.level = 0, margin = 1, main = "Confusion Matrix")

library(caret) 
res = confusionMatrix(cm)
print(res)
Accuracy = res$byClass["Pos Pred Value"]
print("Accuracy: ")
print(Accuracy)

