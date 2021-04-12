install.packages("ggplot2")
install.packages("randomForest")
install.packages('caTools')
install.packages("DAAG")
install.packages("lattice")
install.packages("caret")
install.packages("e1071")

library(ggplot2)
data <- read.table("EWCS_2016.xls",header=1,sep = ',')
apply(data, 2,mean)
apply(data, 2,var)

data.pr <- prcomp(data, center = TRUE, scale = TRUE)

data.pr$rotation = -data.pr$rotation
data.pr$x =- data.pr$x
biplot(data.pr,scale=0)
plot(data.pr$x[,1],data.pr$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")

#par(mfrow=c(1,2))
screeplot(data.pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(data.pr$sdev^2 / sum(data.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),col=c("blue"), lty=5, cex=0.6)

x = data.pr$x[,c(1:2)]
km.out=kmeans(x,2,nstart=20)
km.out$cluster
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=1)
