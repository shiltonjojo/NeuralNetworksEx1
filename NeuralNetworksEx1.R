
library(MASS)
library(neuralnet)
set.seed(123)
data2 <- Boston
hist(data2$medv)
maxs <- apply(data2, 2, max) 
mins <- apply(data2, 2, min)
scaled4 <- 0
scaled4 <- as.data.frame(scale(data2, center = mins, scale = maxs - mins))
hist(scaled4$medv)
index <- sample(1:nrow(data2),round(0.75*nrow(data2)))
train <- data2[index,]
test <- data2[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
train_ <- scaled4[index,]
test_ <- scaled4[-index,]
#train_ <- data1[index,]
#test_ <- data1[-index,]
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(4,2),linear.output=T)
plot(nn)
pr.nn <- compute(nn,test_[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data2$medv)-min(data2$medv))+min(data2$medv)
test.r <- (test_$medv)*(max(data2$medv)-min(data2$medv))+min(data2$medv)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
print(paste(MSE.lm,MSE.nn))

par(mfrow=c(1,2))

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
out <- compute(nn,cbind(1,2,3,4,5,6,7,8,9,10,11,12,13))
out_ <- out$net.result*(max(data2$medv)-min(data2$medv))+min(data2$medv)
out_