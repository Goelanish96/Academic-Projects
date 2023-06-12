# Boston Data Load
rm(list=ls())

library(MASS)
data(Boston)
attach(Boston)

# Data and Variable type
head(Boston)
str(Boston)
n <- dim(Boston)[1]   # sample size
p <- dim(Boston)[2]-1 # number of predictors excluding column of ones

# Train-Test Data Split
set.seed(15214626)
sample_index <- sample(nrow(Boston),nrow(Boston)*0.80)
Boston_train <- Boston[sample_index,]
Boston_test <- Boston[-sample_index,]

#### 'Best' Linear Model fitting on training data ####
Boston.train.lm <- lm(medv ~ .-indus-age, data = Boston_train)
summary(Boston.train.lm)
p1=11

# In-sample fit (prediction) 
yhat.train <- predict(object = Boston.train.lm, newdata = Boston_train) 
mse.train <- sum((Boston_train$medv-yhat.train)^2)/(dim(Boston_train)[1]-p1-1) #in-sample mean sum squared error, including df
ase.train <- mean((Boston_train$medv-yhat.train)^2)

# Out-of-sample prediction  
yhat.test <- predict(object = Boston.train.lm, newdata = Boston_test)
mspe.test <- mean((Boston_test$medv-yhat.test)^2) #MSPE
rmspe.test <- sqrt(mspe.lm) #RMSPE

## lm, MSE = 20.24, ASE= 19.64; MSPE=33.33; RMSPE=5.77; lm(medv ~ -indus-age) ##

# 5-fold Cross Validation
library(boot)
Boston.lm <- glm(medv ~ .-indus-age, data = Boston)
cv5 <- cv.glm(Boston.lm,data=Boston,K=5)$delta[2]
cv5
## 5-fold cv=24.25, comparable with MSPE=33.33 ##

# Leave one out Cross Validation
cv <- cv.glm(Boston.lm,data=Boston,K=n)$delta[2]
cv
## Leave one out Cv = 23.51 ##
### If I rerun 5 fold cross validation, the cv score is different each time but if I rerun leave one out croos validation ###
### I get the same cv score each time. ###


#### Regression Tree ####

library(rpart)
library(rpart.plot)
## rpart with default cp=0.01
Boston_rpart <- rpart(formula = medv ~ ., data = Boston_train) # default cp
prp(Boston_rpart,digits = 4, extra = 1)

Boston_train_pred_tree = predict(Boston_rpart)
mean((Boston_train_pred_tree - Boston_train$medv)^2)
Boston_test_pred_tree = predict(Boston_rpart,Boston_test)
mean((Boston_test_pred_tree - Boston_test$medv)^2)
# ASE train rpart = 14.93; Out of sample MSPE rpart = 26.26 # default cp=0.01

## rpart with small cp for bushy tree and then prune using plotcp
Boston_largetree <- rpart(formula = medv ~ ., data = Boston_train, cp = 0.001)
prp(Boston_largetree,digits = 4, extra = 1)
plotcp(Boston_largetree)
printcp(Boston_largetree)
# choose the leftmost value for which the value lies below the horizontal line
Boston_rpart <- prune(Boston_largetree, cp=0.0058)
prp(Boston_rpart,digits = 4, extra = 1)

Boston_train_pred_tree = predict(Boston_rpart)
mean((Boston_train_pred_tree - Boston_train$medv)^2)
Boston_test_pred_tree = predict(Boston_rpart,Boston_test)
mean((Boston_test_pred_tree - Boston_test$medv)^2)

# ASE train rpart = 14.93; Out of sample MSPE rpart = 26.26 # default cp=0.01
# ASE train rpart = 12.59; Out of sample MSPE rpart = 22.02 # pruned using plotcp

#### k-Nearest Neighbors for Regression (Prediction) ####

# initialize standardized training, testing, and new data frames to originals
train.norm <- Boston_train
test.norm <- Boston_test

## Scaling all predictor variables ##
cols <- colnames(train.norm[, -(p+1)]) #scaling only on p=13 predictors X
for (j in cols) {
  train.norm[[j]] <- (train.norm[[j]] - min(Boston_train[[j]])) / (max(Boston_train[[j]]) - min(Boston_train[[j]]))
  test.norm[[j]] <- (test.norm[[j]] - min(Boston_train[[j]])) / (max(Boston_train[[j]]) - min(Boston_train[[j]]))
}

summary(train.norm)
summary(test.norm)

library(FNN)
set.seed(15214626)

sample_index2 <- sample(nrow(Boston_train),nrow(Boston_train)*0.80)
train2.norm <- train.norm[sample_index2,]
valid.norm <- train.norm[-sample_index2,]

# initialize a data frame with two columns: k and accuracy
RMSE.df <- data.frame(k = seq(1, 30, 1), RMSE.k = rep(0, 30))

# compute knn for different k on validation set
for (i in 1:30) {
  knn.reg.pred <- knn.reg(train = train2.norm[, c(1:p)], test = valid.norm[, c(1:p)], 
                          y = train2.norm$medv, k = i)
  RMSE.df[i, 2] <- sqrt(sum((valid.norm$medv-knn.reg.pred$pred)^2)/length(valid.norm$medv))
}
RMSE.df
k <- which(RMSE.df[,2] == min(RMSE.df[,2]))
k

## Hence, choose "optimal" k=3 ##

## Use selected "optimal k" (from TRAINING data only) for knn ##

Boston.knn.reg <- knn.reg(train = train.norm[, c(1:p)], 
                          test = test.norm[, c(1:p)], 
                          y = train.norm$medv, 
                          k = k)

# Out-of-Sample Testing #
# calculate MSPE (root mean (average) sum squared prediction errors)
MSPE <- sum((Boston_test$medv-Boston.knn.reg$pred)^2)/length(Boston_test$medv)
# calculate RMSE (root mean (average) sum squared (prediction) errors)
RMSE <- sqrt(MSPE)
RMSE
## MSPE=24.08; RMSPE=4.91; knn, "optimal" k=3 ##

#### Bagging ####

library(ipred)

# Train-Test Data Split
set.seed(15214626)
index <- sample(nrow(Boston),nrow(Boston)*0.80)
boston_train <- Boston[index,]
boston_test <- Boston[-index,]

# Determining optimum number of trees for bagging
ntree<- c(1, 3, 5, seq(10, 200, 10))
MSE_test<- rep(0, length(ntree))
for(i in 1:length(ntree)){
  boston_bag1<- bagging(medv~., data = boston_train, nbagg=ntree[i])
  boston_bag_pred1<- predict(boston_bag1, newdata = boston_test)
  MSE_test[i]<- mean((boston_test$medv-boston_bag_pred1)^2)
  df <- rbind(ntree, MSE_test)
}
plot(ntree, MSE_test, type = 'l', col=2, lwd=2, xaxt="n")
axis(1, at = ntree, las=1)
df
# Optimum number of trees to be chosen for bagging is 100, Out of sample MSPE for Bagging- 20.84 #

## Out-of-Bag Prediction Error ##
boston_bag_oob<- bagging(formula = medv~., 
                         data = boston_train, 
                         coob=T, 
                         nbagg=100)
boston_bag_oob
# Out of Bag RMSE- 4.072 #

#### Random Forest ####

library(randomForest)
boston_rf<- randomForest(medv~., data = Boston_train, importance=TRUE)
boston_rf

# Variable Importance 
boston_rf$importance

# Out of Bag Prediction Error vs number of trees
plot(boston_rf$mse, type='l', col=2, lwd=2, xlab = "ntree", ylab = "OOB Error")

# Out of Sample Testing
boston_rf_pred<- predict(boston_rf, Boston_test)
mean((Boston_test$medv-boston_rf_pred)^2)
## MSPE= 15.7332 ##

#### Boosting ####
library(gbm)

boston_boost<- gbm(formula = medv~., 
                   data = Boston_train, 
                   distribution = "gaussian", 
                   n.trees = 10000, 
                   shrinkage = 0.01, 
                   interaction.depth = 8)
summary(boston_boost)

# Prediction on Test Sample
boston_boost_pred_test<- predict(boston_boost, Boston_test, n.trees = 10000)
mean((Boston_test$medv-boston_boost_pred_test)^2)

## MSPE= 8.1081 Out of sample

#### Generalized Additive Models ####

library(mgcv)

#create gam model
Boston_gam <- gam(medv ~ s(crim)+s(zn)+s(indus)+chas+s(nox)
                  +s(rm)+s(age)+s(dis)+rad+s(tax)+s(ptratio)
                  +s(black)+s(lstat),data=Boston_train) # chas and rad are taken as partially linear because they are categorical variables

summary(Boston_gam)

plot(Boston_gam, pages=1)

# Model AIC, BIC and Mean Residual Deviance
AIC(Boston_gam) 
BIC(Boston_gam)
Boston_gam$deviance

# In-sample fit performance
Boston_gam.mse.train <- Boston_gam$dev/Boston_gam$df.residual 
#Average Sum of Squared Error
Boston_gam.mse.train <- Boston_gam$dev/nrow(Boston_train) 

#using the predict() function
pi <- predict(Boston_gam,Boston_train)
mean((pi - Boston_train$medv)^2)
# ASE in-sample = 9.9727 

# Out-of-sample performance
pi.out <- predict(Boston_gam,Boston_test)
mean((pi.out - Boston_test$medv)^2)
# MSPE out-of-sample = 13.3541

#### Neural Network ####

## initialize standardized training, testing, and new data frames to originals ##
train.norm <- Boston_train
test.norm <- Boston_test

summary(train.norm)
summary(test.norm)

## normalize all numerical variables (X&Y) to 0-1 scale, range [0,1]-standardization ##
cols <- colnames(train.norm[, ]) #scaling both X and Y
for (j in cols) {
  train.norm[[j]] <- (train.norm[[j]] - min(Boston_train[[j]])) / (max(Boston_train[[j]]) - min(Boston_train[[j]]))
  test.norm[[j]] <- (test.norm[[j]] - min(Boston_train[[j]])) / (max(Boston_train[[j]]) - min(Boston_train[[j]]))
}

library(neuralnet)
f <- as.formula("medv ~ .")
nn <- neuralnet(f,data=train.norm, hidden=c(5,3), linear.output=T)
plot(nn)

## Q: How many parameters are involved? ##
(13+1)*5+(5+1)*3+4

# MSPE of the above neural network model for Testing data

pr_nn <- compute(nn, test.norm[,1:p])

## recover the predicted value back to the original response scale ## 
pr_nn_org <- pr_nn$net.result*(max(Boston_train$medv)-min(Boston_train$medv))+min(Boston_train$medv)
test_r <- (test.norm$medv)*(max(Boston_train$medv)-min(Boston_train$medv))+min(Boston_train$medv)

## MSPE of testing set ##
MSPE_nn <- sum((test_r - pr_nn_org)^2)/nrow(test.norm)
MSPE_nn
sqrt(MSPE_nn)

## MSPE=15.1742; RMSPE=3.8954; neuralnet, hidden=c(5,3) ##


