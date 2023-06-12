# Credit Card Data Load

rm(list=ls())
setwd("C:/Users/goela/Downloads")

credit <- read.csv("credit_default.csv")
dim(credit)
attach(credit)
library(tidyverse)

##I. Examine data and variable types ##
# Data And Variable type

head(credit)
str(credit)
n <- dim(credit)[1]   # sample size
p <- dim(credit)[2]-1 # number of predictors excluding column of ones

names(credit)

credit <- rename(credit, default=default.payment.next.month)
attach(credit)

## II. Factor/categorical variable ##
## We see all variables are int, but we know that SEX, EDUCATION, MARRIAGE are categorical, we convert them to factor. ##
credit$EDUCATION<- as.factor(credit$EDUCATION)
credit$SEX<- as.factor(credit$SEX)
credit$MARRIAGE<- as.factor(credit$MARRIAGE)

## III. logistic regression (generalized linear model) ##
# Train-Test Data Split #

set.seed(15214626)

sample_index <- sample(nrow(credit),nrow(credit)*0.80)
credit_train <- credit[sample_index,]
credit_test <- credit[-sample_index,]

#### glm Model on all variables ####
credit_glm <- glm(default ~ ., family = binomial, data = credit_train)
summary(credit_glm)

#### ROC (in-Sample) 
library(ROCR)
pred_glm_train<- predict(credit_glm, newdata = credit_train, type="response") #predicted probability
pred <- prediction(pred_glm_train, credit_train$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

## AUC=0.7272, glm, 80% training data 

## (Asymmetric) Cost (Misclassification Rate) function, 5:1 cost ratio 

costfunc <- function(r, pi){
  weight1 <- 5
  weight0 <- 1
  pcut = 1/(weight1+weight0)
  c1 <- (r==1)&(pi<pcut) #logical vector - true if actual 1 but predict 0
  c0 <-(r==0)&(pi>pcut) #logical vector - true if actual 0 but predict 1
  return(mean(weight1*c1+weight0*c0))
}
costfunc(credit_train$default,pred_glm_train)

## asyMR=0.679, glm, 80% training data

#### ROC (Out-of-Sample) 
pred_glm_test<- predict(credit_glm, newdata = credit_test, type="response") #predicted probability
pred <- prediction(pred_glm_test, credit_test$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

## AUC=0.7185, glm, 20% test data ##

## (Asymmetric) Cost (Misclassification Rate) function, 5:1 cost ratio ##
costfunc(credit_test$default,pred_glm_test)

## asyMR=0.6843, glm, 20% test data##

#### AUC as a cost function 
costfunc1 = function(obs, pred.p){
  pred <- prediction(pred.p, obs)
  perf <- performance(pred, "tpr", "fpr")
  cost =unlist(slot(performance(pred, "auc"), "y.values"))
  return(cost)
} 
costfunc1(credit_test$default,pred_glm_test)
## AUC=0.7185, glm, 20% test data ##

#### Cross Validation 
## full data ##

credit0_glm<- glm(default~. , family=binomial, data=credit);  
cv_result  <- cv.glm(data=credit, glmfit=credit0_glm, cost=costfunc, K=5) 
cv_result$delta[2]

## 5-fold CV(asyMR)=0.6852, glm, full model ##

cv_result1  <- cv.glm(data=credit, glmfit=credit0_glm, cost=costfunc1, K=5) 
cv_result1$delta[2]

## 5-fold CV(AUC)=0.7245, glm, full model ##

#### Classification Tree, asymmetric cost ####
library(rpart)
library(rpart.plot)

credit_rpart <- rpart(formula = default ~ . , data = credit_train, method = "class", parms = list(loss=matrix(c(0,5,1,0), nrow = 2)))
prp(credit_rpart, extra = 1)
plotcp(credit_rpart)

credit_train_prob_rpart = predict(credit_rpart, credit_train, type="prob")

pred <- prediction(credit_train_prob_rpart[,2], credit_train$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))
## AUC=0.7311, rpart, 80% training data ##

## (Asymmetric) Cost (Misclassification Rate) function, 5:1 cost ratio ##
costfunc(credit_train$default,credit_train_prob_rpart[,2])
## asyMR=0.5832, rpart, 80% training data##

credit_test_prob_rpart = predict(credit_rpart, credit_test, type="prob")
pred <- prediction(credit_test_prob_rpart[,2], credit_test$default)
unlist(slot(performance(pred, "auc"), "y.values"))
## AUC=0.7191, rpart, 20% testing data ##

## (Asymmetric) Cost (Misclassification Rate) function, 5:1 cost ratio ##
credit_test_prob_rpart = predict(credit_rpart, credit_test, type="prob")
costfunc(credit_test$default,credit_test_prob_rpart[,2])
## asyMR=0.6118, rpart, 20% testing data ##

#### Bagging ####
library(ipred)

credit_bag <- bagging(as.factor(default)~., data = credit_train, nbagg=100)
credit_bag_pred <- predict(credit_bag, newdata = credit_train, type="prob")[,2]
credit_bag_pred_test <- predict(credit_bag, newdata = credit_test, type="prob")[,2]

library(ROCR)
pred = prediction(credit_bag_pred_test, credit_test$default)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))
## AUC=0.7490, rpart, 20% test data ##

credit_bag_pred_test <- predict(credit_bag, newdata = credit_test, type="class")
table(credit_test$default, credit_bag_pred_test, dnn = c("True", "Pred"))
##AUC of training set
predtrain = prediction(credit_bag_pred, credit_train$default)
perftrain = performance(predtrain, "tpr", "fpr")
plot(perftrain, colorize=TRUE)
unlist(slot(performance(predtrain, "auc"), "y.values"))
## AUC=0.9999, rpart, 80% training data ##
credit_bag_pred_train <- predict(credit_bag, newdata = credit_train, type="class")
table(credit_train$default, credit_bag_pred_train, dnn = c("True", "Pred"))


#### Random Forest ####

credit_rf <- randomForest(as.factor(default)~., 
                          data = credit_train,
                          importance=TRUE, ntree=500)
credit_rf

# Error rate vs number of trees
plot(credit_rf, lwd=rep(2, 3))
legend("right", legend = c("OOB Error", "FPR", "FNR"), lwd=rep(2, 3), lty = c(1,2,3), col = c("black", "red", "green"))

# ROC curve and AUC score
credit_rf_pred <- predict(credit_rf, type = "prob")[,2]
library(ROCR)
pred <- prediction(credit_rf_pred, credit_train$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
# Get the AUC
unlist(slot(performance(pred, "auc"), "y.values"))
# AUC training data- 0.7659

## out-of-sample
pcut <- 1/6
credit_rf_pred_test <- predict(credit_rf, newdata=credit_test, type = "prob")[,2]
credit_rf_class_test <- (credit_rf_pred_test>pcut)*1
table(credit_test$default, credit_rf_class_test, dnn = c("True", "Pred"))
pred <- prediction(credit_rf_class_test, credit_test$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#Get the AUC
unlist(slot(performance(pred, "auc"), "y.values"))
# AUC testing data with 1/6 as cut off probability- 0.6637

#### Boosting ####

library(adabag)
credit_train$default= as.factor(credit_train$default)
credit_boost= boosting(default~., data = credit_train, boos = T)

# Training AUC
pred_credit_boost= predict(credit_boost, newdata = credit_train)
pred <- prediction(pred_credit_boost$prob[,2], credit_train$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#Get the AUC
unlist(slot(performance(pred, "auc"), "y.values"))
# AUC in-sample= 0.7932

pred_credit_boost= predict(credit_boost, newdata = credit_test)
# Testing AUC
pred <- prediction(pred_credit_boost$prob[,2], credit_test$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#Get the AUC
unlist(slot(performance(pred, "auc"), "y.values"))
# AUC out-of-sample= 0.7749

#### Generalized Additive Model ####

## Create a formula for a model with a large number of variables:
gam_formula <- as.formula("default~s(LIMIT_BAL)+s(AGE)+s(PAY_0)+s(BILL_AMT1)+s(PAY_AMT1)+SEX+EDUCATION+MARRIAGE")

credit_gam <- gam(formula = gam_formula, family=binomial, data=credit_train);
summary(credit_gam)

plot(credit_gam, shade=TRUE, seWithMean=TRUE, scale=0, pages = 1)

# vis.gam(credit_gam)
# vis.gam(credit_gam, view=c("LIMIT_BAL","AGE"), theta= 140) # different view 

# In-sample fit performance
pcut_gam <- 1/6
prob_gam_in <-predict(credit_gam,credit_train,type="response")
pred_gam_in <- (prob_gam_in>=pcut_gam)*1
table(credit_train$default, pred_gam_in,dnn=c("Observed","Predicted"))

creditcost <- function(r, pi){
  weight1 = 5
  weight0 = 1
  pcut <- weight0/(weight0+weight1)
  c1 = (r==1)&(pi<pcut) #logical vector - true if actual 1 but predict 0
  c0 = (r==0)&(pi>pcut) #logical vector - true if actual 0 but predict 1
  return(mean(weight1*c1+weight0*c0))
}
creditcost(credit_train$default, pred_gam_in)
# Asymmetric cost- 0.5836

# Model AIC, BIC & Mean Residual Deviance
AIC(credit_gam)
BIC(credit_gam)
credit_gam$deviance

# ROC curve 
library(ROCR)
pred <- prediction(predictions = c(prob_gam_in), labels = credit_train$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#Get the AUC
unlist(slot(performance(pred, "auc"), "y.values"))
# AUC in-sample- 0.7625

# Out-of-sample fit performance
pcut <- 1/6
prob_gam_out <- predict(credit_gam, credit_test,type="response")
pred_gam_out <- (prob_gam_out>=pcut)*1
table(credit_test$default, pred_gam_out,dnn=c("Observed","Predicted"))
creditcost(credit_test$default, pred_gam_out)
# Asymmetric cost out-of-sample- 0.6095

# Roc curve
pred <- prediction(predictions = c(prob_gam_out), labels = credit_test$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#Get the AUC
unlist(slot(performance(pred, "auc"), "y.values"))
# AUC out-of sample- 0.7528

