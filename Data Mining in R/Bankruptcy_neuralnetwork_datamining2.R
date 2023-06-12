# Bankruptcy Dataset

Bank_data_scaled <- Bank_data <- 
  read.csv(file = "https://xiaoruizhu.github.io/Data-Mining-R/lecture/data/bankruptcy.csv", header=T)
# summary(Bank_data)
library(MASS)
maxs <- apply(Bank_data[,-c(1:3)], 2, max)
mins <- apply(Bank_data[,-c(1:3)], 2, min)
Bank_data_scaled[,-c(1:3)] <- as.data.frame(scale(Bank_data[,-c(1:3)], center = mins, scale = maxs - mins))

sample_index <- sample(nrow(Bank_data_scaled),nrow(Bank_data_scaled)*0.70)
Bank_train <- Bank_data_scaled[sample_index,]
Bank_test <- Bank_data_scaled[-sample_index,]

# Fitting a neural network model
library(neuralnet)
f <- as.formula("DLRSN ~ R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8 + R9 + R10")
# You may need to specific the formula rather than 
Bank_nn <- neuralnet(f, data=Bank_train, hidden=c(3), algorithm = 'rprop+', linear.output=F, likelihood = T)
plot(Bank_nn)

# In- sample fit performance
pcut_nn <- 1/36
prob_nn_in <- predict(Bank_nn, Bank_train, type="response")
pred_nn_in <- (prob_nn_in>=pcut_nn)*1
table(Bank_train$DLRSN, pred_nn_in, dnn=c("Observed","Predicted"))

# detach(package:neuralnet) ## After fitting the model detach neural network
# ROC curve
library(ROCR)
pred <- prediction(prob_nn_in, Bank_train$DLRSN)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#Get the AUC
unlist(slot(performance(pred, "auc"), "y.values"))
# AUC in-sample- 0.8922

# Model AIC/ BIC
Bank_nn$result.matrix[4:5,]
# AIC- 357.2798, BIC- 588.3104

# Out-of-sample performance
prob_nn_out <- predict(Bank_nn, Bank_test, type="response")
pred_nn_out <- (prob_nn_out>=pcut_nn)*1
table(Bank_test$DLRSN, pred_nn_out, dnn=c("Observed","Predicted"))

# ROC curve
pred <- prediction(prob_nn_out, Bank_test$DLRSN)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

#Get the AUC
unlist(slot(performance(pred, "auc"), "y.values"))
# AUC out-of sample- 0.8834