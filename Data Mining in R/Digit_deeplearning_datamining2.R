
library(keras)
library(dplyr)
mnist <- dataset_mnist()
# ?dataset_mnist
set.seed(15214626)
x_train<-mnist$train$x
g_train<-mnist$train$y
x_test<-mnist$test$x
g_test<-mnist$test$y

dim(x_train)
dim(x_test)

x_train <- array_reshape(x_train,c(nrow(x_train),784))
x_test <- array_reshape(x_test,c(nrow(x_test),784))
y_train <- to_categorical(g_train,10)
y_test <- to_categorical(g_test,10)

# scale to max=1
x_train<-x_train/255
x_test<-x_test/255

# Split data into training and validation sets
# set.seed(15214626)
# train_idx <- sample.int(nrow(x_train), size = floor(0.9 * nrow(x_train)), replace = FALSE)
# x_train_new <- x_train[train_idx, ]
# g_train_new <- g_train[train_idx]
# y_train_new <- to_categorical(g_train_new,10)
modelnn<-keras_model_sequential()
# ?keras_model_sequential
# See Lecture 11C, neural network diagram with multiple layer #
modelnn%>%layer_dense(units = 256, activation = "relu",input_shape=c(784))%>%
  layer_dropout(rate = 0.4)%>%
  layer_dense(units = 128, activation="relu")%>%
  layer_dropout(rate = 0.3)%>%
  layer_dense(units = 10, activation = "softmax")

summary(modelnn)

## Q: How many parameters are involved? ##
(13+1)*5+(5+1)*3+4

modelnn%>%compile(loss="categorical_crossentropy",
                  optimizer=optimizer_rmsprop(),metrics=c("accuracy"))

system.time(history<-modelnn%>%fit(x_train,y_train,epochs=30,batch_size=128,validation_split=0.1))
plot(history,smooth = FALSE)

# accuracy = 1-test error=1-misclassification rate, symmetric loss #
accuracy<-function(pred,truth){
  mean(drop(pred)==drop(truth))
}
accuracy1 <- modelnn%>%predict(x_test)%>% apply(1, function(x) which.max(x)-1) %>% accuracy(g_test)
test_error <- 1-accuracy1
test_error

modellr<-keras_model_sequential()%>%
  layer_dense(input_shape=784, units=10,activation = "softmax")
summary(modellr)

modellr%>%compile(loss="categorical_crossentropy",
                  optimizer=optimizer_rmsprop(),metrics=c("accuracy"))
modellr%>%fit(x_train,y_train,epochs=30,batch_size=128,validation_split=0.2)

accuracy2 <- modellr%>%predict(x_test)%>% apply(1, function(x) which.max(x)-1) %>% accuracy(g_test)
test_error1 <- 1-accuracy2
test_error1


