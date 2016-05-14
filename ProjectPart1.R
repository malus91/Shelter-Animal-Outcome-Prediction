library(randomForest)
library(ipred)
library(plyr)
library(e1071)
library(gbm)
library(survival)
library(adabag)
library(mlbench)
library(caret)
library(doMC)
registerDoMC(5)
library(h2o)

setwd("/Users/Josina/Documents/ML/Final Project/")

train_Data<-read.csv("train_data.csv",header=TRUE)
test_Data<-read.csv("test_data.csv",header=TRUE)

#Boosting and Bagging - cross validation
train_control <- trainControl(method="cv", number=3,allowParallel = TRUE)
#bagging
model_bag <- train(OutcomeType~AnimalType+SimBreed+SimColor+Intact+Agetype+HasName+DayTime, data=train_Data, trControl=train_control, method="treebag")
print(max(model_bag$results$Accuracy))

model_abag <- train(OutcomeType~AnimalType+SimBreed+SimColor+Intact+Agetype+HasName+DayTime, data=train_Data, trControl=train_control, method="bagEarth")
print(max(model_abag$results$Accuracy))

#boosting
#logitboost
model_grad <- train(OutcomeType~AnimalType+SimBreed+SimColor+Intact+Agetype+HasName+DayTime, data=train_Data, method="LogitBoost", trControl=train_control)
print(max(model_grad$results$Accuracy))

#Gradient boost
model_grad <- train(OutcomeType~AnimalType+SimBreed+SimColor+Intact+Agetype+HasName+DayTime , data=train_Data, method="gbm", trControl=train_control)
print(max(model_grad$results$Accuracy))

#Adaboost
Grid <- expand.grid(maxdepth=25,mfinal=10, coeflearn="Breiman")
model_ada <- train(OutcomeType~AnimalType+SimBreed+SimColor+Intact+Agetype+HasName+DayTime, data=train_Data, method = "AdaBoost.M1", trControl = train_control)
print(model_ada$results$Accuracy)

#h2o boositng
h2o.init()

data.hex <- as.h2o(train_Data)

splits <- h2o.splitFrame(
  data.hex,           ##  splitting the H2O frame we read above
  c(0.8,0.1),   ##  create splits of 60% and 20%; 
  seed=1234)    ##  setting a seed will ensure reproducible results (not R's seed)

train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex") 

mod_grad <- h2o.gbm(x = c("AnimalType","Sex","Intact","DayTime","SimBreed","SimColor","HasName","Agetype"),
                    y = "OutcomeType",
                    training_frame =  train,
                    validation_frame = valid,  
                    ntrees = 50, max_depth = 20)

finalRf_predictions<-h2o.predict(
  object = mod_grad
  ,newdata = test)

mean(finalRf_predictions$predict==test$OutcomeType)

#h2o random forest

rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x = c("AnimalType","Sex","Intact","DayTime","SimBreed","SimColor","HasName","Agetype"),
  y = "OutcomeType",                         ## the target index (what we are predicting)
  ntrees = 200,                  ## use a maximum of 200 trees to create the
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  score_each_iteration = T,      ## Predict against training and validation for
  seed = 1000000)                ## Set the random seed so that this can be

final_predictions<-h2o.predict(
  object = rf1
  ,newdata = test)

mean(final_predictions$predict==test$OutcomeType)

#deeplearning

m1 <- h2o.deeplearning( 
  training_frame=train, 
  validation_frame=valid,   ## validation dataset: used for scoring and early stopping
  x = c("AnimalType","Sex","Intact","DayTime","SimBreed","SimColor","HasName","Agetype"),
  y = "OutcomeType", 
  epochs=1,
  variable_importances=T    ## not enabled by default
)

final_prediction<-h2o.predict(
  object = m1
  ,newdata = test)

mean(final_prediction$predict==test$OutcomeType)
