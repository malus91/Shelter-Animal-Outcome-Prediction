#Animal Shelter Problem
#install.packages("randomForest")
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes) # visualization
#install.packages("dplyr")
library(dplyr) # data manipulation
#install.packages("lubridate")
library(lubridate) # dates
library(randomForest)
library(rpart)
#install.packages("rattle")
library(rattle)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(caret)
#install.packages("e1071")
library("e1071")
library(cvTools)
#install.packages("xlsx")
library(xlsx)


setwd("D:/UTD_courses/MachineLearning/Project/data")
train_Data<-read.csv("train_data.csv",header=TRUE,stringsAsFactors = T)
test_Data<-read.csv("test_data.csv",header=TRUE,stringsAsFactors = T)

dataset<-train_Data

dataset$AnimalType <- as.numeric(dataset$AnimalType) #Car
dataset$Sex <- as.numeric(dataset$Sex) #Car
dataset$Intact <- as.numeric(dataset$Intact) #Car
dataset$DayTime <- as.numeric(dataset$DayTime) #Car
dataset$SimBreed <- as.numeric(dataset$SimBreed) #Car
dataset$SimColor <- as.numeric(dataset$SimColor) #Car
dataset$HasName <- as.numeric(dataset$HasName) #Abalone
dataset$Agetype <- as.numeric(dataset$Agetype)

##Accuracy calculation using manual cross validation
d<-dataset
for(i in 1:10) {
  cat("Running sample ",i,"\n")
  sampleInstances<-sample(1:nrow(d),size = 0.9*nrow(d))
  trainingData<-d[sampleInstances,]
  testData<-d[-sampleInstances,]
  Classvar<-trainingData[,9]
  actual<-testData[,9]
  
  ran1<-randomForest(OutcomeType ~.,trainingData,ntree= 600,importance=TRUE)
  pred<-predict(ran1,testData,type=c("response") )
  print("random forest")
  accuracy<-sum((actual==pred))/length(pred)
  method<-"Random Forest"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
  svm_radial <- svm(OutcomeType ~.,trainingData,type = "C",kernel="radial")
  pred_SVMRadial<-predict(svm_radial,testData,type=c("class"))
  print("\n SVM Radial")
  accuracy<-sum((actual==pred_SVMRadial))/length(pred_SVMRadial)
  method<-"SVM Radial"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
  
  svm_linear <- svm(OutcomeType ~.,data=trainingData,type = "C",kernel="linear")
  pred_SVMLinear<-predict(svm_linear,testData,type=c("class"))
  cat("\n SVM linear")
  accuracy<-sum((actual==pred_SVMLinear))/length(pred_SVMLinear)
  method<-"SVM Linear"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
}



#Predictions using Caret package
train_control <- trainControl(method="cv", number=10,allowParallel = TRUE)

#SVM Linear 
svm_mod<-train(OutcomeType ~.,data=dataset,method = "svmLinear",trControl = train_control)
print(svm_mod)

#SVM Radial
svm_Rad<-train(OutcomeType ~ .,data=dataset,method = "svmRadial",trControl = train_control)
print(svm_Rad)

#Random Forest
rf_mod<-train(OutcomeType ~.,data=dataset,method = "rf",trControl = train_control)
print(rf_mod)

#LDA
lda_mod<-train(OutcomeType ~.,data=dataset,method = "lda",trControl = train_control)
print(lda_mod)


#SamPle Output
#[1] "random forest"
#Method =  Random Forest , accuracy=  0.650954 
#[1] "\n SVM Radial"
#Method =  SVM Radial , accuracy=  0.6546951 

#SVM linearMethod =  SVM Linear , accuracy=  0.567153 
#Running sample  2 
#[1] "random forest"
#Method =  Random Forest , accuracy=  0.6154134 
#[1] "\n SVM Radial"
#Method =  SVM Radial , accuracy=  0.6213992 

#SVM linearMethod =  SVM Linear , accuracy=  0.5540591 







