require(cvTools) 
require(class)
require(caret)

dataset <- read.csv('train_data.csv',header = TRUE , sep = ",")

dataset$OutcomeType <- as.numeric(dataset$OutcomeType)
dataset$AnimalType <- as.numeric(dataset$AnimalType)
dataset$Sex <- as.numeric(dataset$Sex) 
dataset$Intact <- as.numeric(dataset$Intact) 
dataset$DayTime <- as.numeric(dataset$DayTime)
dataset$SimBreed <- as.numeric(dataset$SimBreed)
dataset$SimColor <- as.numeric(dataset$SimColor)
dataset$HasName <- as.numeric(dataset$HasName)
dataset$Agetype <- as.numeric(dataset$Agetype)


k <- 10 #the number of folds
folds <- cvFolds(NROW(dataset), K=k)
dataset$holdoutpred <- rep(0,nrow(dataset))

for(i in 1:k)
{
  train <- dataset[folds$subsets[folds$which != i], ] #Set the training set
  validation <- dataset[folds$subsets[folds$which == i], ] #Set the validation set
  cl<-train$OutcomeType
  newpred <- knn(train, validation,k=10, cl, prob=TRUE)
  dataset[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for later use
  
}

mean(ifelse(as.numeric(dataset$OutcomeType)==dataset$holdoutpred,1,0))

#dataset$holdoutpred #do whatever you want with these predictions

