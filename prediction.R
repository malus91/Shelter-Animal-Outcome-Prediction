require(cvTools) 
require(class)
require(caret)

dataset <- read.csv('train_data.csv',header = TRUE , sep = ",")
test <- read.csv('test_data.csv')
dataset$OutcomeType <- as.factor(dataset$OutcomeType) #Car
dataset$AnimalType <- as.numeric(dataset$AnimalType) #Car
dataset$Sex <- as.numeric(dataset$Sex) #Car
dataset$Intact <- as.numeric(dataset$Intact) #Car
dataset$DayTime <- as.numeric(dataset$DayTime) #Car
dataset$SimBreed <- as.numeric(dataset$SimBreed) #Car
dataset$SimColor <- as.numeric(dataset$SimColor) #Car
dataset$HasName <- as.numeric(dataset$HasName) #Abalone
dataset$Agetype <- as.numeric(dataset$Agetype)


test$AnimalType <- as.numeric(test$AnimalType) #Car
test$Sex <- as.numeric(test$Sex) #Car
test$Intact <- as.numeric(test$Intact) #Car
test$DayTime <- as.numeric(test$DayTime) #Car
test$SimBreed <- as.numeric(test$SimBreed) #Car
test$SimColor <- as.numeric(test$SimColor) #Car
test$HasName <- as.numeric(test$HasName) #Abalone
test$Agetype <- as.numeric(test$Agetype)


train <- dataset #Set the training set
validation <- test #Set the validation set

cl<-train$OutcomeType
newpred <- knn(train[,1:8], validation,k=10, cl, prob=TRUE)
require(xlsx)
write.xlsx(newpred,"c:/Bigdata/predictions.xlsx")
write.xlsx(attributes(newpred)$prob,"C:/Bigdata/proportions.xlsx")

