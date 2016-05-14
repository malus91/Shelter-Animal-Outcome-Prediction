#Preprocessing the data
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes) # visualization
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("lubridate")
library(lubridate) # dates
#install.packages("dplyr")
library(dplyr) # data manipulation

#Load Data
train_Data<-read.csv("/Users/Josina/Documents/ML/Final Project/train",header=TRUE,stringsAsFactors = F)
test_Data<-read.csv("/Users/Josina/Documents/ML/Final Project/test",header=TRUE,stringsAsFactors = F)

#Preprocessing on train data
#split sex upon outcome into intact and sex
train_Data$Sex <- ifelse(grepl('Male', train_Data$SexuponOutcome), 'Male',
                         ifelse(grepl('Unknown', train_Data$Sex), 'Unknown', 'Female'))

train_Data$Intact <- ifelse(grepl('Intact', train_Data$SexuponOutcome), 'Intact',
                            ifelse(grepl('Neutered', train_Data$SexuponOutcome), 'Neutered', 
                                   ifelse(grepl('Spayed',train_Data$SexuponOutcome), 'Spayed', 'Unknown')))

#convert age in terms of days

# Get the time value
Number <- sapply(train_Data$AgeuponOutcome,  function(x) strsplit(x, split = ' ')[[1]][1])

# Get the unit of time
UnitofTime <- sapply(train_Data$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][2])

#calculate the days
train_Data$AgeDays <- as.numeric(Number) * (ifelse(grepl('day', UnitofTime), 1,
                                                   ifelse(grepl('week', UnitofTime), 7,
                                                          ifelse(grepl('month', UnitofTime), 30,
                                                                 ifelse(grepl('year', UnitofTime), 365, NA)))))

#Handle the date and time

train_Data$Hour<-hour(train_Data$DateTime)
train_Data$WeekDay<-week(train_Data$DateTime)
train_Data$Month<-month(train_Data$DateTime)
train_Data$Year<-year(train_Data$DateTime)

train_Data$DayTime<-ifelse(train_Data$Hour >5&train_Data$Hour<12,'morning',
                           ifelse(train_Data$Hour>=12&train_Data$Hour<16,'afternoon',
                                  ifelse(train_Data$Hour>=16& train_Data$Hour<20,'evening','night')))
#get Simplebreed
train_Data$SimBreed<-sapply(train_Data$Breed,function(x) strsplit(x,split = '/')[[1]][1])

#get SimpleColor
train_Data$SimColor <- sapply(train_Data$Color,function(x) strsplit(x,split = '/')[[1]][1])

# Replace blank names with "Nameless"
train_Data$Name <- ifelse(nchar(train_Data$Name)==0, 'Nameless', train_Data$Name)

# Make a name v. no name variable
train_Data$HasName[train_Data$Name == 'Nameless'] <- 0
train_Data$HasName[train_Data$Name != 'Nameless'] <- 1
train_Data$AgeType[train_Data$AgeDays < 365 & train_Data$AgeDays > 0] <- 'baby'
train_Data$AgeType[train_Data$AgeDays >= 365] <- 'adult'
train_Data$AgeType[train_Data$AgeDays == 0] <- 'Unknown'
