#Libraries
library(readxl)
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)

#Read Data
Dataset0 <- read_excel("D:/UNSW/1st Year/T2/COMM1190/Assignment 2/A2_Dataset_V2.xlsx")
Dataset <- na.omit(Dataset0)
DataPredict <- Dataset0 %>% filter(is.na(Attrition))

#Split Data
set.seed(1) 
train <- sample(nrow(Dataset), nrow(Dataset)*0.8)
DataTrain <- Dataset[train, ]
DataTest <- Dataset[-train, ]

#3.Post-pruning YAY!!
  #Tune Length
tree_postprunCP <- train(
  Attrition ~., data = DataTrain, method = "rpart",
  tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)))

ggplot(tree_postprunCP)

tree_postprun <- prune(tree_base, cp = 0.01635992)
rpart.plot(tree_postprun)

#A.Training Accuracy
traintree_postprun <- predict(tree_postprun, DataTrain, type = "class")
traintable_postprun <- table(traintree_postprun,DataTrain$Attrition)
traintable_postprun
#Accuracy No = 0.9770905
840/(840+33)
#Accuracy Yes = 0.4355828
71/(71+92)
#Accuracy = 0.8918919
(840+71)/nrow(DataTrain)

#B.Testing Accuracy
testtree_postprun <- predict(tree_postprun, DataTest, type = "class")
testtable_postprun <- table(testtree_postprun,DataTest$Attrition)
testtable_postprun
#Accuracy No = 0.9668246
199/(199+12)
#Accuracy Yes = 0.2291667
9/(9+39)
#Accuracy = 0.8301158
(199+9)/nrow(DataTest)

#Predict 30 People
predicttree_base <- predict(tree_postprun, DataPredict, type = "class")
predicttree_base




