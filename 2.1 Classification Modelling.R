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


#ONE: CLASSIFICATION TREE
  #1.Base Model
tree_base <- rpart(Attrition ~ ., data=DataTrain, method="class", 
                  control=rpart.control(cp=0))
summary(tree_base)
rpart.plot(tree_base, extra=106)
printcp(tree_base)
plotcp(tree_base)

    #A.Training Accuracy
traintree_base <- predict(tree_base, DataTrain, type = "class")
traintable_base <- table(traintree_base,DataTrain$Attrition)
traintable_base

      #Accuracy No = 0.9667812
844/(844+29)
      #Accuracy Yes = 0.5092025
83/(83+80)
      #Accuracy = 0.8947876
(844+83)/nrow(DataTrain)

    #B.Testing Accuracy
testtree_base <- predict(tree_base, DataTest, type = "class")
testtable_base <- table(testtree_base,DataTest$Attrition)
testtable_base
      #Accuracy No = 0.9620853
203/(203+8)
      #Accuracy Yes = 0.25
12/(12+36)
      #Accuracy = 0.8301158
(203+12)/nrow(DataTest)

    #Predict 30 People
predicttree_base <- predict(tree_base, DataPredict, type = "class")
predicttree_base

  #2.Pre-pruning
tree_preprun <- rpart(Attrition ~ ., data = DataTrain, method = "class", 
                          control=rpart.control(cp=0, maxdepth=5,minsplit=6,minbucket=3))
rpart.plot(tree_preprun)
summary(tree_preprun)
printcp(tree_preprun)
plotcp(tree_preprun)

    #A.Training Accuracy
traintree_preprun <- predict(tree_preprun, DataTrain, type = "class")
traintable_preprun <- table(traintree_preprun,DataTrain$Attrition)
traintable_preprun
      #Accuracy No = 0.9713631
848/(848+25)
      #Accuracy Yes = 0.5460123
89/(89+74)
      #Accuracy = 0.9044402
(848+89)/nrow(DataTrain)

    #B.Testing Accuracy
testtree_preprun <- predict(tree_preprun, DataTest, type = "class")
testtable_preprun <- table(testtree_preprun,DataTest$Attrition)
testtable_preprun
      #Accuracy No = 0.9478673
200/(200+11)
      #Accuracy Yes = 0.2708333
13/(13+35)
      #Accuracy = 0.8223938
(200+13)/nrow(DataTest)

    #Predict 30 People
predicttree_base <- predict(tree_preprun, DataPredict, type = "class")
predicttree_base

  #3.Post-pruning
tree_postprun <- prune(tree_base, cp = 0.0092)
rpart.plot(tree_postprun)

    #A.Training Accuracy
traintree_postprun <- predict(tree_postprun, DataTrain, type = "class")
traintable_postprun <- table(traintree_postprun,DataTrain$Attrition)
traintable_postprun
      #Accuracy No = 0.9770905
853/(853+20)
      #Accuracy Yes = 0.4355828
71/(71+92)
      #Accuracy = 0.8918919
(853+71)/nrow(DataTrain)

    #B.Testing Accuracy
testtree_postprun <- predict(tree_postprun, DataTest, type = "class")
testtable_postprun <- table(testtree_postprun,DataTest$Attrition)
testtable_postprun
    #Accuracy No = 0.9668246
204/(204+7)
    #Accuracy Yes = 0.2291667
11/(11+37)
    #Accuracy = 0.8301158
(204+11)/nrow(DataTest)

    #Predict 30 People
predicttree_base <- predict(tree_postprun, DataPredict, type = "class")
predicttree_base

  #4.Pre+Post-pruning
tree_prepostprun <- rpart(Attrition ~ ., data = DataTrain, method = "class", 
                      control=rpart.control(cp=0.0092, maxdepth=5,minsplit=6,minbucket=3))
rpart.plot(tree_prepostprun)

    #A.Training Accuracy
traintree_prepostprun <- predict(tree_prepostprun, DataTrain, type = "class")
traintable_prepostprun <- table(traintree_prepostprun,DataTrain$Attrition)
traintable_prepostprun
      #Accuracy No = 0.9621993
840/(840+33)
      #Accuracy Yes = 0.5705521
93/(93+70)
      #Accuracy = 0.9005792
(840+93)/nrow(DataTrain)

    #B.Testing Accuracy
testtree_prepostprun <- predict(tree_prepostprun, DataTest, type = "class")
testtable_prepostprun <- table(testtree_prepostprun,DataTest$Attrition)
testtable_prepostprun
      #Accuracy No = 0.9383886
198/(198+13)
      #Accuracy Yes = 0.2708333
13/(13+35)
      #Accuracy = 0.8146718
(198+13)/nrow(DataTest)

    #Predict 30 People
predicttree_base <- predict(tree_prepostprun, DataPredict, type = "class")
predicttree_base
PredictWillLeave <- Dataset0 %>% filter(EmployeeNumber=="2080")
glimpse(PredictWillLeave)


#5. More Tuning
modelLookup('rpart')
model_rpart <- train(Attrition ~., data=DataTrain, method='rpart')
model_rpart