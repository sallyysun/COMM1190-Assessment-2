#Libraries
library(readxl)
library(rpart)
library(rpart.plot)
library(dplyr)

#Read Data
Dataset <- read_excel("D:/UNSW/1st Year/T2/COMM1190/Assignment 2/A2_Dataset_V2.xlsx")

#Split Data
set.seed(1) 
train <- sample(nrow(Dataset), nrow(Dataset)*0.7)
DataTrain <- Dataset[train, ]
DataTest <- Dataset[-train, ]
dim(DataTrain)
dim(DataTest)

#ONE: CLASSIFICATION TREE
  #1.Base Model
tree_base <- rpart(Attrition ~ ., data=DataTrain, method="class", 
                  control=rpart.control(cp=0))
summary(tree_base)
rpart.plot(tree_base, extra=106)
printcp(tree_base)
plotcp(tree_base)

    #A.Training Accuracy
traintable_base <- table(DataTrain,DataTrain$Attrition)
traintable_base
      #Accuracy No = 0.976593
751/(751+18)
      #Accuracy Yes = 0.4928571
69/(71+69)
      #Accuracy = 0.8845739
(751+69)/927

    #B.Testing Accuracy
testtree_base <- predict(tree_base, DataTest, type = "class")
testtable_base <- table(testtree_base,DataTest$Attrition)
testtable_base
      #Accuracy No = 0.9142857
288/(288+27)
      #Accuracy Yes = 0.2676056
19/(52+19)
      #Accuracy = 0.7713568
(288+19)/398

  #2.Pre-pruning
tree_preprun <- rpart(Attrition ~ ., data = DataTrain, method = "class", 
                          control=rpart.control(cp=0, maxdepth=5,minsplit=7,minbucket=3))
summary(tree_preprun)
rpart.plot(tree_preprun)
printcp(tree_preprun)
plotcp(tree_preprun)

    #A.Training Accuracy
traintree_preprun <- predict(tree_preprun, DataTrain, type = "class")
traintable_preprun <- table(traintree_preprun,DataTrain$Attrition)
traintable_preprun
      #Accuracy No = 0.9505852
731/(731+38)
      #Accuracy Yes = 0.5571429
78/(78+62)
      #Accuracy = 0.8727077
(731+78)/927

    #B.Testing Accuracy
testtree_preprun <- predict(tree_preprun, DataTest, type = "class")
testtable_preprun <- table(testtree_preprun,DataTest$Attrition)
testtable_preprun
      #Accuracy No = 0.9206349
290/(290+25)
      #Accuracy Yes = 0.3098592
22/(49+22)
      #Accuracy = 0.7839196
(290+22)/398

  #3.Post-pruning
tree_postprun <- prune(tree_base, cp = 0.0143)
rpart.plot(tree_postprun)

    #A.Training Accuracy
traintree_postprun <- predict(tree_postprun, DataTrain, type = "class")
traintable_postprun <- table(traintree_postprun,DataTrain$Attrition)
traintable_postprun
      #Accuracy No = 0.9895969
761/(761+8)
      #Accuracy Yes = 0.3714286
52/(52+88)
      #Accuracy = 0.8770227
(761+52)/927

    #B.Testing Accuracy
testtree_postprun <- predict(tree_postprun, DataTest, type = "class")
testtable_postprun <- table(testtree_postprun,DataTest$Attrition)
testtable_postprun
    #Accuracy No = 0.9777778
308/(308+7)
    #Accuracy Yes = 0.2112676
15/(15+56)
    #Accuracy = 0.8115578
(308+15)/398

  #4.Pre+Post-pruning
tree_prepostprun <- rpart(Attrition ~ ., data = DataTrain, method = "class", 
                      control=rpart.control(cp=0.0143, maxdepth=5,minsplit=7,minbucket=3))
rpart.plot(tree_prepostprun)

    #A.Training Accuracy
traintree_prepostprun <- predict(tree_prepostprun, DataTrain, type = "class")
traintable_prepostprun <- table(traintree_prepostprun,DataTrain$Attrition)
traintable_prepostprun
      #Accuracy No = 0.9622887
740/(740+29)
      #Accuracy Yes = 0.4285714
60/(60+80)
      #Accuracy = 0.8629989
(740+60)/927

    #B.Testing Accuracy
testtree_prepostprun <- predict(tree_prepostprun, DataTest, type = "class")
testtable_prepostprun <- table(testtree_prepostprun,DataTest$Attrition)
testtable_prepostprun
      #Accuracy No = 0.9619048
303/(303+12)
      #Accuracy Yes = 0.2676056
19/(19+52)
      #Accuracy = 0.8090452
(303+19)/398
