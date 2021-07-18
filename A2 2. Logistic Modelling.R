#Libraries
library(readxl)
library(aod)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)

#Read Data
Dataset0 <- read_excel("D:/UNSW/1st Year/T2/COMM1190/Assignment 2/A2_Dataset_V2.xlsx")

#Cleaning Data
Dataset0$Attrition <- ifelse(Dataset0$Attrition == "Yes", 1, 0) 
Dataset0$Attrition <- as.factor(Dataset0$Attrition)
Dataset0$OverTime <- ifelse(Dataset0$OverTime == "Yes", 1, 0) 
Dataset0$Gender <- ifelse(Dataset0$Gender == "Male", 1, 0) 
Dataset0$MaritalStatus <- ifelse(Dataset0$MaritalStatus == "Married", 1, 0) 
glimpse(Dataset)

#Choosing Data
Dataset <- na.omit(Dataset0)
DataPredict <- Dataset0 %>% filter(is.na(Attrition))

#Split Data
set.seed(1) 
train <- sample(nrow(Dataset), nrow(Dataset)*0.8)
DataTrain <- Dataset[train, ]
DataTest <- Dataset[-train, ]

#TWO: LOGISTIC REGRESSION
#1. All
DataTrain_All <- glm(Attrition ~ ., data = DataTrain, family="binomial")
summary(DataTrain_All)

  #Training Accuracy
trainpredict_all <- predict(DataTrain_All, newdata = DataTrain, type = "response") 
predict_all <- rep("No", nrow(DataTrain))
predict_all[trainpredict_all > 0.5] <- "Yes" 
table(predict_all, DataTrain$Attrition)
    #Accuracy No = 0.9747995
851/(851+22)
    #Accuracy Yes = 0.4601227
75/(75+88)
    #Accuracy = 0.8938224
(851+75)/nrow(DataTrain)

  #Testing Accuracy
testpredict_all <- predict(DataTrain_All, newdata = DataTest, type = "response") 
predict_all1 <- rep("No", nrow(DataTest))
predict_all1[testpredict_all > 0.5] <- "Yes" 
table(predict_all1, DataTest$Attrition)

    #Accuracy No = 0.9383886
198/(198+13)
    #Accuracy Yes = 0.4583333
22/(22+26)
    #Accuracy = 0.8494208
(198+22)/nrow(DataTest)

  #Predict 30 People
predictpredict_all <- predict(DataTrain_All, newdata = DataPredict, type = "response") 
predict_all2 <- rep("No", nrow(DataPredict))
predict_all2[predictpredict_all > 0.5] <- "Yes" 
predict_all2


#2. Filter 1
DataTrain_1 <- glm(Attrition ~ .
                   -Education-MaritalStatus-SalaryIncrease-HighPerformance-st1, 
                   data = DataTrain, family="binomial")
summary(DataTrain_1)

  #Training Accuracy
trainpredict_1 <- predict(DataTrain_1, newdata = DataTrain, type = "response") 
predict_1 <- rep("No", nrow(DataTrain))
predict_1[trainpredict_1 > 0.5] <- "Yes" 
table(predict_1, DataTrain$Attrition)
    #Accuracy No = 0.9713631
848/(848+25)
    #Accuracy Yes = 0.4355828
71/(71+92)
    #Accuracy = 0.8870656
(848+71)/nrow(DataTrain)

  #Testing Accuracy
testpredict_1 <- predict(DataTrain_1, newdata = DataTest, type = "response") 
predict_11 <- rep("No", nrow(DataTest))
predict_11[testpredict_1 > 0.5] <- "Yes" 
table(predict_11, DataTest$Attrition)

    #Accuracy No = 0.9478673
200/(200+11)
    #Accuracy Yes = 0.4375
21/(21+27)
    #Accuracy = 0.8532819
(200+21)/nrow(DataTest)

  #Predict 30 People
predictpredict_1 <- predict(DataTrain_1, newdata = DataPredict, type = "response") 
predict_12 <- rep("No", nrow(DataPredict))
predict_12[predictpredict_1 > 0.5] <- "Yes" 
predict_12

#3. Filter 2
DataTrain_2 <- glm(Attrition ~ .
                   -Education-MaritalStatus-SalaryIncrease-HighPerformance-st1
                   -Department-EmployeeNumber-Gender-MonthlyIncome-TotalWorkingYears-WorkLifeBalance, 
                   data = DataTrain, family="binomial")
summary(DataTrain_2)
confint(DataTrain_2)

  #Training Accuracy
trainpredict_2 <- predict(DataTrain_2, newdata = DataTrain, type = "response") 
predict_2 <- rep("No", nrow(DataTrain))
predict_2[trainpredict_2 > 0.5] <- "Yes" 
table(predict_2, DataTrain$Attrition)
    #Accuracy No = 0.9690722
846/(846+27)
    #Accuracy Yes = 0.4110429
67/(67+96)
    #Accuracy = 0.8812741
(846+67)/nrow(DataTrain)

  #Testing Accuracy
testpredict_2 <- predict(DataTrain_2, newdata = DataTest, type = "response") 
predict_21 <- rep("No", nrow(DataTest))
predict_21[testpredict_2 > 0.5] <- "Yes" 
table(predict_21, DataTest$Attrition)

    #Accuracy No = 0.9478673
200/(200+11)
    #Accuracy Yes = 0.4375
21/(21+27)
    #Accuracy = 0.8532819
(200+21)/nrow(DataTest)

  #Predict 30 People
predictpredict_2 <- predict(DataTrain_2, newdata = DataPredict, type = "response") 
predict_22 <- rep("No", nrow(DataPredict))
predict_22[predictpredict_2 > 0.5] <- "Yes" 
predict_22

#4. Filter 3
DataTrain_3 <- train(Attrition ~., data=DataTrain, method='glm',
                    tuneGrid=expand.grid(parameter=c(0.001, 0.01, 0.1, 1,10,100, 1000)))
summary(DataTrain_3)

  #Training Accuracy
trainpredict_3 <- predict(DataTrain_3, newdata = DataTrain, type = "response") 
predict_3 <- rep("No", nrow(DataTrain))
predict_3[trainpredict_3 > 0.5] <- "Yes" 
table(predict_3, DataTrain$Attrition)
    #Accuracy No = 0.9690722
846/(846+27)
    #Accuracy Yes = 0.4110429
67/(67+96)
    #Accuracy = 0.8812741
(846+67)/nrow(DataTrain)

  #Testing Accuracy
testpredict_3 <- predict(DataTrain_3, newdata = DataTest, type = "response") 
predict_31 <- rep("No", nrow(DataTest))
predict_31[testpredict_3 > 0.5] <- "Yes" 
table(predict_31, DataTest$Attrition)

    #Accuracy No = 0.9478673
200/(200+11)
    #Accuracy Yes = 0.4375
21/(21+27)
    #Accuracy = 0.8532819
(200+21)/nrow(DataTest)

  #Predict 30 People
predictpredict_3 <- predict(DataTrain_3, newdata = DataPredict, type = "response") 
predict_32 <- rep("No", nrow(DataPredict))
predict_32[predictpredict_3 > 0.5] <- "Yes" 
predict_32
