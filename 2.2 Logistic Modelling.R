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
Dataset0$Attrition <- factor(Dataset0$Attrition)
Dataset0$BusinessTravel <- factor(Dataset0$BusinessTravel)
Dataset0$Department <- factor(Dataset0$Department)
Dataset0$Education <- factor(Dataset0$Education)
Dataset0$EnvironmentSatisfaction <- factor(Dataset0$EnvironmentSatisfaction)
Dataset0$Gender <- factor(Dataset0$Gender)
Dataset0$JobInvolvement <- factor(Dataset0$JobInvolvement)
Dataset0$JobRole <- factor(Dataset0$JobRole)
Dataset0$JobSatisfaction <- factor(Dataset0$JobSatisfaction)
Dataset0$MaritalStatus <- factor(Dataset0$MaritalStatus)
Dataset0$NumCompaniesWorked <- factor(Dataset0$NumCompaniesWorked)
Dataset0$OverTime <- factor(Dataset0$OverTime)
Dataset0$RelationshipSatisfaction <- factor(Dataset0$RelationshipSatisfaction)
Dataset0$StockOptionLevel <- factor(Dataset0$StockOptionLevel)
Dataset0$WorkLifeBalance <- factor(Dataset0$WorkLifeBalance)
Dataset0$HighPerformance <- factor(Dataset0$HighPerformance)
Dataset0$st1 <- factor(Dataset0$st1)
str(Dataset0)

#Choosing Data
Dataset <- na.omit(Dataset0)
DataPredict <- Dataset0 %>% filter(is.na(Attrition))

#Split Data
set.seed(3) 
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
    #Accuracy No = 0.9736541
850/(850+23)
    #Accuracy Yes = 0.4723926
77/(77+86)
    #Accuracy = 0.8947876
(850+77)/nrow(DataTrain)

  #Testing Accuracy
testpredict_all <- predict(DataTrain_All, newdata = DataTest, type = "response") 
predict_all1 <- rep("No", nrow(DataTest))
predict_all1[testpredict_all > 0.5] <- "Yes" 
table(predict_all1, DataTest$Attrition)

    #Accuracy No = 0.943128
199/(199+12)
    #Accuracy Yes = 0.4583333
22/(22+26)
    #Accuracy = 0.8532819
(199+22)/nrow(DataTest)

  #Predict 30 People
predictpredict_all <- predict(DataTrain_All, newdata = DataPredict, type = "response") 
predict_all2 <- rep("No", nrow(DataPredict))
predict_all2[predictpredict_all > 0.5] <- "Yes" 
predict_all2
predictpredict_all

#2. Filter 1 // Exploratory Analysis
DataTrain_1 <- glm(Attrition ~ .
                   -Education-EmployeeNumber-Gender-SalaryIncrease-RelationshipSatisfaction
                   -YearsSinceLastPromotion-HighPerformance-st1, 
                   data = DataTrain, family="binomial")
summary(DataTrain_1)

  #Training Accuracy
trainpredict_1 <- predict(DataTrain_1, newdata = DataTrain, type = "response") 
predict_1 <- rep("No", nrow(DataTrain))
predict_1[trainpredict_1 > 0.5] <- "Yes" 
table(predict_1, DataTrain$Attrition)
    #Accuracy No = 0.9736541
850/(848+25)
    #Accuracy Yes = 0.4601227
75/(75+88)
    #Accuracy = 0.8928571
(850+75)/nrow(DataTrain)

  #Testing Accuracy
testpredict_1 <- predict(DataTrain_1, newdata = DataTest, type = "response") 
predict_11 <- rep("No", nrow(DataTest))
predict_11[testpredict_1 > 0.5] <- "Yes" 
table(predict_11, DataTest$Attrition)

    #Accuracy No = 0.957346
202/(200+11)
    #Accuracy Yes = 0.375
18/(20+28)
    #Accuracy = 0.8494208
(202+18)/nrow(DataTest)

#Predict 30 People
predictpredict_1 <- predict(DataTrain_1, newdata = DataPredict, type = "response") 
predict_12 <- rep("No", nrow(DataPredict))
predict_12[predictpredict_1 > 0.5] <- "Yes" 
predict_12
predictpredict_1

#3. Filter 2 // Based on Logistic 
DataTrain_2 <- glm(Attrition ~ .
                   -Age-Department-Education-EmployeeNumber-Gender-MaritalStatus
                   -MonthlyIncome-SalaryIncrease-TotalWorkingYears-YearsWithCurrManager
                   -HighPerformance-st1, 
                   data = DataTrain, family="binomial")
summary(DataTrain_2)

  #Training Accuracy
trainpredict_2 <- predict(DataTrain_2, newdata = DataTrain, type = "response") 
predict_2 <- rep("No", nrow(DataTrain))
predict_2[trainpredict_2 > 0.5] <- "Yes" 
table(predict_2, DataTrain$Attrition)
    #Accuracy No = 0.9725086
849/(849+24)
    #Accuracy Yes = 0.4539877
74/(74+89)
    #Accuracy = 0.8909266
(849+74)/nrow(DataTrain)

  #Testing Accuracy
testpredict_2 <- predict(DataTrain_2, newdata = DataTest, type = "response") 
predict_21 <- rep("No", nrow(DataTest))
predict_21[testpredict_2 > 0.5] <- "Yes" 
table(predict_21, DataTest$Attrition)

    #Accuracy No = 0.9383886
198/(198+13)
    #Accuracy Yes = 0.375
18/(18+30)
    #Accuracy = 0.8339768
(198+18)/nrow(DataTest)

  #Predict 30 People
predictpredict_2 <- predict(DataTrain_2, newdata = DataPredict, type = "response") 
predict_22 <- rep("No", nrow(DataPredict))
predict_22[predictpredict_2 > 0.5] <- "Yes" 
predict_22
predictpredict_2


#4. Filter 3 // Based on Classification
DataTrain_3 <- glm(Attrition ~ .
                   -DistanceFromHome-Education-EmployeeNumber-JobSatisfaction
                   -MaritalStatus-NumCompaniesWorked-SalaryIncrease
                   -RelationshipSatisfaction-WorkLifeBalance-YearsInCurrentRole-YearsSinceLastPromotion
                   -HighPerformance-st1, 
                   data = DataTrain, family="binomial")
summary(DataTrain_3)

#Training Accuracy
trainpredict_3 <- predict(DataTrain_3, newdata = DataTrain, type = "response") 
predict_3 <- rep("No", nrow(DataTrain))
predict_3[trainpredict_3 > 0.5] <- "Yes" 
table(predict_3, DataTrain$Attrition)
#Accuracy No = 0.9725086
849/(846+27)
#Accuracy Yes = 0.3680982
60/(67+96)
#Accuracy = 0.8774131
(849+60)/nrow(DataTrain)

#Testing Accuracy
testpredict_3 <- predict(DataTrain_3, newdata = DataTest, type = "response") 
predict_31 <- rep("No", nrow(DataTest))
predict_31[testpredict_3 > 0.5] <- "Yes" 
table(predict_31, DataTest$Attrition)

#Accuracy No = 0.9383886
198/(200+11)
#Accuracy Yes = 0.25
12/(21+27)
#Accuracy = 0.8108108
(198+12)/nrow(DataTest)

#Predict 30 People
predictpredict_3 <- predict(DataTrain_3, newdata = DataPredict, type = "response") 
predict_32 <- rep("No", nrow(DataPredict))
predict_32[predictpredict_3 > 0.5] <- "Yes" 
predict_32
predictpredict_3

#5. Filter 4 // Final
DataTrain_4 <- glm(Attrition ~ .
                   -Education-EmployeeNumber-SalaryIncrease
                   -MaritalStatus-TotalWorkingYears-Gender
                   -YearsAtCompany-YearsInCurrentRole-Department
                   -TrainingTimesLastYear
                   -HighPerformance-st1, 
                   data = DataTrain, family="binomial")
summary(DataTrain_4)

#Training Accuracy
trainpredict_4 <- predict(DataTrain_4, newdata = DataTrain, type = "response") 
predict_4 <- rep("No", nrow(DataTrain))
predict_4[trainpredict_4 > 0.5] <- "Yes" 
table(predict_4, DataTrain$Attrition)
#Accuracy No = 0.9725086
849/(846+27)
#Accuracy Yes = 0.4355828
71/(67+96)
#Accuracy = 0.8880309
(849+71)/nrow(DataTrain)

#Testing Accuracy
testpredict_4 <- predict(DataTrain_4, newdata = DataTest, type = "response") 
predict_41 <- rep("No", nrow(DataTest))
predict_41[testpredict_4 > 0.5] <- "Yes" 
table(predict_41, DataTest$Attrition)

#Accuracy No = 0.92891
196/(200+11)
#Accuracy Yes = 0.5
27/(20+27)
#Accuracy = 0.8494208
(196+24)/nrow(DataTest)

#Predict 30 People
predictpredict_4 <- predict(DataTrain_4, newdata = DataPredict, type = "response") 
predict_42 <- rep("No", nrow(DataPredict))
predict_42[predictpredict_4 > 0.5] <- "Yes" 
predict_42
predictpredict_4