# Libraries
library(readxl)

# Read Data
Dataset <- read_excel("D:/UNSW/1st Year/T2/COMM1190/Assignment 2/A2_Dataset_V2.xlsx")

# Plots
boxplot(MonthlyIncome ~ Attrition, data = Dataset) #no=higher monthly income
boxplot(Age ~ Attrition, data = Dataset) #no=higher age
boxplot(StockOptionLevel ~ Attrition, data = Dataset) #no=higher stock
boxplot(SalaryIncrease ~ Attrition, data = Dataset) #useless
boxplot(DistanceFromHome ~ Attrition, data = Dataset) #no=smaller distance
boxplot(EnvironmentSatisfaction ~ Attrition, data = Dataset) #half useless
boxplot(RelationshipSatisfaction ~ Attrition, data = Dataset) #useless
mosaicplot(MaritalStatus ~ Attrition, data = Dataset, color = TRUE) #yes=single
mosaicplot(Gender ~ Attrition, data = Dataset, color = TRUE) #useless
mosaicplot(Department ~ Attrition, data = Dataset, color = TRUE) #some effect
mosaicplot(RelationshipSatisfaction ~ Attrition, data = Dataset, color = TRUE) #yes=1


