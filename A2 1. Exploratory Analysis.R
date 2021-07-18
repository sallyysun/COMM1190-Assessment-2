# Libraries
library(readxl)
library(dplyr)

#Read Data
Dataset0 <- read_excel("D:/UNSW/1st Year/T2/COMM1190/Assignment 2/A2_Dataset_V2.xlsx")
Dataset <- na.omit(Dataset0)
DataPredict <- Dataset0 %>% filter(is.na(Attrition))

#Testing
xtabs(~Attrition + JobSatisfaction, data = Dataset)
xtabs(~Attrition + EnvironmentSatisfaction, data = Dataset)
xtabs(~JobSatisfaction + EnvironmentSatisfaction, data = Dataset)
xtabs(~Attrition + Overtime=unlist(Overtime), data = Dataset)
table1 <- table(Dataset$Attrition, Dataset$OverTime)
prop.table(table1)

# Plots
#Age
boxplot(Age ~ Attrition, data = Dataset)

#Business Travel
mosaicplot(BusinessTravel ~ Attrition, data = Dataset, color = TRUE)

#Department 
mosaicplot(Department ~ Attrition, data = Dataset, color = TRUE)

#Distance Form Home
boxplot(DistanceFromHome ~ Attrition, data = Dataset) 

#Education
mosaicplot(Education ~ Attrition, data = Dataset, color = TRUE) #minimal effect

#Employee Number
boxplot(EmployeeNumber ~ Attrition, data = Dataset) #no effect

#Environment Satisfaction
mosaicplot(EnvironmentSatisfaction ~ Attrition, data = Dataset, color = TRUE)

#Gender
mosaicplot(Gender ~ Attrition, data = Dataset, color = TRUE) #minimal effect

#Job Involvement
mosaicplot(JobInvolvement ~ Attrition, data = Dataset, color = TRUE)

#Job Role
mosaicplot(JobRole ~ Attrition, data = Dataset, color = TRUE)

#Job Satisfaction
mosaicplot(JobSatisfaction ~ Attrition, data = Dataset, color = TRUE)

#Marital Status
mosaicplot(MaritalStatus ~ Attrition, data = Dataset, color = TRUE)

#Monthly Income
boxplot(MonthlyIncome ~ Attrition, data = Dataset) 

#Number Companies Worked
mosaicplot(NumCompaniesWorked ~ Attrition, data = Dataset, color = TRUE)
boxplot(NumCompaniesWorked ~ Attrition, data = Dataset) 

#Overtime
mosaicplot(OverTime ~ Attrition, data = Dataset, color = TRUE)

#Salary Increase
boxplot(SalaryIncrease ~ Attrition, data = Dataset) #no effect

#Relationship Satisfaction
mosaicplot(RelationshipSatisfaction ~ Attrition, data = Dataset, color = TRUE) #minimal effect

#Stock Option Level
boxplot(StockOptionLevel ~ Attrition, data = Dataset)
mosaicplot(StockOptionLevel ~ Attrition, data = Dataset, color = TRUE)

#Total Working Years
boxplot(TotalWorkingYears ~ Attrition, data = Dataset)

#Training Times Per Year
boxplot(TrainingTimesLastYear ~ Attrition, data = Dataset)
mosaicplot(TrainingTimesLastYear ~ Attrition, data = Dataset, color = TRUE)

#Work Life Balance
mosaicplot(WorkLifeBalance ~ Attrition, data = Dataset, color = TRUE)

#Years at Company
boxplot(YearsAtCompany ~ Attrition, data = Dataset)

#Years in Current Role
boxplot(YearsInCurrentRole ~ Attrition, data = Dataset)

#Years Since Last Promotion
boxplot(YearsSinceLastPromotion ~ Attrition, data = Dataset) #minimal effect

#Years with Current Manager
boxplot(YearsWithCurrManager ~ Attrition, data = Dataset)

#High Performance
mosaicplot(HighPerformance ~ Attrition, data = Dataset, color = TRUE) #no effect

#st1
mosaicplot(st1 ~ Attrition, data = Dataset, color = TRUE) #no effect














