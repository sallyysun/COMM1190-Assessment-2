#Libraries
library(readxl)
library(aod)
library(dplyr)

#Read Data
Dataset0 <- read_excel("D:/UNSW/1st Year/T2/COMM1190/Assignment 2/A2_Dataset_V2.xlsx")

#Choosing Data
Dataset <- na.omit(Dataset0)
DataPredict <- Dataset0 %>% filter(is.na(Attrition))

#Will Leave
DataLeave <- DataPredict %>% slice(1,11,21,22,29,30)
t(DataLeave)