library(readxl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(extrafont)
library(RColorBrewer)

#Read Data
Dataset <- read_excel("D:/UNSW/1st Year/T2/COMM1190/Assignment 2/A2_Dataset_V2.xlsx")
nrows <- dim(Dataset) [1]
ncols <- dim(Dataset) [2]

#Sort Data



