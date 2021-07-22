library(readxl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(scales)
library(psych)
library(extrafont)
library(RColorBrewer)

#Read Data
Dataset0 <- read_excel("D:/UNSW/1st Year/T2/COMM1190/Assignment 2/A2_Dataset_V2.xlsx")

#Choosing Data
Dataset <- na.omit(Dataset0)
DataPredict <- Dataset0 %>% filter(is.na(Attrition))

#ATTRITION IN DEPARTMENTS AND JOB ROLE
#Attrition ~ Job Role
Dataset %>%
  ggplot(aes(fill=Attrition,y=100,x=JobRole)) + 
  geom_bar(position="fill", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5)
  ) +
  labs (fill="Attrition") +
  ggtitle("Employee Attrition in Particular Job Roles") +
  ylab("Employee Attrition Percentage") +
  xlab("Job Role") + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent)

#Attrition ~ Department
Dataset %>%
  ggplot(aes(fill=Attrition,y=100,x=Department)) + 
  geom_bar(position="fill", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5)
  ) +
  labs (fill="Attrition") +
  ggtitle("Employee Attrition in Departments") +
  ylab("Employee Attrition Percentage") +
  xlab("Department")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)

#Normal Attrition ~ Department
Dataset %>%
  filter(JobRole=="Sales Representative"|
           JobRole=="Research Scientist"|
           JobRole=="Laboratory Technician"|
           JobRole=="Human Resources"|
           JobRole=="Healthcare Representative") %>%
  ggplot(aes(fill=Attrition,y=100,x=Department)) + 
  geom_bar(position="fill", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5)
  ) +
  labs (fill="Attrition") +
  ggtitle("Normal Employee Attrition in Departments") +
  ylab("Normal Employee Attrition Percentage") +
  xlab("Department")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)

#Management Attrition ~ Department
Dataset %>%
  filter(JobRole=="Manager"|
           JobRole=="Sales Executive"|
           JobRole=="Research Director"|
           JobRole=="Manufacturing Director") %>%
  ggplot(aes(fill=Attrition,y=100,x=Department)) + 
  geom_bar(position="fill", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5)
  ) +
  labs (fill="Attrition") +
  ggtitle("Management Attrition in Departments") +
  ylab("Management Attrition Percentage") +
  xlab("Department")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)