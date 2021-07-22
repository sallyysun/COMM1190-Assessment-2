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

#PREFACE
#Attrition ~ Work Life Balance
Dataset %>%
  ggplot(aes(fill=Attrition,y=100,x=WorkLifeBalance)) + 
  geom_bar(position="fill", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size=8,face="bold"),
    legend.text = element_text(size=8)
  ) +
  labs (fill="Attrition") +
  ggtitle("Employee Attrtion against Work Life Balance") +
  ylab("Employee Attrition")+
  scale_y_continuous(labels = scales::percent)
xtabs(~Attrition + WorkLifeBalance, data = Dataset)

Dataset$WorkLifeBalance <- ifelse(Dataset$WorkLifeBalance == "1", "Bad", "Satisfactory")
Dataset %>%
  ggplot(aes(fill=Attrition,y=100,x=WorkLifeBalance)) + 
  geom_bar(position="fill", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size=8,face="bold"),
    legend.text = element_text(size=8)
  ) +
  labs (fill="Attrition") +
  ggtitle("Employee Attrtion against Work Life Balance") +
  ylab("Employee Attrition")+
  scale_y_continuous(labels = scales::percent)
xtabs(~Attrition + WorkLifeBalance, data = Dataset)

#Attrition ~ Environment Satisfaction
mosaicplot(EnvironmentSatisfaction ~ Attrition, data = Dataset, color = TRUE)
Dataset %>%
  ggplot(aes(fill=Attrition,y=100,x=EnvironmentSatisfaction)) + 
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
  ggtitle("Employee Attrition against Environmental Satisfaction") +
  ylab("Employee Attrition Percentage") +
  xlab("Environmental Satisfaction") +
  scale_y_continuous(labels = scales::percent)
xtabs(~Attrition + EnvironmentSatisfaction, data = Dataset)


#ONE: OVERTIME
mosaicplot(OverTime ~ Attrition, data = Dataset, color = TRUE)
mosaicplot(EnvironmentSatisfaction ~ OverTime, data = Dataset, color = TRUE)
xtabs(~Attrition + OverTime, data = Dataset)
117/(117+94)

  #Overtime ~ Attrition
Dataset %>%
  ggplot(aes(fill=OverTime,y=100,x=Attrition)) + 
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
  labs (fill="Overtime") +
  ggtitle("Employee Attrition against Working Overtime") +
  ylab("Percentage Work Overtime") +
  xlab("Attrition") +
  scale_y_continuous(labels = scales::percent)

#Overtime ~ Attrition
Dataset %>%
  ggplot(aes(fill=Attrition,y=100,x=OverTime)) + 
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
  ggtitle("Employee Attrition against Working Overtime") +
  ylab("Employee Attrition Percentage") +
  xlab("Overtime") +
  scale_y_continuous(labels = scales::percent)

#Work Life Balance ~ Overtime
Dataset %>% 
  ggplot(aes(fill=OverTime,y=100,x=WorkLifeBalance)) + 
  geom_bar(position="fill", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size=8,face="bold"),
    legend.text = element_text(size=8)
  ) +
  labs (fill="OverTime") +
  ggtitle("Work Life Balance of Employees Working Overtime") +
  ylab("Percentage Work Overtime") +
  xlab("Work Life Balance")+
  scale_y_continuous(labels = scales::percent)
xtabs(~WorkLifeBalance + OverTime, data=Dataset)
33/(33+106)
220/(220+550)
90/(90+212)
25/(25+59)

#Position ~ Overtime
Dataset$Position <- 
  ifelse(Dataset$JobRole=="Sales Representative"|
          Dataset$JobRole=="Research Scientist"|
          Dataset$JobRole=="Laboratory Technician"|
          Dataset$JobRole=="Human Resources"|
          Dataset$JobRole=="Healthcare Representative",
          "Normal", "Management")

Dataset %>%  
  ggplot(aes(fill=OverTime,y=100,x=Position)) + 
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
  labs (fill="Overtime") +
  ggtitle("Employee Position Working Overtime") +
  ylab("Percentage Work Overtime (%)") +
  xlab("Employee Position") +
  scale_y_continuous(labels = scales::percent)

#Department ~ Overtime
Dataset %>%
ggplot(aes(fill=OverTime,y=100,x=Department)) + 
  geom_bar(position="fill", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size=8,face="bold"),
    legend.text = element_text(size=8)
  ) +
  labs (fill="OverTime") +
  ggtitle("Working Overtime Across Different Departments") +
  ylab("Percentage Work Overtime") +
  xlab("Department")+
  coord_flip() +
  scale_y_continuous(labels = scales::percent)


#TWO: DISTANCE FROM HOME
boxplot(DistanceFromHome ~ Attrition, data = Dataset) 

  #Attrition ~ Distance From Home
Dataset %>% 
  ggplot(aes(x=Attrition, y=DistanceFromHome, fill=Attrition)) + 
  geom_boxplot() + 
  geom_jitter(color="darkgrey", size=0.4, alpha=0.7) +
  theme_fivethirtyeight() +
  scale_fill_few("Light") +
  theme(
    legend.position="none",
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("Impact of Distance From Home on Attrition") +
  ylab("Distance From Home") +
  xlab("Attrition")

describeBy(Dataset$DistanceFromHome, group=Dataset$Attrition)

  #Attrition ~ Distance From Home FOR PEOPLE WORKING OVERTIME
Dataset %>% filter(OverTime=="Yes") %>%
  ggplot(aes(x=Attrition, y=DistanceFromHome, fill=Attrition)) + 
  geom_boxplot() + 
  geom_jitter(color="darkgrey", size=0.4, alpha=0.7) +
  theme_fivethirtyeight() +
  scale_fill_few("Light") +
  theme(
    legend.position="none",
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("Impact of Distance From Home on Attrition\nfor Employees who Usually Work Overtime") +
  ylab("Distance From Home") +
  xlab("Attrition")

#Overtime ~ Attrition (far from home)
Dataset %>% filter(DistanceFromHome>35) %>%
  ggplot(aes(fill=Attrition,y=100,x=OverTime)) + 
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
  ggtitle("Employee Attrition against Working Overtime\n Living Over 35km from Home") +
  ylab("Employee Attrition Percentage") +
  xlab("Overtime") +
  scale_y_continuous(labels = scales::percent)

xtabs(~Attrition + OverTime, data=Dataset)
117/(117+251)

#THREE: BUSINESS TRAVEL
Dataset %>%
  ggplot(aes(fill=Attrition,y=100,x=BusinessTravel)) + 
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
  ggtitle("Employee Attrition against Business Travel") +
  ylab("Employee Attrition Percentage") +
  xlab("Business Travel")+
  scale_y_continuous(labels = scales::percent)

Dataset %>%
  ggplot(aes(fill=BusinessTravel,y=100,x=Position)) + 
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
  labs (fill="Business Travel") +
  ggtitle("Percentage Business Travel in Different Positions") +
  ylab("Business Travel Percentage (%)") +
  xlab("Position")+
  scale_y_continuous(labels = scales::percent)

#Business Travel ~ Department
Dataset %>% 
  ggplot(aes(fill=BusinessTravel,y=100,x=Department)) + 
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
  labs (fill="Business Travel") +
  ggtitle("Business Travel in Different Departments") +
  ylab("Business Travel Percentage (%)") +
  xlab("Business Travel")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)

Dataset %>% filter(Position=="Normal") %>%
  ggplot(aes(fill=BusinessTravel,y=100,x=Department)) + 
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
  labs (fill="Business Travel") +
  ggtitle("Business Travel in Different Departments (Normal Employees)") +
  ylab("Business Travel Percentage (%)") +
  xlab("Business Travel")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)

Dataset %>% filter(Position=="Management") %>%
  ggplot(aes(fill=BusinessTravel,y=100,x=Department)) + 
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
  labs (fill="Business Travel") +
  ggtitle("Business Travel in Different Departments (Management)") +
  ylab("Business Travel Percentage (%)") +
  xlab("Business Travel")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)