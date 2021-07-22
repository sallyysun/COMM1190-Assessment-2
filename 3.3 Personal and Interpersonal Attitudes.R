library(readxl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(psych)
library(extrafont)
library(RColorBrewer)

#Read Data
Dataset0 <- read_excel("D:/UNSW/1st Year/T2/COMM1190/Assignment 2/A2_Dataset_V2.xlsx")

#Choosing Data
Dataset <- na.omit(Dataset0)
DataPredict <- Dataset0 %>% filter(is.na(Attrition))

#ONE: PURPOSE

boxplot(YearsInCurrentRole ~ Attrition, data = Dataset)

#TWO: CURRENT MANAGER
#Attrition ~ Years with Current Manager
Dataset %>% 
  ggplot(aes(x=Attrition, y=YearsWithCurrManager, fill=Attrition)) + 
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
  ggtitle("Impact of Years with Current Manager on Attrition") +
  ylab("Years with Current Manager") +
  xlab("Attrition")

describeBy(Dataset$YearsWithCurrManager, group=Dataset$Attrition)

#Years with Current Manager ~ Environment 

Dataset %>% 
  ggplot(aes(x=EnvironmentSatisfaction, y=YearsWithCurrManager, fill=EnvironmentSatisfaction)) + 
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
  ggtitle("Impact of Years with Current Manager on Attrition") +
  ylab("Years with Current Manager") +
  xlab("Environment Satisfaction")


#THREE: AGE
  #Attrition ~ Age
Dataset %>% 
  ggplot(aes(x=Attrition, y=Age, fill=Attrition)) + 
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
  ggtitle("Impact of Age on Attrition") +
  ylab("Age") +
  xlab("Attrition")

describeBy(Dataset$Age, group=Dataset$Attrition)

#Age ~ Marital Status
Dataset %>% 
  ggplot(aes(x=MaritalStatus, y=Age, fill=MaritalStatus)) + 
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
  ggtitle("Impact of Age on Marital Status") +
  ylab("Age") +
  xlab("Marital Status")
describeBy(Dataset$Age, group=Dataset$MaritalStatus)

#Marital Status ~ Attrition
Dataset %>%
  ggplot(aes(fill=Attrition,y=100,x=MaritalStatus)) + 
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
  ggtitle("Impact of Marital Status on Attrition") +
  ylab("Employee Attrition Percentage (%)")+
  xlab("Marital Status") +
  scale_y_continuous(labels = scales::percent)
xtabs(~MaritalStatus + Attrition, data = Dataset)
107/(107+311)

#FOUR: NUMBER COMPANIES WORKED
boxplot(NumCompaniesWorked ~ Attrition, data = Dataset) 

Dataset %>%
  ggplot(aes(x=Attrition, y=NumCompaniesWorked, fill=Attrition)) + 
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
  ggtitle("Impact of Number Companies Worked on Attrition") +
  ylab("Number of Companies Worked") +
  xlab("Attrition")

describeBy(Dataset$NumCompaniesWorked, group=Dataset$Attrition)