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

#ONE: MONTHLY INCOME
#Attrition ~ Monthly Income
Dataset %>% 
  ggplot(aes(x=Attrition, y=MonthlyIncome, fill=Attrition)) + 
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
  ggtitle("Impact of Monthly Income on Attrition") +
  ylab("Monthly Income") +
  xlab("Attrition")
describeBy(Dataset$MonthlyIncome, group=Dataset$Attrition)

    #Normal
Dataset %>% filter(JobRole=="Sales Representative"|
                     JobRole=="Research Scientist"|
                     JobRole=="Laboratory Technician"|
                     JobRole=="Human Resources"|
                     JobRole=="Healthcare Representative") %>%
  ggplot(aes(x=Attrition, y=MonthlyIncome, fill=Attrition)) + 
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
  ggtitle("Impact of Monthly Income on Attrition\nof Normal Employees") +
  ylab("Monthly Income") +
  xlab("Attrition")

    #Management
Dataset %>% filter(JobRole=="Manager"|
                     JobRole=="Sales Executive"|
                     JobRole=="Research Director"|
                     JobRole=="Manufacturing Director") %>%
  ggplot(aes(x=Attrition, y=MonthlyIncome, fill=Attrition)) + 
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
  ggtitle("Impact of Monthly Income on Attrition\nof Management") +
  ylab("Monthly Income") +
  xlab("Attrition")

  #Income ~ Department
Dataset %>% 
  ggplot(aes(x=Department, y=MonthlyIncome, fill=Department)) + 
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
  ggtitle("Monthly Income in Particular Departments") +
  ylab("Monthly Income") +
  xlab("Department")

    #Normal Employee
Dataset %>% filter(JobRole=="Sales Representative"|
                     JobRole=="Research Scientist"|
                     JobRole=="Laboratory Technician"|
                     JobRole=="Human Resources"|
                     JobRole=="Healthcare Representative") %>%
  ggplot(aes(x=Department, y=MonthlyIncome, fill=Department)) + 
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
  ggtitle("Normal Employee Monthly Income\nin Particular Departments") +
  ylab("Monthly Income") +
  xlab("Department")

    #Management
Dataset %>%  filter(JobRole=="Manager"|
                      JobRole=="Sales Executive"|
                      JobRole=="Research Director"|
                      JobRole=="Manufacturing Director") %>%
  ggplot(aes(x=Department, y=MonthlyIncome, fill=Department)) + 
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
  ggtitle("Management Monthly Income\nin Particular Departments") +
  ylab("Monthly Income") +
  xlab("Department")

#Monthly Income ~ Job Role
Dataset %>% 
  ggplot(aes(x=JobRole, y=MonthlyIncome, fill=JobRole)) + 
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
  ggtitle("Impact of Job Role On Monthly Income") +
  ylab("Monthly Income") +
  xlab("Job Role") +
  coord_flip()
describeBy(Dataset$MonthlyIncome, group=Dataset$JobRole)
describeBy(Dataset$MonthlyIncome, group=Dataset$Department)


#TWO: STOCK OPTION LEVEL
#Attrition ~ Stock
Dataset %>% 
  ggplot(aes(x=Attrition, y=StockOptionLevel, fill=Attrition)) + 
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
  ggtitle("Impact of Stock Option Level on Attrition") +
  ylab("Stock Option Level") +
  xlab("Attrition")
describeBy(Dataset$StockOptionLevel, group=Dataset$Attrition)

Dataset %>%
  ggplot(aes(fill=Attrition,y=100,x=StockOptionLevel)) + 
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
  ggtitle("Employee Attrition against Stock Option Level") +
  ylab("Employee Attrition Percentage (%)") +
  xlab("Stock Option Level")+
  scale_y_continuous(labels = scales::percent)

#Stock Option vs none
Dataset$StockOptionLevel <- ifelse(Dataset$StockOptionLevel == "0", "No", "Yes")
Dataset %>% 
  ggplot(aes(fill=Attrition,y=100,x=StockOptionLevel)) + 
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
  ggtitle("Employee Attrition against Stock Option Level") +
  ylab("Employee Attrition Percentage (%)") +
  xlab("Stock Option Level") +
  scale_y_continuous(labels = scales::percent)

#Number of Employees Each Stock FIXX
Dataset$StockOptionLevel <- factor(Dataset$StockOptionLevel)
Dataset %>%
  ggplot(aes(fill=StockOptionLevel,x=StockOptionLevel,y=1)) + 
  geom_bar(stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    legend.position="none",
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8),
  ) +
  labs (fill="Attrition") +
  ggtitle("Number of Employees Receiving each\nStock Option Level") +
  ylab("Number of Employees") +
  xlab("Stock Option Level")

#Stock Option in Department !!!!FIX UP!!!!!
Dataset %>%
  ggplot(aes(fill=StockOptionLevel,x=Department,y=1)) + 
  geom_bar(position="fill", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    legend.title = element_text(size=8,face="bold"),
    legend.text = element_text(size=8),
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8),
  ) +
  labs (fill="Stock Option Level") +
  ggtitle("Percentage of Employees Receiving each\nStock Option Level in each Department") +
  ylab("Percentage of Employees Receiving Stock Option Level (%)") +
  xlab("Department") +
  coord_flip()+
  scale_y_continuous(labels = scales::percent)

#Income vs Stock Option Level
Dataset %>% 
  ggplot(aes(x=StockOptionLevel, y=MonthlyIncome, fill=StockOptionLevel)) + 
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
  ggtitle("Impact of Monthly Income on Stock Option") +
  ylab("Monthly Income ($)") +
  xlab("Stock Option Level")
describeBy(Dataset$MonthlyIncome, group=Dataset$StockOptionLevel)

  #HR
Dataset %>% filter(Department=="Human Resources")
  ggplot(aes(x=StockOptionLevel, y=MonthlyIncome, fill=StockOptionLevel)) + 
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
  ggtitle("Impact of Monthly Income on Stock Option\nin Human Resources Department") +
  ylab("Monthly Income ($)") +
  xlab("Stock Option Level")

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
  ylab("Age (Years)") +
  xlab("Attrition")

describeBy(Dataset$Age, group=Dataset$Attrition)

#Age ~ Income 
Dataset %>%
  ggplot(aes(x=Age, y=MonthlyIncome)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_fivethirtyeight() +
  theme(
    legend.position="none",
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=12),
    axis.title.y = element_text(size=10,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=10,face="bold"),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("Correlation between Age and Monthly Income") +
  ylab("Monthly Income ($)") +
  xlab("Age (Years)")