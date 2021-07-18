library(readxl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(extrafont)
library(RColorBrewer)

#Read Data
A1_Dataset <- read_excel("D:/UNSW/1st Year/T2/COMM1190/Assignment 2/A2_Dataset_V2.xlsx")
nrows <- dim(A1_Dataset) [1]
ncols <- dim(A1_Dataset) [2]

#Sort Data
Age <- A1_Dataset[,1]
BusinessTravel <- A1_Dataset[,3]
Department <- A1_Dataset[,4]
DistanceFromHome <- A1_Dataset[,5]
Education <- A1_Dataset[,6]
EmployeeNumber <- A1_Dataset[,7]
EnvironmentSatisfaction <- A1_Dataset[,8]
Gender <- A1_Dataset[,9]
JobInvolvement <- A1_Dataset[,10]
JobRole <- A1_Dataset[,11]
JobSatisfaction <- A1_Dataset[,12]
MaritalStatus <- A1_Dataset[,13]
MonthlyIncome <- A1_Dataset[,14]
NumCompaniesWorked <- A1_Dataset[,15]
Overtime <- A1_Dataset[,16]
SalaryIncrease <- A1_Dataset[,17]
RelationshipSatisfaction <- A1_Dataset[,18]
TotalWorkingYears <- A1_Dataset[,20]
TrainingTimesLastYear <- A1_Dataset[,21]
WorkLifeBalance <- A1_Dataset[,22]
HighPerformance <- A1_Dataset[,27]

#Factor 1: Attitude Towards Job Role
#1. Department Involvement
#Sort Data
DJS1 <- A1_Dataset %>% 
  select(Department) %>%
  filter(JobInvolvement == "1")
D0JS1 <- DJS1 %>% 
  filter(Department == "Human Resources")
D1JS1 <- DJS1 %>% 
  filter(Department == "Sales")
D2JS1 <- DJS1 %>% 
  filter(Department == "Research & Development")

DJS2 <- A1_Dataset %>% 
  select(Department) %>%
  filter(JobInvolvement == "2")
D0JS2 <- DJS2 %>% 
  filter(Department == "Human Resources")
D1JS2 <- DJS2 %>% 
  filter(Department == "Sales")
D2JS2 <- DJS2 %>% 
  filter(Department == "Research & Development")

DJS3 <- A1_Dataset %>% 
  select(Department) %>%
  filter(JobInvolvement == "3")
D0JS3 <- DJS3 %>% 
  filter(Department == "Human Resources")
D1JS3 <- DJS3 %>% 
  filter(Department == "Sales")
D2JS3 <- DJS3 %>% 
  filter(Department == "Research & Development")

DJS4 <- A1_Dataset %>% 
  select(Department) %>%
  filter(JobInvolvement == "4")
D0JS4 <- DJS4 %>% 
  filter(Department == "Human Resources")
D1JS4 <- DJS4 %>% 
  filter(Department == "Sales")
D2JS4 <- DJS4 %>% 
  filter(Department == "Research & Development")

sumHR <- nrow(D0JS1)+nrow(D0JS2)+nrow(D0JS3)+nrow(D0JS4)
sumSales <- nrow(D1JS1)+nrow(D1JS2)+nrow(D1JS3)+nrow(D1JS4)
sumRND <- nrow(D2JS1)+nrow(D2JS2)+nrow(D2JS3)+nrow(D2JS4)

DJSone <- data.frame(JSlevel = "1", department = "Human Resources", percentage = nrow(D0JS1)*100/sumHR)
DJStwo <- data.frame(JSlevel = "1", department = "Sales", percentage = nrow(D1JS1)*100/sumSales)
DJSthree <- data.frame(JSlevel = "1", department = "Research & Development", percentage = nrow(D2JS1)*100/sumRND)
DJSfour <- data.frame(JSlevel = "2", department = "Human Resources", percentage = nrow(D0JS2)*100/sumHR)
DJSfive <- data.frame(JSlevel = "2", department = "Sales", percentage = nrow(D1JS2)*100/sumSales)
DJSsix <- data.frame(JSlevel = "2", department = "Research & Development", percentage = nrow(D2JS2)*100/sumRND)
DJSseven <- data.frame(JSlevel = "3", department = "Human Resources", percentage = nrow(D0JS3)*100/sumHR)
DJSeight <- data.frame(JSlevel = "3", department = "Sales", percentage = nrow(D1JS3)*100/sumSales)
DJSnine <- data.frame(JSlevel = "3", department = "Research & Development", percentage = nrow(D2JS3)*100/sumRND)
DJSten <- data.frame(JSlevel = "4", department = "Human Resources", percentage = nrow(D0JS4)*100/sumHR)
DJSeleven <- data.frame(JSlevel = "4", department = "Sales", percentage = nrow(D1JS4)*100/sumSales)
DJStwelve <- data.frame(JSlevel = "4", department = "Research & Development", percentage = nrow(D2JS4)*100/sumRND)

DepartmentJS <- rbind(DJSone,DJStwo,DJSthree,DJSfour,DJSfive,DJSsix,DJSseven,DJSeight,DJSnine,DJSten,DJSeleven,DJStwelve)

#Percentage Stack Chart
DepartmentJS %>%
  ggplot(aes(fill=department, y=percentage, x=JSlevel)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(vjust=4, size=10,face="bold"),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5)
  ) +
  labs (fill="Department") +
  ggtitle("Correlation Between Job Involvement\nand Department") +
  ylab("Ratio of Department (%)") +
  xlab("Job Involvement")

#2.Training Days
#Choose Data
tHR1 <- A1_Dataset %>% 
  select(TrainingTimesLastYear) %>%
  filter(Department == "Human Resources") 
tRND1 <- A1_Dataset %>% 
  select(TrainingTimesLastYear) %>%
  filter(Department == "Research & Development")
tSales1 <- A1_Dataset %>% 
  select(TrainingTimesLastYear) %>%
  filter(Department == "Sales")

tAll <- data.frame(group1 = "All", value = TrainingTimesLastYear[,1])
tHR <- data.frame(group1 = "Human Resources", value = tHR1[,1])
tRND <- data.frame(group1 = "Research & Development", value = tRND1[,1])
tSales <- data.frame(group1 = "Sales", value = tSales1[,1])

tDepartment <- rbind(tAll,tHR,tRND,tSales)

#Box Plot
tDepartment %>%
  ggplot(aes(x=group1, y=TrainingTimesLastYear, fill=group1)) + 
  geom_boxplot() + 
  geom_jitter(color="darkgrey", size=0.4, alpha=0.7) +
  theme_fivethirtyeight() +
  scale_fill_few("Light") +
  theme(
    legend.position="none",
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=11),
    axis.title.y = element_text(size=9, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5)
  ) +
  ggtitle("Training Times Last Year in Each Department") +
  ylab("Training Times Last Year") +
  xlab("Department")

#3. Number Companies Worked
#Choose Data
tHR1 <- A1_Dataset %>% 
  select(NumCompaniesWorked) %>%
  filter(Department == "Human Resources") 
tRND1 <- A1_Dataset %>% 
  select(NumCompaniesWorked) %>%
  filter(Department == "Research & Development")
tSales1 <- A1_Dataset %>% 
  select(NumCompaniesWorked) %>%
  filter(Department == "Sales")

tAll <- data.frame(group1 = "All", value = NumCompaniesWorked[,1])
tHR <- data.frame(group1 = "Human Resources", value = tHR1[,1])
tRND <- data.frame(group1 = "Research & Development", value = tRND1[,1])
tSales <- data.frame(group1 = "Sales", value = tSales1[,1])

tDepartment <- rbind(tAll,tHR,tRND,tSales)

#Box Plot
tDepartment %>%
  ggplot(aes(x=group1, y=NumCompaniesWorked, fill=group1)) + 
  geom_boxplot() + 
  geom_jitter(color="darkgrey", size=0.4, alpha=0.7) +
  theme_fivethirtyeight() +
  scale_fill_few("Light") +
  theme(
    legend.position="none",
    text = element_text(family="Century Gothic"),
    plot.title = element_text(size=11),
    axis.title.y = element_text(size=9, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5)
  ) +
  ggtitle("Number of Companies Worked in Each Department") +
  ylab("Number of Companies Worked") +
  xlab("Department")

#4. Education Level
#Sort Data
DJS1 <- A1_Dataset %>% 
  select(Department) %>%
  filter(Education == "1")
D0JS1 <- DJS1 %>% 
  filter(Department == "Human Resources")
D1JS1 <- DJS1 %>% 
  filter(Department == "Sales")
D2JS1 <- DJS1 %>% 
  filter(Department == "Research & Development")

DJS2 <- A1_Dataset %>% 
  select(Department) %>%
  filter(Education == "2")
D0JS2 <- DJS2 %>% 
  filter(Department == "Human Resources")
D1JS2 <- DJS2 %>% 
  filter(Department == "Sales")
D2JS2 <- DJS2 %>% 
  filter(Department == "Research & Development")

DJS3 <- A1_Dataset %>% 
  select(Department) %>%
  filter(Education == "3")
D0JS3 <- DJS3 %>% 
  filter(Department == "Human Resources")
D1JS3 <- DJS3 %>% 
  filter(Department == "Sales")
D2JS3 <- DJS3 %>% 
  filter(Department == "Research & Development")

DJS4 <- A1_Dataset %>% 
  select(Department) %>%
  filter(Education == "4")
D0JS4 <- DJS4 %>% 
  filter(Department == "Human Resources")
D1JS4 <- DJS4 %>% 
  filter(Department == "Sales")
D2JS4 <- DJS4 %>% 
  filter(Department == "Research & Development")

DJS5 <- A1_Dataset %>% 
  select(Department) %>%
  filter(Education == "5")
D0JS5 <- DJS5 %>% 
  filter(Department == "Human Resources")
D1JS5 <- DJS5 %>% 
  filter(Department == "Sales")
D2JS5 <- DJS5 %>% 
  filter(Department == "Research & Development")

sumHR <- nrow(D0JS1)+nrow(D0JS2)+nrow(D0JS3)+nrow(D0JS4)+nrow(D0JS5)
sumSales <- nrow(D1JS1)+nrow(D1JS2)+nrow(D1JS3)+nrow(D1JS4)+nrow(D1JS5)
sumRND <- nrow(D2JS1)+nrow(D2JS2)+nrow(D2JS3)+nrow(D2JS4)+nrow(D2JS5)

DJSone <- data.frame(JSlevel = "1", department = "Human Resources", percentage = nrow(D0JS1)*100/sumHR)
DJStwo <- data.frame(JSlevel = "1", department = "Sales", percentage = nrow(D1JS1)*100/sumSales)
DJSthree <- data.frame(JSlevel = "1", department = "Research & Development", percentage = nrow(D2JS1)*100/sumRND)
DJSfour <- data.frame(JSlevel = "2", department = "Human Resources", percentage = nrow(D0JS2)*100/sumHR)
DJSfive <- data.frame(JSlevel = "2", department = "Sales", percentage = nrow(D1JS2)*100/sumSales)
DJSsix <- data.frame(JSlevel = "2", department = "Research & Development", percentage = nrow(D2JS2)*100/sumRND)
DJSseven <- data.frame(JSlevel = "3", department = "Human Resources", percentage = nrow(D0JS3)*100/sumHR)
DJSeight <- data.frame(JSlevel = "3", department = "Sales", percentage = nrow(D1JS3)*100/sumSales)
DJSnine <- data.frame(JSlevel = "3", department = "Research & Development", percentage = nrow(D2JS3)*100/sumRND)
DJSten <- data.frame(JSlevel = "4", department = "Human Resources", percentage = nrow(D0JS4)*100/sumHR)
DJSeleven <- data.frame(JSlevel = "4", department = "Sales", percentage = nrow(D1JS4)*100/sumSales)
DJStwelve <- data.frame(JSlevel = "4", department = "Research & Development", percentage = nrow(D2JS4)*100/sumRND)
DJSthirteen <- data.frame(JSlevel = "5", department = "Human Resources", percentage = nrow(D0JS5)*100/sumHR)
DJSfourteen <- data.frame(JSlevel = "5", department = "Sales", percentage = nrow(D1JS5)*100/sumSales)
DJSfifteen <- data.frame(JSlevel = "5", department = "Research & Development", percentage = nrow(D2JS5)*100/sumRND)

DepartmentJS <- rbind(DJSone,DJStwo,DJSthree,DJSfour,DJSfive,DJSsix,DJSseven,DJSeight,DJSnine,DJSten,DJSeleven,DJStwelve,DJSthirteen,DJSfourteen,DJSfifteen)

#Percentage Stack Chart
DepartmentJS %>%
  ggplot(aes(fill=department, y=percentage, x=JSlevel)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(vjust=4, size=10,face="bold"),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5)
  ) +
  labs (fill="Department") +
  ggtitle("Education Level in Departments") +
  ylab("Ratio of Department (%)") +
  xlab("Education Level")



#Factor 2: Work Environment
#JOB SATISFACTION
#1. Work-Life Balance
#Sort Data
WLJS1 <- A1_Dataset %>% 
  select(WorkLifeBalance) %>%
  filter(JobSatisfaction == "1")
WL1JS1 <- WLJS1 %>% 
  filter(WorkLifeBalance == "1")
WL2JS1 <- WLJS1 %>% 
  filter(WorkLifeBalance == "2")
WL3JS1 <- WLJS1 %>% 
  filter(WorkLifeBalance == "3")
WL4JS1 <- WLJS1 %>% 
  filter(WorkLifeBalance == "4")

WLJS2 <- A1_Dataset %>% 
  select(WorkLifeBalance) %>%
  filter(JobSatisfaction == "2")
WL1JS2 <- WLJS2 %>% 
  filter(WorkLifeBalance == "1")
WL2JS2 <- WLJS2 %>% 
  filter(WorkLifeBalance == "2")
WL3JS2 <- WLJS2 %>% 
  filter(WorkLifeBalance == "3")
WL4JS2 <- WLJS2 %>% 
  filter(WorkLifeBalance == "4")

WLJS3 <- A1_Dataset %>% 
  select(WorkLifeBalance) %>%
  filter(JobSatisfaction == "3")
WL1JS3 <- WLJS3 %>% 
  filter(WorkLifeBalance == "1")
WL2JS3 <- WLJS3 %>% 
  filter(WorkLifeBalance == "2")
WL3JS3 <- WLJS3 %>% 
  filter(WorkLifeBalance == "3")
WL4JS3 <- WLJS3 %>% 
  filter(WorkLifeBalance == "4")

WLJS4 <- A1_Dataset %>% 
  select(WorkLifeBalance) %>%
  filter(JobSatisfaction == "4")
WL1JS4 <- WLJS4 %>% 
  filter(WorkLifeBalance == "1")
WL2JS4 <- WLJS4 %>% 
  filter(WorkLifeBalance == "2")
WL3JS4 <- WLJS4 %>% 
  filter(WorkLifeBalance == "3")
WL4JS4 <- WLJS4 %>% 
  filter(WorkLifeBalance == "4")

WLJSone <- data.frame(JSlevel = "1", WLlevel = "1", percentage = nrow(WL1JS1)*100/nrow(WLJS1))
WLJStwo <- data.frame(JSlevel = "1", WLlevel = "2", percentage = nrow(WL2JS1)*100/nrow(WLJS1))
WLJSthree <- data.frame(JSlevel = "1", WLlevel = "3", percentage = nrow(WL3JS1)*100/nrow(WLJS1))
WLJSfour <- data.frame(JSlevel = "1", WLlevel = "4", percentage = nrow(WL4JS1)*100/nrow(WLJS1))
WLJSfive <- data.frame(JSlevel = "2", WLlevel = "1", percentage = nrow(WL1JS2)*100/nrow(WLJS2))
WLJSsix <- data.frame(JSlevel = "2", WLlevel = "2", percentage = nrow(WL2JS2)*100/nrow(WLJS2))
WLJSseven <- data.frame(JSlevel = "2", WLlevel = "3", percentage = nrow(WL3JS2)*100/nrow(WLJS2))
WLJSeight <- data.frame(JSlevel = "2", WLlevel = "4", percentage = nrow(WL4JS2)*100/nrow(WLJS2))
WLJSnine <- data.frame(JSlevel = "3", WLlevel = "1", percentage = nrow(WL1JS3)*100/nrow(WLJS3))
WLJSten <- data.frame(JSlevel = "3", WLlevel = "2", percentage = nrow(WL2JS3)*100/nrow(WLJS3))
WLJSeleven <- data.frame(JSlevel = "3", WLlevel = "3", percentage = nrow(WL3JS3)*100/nrow(WLJS3))
WLJStwelve <- data.frame(JSlevel = "3", WLlevel = "4", percentage = nrow(WL4JS3)*100/nrow(WLJS3))
WLJSthirteen <- data.frame(JSlevel = "4", WLlevel = "1", percentage = nrow(WL1JS4)*100/nrow(WLJS4))
WLJSfourteen <- data.frame(JSlevel = "4", WLlevel = "2", percentage = nrow(WL2JS4)*100/nrow(WLJS4))
WLJSfifteen <- data.frame(JSlevel = "4", WLlevel = "3", percentage = nrow(WL3JS4)*100/nrow(WLJS4))
WLJSsixteen <- data.frame(JSlevel = "4", WLlevel = "4", percentage = nrow(WL4JS4)*100/nrow(WLJS4))

worklifeJS <- rbind(WLJSone,WLJStwo,WLJSthree,WLJSfour,WLJSfive,WLJSsix,WLJSseven,WLJSeight,WLJSnine,WLJSten,WLJSeleven,WLJStwelve,WLJSthirteen,WLJSfourteen,WLJSfifteen,WLJSsixteen)

#Heatmap 
worklifeJS %>%
  ggplot(aes(x=JSlevel, y=WLlevel, fill=floor(percentage))) + 
  geom_tile() +
  theme_few() +
  scale_fill_distiller(palette = "RdPu") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(hjust=-4,vjust=4, size=10,face="bold"),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5),
  ) +
  labs (fill="Percentage\nEmployees (%)") +
  ggtitle("Correlation Between Job Satisfaction\n                  and Work Life Balance") +
  ylab("Work Life Balance") +
  xlab("Job Satisfaction")

#2. Gender
#Sort Data
GJS1 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(JobSatisfaction == "1")
G0JS1 <- GJS1 %>% 
  filter(Gender == "Male")
G1JS1 <- GJS1 %>% 
  filter(Gender == "Female")

GJS2 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(JobSatisfaction == "2")
G0JS2 <- GJS2 %>% 
  filter(Gender == "Male")
G1JS2 <- GJS2 %>% 
  filter(Gender == "Female")

GJS3 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(JobSatisfaction == "3")
G0JS3 <- GJS3 %>% 
  filter(Gender == "Male")
G1JS3 <- GJS3 %>% 
  filter(Gender == "Female")

GJS4 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(JobSatisfaction == "4")
G0JS4 <- GJS4 %>% 
  filter(Gender == "Male")
G1JS4 <- GJS4 %>% 
  filter(Gender == "Female")

GJSone <- data.frame(JSlevel = "1", gender = "Male", percentage = nrow(G0JS1)*100/nrow(GJS1))
GJStwo <- data.frame(JSlevel = "1", gender = "Female", percentage = nrow(G1JS1)*100/nrow(GJS1))
GJSthree <- data.frame(JSlevel = "2", gender = "Male", percentage = nrow(G0JS2)*100/nrow(GJS2))
GJSfour <- data.frame(JSlevel = "2", gender = "Female", percentage = nrow(G1JS2)*100/nrow(GJS2))
GJSfive <- data.frame(JSlevel = "3", gender = "Male", percentage = nrow(G0JS3)*100/nrow(GJS3))
GJSsix <- data.frame(JSlevel = "3", gender = "Female", percentage = nrow(G1JS3)*100/nrow(GJS3))
GJSseven <- data.frame(JSlevel = "4", gender = "Male", percentage = nrow(G0JS4)*100/nrow(GJS4))
GJSeight <- data.frame(JSlevel = "4", gender = "Female", percentage = nrow(G1JS4)*100/nrow(GJS4))

GenderJS <- rbind(GJSone,GJStwo,GJSthree,GJSfour,GJSfive,GJSsix,GJSseven,GJSeight)

#Percentage Stack Chart
GenderJS %>%
  ggplot(aes(fill=gender, y=percentage, x=JSlevel)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(hjust=0.28,vjust=4, size=10,face="bold"),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5)
  ) +
  labs (fill="Gender") +
  ggtitle("Correlation Between Job Satisfaction and Gender Ratio") +
  ylab("Gender Ratio (%)") +
  xlab("Job Satisfaction")

#3. Department Satisfaction
#Sort Data
DJS1 <- A1_Dataset %>% 
  select(Department) %>%
  filter(JobSatisfaction == "1")
D0JS1 <- DJS1 %>% 
  filter(Department == "Human Resources")
D1JS1 <- DJS1 %>% 
  filter(Department == "Sales")
D2JS1 <- DJS1 %>% 
  filter(Department == "Research & Development")

DJS2 <- A1_Dataset %>% 
  select(Department) %>%
  filter(JobSatisfaction == "2")
D0JS2 <- DJS2 %>% 
  filter(Department == "Human Resources")
D1JS2 <- DJS2 %>% 
  filter(Department == "Sales")
D2JS2 <- DJS2 %>% 
  filter(Department == "Research & Development")

DJS3 <- A1_Dataset %>% 
  select(Department) %>%
  filter(JobSatisfaction == "3")
D0JS3 <- DJS3 %>% 
  filter(Department == "Human Resources")
D1JS3 <- DJS3 %>% 
  filter(Department == "Sales")
D2JS3 <- DJS3 %>% 
  filter(Department == "Research & Development")

DJS4 <- A1_Dataset %>% 
  select(Department) %>%
  filter(JobSatisfaction == "4")
D0JS4 <- DJS4 %>% 
  filter(Department == "Human Resources")
D1JS4 <- DJS4 %>% 
  filter(Department == "Sales")
D2JS4 <- DJS4 %>% 
  filter(Department == "Research & Development")

sumHR <- nrow(D0JS1)+nrow(D0JS2)+nrow(D0JS3)+nrow(D0JS4)
sumSales <- nrow(D1JS1)+nrow(D1JS2)+nrow(D1JS3)+nrow(D1JS4)
sumRND <- nrow(D2JS1)+nrow(D2JS2)+nrow(D2JS3)+nrow(D2JS4)

DJSone <- data.frame(JSlevel = "1", department = "Human Resources", percentage = nrow(D0JS1)*100/sumHR)
DJStwo <- data.frame(JSlevel = "1", department = "Sales", percentage = nrow(D1JS1)*100/sumSales)
DJSthree <- data.frame(JSlevel = "1", department = "Research & Development", percentage = nrow(D2JS1)*100/sumRND)
DJSfour <- data.frame(JSlevel = "2", department = "Human Resources", percentage = nrow(D0JS2)*100/sumHR)
DJSfive <- data.frame(JSlevel = "2", department = "Sales", percentage = nrow(D1JS2)*100/sumSales)
DJSsix <- data.frame(JSlevel = "2", department = "Research & Development", percentage = nrow(D2JS2)*100/sumRND)
DJSseven <- data.frame(JSlevel = "3", department = "Human Resources", percentage = nrow(D0JS3)*100/sumHR)
DJSeight <- data.frame(JSlevel = "3", department = "Sales", percentage = nrow(D1JS3)*100/sumSales)
DJSnine <- data.frame(JSlevel = "3", department = "Research & Development", percentage = nrow(D2JS3)*100/sumRND)
DJSten <- data.frame(JSlevel = "4", department = "Human Resources", percentage = nrow(D0JS4)*100/sumHR)
DJSeleven <- data.frame(JSlevel = "4", department = "Sales", percentage = nrow(D1JS4)*100/sumSales)
DJStwelve <- data.frame(JSlevel = "4", department = "Research & Development", percentage = nrow(D2JS4)*100/sumRND)

DepartmentJS <- rbind(DJSone,DJStwo,DJSthree,DJSfour,DJSfive,DJSsix,DJSseven,DJSeight,DJSnine,DJSten,DJSeleven,DJStwelve)

#Percentage Stack Chart
DepartmentJS %>%
  ggplot(aes(fill=department, y=percentage, x=JSlevel)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(hjust=0.18,vjust=4, size=10,face="bold"),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5)
  ) +
  labs (fill="Department") +
  ggtitle("Correlation Between Job Satisfaction and Department") +
  ylab("Ratio of Department (%)") +
  xlab("Job Satisfaction")

#4. Department Gender
#Sort Data
D0G1 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(Gender == "Male")
D1G1 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(Gender == "Female")

DG2 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(Department == "Human Resources")
D0G2 <- DG2 %>% 
  filter(Gender == "Male")
D1G2 <- DG2 %>% 
  filter(Gender == "Female")

DG3 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(Department == "Sales")
D0G3 <- DG3 %>% 
  filter(Gender == "Male")
D1G3 <- DG3 %>% 
  filter(Gender == "Female")

DG4 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(Department == "Research & Development")
D0G4 <- DG4 %>% 
  filter(Gender == "Male")
D1G4 <- DG4 %>% 
  filter(Gender == "Female")

DGone <- data.frame(department = "All", gender = "Male", percentage = nrow(D0G1)*100/nrow(A1_Dataset))
DGtwo <- data.frame(department = "All", gender = "Female", percentage = nrow(D1G1)*100/nrow(A1_Dataset))
DGthree <- data.frame(department = "Human Resources", gender = "Male", percentage = nrow(D0G2)*100/nrow(DG2))
DGfour <- data.frame(department = "Human Resources", gender = "Female", percentage = nrow(D1G2)*100/nrow(DG2))
DGfive <- data.frame(department = "Sales", gender = "Male", percentage = nrow(D0G3)*100/nrow(DG3))
DGsix <- data.frame(department = "Sales", gender = "Female", percentage = nrow(D1G3)*100/nrow(DG3))
DGseven <- data.frame(department = "Research & Development", gender = "Male", percentage = nrow(D0G4)*100/nrow(DG4))
DGeight <- data.frame(department = "Research & Development", gender = "Female", percentage = nrow(D1G4)*100/nrow(DG4))

dGender <- rbind(DGone,DGtwo,DGthree,DGfour,DGfive,DGsix,DGseven,DGeight)

#Percentage Stack Chart
dGender %>%
  ggplot(aes(fill=gender, y=percentage, x=department)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(hjust=-0.17,vjust=4, size=10,face="bold"),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5)
  ) +
  labs (fill="Gender") +
  ggtitle("Gender Ratios in Each Department") +
  ylab("Gender Ratio (%)") +
  xlab("Department")

#ENVIRONMENT SATISFACTION
#1. Work-Life Balance
#Sort Data
WLJS1 <- A1_Dataset %>% 
  select(WorkLifeBalance) %>%
  filter(EnvironmentSatisfaction == "1")
WL1JS1 <- WLJS1 %>% 
  filter(WorkLifeBalance == "1")
WL2JS1 <- WLJS1 %>% 
  filter(WorkLifeBalance == "2")
WL3JS1 <- WLJS1 %>% 
  filter(WorkLifeBalance == "3")
WL4JS1 <- WLJS1 %>% 
  filter(WorkLifeBalance == "4")

WLJS2 <- A1_Dataset %>% 
  select(WorkLifeBalance) %>%
  filter(EnvironmentSatisfaction == "2")
WL1JS2 <- WLJS2 %>% 
  filter(WorkLifeBalance == "1")
WL2JS2 <- WLJS2 %>% 
  filter(WorkLifeBalance == "2")
WL3JS2 <- WLJS2 %>% 
  filter(WorkLifeBalance == "3")
WL4JS2 <- WLJS2 %>% 
  filter(WorkLifeBalance == "4")

WLJS3 <- A1_Dataset %>% 
  select(WorkLifeBalance) %>%
  filter(EnvironmentSatisfaction == "3")
WL1JS3 <- WLJS3 %>% 
  filter(WorkLifeBalance == "1")
WL2JS3 <- WLJS3 %>% 
  filter(WorkLifeBalance == "2")
WL3JS3 <- WLJS3 %>% 
  filter(WorkLifeBalance == "3")
WL4JS3 <- WLJS3 %>% 
  filter(WorkLifeBalance == "4")

WLJS4 <- A1_Dataset %>% 
  select(WorkLifeBalance) %>%
  filter(EnvironmentSatisfaction == "4")
WL1JS4 <- WLJS4 %>% 
  filter(WorkLifeBalance == "1")
WL2JS4 <- WLJS4 %>% 
  filter(WorkLifeBalance == "2")
WL3JS4 <- WLJS4 %>% 
  filter(WorkLifeBalance == "3")
WL4JS4 <- WLJS4 %>% 
  filter(WorkLifeBalance == "4")

WLJSone <- data.frame(JSlevel = "1", WLlevel = "1", percentage = nrow(WL1JS1)*100/nrow(WLJS1))
WLJStwo <- data.frame(JSlevel = "1", WLlevel = "2", percentage = nrow(WL2JS1)*100/nrow(WLJS1))
WLJSthree <- data.frame(JSlevel = "1", WLlevel = "3", percentage = nrow(WL3JS1)*100/nrow(WLJS1))
WLJSfour <- data.frame(JSlevel = "1", WLlevel = "4", percentage = nrow(WL4JS1)*100/nrow(WLJS1))
WLJSfive <- data.frame(JSlevel = "2", WLlevel = "1", percentage = nrow(WL1JS2)*100/nrow(WLJS2))
WLJSsix <- data.frame(JSlevel = "2", WLlevel = "2", percentage = nrow(WL2JS2)*100/nrow(WLJS2))
WLJSseven <- data.frame(JSlevel = "2", WLlevel = "3", percentage = nrow(WL3JS2)*100/nrow(WLJS2))
WLJSeight <- data.frame(JSlevel = "2", WLlevel = "4", percentage = nrow(WL4JS2)*100/nrow(WLJS2))
WLJSnine <- data.frame(JSlevel = "3", WLlevel = "1", percentage = nrow(WL1JS3)*100/nrow(WLJS3))
WLJSten <- data.frame(JSlevel = "3", WLlevel = "2", percentage = nrow(WL2JS3)*100/nrow(WLJS3))
WLJSeleven <- data.frame(JSlevel = "3", WLlevel = "3", percentage = nrow(WL3JS3)*100/nrow(WLJS3))
WLJStwelve <- data.frame(JSlevel = "3", WLlevel = "4", percentage = nrow(WL4JS3)*100/nrow(WLJS3))
WLJSthirteen <- data.frame(JSlevel = "4", WLlevel = "1", percentage = nrow(WL1JS4)*100/nrow(WLJS4))
WLJSfourteen <- data.frame(JSlevel = "4", WLlevel = "2", percentage = nrow(WL2JS4)*100/nrow(WLJS4))
WLJSfifteen <- data.frame(JSlevel = "4", WLlevel = "3", percentage = nrow(WL3JS4)*100/nrow(WLJS4))
WLJSsixteen <- data.frame(JSlevel = "4", WLlevel = "4", percentage = nrow(WL4JS4)*100/nrow(WLJS4))

worklifeJS <- rbind(WLJSone,WLJStwo,WLJSthree,WLJSfour,WLJSfive,WLJSsix,WLJSseven,WLJSeight,WLJSnine,WLJSten,WLJSeleven,WLJStwelve,WLJSthirteen,WLJSfourteen,WLJSfifteen,WLJSsixteen)

#Heatmap 
worklifeJS %>%
  ggplot(aes(x=JSlevel, y=WLlevel, fill=floor(percentage))) + 
  geom_tile() +
  theme_few() +
  scale_fill_distiller(palette = "RdPu") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(vjust=4, size=10,face="bold"),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5),
  ) +
  labs (fill="Percentage\nEmployees (%)") +
  ggtitle("Correlation Between Environment Satisfaction\nand Work Life Balance") +
  ylab("Work Life Balance") +
  xlab("Environment Satisfaction")

#2. Gender
#Sort Data
GJS1 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(EnvironmentSatisfaction == "1")
G0JS1 <- GJS1 %>% 
  filter(Gender == "Male")
G1JS1 <- GJS1 %>% 
  filter(Gender == "Female")

GJS2 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(EnvironmentSatisfaction == "2")
G0JS2 <- GJS2 %>% 
  filter(Gender == "Male")
G1JS2 <- GJS2 %>% 
  filter(Gender == "Female")

GJS3 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(EnvironmentSatisfaction == "3")
G0JS3 <- GJS3 %>% 
  filter(Gender == "Male")
G1JS3 <- GJS3 %>% 
  filter(Gender == "Female")

GJS4 <- A1_Dataset %>% 
  select(Gender) %>%
  filter(EnvironmentSatisfaction == "4")
G0JS4 <- GJS4 %>% 
  filter(Gender == "Male")
G1JS4 <- GJS4 %>% 
  filter(Gender == "Female")

GJSone <- data.frame(JSlevel = "1", gender = "Male", percentage = nrow(G0JS1)*100/nrow(GJS1))
GJStwo <- data.frame(JSlevel = "1", gender = "Female", percentage = nrow(G1JS1)*100/nrow(GJS1))
GJSthree <- data.frame(JSlevel = "2", gender = "Male", percentage = nrow(G0JS2)*100/nrow(GJS2))
GJSfour <- data.frame(JSlevel = "2", gender = "Female", percentage = nrow(G1JS2)*100/nrow(GJS2))
GJSfive <- data.frame(JSlevel = "3", gender = "Male", percentage = nrow(G0JS3)*100/nrow(GJS3))
GJSsix <- data.frame(JSlevel = "3", gender = "Female", percentage = nrow(G1JS3)*100/nrow(GJS3))
GJSseven <- data.frame(JSlevel = "4", gender = "Male", percentage = nrow(G0JS4)*100/nrow(GJS4))
GJSeight <- data.frame(JSlevel = "4", gender = "Female", percentage = nrow(G1JS4)*100/nrow(GJS4))

GenderJS <- rbind(GJSone,GJStwo,GJSthree,GJSfour,GJSfive,GJSsix,GJSseven,GJSeight)

#Percentage Stack Chart
GenderJS %>%
  ggplot(aes(fill=gender, y=percentage, x=JSlevel)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(vjust=4, size=10,face="bold"),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5)
  ) +
  labs (fill="Gender") +
  ggtitle("Correlation Between Environment Satisfaction\nand Gender Ratio") +
  ylab("Gender Ratio (%)") +
  xlab("Environment Satisfaction")

#3. Department Satisfaction
#Sort Data
DJS1 <- A1_Dataset %>% 
  select(Department) %>%
  filter(EnvironmentSatisfaction == "1")
D0JS1 <- DJS1 %>% 
  filter(Department == "Human Resources")
D1JS1 <- DJS1 %>% 
  filter(Department == "Sales")
D2JS1 <- DJS1 %>% 
  filter(Department == "Research & Development")

DJS2 <- A1_Dataset %>% 
  select(Department) %>%
  filter(EnvironmentSatisfaction == "2")
D0JS2 <- DJS2 %>% 
  filter(Department == "Human Resources")
D1JS2 <- DJS2 %>% 
  filter(Department == "Sales")
D2JS2 <- DJS2 %>% 
  filter(Department == "Research & Development")

DJS3 <- A1_Dataset %>% 
  select(Department) %>%
  filter(EnvironmentSatisfaction == "3")
D0JS3 <- DJS3 %>% 
  filter(Department == "Human Resources")
D1JS3 <- DJS3 %>% 
  filter(Department == "Sales")
D2JS3 <- DJS3 %>% 
  filter(Department == "Research & Development")

DJS4 <- A1_Dataset %>% 
  select(Department) %>%
  filter(EnvironmentSatisfaction == "4")
D0JS4 <- DJS4 %>% 
  filter(Department == "Human Resources")
D1JS4 <- DJS4 %>% 
  filter(Department == "Sales")
D2JS4 <- DJS4 %>% 
  filter(Department == "Research & Development")

sumHR <- nrow(D0JS1)+nrow(D0JS2)+nrow(D0JS3)+nrow(D0JS4)
sumSales <- nrow(D1JS1)+nrow(D1JS2)+nrow(D1JS3)+nrow(D1JS4)
sumRND <- nrow(D2JS1)+nrow(D2JS2)+nrow(D2JS3)+nrow(D2JS4)

DJSone <- data.frame(JSlevel = "1", department = "Human Resources", percentage = nrow(D0JS1)*100/sumHR)
DJStwo <- data.frame(JSlevel = "1", department = "Sales", percentage = nrow(D1JS1)*100/sumSales)
DJSthree <- data.frame(JSlevel = "1", department = "Research & Development", percentage = nrow(D2JS1)*100/sumRND)
DJSfour <- data.frame(JSlevel = "2", department = "Human Resources", percentage = nrow(D0JS2)*100/sumHR)
DJSfive <- data.frame(JSlevel = "2", department = "Sales", percentage = nrow(D1JS2)*100/sumSales)
DJSsix <- data.frame(JSlevel = "2", department = "Research & Development", percentage = nrow(D2JS2)*100/sumRND)
DJSseven <- data.frame(JSlevel = "3", department = "Human Resources", percentage = nrow(D0JS3)*100/sumHR)
DJSeight <- data.frame(JSlevel = "3", department = "Sales", percentage = nrow(D1JS3)*100/sumSales)
DJSnine <- data.frame(JSlevel = "3", department = "Research & Development", percentage = nrow(D2JS3)*100/sumRND)
DJSten <- data.frame(JSlevel = "4", department = "Human Resources", percentage = nrow(D0JS4)*100/sumHR)
DJSeleven <- data.frame(JSlevel = "4", department = "Sales", percentage = nrow(D1JS4)*100/sumSales)
DJStwelve <- data.frame(JSlevel = "4", department = "Research & Development", percentage = nrow(D2JS4)*100/sumRND)

DepartmentJS <- rbind(DJSone,DJStwo,DJSthree,DJSfour,DJSfive,DJSsix,DJSseven,DJSeight,DJSnine,DJSten,DJSeleven,DJStwelve)

#Percentage Stack Chart
DepartmentJS %>%
  ggplot(aes(fill=department, y=percentage, x=JSlevel)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_few() +
  scale_fill_few("Light") +
  theme(
    text = element_text(family="Century Gothic"),
    plot.title = element_text(vjust=4, size=10,face="bold"),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5),
    legend.title = element_text(size=7.5,face="bold"),
    legend.text = element_text(size=7.5)
  ) +
  labs (fill="Department") +
  ggtitle("Correlation Between Environment Satisfaction\nand Department") +
  ylab("Ratio of Department (%)") +
  xlab("Environment Satisfaction")


#Factor 3: Financial Motivations
#1.Monthly Income
#Choose Data
mIncomeLow <- A1_Dataset %>% 
  select(MonthlyIncome) %>%
  filter(HighPerformance == "0") 
mIncomeHigh <- A1_Dataset %>% 
  select(MonthlyIncome) %>%
  filter(HighPerformance == "1")

mAll <- data.frame(group = "All", value = MonthlyIncome[,1])
mLow <- data.frame(group = "Satisfactory", value = mIncomeLow[,1])
mHigh <- data.frame(group = "High", value = mIncomeHigh[,1])

mPerformance <- rbind(mAll, mLow, mHigh)

#Box Plot
mPerformance %>%
  ggplot(aes(x=group, y=MonthlyIncome, fill=group)) + 
  geom_boxplot() + 
  geom_jitter(color="darkgrey", size=0.4, alpha=0.7) +
  theme_fivethirtyeight() +
  scale_fill_few("Light") +
  theme(
    legend.position="none",
    text = element_text(family="Century Gothic"),
    plot.title = element_text(hjust=3.5,vjust=4, size=11.5),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5)
  ) +
  ggtitle("Impact of Monthly Income on Performance") +
  ylab("Monthly Income ($)") +
  xlab("Performance Group")

#2.Salary Increase
#Choose Data
sSalaryLow <- A1_Dataset %>% 
  select(SalaryIncrease) %>%
  filter(HighPerformance == "0") 
sSalaryHigh <- A1_Dataset %>% 
  select(SalaryIncrease) %>%
  filter(HighPerformance == "1")

sAll <- data.frame(group1 = "All", value = SalaryIncrease[,1])
sLow <- data.frame(group1 = "Satisfactory", value = sSalaryLow[,1])
sHigh <- data.frame(group1 = "High", value = sSalaryHigh[,1])

sPerformance <- rbind(sAll, sLow, sHigh)

#Box Plot
sPerformance %>%
  ggplot(aes(x=group1, y=SalaryIncrease, fill=group1)) + 
  geom_boxplot() + 
  geom_jitter(color="darkgrey", size=0.4, alpha=0.7) +
  theme_fivethirtyeight() +
  scale_fill_few("Light") +
  theme(
    legend.position="none",
    text = element_text(family="Century Gothic"),
    plot.title = element_text(hjust=-3.5, size=11.5),
    axis.title.y = element_text(size=9, face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5)
  ) +
  ggtitle("Impact of Salary Increase on Performance") +
  ylab("Last Salary Increase (%)") +
  xlab("Performance Group")

#3.Age and Total Working Years
#Age vs Income Scatter
A1_Dataset %>%
  ggplot(aes(x=Age, y=MonthlyIncome)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_fivethirtyeight() +
  theme(
    legend.position="none",
    text = element_text(family="Century Gothic"),
    plot.title = element_text(hjust=1.2,vjust=4, size=11.5),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5)
  ) +
  ggtitle("Correlation between Age and Monthly Income") +
  ylab("Monthly Income ($)") +
  xlab("Age (Years)")

#Total Working Years vs Income Scatter
A1_Dataset %>%
  ggplot(aes(x=TotalWorkingYears, y=MonthlyIncome)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_fivethirtyeight() +
  theme(
    legend.position="none",
    text = element_text(family="Century Gothic"),
    plot.title = element_text(hjust=-2, vjust=5, size=10.5),
    axis.title.y = element_text(size=9,face="bold"),
    axis.title.x = element_text(vjust= -2.2, size=9,face="bold"),
    axis.text = element_text(size = 7.5)
  ) +
  ggtitle("Correlation between Total Working Years\n                      and Monthly Income") +
  ylab("Monthly Income ($)") +
  xlab("Total Working Years (Years)")