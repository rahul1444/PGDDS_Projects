######### HR CASE STUDY #########

##### STEPS INVOLVED #########
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation


### Business Understanding:

# Based on the past and current employee information,
# the company has maintained a database containing personal, performance and other survey information
# 

## AIM:

# The aim is to automate the process of predicting 
# the probability of a employee leaving the company and to find the factors affecting the attrition 
# 


################################################################

## set the working directory
getwd()
setwd("C:/Users/Sir 'M'/Desktop/Upgrad/Assignment/HRA")
getwd()


## install required packages

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("lubridate")
#install.packages("plotrix")
#install.packages("e1071")
#install.packages("caret")
#install.packages("MASS")
#install.packages("car")
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("reshape2")

## load required Libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(plotrix)
library(e1071)
library(lattice)
library(caret)
library(MASS)
library(car)
library(cowplot)
library(GGally)
library(reshape)
library(ROCR)
library(caTools)

### DATA UNDERSTANDING, PREPARATION AND EDA

## Reading data files

in.time <- read.csv("in_time.csv", stringsAsFactors = FALSE)
out.time <- read.csv("out_time.csv", stringsAsFactors = FALSE)
gen.data <- read.csv("general_data.csv", stringsAsFactors = FALSE)
emp.sur <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
man.sur <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)


### 1. DATA PREPARTION & CHECKS

## checking if employee id is unique and same in all data files

length(unique(tolower(gen.data$EmployeeID)))    # 4410, confirming EmployeeID is key

length(unique(tolower(emp.sur$EmployeeID)))    # 4410, confirming EmployeeID is key 

length(unique(tolower(man.sur$EmployeeID)))    # 4410, confirming EmployeeID is key 

length(unique(tolower(in.time$X)))    # 4410, confirming EmployeeID is key 

length(unique(tolower(out.time$X)))    # 4410, confirming EmployeeID is key 

## Checking if EmployeeID is same across

setdiff(gen.data$EmployeeID,emp.sur$EmployeeID) # Identical EmployeeID across these datasets

setdiff(gen.data$EmployeeID,man.sur$EmployeeID) # Identical EmployeeID across these datasets

setdiff(gen.data$EmployeeID,in.time$X) # Identical EmployeeID across these datasets

setdiff(gen.data$EmployeeID,out.time$X) # Identical EmployeeID across these datasets


## Merge general data,employee and manager survey data together

hra <- merge(gen.data, emp.sur, by = "EmployeeID")
#View(hra)
hra <- merge(hra, man.sur, by = "EmployeeID")
#View(hra)


## Calculating Average working hours field using in.time and out.time data
## log in and log out time is given for year 2015 for each employee

in.time <- sapply(in.time, function(x) as.POSIXlt(x, origin="1970-01-01",format = "%Y-%m-%d %H:%M:%S"))
out.time <- sapply(out.time, function(x) as.POSIXlt(x, origin="1970-01-01",format = "%Y-%m-%d %H:%M:%S"))

class(in.time)
class(out.time)


# converting it to a data frame

in.time <- as.data.frame(in.time)
in.time <- in.time[,-1]
out.time <- as.data.frame(out.time)
out.time <- out.time[,-1]


# check for na

sum(is.na(in.time)) #109080 of nas in in time
sum(is.na(out.time))#109080 ie same number of nas in out time

table(which(is.na(in.time)) == which(is.na(out.time))) #location of nas is same in both dataframes


# Deriving metrics for analysis

# creating derived variable Employee Leave count

emp.na <- rowSums(is.na(in.time)) # if we consider na as no show by an employee then row wise sum will give no. of no shows by employee
day.na <- colSums(is.na(in.time)) == nrow(in.time) #finding no of cols with all nas
table(day.na) # FALSE 249 TRUE 12 so there are 12 days with all employee on leave - so safe to consider as holiday
emp.leave <- data.frame(emp.na - 12)
colnames(emp.leave) <- "Emp.Leaves"
par(mar=c(1,1,1,1)) # initial set for plots
hist(emp.leave$Emp.Leaves)

#View(in.time)
#View(out.time)

work_hr <- out.time-in.time # calculating working hrs per day

#View(work_hr)

work_hr_re <- work_hr[, c(colSums(is.na(work_hr)) != nrow(work_hr))] #retaining only those which do not have all NAs
work_hr_re <- work_hr_re[,-1] #remove first column
#View(work_hr_re)

class(work_hr_re[1,1]) #class is diff time changing it to numeric

work_hr_re <- sapply(work_hr_re, function (x) as.numeric(x))

Avg_workhr <- rowMeans(work_hr_re,na.rm = TRUE) # row wise average to calculate average working hours
hist(Avg_workhr)
#View(Avg_workhr)

hra$Emp.LeaveCount <- emp.leave   #add employee leave count
hra$Avg_workhr <- Avg_workhr      # add average working hour

# derived metric for employee working hour maintenance

# Seggregating good employee with the average hours >= 8 and < 8 as bad

hra$EmpWrkHrMaintenace <- ifelse(hra$Avg_workhr >= 8,"Good","Bad")


# deriving numeric ratio metrics for analysis

# MonthlyIncome to TotalWorkingYears Ratio

hra$IncometoExprnc <- hra$MonthlyIncome/hra$TotalWorkingYears

# YearsAtCompany to YearsWithCurrManager ratio

hra$ComYrstoMgrYrs <- hra$YearsAtCompany/hra$YearsWithCurrManager



View(hra)

#checking on nas along column
NaCol <- colSums(is.na(hra))
View (NaCol)# below five columns have nas
#WorkLifeBalance 38
#EnvironmentSatisfaction 25
#JobSatisfaction 20
#NumCompaniesWorked 19
#TotalWorkingYears 9

# TotalWorkingYears and NumCompaniesWorked na's treatment 
# we assume na means employee is freasher with no expr and this one is his first company

hra$NumCompaniesWorked[is.na(hra$NumCompaniesWorked)] <- 0
hra$TotalWorkingYears[is.na(hra$TotalWorkingYears)] <- 0

hra$IncometoExprnc[is.na(hra$IncometoExprnc)] <- 0  # if denominator is 0 or NA then we get NA so replacing it with 0

hra$ComYrstoMgrYrs[is.na(hra$ComYrstoMgrYrs)] <- 0  # if denominator is 0 or Na then we get NA so replacing it with 0

# JobSatisfaction na treatment 4 levels 1, low 2. medium 3. high 4. veryhigh
#adding 5th no response with value 0

hra$JobSatisfaction[is.na(hra$JobSatisfaction)] <- 0

# EnvironmentSatisfaction na treatment 4 levels 1, low 2. medium 3. high 4. veryhigh
#adding 5th no response with value 0

hra$EnvironmentSatisfaction[is.na(hra$EnvironmentSatisfaction)] <- 0

# WorkLifeBalance na treatment 4 levels 1, bad 2. good 3. better 4. best
#adding 5th no response with value 0

hra$WorkLifeBalance[is.na(hra$WorkLifeBalance)] <- 0

### adding given levels to categorical variables

hra$Education[which(hra$Education==1)]<-'Below College'
hra$Education[which(hra$Education==2)]<-'College'
hra$Education[which(hra$Education==3)]<-'Bachelor'
hra$Education[which(hra$Education==4)]<-'Master'
hra$Education[which(hra$Education==5)]<-'Doctor'

hra$EnvironmentSatisfaction[which(hra$EnvironmentSatisfaction==0)]<-'No response'
hra$EnvironmentSatisfaction[which(hra$EnvironmentSatisfaction==1)]<-'Low'
hra$EnvironmentSatisfaction[which(hra$EnvironmentSatisfaction==2)]<-'Medium'
hra$EnvironmentSatisfaction[which(hra$EnvironmentSatisfaction==3)]<-'High'
hra$EnvironmentSatisfaction[which(hra$EnvironmentSatisfaction==4)]<-'Very High'

hra$JobInvolvement[which(hra$JobInvolvement==1)]<-'Low'
hra$JobInvolvement[which(hra$JobInvolvement==2)]<-'Medium'
hra$JobInvolvement[which(hra$JobInvolvement==3)]<-'High'
hra$JobInvolvement[which(hra$JobInvolvement==4)]<-'Very High'

hra$JobSatisfaction[which(hra$JobSatisfaction==0)]<-'No response'
hra$JobSatisfaction[which(hra$JobSatisfaction==1)]<-'Low'
hra$JobSatisfaction[which(hra$JobSatisfaction==2)]<-'Medium'
hra$JobSatisfaction[which(hra$JobSatisfaction==3)]<-'High'
hra$JobSatisfaction[which(hra$JobSatisfaction==4)]<-'Very High'

hra$WorkLifeBalance[which(hra$WorkLifeBalance==0)]<-'No Response'
hra$WorkLifeBalance[which(hra$WorkLifeBalance==1)]<-'Bad'
hra$WorkLifeBalance[which(hra$WorkLifeBalance==2)]<-'Good'
hra$WorkLifeBalance[which(hra$WorkLifeBalance==3)]<-'Better'
hra$WorkLifeBalance[which(hra$WorkLifeBalance==4)]<-'Best'

hra$PerformanceRating[which(hra$PerformanceRating==1)]<-'Low'
hra$PerformanceRating[which(hra$PerformanceRating==2)]<-'Good'
hra$PerformanceRating[which(hra$PerformanceRating==3)]<-'Excellent'
hra$PerformanceRating[which(hra$PerformanceRating==4)]<-'Outstanding'

hra$JobLevel[which(hra$JobLevel==1)]<-'Lv1'
hra$JobLevel[which(hra$JobLevel==2)]<-'Lv2'
hra$JobLevel[which(hra$JobLevel==3)]<-'Lv3'
hra$JobLevel[which(hra$JobLevel==4)]<-'Lv4'
hra$JobLevel[which(hra$JobLevel==5)]<-'Lv5'




View(hra) # visual check on data
str(hra) # structure of data


## Over18, Standard hours and employee count are not useful as they repeat same data for all employees
hra <- hra[,!names(hra) %in% c("EmployeeID","Over18", "StandardHours", "EmployeeCount")]
View(hra) # visual check on data


## 2. Exploratory Data Analysis

# Barcharts for categorical features with stacked attrition information

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")
bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Chart 1 atrrition count by business travel and dept
plot_grid(ggplot(hra, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme, 
          ggplot(hra, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

# Chart 1 For BusinessTravel-Travel Rarely and Department - Resarch and Development the attrition count is quite high

# Chart 2 attrition count by education level, field, gender and job level
plot_grid(ggplot(hra, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hra, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hra, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hra, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

# Chart 2 Education Levels-2,3,4 college, Bachelor, Master, EducationField- Lifesciences and Medical, Gender-Male
# Chart 2 and JobLevel- 1 & 2 have some notable count of attrition 

# Chart 3 attrition by job role and marital status

plot_grid(ggplot(hra, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme, 
          ggplot(hra, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

# Chart 3 MaritalStatus - Single and Married, JobRole - Research Scientist, Sales Executive and Labaratory Technician
# Chart 3 have significant count of attrition 

# Chart 4 attrition by worklife balance, job involvement, performance rating, environment satisfaction , job satisfaction, stock option level
plot_grid(ggplot(hra, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme, 
          ggplot(hra, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hra, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hra, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hra, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hra, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  

# Chart 4 Performance Rating - 3, StockOptionLevel - 1 and 2, JobInvolvement level-3, WorkLifeBalance level-3 seem to 
# Chart 4 have some count of attrition among other categories
# Chart 4 count of no response remain relatively very low 

plot_grid(ggplot(hra, aes(x=EmpWrkHrMaintenace,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 


# Histogram and Boxplots for numeric variables 
# Chart 5
plot_grid(ggplot(hra, aes(Age))+ geom_histogram(binwidth = 10,fill="greenyellow",col="green4"),
          ggplot(hra, aes(DistanceFromHome))+ geom_histogram(binwidth = 5,fill="greenyellow",col="green4"),
          ggplot(hra, aes(MonthlyIncome))+ geom_histogram(binwidth = 10000,fill="greenyellow",col="green4"),
          ggplot(hra, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 3,fill="greenyellow",col="green4"),
          align = "h")

# Chart 5from above plot we infer that most employees are aged in 30s and 40s, with DistanceFromHome Less than 10 kms 
# Chart 5with MonthlyIncome less than 1lakh and number of previous companies less than 5

# Chart 6
plot_grid(ggplot(hra, aes(PercentSalaryHike))+ geom_histogram(binwidth = 1,fill="greenyellow",col="green4"),
          ggplot(hra, aes(TotalWorkingYears))+ geom_histogram(binwidth = 5,fill="greenyellow",col="green4"),
          ggplot(hra, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 1,fill="greenyellow",col="green4"),
          ggplot(hra, aes(YearsAtCompany))+ geom_histogram(binwidth = 1,fill="greenyellow",col="green4"),
          align = "h")

# Chart 6 from above plot we infer that for most of the employees PercentSalaryHike is less than 15% in previous year,
# Chart 6 their TotalWorkingYears is less than 10 for most, TrainingTimesLastYear is between 2 to 4 and YearsAtCompany
# Chart 6 is less than 10 for most

# Chart 7
plot_grid(ggplot(hra, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 1,fill="greenyellow",col="green4"),
          ggplot(hra, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 5,fill="greenyellow",col="green4"),
          ggplot(hra, aes(IncometoExprnc))+ geom_histogram(binwidth = 10000,fill="greenyellow",col="green4"),
          ggplot(hra, aes(ComYrstoMgrYrs))+ geom_histogram(binwidth = 5,fill="greenyellow",col="green4"),
          align = "h")

# Chart 7 from above plot we infer that YearsSinceLastPromotion is <3 for most and YearsWithCurrManager is <5


# Chart 8 box plots to check outliers

box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(hra, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip(),
          ggplot(hra, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip(), 
          ggplot(hra, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip(), 
          ggplot(hra, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip(), 
          align = "v")

# Chart 8 As we observe from above plot MonthlyIncome and NumCompaniesWorked have outliers

#Chart 9
plot_grid(ggplot(hra, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip(),
          ggplot(hra, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip(), 
          ggplot(hra, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip(), 
          ggplot(hra, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip(), 
          align = "v")

#Chart 9 As we observe from above plot TotalWorkingYears, TrainingTimesLastYear and YearsAtCompany have outliers

#Chart 10
plot_grid(ggplot(hra, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip(),
          ggplot(hra, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip(),
          ggplot(hra, aes(x="",y=IncometoExprnc))+ geom_boxplot(width=0.1)+coord_flip(), 
          ggplot(hra, aes(x="",y=ComYrstoMgrYrs))+ geom_boxplot(width=0.1)+coord_flip(), 
          align = "v")

#Chart 10 As we observe from above plot YearsSinceLastPromotion and YearsWithCurrManager have outliers

#Chart 11 Boxplots of numeric variables relative to Attrition status

plot_grid(ggplot(hra, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hra, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hra, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hra, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

#Chart 11 From above plot we observe that most of attrition is with employees  of Age < 40, MonthlyIncome < 75, and increases as NumCompaniesWorked  
#Chart 11 increases and is slightly icreases with increasing DistanceFromHome

#Chart 12
plot_grid(ggplot(hra, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hra, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hra, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hra, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

#Chart12 From above plot we observe that attrition is when PercentSalaryHike < 20, TotalWorkingYears is around 10,YearsAtCompany < 10 and 
#Chart12 doesn't vary much with TrainingTimesLastYear

#Chart 13
plot_grid(ggplot(hra, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hra, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hra, aes(x=Attrition,y=IncometoExprnc, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hra, aes(x=Attrition,y=ComYrstoMgrYrs, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          
          align = "v",nrow = 1)

#Chart 13 From above plot we observe that attrition is when YearsSinceLastPromotion < 3, YearsWithCurrManager < 6

### Treatment of outlliers

#check and remove outliers custom functions

checkOutliers <- function(dt, var) {  # function to check any variables have outliers or not with mean , percentage of outliers
  var_name <- eval(substitute(var),eval(hra))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
}

remove_outliers <- function(x, na.rm = TRUE, ...) { #function is used to remove outliers and  replace of outliers with .05-.95 values 
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  y[x < (qnt[1] - H)] <- caps[1]
  y[x > (qnt[2] + H)] <- caps[2]
  y
}

checkOutliers(hra,Age)  #no outliers

checkOutliers(hra,DistanceFromHome) #no outliers

checkOutliers(hra,MonthlyIncome) #Outliers identified: 342 from 4410 observations
hra$MonthlyIncome <- remove_outliers(hra$MonthlyIncome)

checkOutliers(hra,NumCompaniesWorked) #Outliers identified: 156 from 4410 observations
hra$NumCompaniesWorked <- remove_outliers(hra$NumCompaniesWorked)

checkOutliers(hra,PercentSalaryHike) #no outliers

checkOutliers(hra,TotalWorkingYears) #Outliers identified: 189 from 4410 observations
hra$TotalWorkingYears <- remove_outliers(hra$TotalWorkingYears)

checkOutliers(hra,TrainingTimesLastYear) #Outliers identified: 714 from 4410 observations
hra$TrainingTimesLastYear <- remove_outliers(hra$TrainingTimesLastYear)

checkOutliers(hra,YearsAtCompany) #Outliers identified: 312 from 4410 observations
hra$YearsAtCompany <- remove_outliers(hra$YearsAtCompany)

checkOutliers(hra,YearsSinceLastPromotion) #Outliers identified: 321 from 4410 observations
hra$YearsSinceLastPromotion <- remove_outliers(hra$YearsSinceLastPromotion)

checkOutliers(hra,YearsWithCurrManager) #Outliers identified: 42 from 4410 observations
hra$YearsWithCurrManager <- remove_outliers(hra$YearsWithCurrManager)

checkOutliers(hra,IncometoExprnc) #Outliers identified
hra$IncometoExprnc <- remove_outliers(hra$IncometoExprnc)

checkOutliers(hra,ComYrstoMgrYrs) #Outliers identified
hra$ComYrstoMgrYrs <- remove_outliers(hra$ComYrstoMgrYrs)

# converting target variable telecom from No/Yes character to factor with levels 0/1 

hra$Attrition <- ifelse(hra$Attrition=="Yes",1,0)

# Checking churn rate of prospect customer

Attrition <- sum(hra$Attrition)/nrow(hra)
Attrition # 16.12% churn rate. 

## splitting data frame into categorical and numerical
cat_hra <- hra[,which(names(hra) %in% c("JobLevel","BusinessTravel","Department","Education","EducationField","Gender","JobRole","MaritalStatus","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","JobInvolvement","PerformanceRating","EmpWrkHrMaintenace"))]
View(cat_hra)
num_hra <-hra[,-which(names(hra) %in% c("JobLevel","BusinessTravel","Department","Education","EducationField","Gender","JobRole","MaritalStatus","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","JobInvolvement","PerformanceRating","EmpWrkHrMaintenace"))] 
View(num_hra)


## creating dummies for categorical variables
dummy_all <- data.frame(sapply(cat_hra, function (x) data.frame(model.matrix(~x-1, data = cat_hra))))
View(dummy_all)



## Quick correlation matrix for numeric variables
summary(num_hra)
# removing columns with just one thing repeated for all employees
cor_num_hra <- num_hra
cor_num_hra <- sapply(cor_num_hra, function(x) as.numeric(unlist(x))) #converting them to numerics

#View(cor_num_hra)
mat_cor <- cor(cor_num_hra) #correlation matrix
#View(mat_cor)
#correlation visualisation using ggplot and reshape
m <- melt(mat_cor)
ggplot(data = m, aes(x=X1,y=X2,fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Above plot very few correlations among variables, particularly greater correlation in YEars with current manager, years since last promotion, years at company, age, total working hours and employee leave count

## Scaling of all numeric variables
names(num_hra)

num_hra$Age <- scale(num_hra$Age,center = TRUE,scale = TRUE)
num_hra$DistanceFromHome <- scale(num_hra$DistanceFromHome,center = TRUE,scale = TRUE)
num_hra$MonthlyIncome <- scale(num_hra$MonthlyIncome,center = TRUE,scale = TRUE)
num_hra$NumCompaniesWorked <- scale(num_hra$NumCompaniesWorked,center = TRUE,scale = TRUE)
num_hra$PercentSalaryHike <- scale(num_hra$PercentSalaryHike,center = TRUE,scale = TRUE)
num_hra$StockOptionLevel <- scale(num_hra$StockOptionLevel,center = TRUE,scale = TRUE)
num_hra$TotalWorkingYears <- scale(num_hra$TotalWorkingYears,center = TRUE,scale = TRUE)
num_hra$TrainingTimesLastYear <- scale(num_hra$TrainingTimesLastYear,center = TRUE,scale = TRUE)
num_hra$YearsAtCompany <- scale(num_hra$YearsAtCompany,center = TRUE,scale = TRUE)
num_hra$YearsSinceLastPromotion <- scale(num_hra$YearsSinceLastPromotion,center = TRUE,scale = TRUE)
num_hra$YearsWithCurrManager <- scale(num_hra$YearsWithCurrManager,center = TRUE,scale = TRUE)
num_hra$Emp.LeaveCount <- scale(num_hra$Emp.LeaveCount,center = TRUE,scale = TRUE)
num_hra$Avg_workhr <- scale(num_hra$Avg_workhr,center = TRUE,scale = TRUE)
num_hra$IncometoExprnc <- scale(num_hra$IncometoExprnc,center = TRUE,scale = TRUE)
num_hra$ComYrstoMgrYrs <- scale(num_hra$ComYrstoMgrYrs,center = TRUE,scale = TRUE)

## creating final data set for analysis
#class(dummy_all)
#class(num_hra)

fhra <- merge(dummy_all,num_hra, by = 0) #final data set
fhra <- fhra[,-1] # removing rownames column



#### MODEL BUILDING ###########

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(fhra$Attrition, SplitRatio = 0.7)

train = fhra[indices,]

test = fhra[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model

model_1 = glm(Attrition ~ ., data = train, family = "binomial")

summary(model_1) #AIC 2136.7  nullDev 2728.0  resDev 2014.7


# Stepwise selection

model_2<- stepAIC(model_1, direction="both")

summary(model_2) #AIC 2096.5  nullDev 2728.0  resDev 2032.5

# Removing multicollinearity through VIF check

vif(model_2)

# JobLevel.xLv2 has highest vif value  8.134387 but its significant. the next highest JobLevel.xLv1  7.772565 is less significant
# among both. So let's remove JobLevel.xLv1

# Excluding JobLevel.xLv1
model3 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + JobLevel.xLv2 + 
                JobLevel.xLv3 + JobLevel.xLv4 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xDivorced + 
                MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                WorkLifeBalance.xGood + JobInvolvement.xHigh + EmpWrkHrMaintenace.xBad + 
                Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + IncometoExprnc + 
                ComYrstoMgrYrs + EnvironmentSatisfaction.xVery.High, family = "binomial", 
              data = train)


summary(model3) #AIC 2098.2  nullDev 2728.0  resDev 2036.2

vif(model3)  # vif of all variables is low below 3. So we can start removing variables based on p-value

# JobLevel.xLv3 p:0.752442 has highest p-value. So we can remove JobLevel.xLv3

#Excluding JobLevel.xLv3
model4 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + JobLevel.xLv2 + 
                JobLevel.xLv4 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xDivorced + 
                MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                WorkLifeBalance.xGood + JobInvolvement.xHigh + EmpWrkHrMaintenace.xBad + 
                Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + IncometoExprnc + 
                ComYrstoMgrYrs + EnvironmentSatisfaction.xVery.High, family = "binomial", 
              data = train)

summary(model4) #AIC 2096.3  nullDev 2728.0  resDev 2036.3

# JobLevel.xLv4 p:0.500265 has highest p-value. So we can remove JobLevel.xLv4


#Excluding JobLevel.xLv4
model5 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + JobLevel.xLv2 + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xDivorced + 
                MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                WorkLifeBalance.xGood + JobInvolvement.xHigh + EmpWrkHrMaintenace.xBad + 
                Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + IncometoExprnc + 
                ComYrstoMgrYrs + EnvironmentSatisfaction.xVery.High, family = "binomial", 
              data = train)

summary(model5) #AIC 2094.7  nullDev 2728.0  resDev 2036.7

# EducationField.xMedical p:0.103238 has highest p-value. So we can remove EducationField.xMedical

#Excluding EducationField.xMedical
model6 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                JobLevel.xLv2 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xDivorced + 
                MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                WorkLifeBalance.xGood + JobInvolvement.xHigh + EmpWrkHrMaintenace.xBad + 
                Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + IncometoExprnc + 
                ComYrstoMgrYrs + EnvironmentSatisfaction.xVery.High, family = "binomial", 
              data = train)

summary(model6) #AIC 2095.4  nullDev 2728.0  resDev 2039.4

# WorkLifeBalance.xGood p:0.083779 has highest p-value. So we can remove WorkLifeBalance.xGood

#Excluding WorkLifeBalance.xGood
model7 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                JobLevel.xLv2 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xDivorced + 
                MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                JobInvolvement.xHigh + EmpWrkHrMaintenace.xBad + 
                Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + IncometoExprnc + 
                ComYrstoMgrYrs + EnvironmentSatisfaction.xVery.High, family = "binomial", 
              data = train)

summary(model7) #AIC 2096.3  nullDev 2728.0  resDev 2042.3

# EnvironmentSatisfaction.xVery.High  p:0.050654 has highest p-value. So we can remove EnvironmentSatisfaction.xVery.High

#Excluding EnvironmentSatisfaction.xVery.High
model8 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                JobLevel.xLv2 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xDivorced + 
                MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                JobInvolvement.xHigh + EmpWrkHrMaintenace.xBad + 
                Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + IncometoExprnc + 
                ComYrstoMgrYrs, family = "binomial", 
              data = train)

summary(model8) #AIC 2098.2  nullDev 2728.0  resDev 2046.2

# ComYrstoMgrYrs  p:0.05028 has highest p-value. So we can remove ComYrstoMgrYrs

#Excluding ComYrstoMgrYrs
model9 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                JobLevel.xLv2 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xDivorced + 
                MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                JobInvolvement.xHigh + EmpWrkHrMaintenace.xBad + 
                Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + IncometoExprnc , family = "binomial", 
              data = train)

summary(model9) #AIC 2100  nullDev 2728.0  resDev 2050

# JobRole.xSales.Executive  p:0.066386 has highest p-value. So we can remove JobRole.xSales.Executive

#Excluding JobRole.xSales.Executive
model10 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                JobLevel.xLv2 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xDivorced + 
                MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                JobInvolvement.xHigh + EmpWrkHrMaintenace.xBad + 
                Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + IncometoExprnc , family = "binomial", 
              data = train)

summary(model10) #AIC 2101.4  nullDev 2728.0  resDev 2053.4

# MonthlyIncome  p:0.06884 has highest p-value. So we can remove MonthlyIncome

#Excluding MonthlyIncome
model11 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                 JobLevel.xLv2 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xDivorced + 
                 MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 JobInvolvement.xHigh + EmpWrkHrMaintenace.xBad + 
                 Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + IncometoExprnc , family = "binomial", 
               data = train)

summary(model11) #AIC 2102.7  nullDev 2728.0  resDev 2056.7

# IncometoExprnc  p:0.33023 has highest p-value. So we can remove IncometoExprnc

#Excluding IncometoExprnc
model12 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                 JobLevel.xLv2 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xDivorced + 
                 MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 JobInvolvement.xHigh + EmpWrkHrMaintenace.xBad + 
                 Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager , family = "binomial", 
               data = train)

summary(model12) #AIC 2101.7  nullDev 2728.0  resDev 2057.7

# EducationField.xLife.Sciences   p:0.06047 has highest p-value. So we can remove EducationField.xLife.Sciences 


#Excluding EducationField.xLife.Sciences
model13 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources +  
                 JobLevel.xLv2 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xDivorced + 
                 MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 JobInvolvement.xHigh + EmpWrkHrMaintenace.xBad + 
                 Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager , family = "binomial", 
               data = train)

summary(model13) #AIC 2103.2  nullDev 2728.0  resDev 2061.2

# JobInvolvement.xHigh   p:0.03011 has highest p-value. So we can remove JobInvolvement.xHigh 

#Excluding JobInvolvement.xHigh
model14 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources +  
                 JobLevel.xLv2 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xDivorced + 
                 MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 EmpWrkHrMaintenace.xBad + Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager , family = "binomial", 
               data = train)

summary(model14) #AIC 2105.9  nullDev 2728.0  resDev 2065.9

# JobRole.xResearch.Director   p:0.01755 has highest p-value. So we can remove JobRole.xResearch.Director 

#Excluding JobRole.xResearch.Director
model15 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources + JobLevel.xLv2 + JobRole.xManufacturing.Director + 
                 MaritalStatus.xDivorced + MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 EmpWrkHrMaintenace.xBad + Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager , family = "binomial", 
               data = train)

summary(model15) #AIC 2109.3  nullDev 2728.0  resDev 2071.3

# JobLevel.xLv2    p:0.00560 has highest p-value. So we can remove JobLevel.xLv2  

#Excluding JobLevel.xLv2
model16 <- glm(formula = Attrition ~ BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources + JobRole.xManufacturing.Director + 
                 MaritalStatus.xDivorced + MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 EmpWrkHrMaintenace.xBad + Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager , family = "binomial", 
               data = train)

summary(model16) #AIC 2114.9  nullDev 2728.0  resDev 2078.9

# BusinessTravel.xNon.Travel    p:0.00524 has highest p-value. So we can remove BusinessTravel.xNon.Travel 

#Excluding BusinessTravel.xNon.Travel
model17 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources + JobRole.xManufacturing.Director + 
                 MaritalStatus.xDivorced + MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 EmpWrkHrMaintenace.xBad + Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager , family = "binomial", 
               data = train)

summary(model17) #AIC 2121.6  nullDev 2728.0  resDev 2087.6

# JobSatisfaction.xMedium    p:0.00150 has highest p-value. So we can remove JobSatisfaction.xMedium 

#Excluding JobSatisfaction.xMedium
model18 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources + JobRole.xManufacturing.Director + 
                 MaritalStatus.xDivorced + MaritalStatus.xMarried + EnvironmentSatisfaction.xLow + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + WorkLifeBalance.xBad + 
                 EmpWrkHrMaintenace.xBad + Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager , family = "binomial", 
               data = train)

summary(model18) #AIC 2129.6  nullDev 2728.0  resDev 2097.6

# JobSatisfaction.xHigh    p:0.04491 has highest p-value. So we can remove JobSatisfaction.xHigh 

#Excluding JobSatisfaction.xHigh
model19 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources + JobRole.xManufacturing.Director + 
                 MaritalStatus.xDivorced + MaritalStatus.xMarried + EnvironmentSatisfaction.xLow +  
                 JobSatisfaction.xLow + WorkLifeBalance.xBad + 
                 EmpWrkHrMaintenace.xBad + Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager , family = "binomial", 
               data = train)

summary(model19) #AIC 2131.6  nullDev 2728.0  resDev 2101.6

# Now all the variables in the model are significant to the level of  0.001. Hence we can stop removing variables based on p-value.
# Now lets check again if there are any variables still with high vif

vif(model19) # all variables are with low vif values too


# Let's keep this as our final model

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

attr_colno <- which( colnames(test)=="Attrition" )

test_pred = predict(model19, type = "response", newdata = test[,-attr_colno])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred

View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrn <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrn <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrn,test_pred_attrn)


#######################################################################
test_pred_attrn <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))



test_conf <- confusionMatrix(test_pred_attrn, test_actual_attrn, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrn, test_actual_attrn, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Let's choose a cutoff value of 0.1855556 for final model

test_cutoff_attrn <- factor(ifelse(test_pred >=0.1855556, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrn, test_actual_attrn, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec


### KS -statistic - Test Data ######

test_cutoff_attrn <- ifelse(test_cutoff_attrn=="Yes",1,0)
test_actual_attrn <- ifelse(test_actual_attrn=="Yes",1,0)



pred_object_test<- prediction(test_cutoff_attrn, test_actual_attrn)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)



# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrn, test_pred, groups = 10)
Attrition_decile

#Plotting the charts

plot(performance_measures_test, main="ROC curve", colorize=T)

# And then a lift chart
perf <- performance(pred_object_test,"lift","rpp")
plot(perf, main="lift curve", colorize=T)
