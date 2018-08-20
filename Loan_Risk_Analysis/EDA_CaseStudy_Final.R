################ EDA Case Study - Lending Club Loan Data Analysis ################

#Required Libraries

install.packages(c("dplyr", "tidyr", "stringi", "stringr", "ggplot2", "reshape2", "lubridate", "DescTools"))
install.packages("gridExtra")
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(ggplot2)
library(reshape2)
library(lubridate)
library(DescTools)
library(forcats)
library(gridExtra)


#Check and set working directory
getwd()
setwd("Q:\\")

master_loandata <- read.csv("loan.csv", stringsAsFactors = FALSE, na.strings = c("","NA"))
str(master_loandata)
summary(master_loandata)

#Duplicate data set for cleaning and analysis

loandata <- as.data.frame(master_loandata)
nrow(loandata)
ncol(loandata)
View(loandata)

################ Data Cleaning (DC) ################

#Visual check indicate some rows just contain only NA
#DC1. Removing columns that only contain NA using ColSums and is.na
#..which checks for number of NAs in a column and remove them
loandata <- loandata[,colSums(is.na(loandata)) != nrow(loandata)]
View(loandata)
ncol(loandata) # No. of columns now down to 57 only

#DC2. Check for duplicates in id and member column to identify which
#..can be used for identifying total unique applicants

length(loandata$id)
length(unique(loandata$id)) == length(loandata$id) #no duplicates in id column
length(which(is.na(loandata$id))) # no NAs in id column

length(loandata$member_id)
length(unique(loandata$member_id)) == length(loandata$member_id) #no duplicates in member_id column
length(which(is.na(loandata$member_id))) # no NAs in member_id column

# either columns can be used for analysis as no duplicates and no NAs

#DC3. Identify columns which can be removed

#Check and remove columns which have only one value in a particular column
rept <- sapply(loandata, function(x) length(unique(x)) >1)
loandata <- loandata[rept]
ncol(loandata) #variables down to 51

#Check and remove columns which have high NAs and hence couldn't help with analysis
na_count <- colSums(is.na(loandata))
View (na_count) #identified that "mths_since_last_record", "mths_since_last_delinq" contains large number of NAs and hence removed
loandata <- loandata[,-which(names(loandata) %in% c("mths_since_last_record",
                                                    "mths_since_last_delinq"))]
ncol(loandata) #variables down to 49
names(loandata)
#Check and remove columns which doesnt have new information or data cannot be used and hence couldn't help with analysis
loandata <- loandata[,-which(names(loandata) %in% c("url", # url is not useful as it doesnt have any new information except for id
                                                    "zip_code", #masked zip code hence removed
                                                    "emp_title", #Title contains values which are not relevant for analysis
                                                    "desc", #contains comments from applicants
                                                    "collections_12_mths_ex_med", #not useful
                                                    "tax_liens" #not useful
                                                      ))]
ncol(loandata) #variables down to 43

#DC4. Cleaning various columns
str(loandata)

#DC4.a. Interest rate & Revolving Utilisation column
loandata$int_rate <- as.numeric(gsub("\\%","",loandata$int_rate))
loandata$revol_util <- as.numeric(gsub("\\%","",loandata$revol_util))

#DC4.b.Fixing column emp_length where missing values are marked as n/a instead of NA
loandata$emp_length <- str_replace(loandata$emp_length,"n/a","NA")

#DC4.c. Standardising precision for various numberic columns

loandata$funded_amnt_inv <- round(loandata$funded_amnt_inv,digits = 2)
loandata$total_pymnt <- round(loandata$total_pymnt,digits = 2)
loandata$total_rec_late_fee <- round(loandata$total_rec_late_fee,digits = 2)
loandata$installment <- round(loandata$installment,digits = 2)
loandata$total_pymnt_inv <- round(loandata$total_pymnt_inv,digits = 2)
loandata$total_rec_int <- round(loandata$total_rec_int,digits = 2)
loandata$last_pymnt_amnt <- round(loandata$last_pymnt_amnt,digits = 2)

#DC4.d. Remove extra characters in term and emp_length

loandata$term <- gsub("months", "", loandata$term)
loandata$emp_length <- gsub("years|year", "", loandata$emp_length)

#DC4.e. Fixing dates in issue_d, earliest_cr_line, last_pymnt_d, next_pymnt_d and last_credit_pull_d
# function to clean dates

loandata$issue_d<- parse_date_time(loandata$issue_d, c("%b-%y"))
loandata$earliest_cr_line<- parse_date_time(loandata$earliest_cr_line, c("%b-%y"))
loandata$last_pymnt_d<- parse_date_time(loandata$last_pymnt_d, c("%b-%y"))
loandata$next_pymnt_d<- parse_date_time(loandata$next_pymnt_d, c("%b-%y"))
loandata$last_credit_pull_d<- parse_date_time(loandata$last_credit_pull_d, c("%b-%y"))

View(loandata)
#DC5. Derived metrics for analysis
loandata$Fundedper <- round(loandata$funded_amnt/loandata$loan_amnt, digits = 2)
loandata$Fundedper_inv <- round(loandata$funded_amnt_inv/loandata$loan_amnt, digits = 2)
loandata$len_credit_hist <- as.numeric(round((loandata$issue_d - loandata$earliest_cr_line)/365, digits = 0))
loandata$act_acc <- round(loandata$open_acc/loandata$total_acc, digits = 2)


#DC6. Box plots & Histogram to check outliers

boxplot(loandata[,c("loan_amnt", "funded_amnt","funded_amnt_inv","annual_inc")]) #graph is distorted due to annual income
boxplot(loandata[,c("loan_amnt", "funded_amnt","funded_amnt_inv")]) #reasonably spread out with few outliers

par(mfrow=c(1,3))
hist(loandata$loan_amnt, main = "Histogram for Loan Amount", xlab = "Loan Amount", col = "red")
hist(loandata$funded_amnt, main = "Histogram for Funded Amount", xlab = "Funded Amount", col = "blue")
hist(loandata$annual_inc, main = "Histogram for Annual Income", xlab = "Annual Income", col = "green")
# above histogram chart indicate binning is required for Annual Income for using it in analysis

#DC7. Creating appropriate bins for annual income

for (i in 1:nrow(loandata)){
  if(loandata$annual_inc[i] < 40000 & loandata$annual_inc[i] >= 20000){
    loandata$ann_inc_bin[i] = "20k-40k"
  } else if (loandata$annual_inc[i] <= 60000 & loandata$annual_inc[i] >=40000){
    loandata$ann_inc_bin[i] = "40k-60k"
  } else if (loandata$annual_inc[i] <80000 & loandata$annual_inc[i] >= 60000){
    loandata$ann_inc_bin[i] = "60k-80k"
  } else if (loandata$annual_inc[i] <100000 & loandata$annual_inc[i] >=80000){
    loandata$ann_inc_bin[i] = "80k-100k"
  } else if (loandata$annual_inc[i] <120000 & loandata$annual_inc[i] >=100000){
    loandata$ann_inc_bin[i] = "100k-120k"
  } else if(loandata$annual_inc[i] >=120000){
    loandata$ann_inc_bin[i] = "120k+"
  } else loandata$ann_inc_bin[i] = "<20k"
}
#sequencing levels in annual income bins
loandata$ann_inc_bin <- factor(loandata$ann_inc_bin, levels = c("<20k","20k-40k","40k-60k","60k-80k","80k-100k","100k-120k","120k+"))


#DC7.b. Creating appropriate bins for interest rate
for (i in 1:nrow(loandata)){
   if (loandata$int_rate[i] > 7.5 & loandata$int_rate[i] <=10){
    loandata$int_rate_bin[i] = "7.5-10"
  } else if (loandata$int_rate[i] >10 & loandata$int_rate[i] <= 12.5){
    loandata$int_rate_bin[i] = "10-12.5"
  } else if (loandata$int_rate[i] >12.5 & loandata$int_rate[i] <=15){
    loandata$int_rate_bin[i] = "12.5-15"
  } else if(loandata$int_rate[i] >15){
    loandata$int_rate_bin[i] = "15+"
  } else loandata$int_rate_bin[i] = "< 7.5"
}



#DC8. Below is the list of us state codes obtained from wiki site
usa_state_codes <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA",
                     "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
                     "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
                     "WV","WI","WY","AS","GU","MH","FM","MP","PW","PR","VI")

sum(is.element(loandata$addr_state, usa_state_codes)) # is.element returns true if element present in the given vector
nrow(loandata)
# sum is 39717 same as nrow(loandata) which indicates all elements in addr_state are  from the above list usa_state_codes

#DC9. Creating new column for loan status
loandata$loan_status_Bool <- ifelse(loandata$loan_status=="Charged Off", 1, 0)
loandata$loan_status_Current <- ifelse(loandata$loan_status=="Current", 1, 0)
loandata$loan_status_Paid <- ifelse(loandata$loan_status=="Fully Paid", 1, 0)



################ Analysis ################

##HeatMap: All continuos variables ##
loandata['loan_income_ratio']= loandata['loan_amnt']/loandata['annual_inc']
heatMapData <- loandata[, c('loan_amnt', 'funded_amnt' ,'funded_amnt_inv','int_rate','installment','annual_inc','dti','loan_income_ratio','loan_status_Bool')]
heatMapData <- round(cor(heatMapData),2)
melted_cormat <- melt(heatMapData)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  ylab("List of Categories ") +
  xlab("List of Categories") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "value")


##Univariate Analysis##
## A quick plot analysis to analyse main variables
barplot(table(loandata$ann_inc_bin), main = "UV1. Annual Income bins") 
barplot(table(loandata$grade), main = "UV3.Count by Grade")
barplot(table(loandata$sub_grade), main = "UV4.Count by Sub-Grade")
barplot(table(loandata$term), main = "UV5.Count by term")
hist(loandata$int_rate, main = "UV6. Histogram of Interest Rate")
barplot(table(loandata$purpose), main = "UV7. Histogram - Loan Purpose")
hist(loandata$Fundedper, main = "UV8. Histogram - Fundedper")
hist(loandata$len_credit_hist, main = "UV9. Histogram - Length of credit History")
hist(loandata$act_acc, main = "UV10. Histogram - Active Account")
hist(loandata$inq_last_6mths, main = "UV11. Histogram - Inquiry Last 6 months")
hist(loandata$delinq_2yrs, main = "UV12. Histogram - Delinq 2 years")
barplot(table(loandata$verification_status), main = "UV13. Verification Status")

#Univariate Segmented Analysis

# a set of variables are selected from the data set uisng visual observation and business understanding
# which can be the driver variables for a loan to be defaulted.

# first lets look how many defaulters are there in the total dataset

ggplot(loandata,aes(x=fct_infreq(loan_status),fill=factor(loan_status))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Loan Status",y="Count", fill="Loan Status")

# the above plot shows that nearly 14% (count-5627) of the total loans are in charged off status i.e defaulted.
# Thats a huge number and definitely a big loss for the company

# Lets look at some of the variables that influence defaulting

# Lets see funded_amnt ie. The amount provided as loan 

ggplot(loandata, aes(x = funded_amnt)) + geom_histogram(fill="greenyellow",col="green4") + labs(x="Funded amount",y="Frequency")

# From the above plot we can observe that most of the funded amount range is between 1000 to 15000

# Very small no of loans are funded in higher range of 25000 to 35000

# Now lets see how this funded amount affects loan defaulting

ggplot(subset(loandata,loan_status=="Charged Off"), aes(x = funded_amnt)) + geom_histogram(fill="greenyellow",col="green4") + labs(x="Funded amount (only Charged Off)",y="Frequency")

# From the above plot it is seen that most of the defaulting happend between 1000 to 25000 range with few peaks
# We are also able to see that there is some significant number of  defaulting in the higher range of 35000 funded amount
# which would also result in a big loss due to large amount funded

# Lets see term variable. The total repayment term for the loan

ggplot(loandata,aes(x=fct_infreq(loan_status),fill=factor(loan_status))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Term",y="Count", fill="Loan Status") +facet_wrap(~factor(term))

# From above plot we can see that even though 36 term loans have more charged off count the percentage of 60 term
# charged off loans is high as 23% when compared to 11% for 36 term

# Lets see int_rate the rate of interest provide for the loan. The higher the risk of the loan the more the interest and 
# also higher interest rate for lower individual grades 

ggplot(subset(loandata,loan_status=="Charged Off"), aes(x = int_rate)) + geom_histogram(fill="greenyellow",col="green4") + labs(x="Interest Rate",y="Frequency")

# From above plot we can see that charged off count is more between interest rate range of 9% - 20%

# Lets see how grade- the grade provided based on the fico score of each individual based on individuals
#  credit history

ggplot(loandata,aes(x=fct_infreq(grade),fill=factor(grade))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Grade",y="Count", fill="Grade") +facet_wrap(~factor(loan_status))

ggplot(subset(loandata,loan_status=="Charged Off"),aes(x=fct_infreq(grade),fill=factor(grade))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Grade",y="Count", fill="Grade") 

# From the above plot its seen that grades B,C and D have significantky more charged off count and
# grades E and A have still a good no for charged off whereas grades F and G have very less charged off count

# Lets see now the home ownership variable affects defualting

ggplot(loandata,aes(x=fct_infreq(home_ownership),fill=factor(home_ownership))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Home Ownership",y="Count", fill="Home Ownership") +facet_wrap(~factor(loan_status))

ggplot(subset(loandata,loan_status=="Charged Off"),aes(x=fct_infreq(home_ownership),fill=factor(home_ownership))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Home Ownership",y="Count", fill="Home Ownership") 

# As we can see from above plots charged off count is more for Rent and Mortgage. For Own house even though
# count is small percentage is more


# Let see how dti affects loan defaulting

ggplot(subset(loandata,loan_status=="Charged Off"), aes(x = dti)) + geom_histogram(fill="greenyellow",col="green4") + labs(x="dti",y="Frequency")
ggplot(loandata, aes(x = dti)) + geom_histogram(fill="greenyellow",col="green4") + labs(x="dti",y="Frequency") + ggtitle("DTI v/s Loan Defaulting")
# The charged off count increases gradually from 2 to 14 range of dti is high from 14 to 22 then gradually decreases till
# 25 after which is very low


# Lets see how defaulting  varies for the loan purpose

ggplot(loandata,aes(x=fct_infreq(purpose),fill=factor(purpose))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Purpose",y="Count", fill="Purpose") + facet_wrap(~factor(loan_status))


ggplot(subset(loandata,loan_status=="Charged Off"),aes(x=fct_infreq(purpose),fill=factor(purpose))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Purpose",y="Count", fill="Purpose") 

# From above plot we can observe that defaulting is significantly high when purpose of borowing is
# debt_consolidation

# Lets see how emp_length ie the employment length of employee affects defaulting

ggplot(loandata,aes(x=fct_infreq(emp_length),fill=factor(emp_length))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Emp length",y="Count", fill="Emp length") + facet_wrap(~factor(loan_status))+ggtitle("Employee Length V/s Loan Status")


ggplot(subset(loandata,loan_status=="Charged Off"),aes(x=fct_infreq(emp_length),fill=factor(emp_length))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Emp length",y="Count", fill="Emp length") 

# From above plot it is observed that the default count is high for 10+ yrs of employment length, 
# medium from 1 to 5 and is low for 6 to 9


# Lets see how Verification status affects loan defaulting

ggplot(loandata,aes(x=fct_infreq(verification_status),fill=factor(verification_status))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Verification Status",y="Count", fill="Verification Status") + facet_wrap(~factor(loan_status))


ggplot(subset(loandata,loan_status=="Charged Off"),aes(x=fct_infreq(verification_status),fill=factor(verification_status))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Verification Status (only Charged off)",y="Count", fill="Verification Status") 

# From above plot we can infer that source verfied status has quiet less defaulting count when compared to 
# other statuses

# Lets see how inq_last_6mths - The no of inquiries for more loans in past 6 months affects defaulting

ggplot(loandata,aes(x=fct_infreq(factor(inq_last_6mths)),fill=factor(inq_last_6mths))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Enquiry In Last 6 Months",y="Count", fill="Enquiry In Last 6 Months") + facet_wrap(~factor(loan_status))


ggplot(subset(loandata,loan_status=="Charged Off"),aes(x=fct_infreq(factor(inq_last_6mths)),fill=factor(inq_last_6mths))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Enquiry In Last 6 Months (only Charged Off)",y="Count", fill="Enquiry In Last 6 Months") 

# From above plot we observe that the less the no of enquiries the more the count of defaulting



##Bivariate Analysis##
#Loan Amoun Vs Term

ggplot(loandata, aes(loandata$term,loandata$loan_amnt)) + geom_boxplot(aes(fill = loandata$term)) +
  labs(title = "BV1. Loan amount by term",
       x = "Term",
       y = "Loan amount")

#Loan Status Vs Grade

grade_plot <- ggplot(loandata, aes(x = loandata$grade, fill = loan_status)) + geom_bar(position = "dodge") + 
  labs(title = "BV2. Loan status by grade",
       x = "Grade",
       y = "Count")
grade_plot

gp <- ggplot(loandata,aes(x = grade,y =loan_status)) +geom_point(position = "jitter")
gp + ggtitle("BV3. Loan status by grade")

#Loan Status Vs Home Ownership

home_plot <- ggplot(loandata,aes(x=home_ownership,fill = home_ownership))+ geom_bar()+facet_wrap(~loan_status)

home_plot + ggtitle("BV4. Loan status by Home Ownership")

Home_plot <- ggplot(data = loandata %>% 
                      group_by(home_ownership,loan_status) %>% 
                      summarize(cnt=length(id))) + 
  geom_col(aes(x=home_ownership,y=cnt,fill=loan_status),position="fill") + 
  labs(title="BV5. Loan status by Home Ownership",x="Home Ownership",y="Ratio/Percentage",fill="Loan Status")

Home_plot

#Verification Status - Loan Status
Verification_plot <- ggplot(data = loandata %>% 
                              group_by(verification_status,loan_status) %>% 
                              summarize(cnt=length(id))) + 
  geom_col(aes(x=verification_status,y=cnt,fill=loan_status),position="fill") + 
  labs(title="BV6. Verification Status - Loan Status",x="Verification Status",y="Ratio/Percentage",fill="Loan Status")

Verification_plot

#Purpose Vs Loan Status

Purpose_plot <- ggplot(loandata) + 
  geom_bar(aes(x=purpose,fill=loan_status),position="stack") +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="BV7.Purpose Vs Loan Status",x="Purpose",y="Count",fill="Loan Status")


Purpose_plot

purpose_plot <- ggplot(loandata,aes(x=fct_infreq(purpose),fill = purpose))+ geom_bar()+geom_text(stat = "count",aes(label = ..count..),size = 3,position = position_stack(vjust = 0.5))+facet_wrap(~loan_status) + theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                         axis.text.x=element_blank(),
                                                                                                                                                                                                                         axis.ticks.x=element_blank())
purpose_plot + ggtitle("BV8.Purpose Vs Loan Status")

#Loan Amount Distribution among Loan Status

amt_group = loandata %>% 
  select(issue_d, loan_status, loan_amnt) %>% 
  group_by(issue_d, loan_status) %>% 
  summarise(Amount = sum(loan_amnt))

summary(amt_group)

LoanAmt_plot <- ggplot(amt_group, aes(x = issue_d, y = Amount, col = factor(loan_status))) + geom_smooth() + 
  labs(title = "BV9.Loan amount distribution among loan status",
       x = "Issued date",
       y = "Amount")

LoanAmt_plot


#Annual Income vs Grade

ggplot(loandata, aes(grade,annual_inc,fill = loan_status)) + geom_boxplot() + ylim(0,100000) +
  labs(title = "BV10.Annual Income by Grade",
       x = "Grade",
       y = "Annual income")


#Loan amount distribution Vs Loan Status per Verification

Loand_group_table = loandata %>% 
  select(loan_status, loan_amnt, verification_status) %>% 
  group_by(loan_status, verification_status) %>% 
  summarise(Amount = sum(loan_amnt))

summary(Loand_group_table)

ggplot(Loand_group_table, aes(x = verification_status,y = Amount, fill = loan_status)) + 
  geom_bar(stat="identity",position = "dodge") + geom_text(aes(label = Amount), position= position_dodge(width=0.9), vjust=-.5, color="black") +
  theme(legend.position = "bottom") +
  labs(title = "BV11.Loan amount distribution Vs Loan Status per Verification",
       x = "Verification Status",
       y = "Amount")


#Grade vs Status

grp_plot <- ggplot(loandata,aes(x=grade,fill = grade))+ geom_bar()+facet_wrap(~loan_status)

grp_plot + labs(title="BV12.Grade Assigned by LC and their Status",x = "Grade",y="No of Loan Availed",fill = "Grade")

grp_plot2 <- ggplot(data = loandata %>% 
                      group_by(grade,loan_status) %>% 
                      summarize(cnt=length(id))) + 
  geom_col(aes(x=grade,y=cnt,fill=loan_status),position="fill") + 
  labs(title="BV13. Grade Assigned by LC and their Status",x="Grade",y="Ratio/Percentage",fill="Loan Status")

grp_plot2

#DTI Vs Loan Status

dti_stat <- ggplot(loandata,aes(x=loan_status,y=dti,fill = loan_status)) + geom_boxplot()

dti_stat + labs(title = "BV14.Distribution of DTI over Loan Status",x="",y="Debt To Income Ratio",fill = "Loan Status")

# Interest VS Grades

Interest_grades <- ggplot(loandata) + 
  geom_boxplot(aes(x=sub_grade,y=int_rate,fill=grade)) + 
  geom_line(data=(loandata %>% 
                    group_by(sub_grade) %>% 
                    summarize(avg_dti=mean(int_rate,na.rm=TRUE))),
            aes(x=sub_grade,y=avg_dti,group=1)) +
  scale_y_continuous(breaks=seq(0,25,1)) +
  labs(title="BV15.Grades Vs Interest Rate",x="Sub Grade",y="Interest Rate",fill="Grade")
Interest_grades

#G18 - Grade Vs Sub Grade Vs Median DTI\nWith percentage Charged off for each\nSub Grade

Charged_of_grade <- ggplot() + 
  geom_tile(data = loandata %>% 
              group_by(grade,sub_grade) %>% 
              summarize(med_dti = median(dti,na.rm=TRUE)),aes(x=grade,y=sub_grade,fill=med_dti)) +
  geom_text(data = (loandata %>% 
                      group_by(grade,sub_grade,loan_status) %>% 
                      summarize(cnt=length(id)) %>% 
                      mutate(ratio=paste("Charged Off =",round(cnt/sum(cnt),4)*100,"%")) %>% 
                      filter(loan_status=="Charged Off")),
            aes(x=grade,y=sub_grade,label=ratio),col="white") +
  
            
  labs(title="G18 - Grade Vs Sub Grade Vs Median DTI\nWith percentage Charged off for each\nSub Grade",
       x="Grade",y="Sub Grade",fill="Median DTI",label="Percentage of Charged Off Loans")

grid.arrange(Charged_of_grade)





#Funded Amount Vs Loan Status

Fun_Stat <- ggplot(loandata,aes(x=loan_status,y=funded_amnt,fill = loan_status)) + geom_boxplot()

Fun_Stat + labs(title = "BV16.Distribution of Funded Amount On Loan Status",x="",y="Funded Amount",fill = "Loan Status")



#Loan Status Vs Emp Length

emp_plot <- ggplot(loandata,aes(x=emp_length,fill = emp_length))+ geom_bar()+facet_wrap(~loan_status)

emp_plot + labs(title="BV17.No of loan taken by each employment group and their Status",x = "",y="No of Loan Availed",fill = "Employment Group")


#Loan Status Vs Purpose

pur_plot <- ggplot(loandata,aes(x=purpose,fill = purpose))+ geom_bar()+facet_wrap(~loan_status)

pur_plot + labs(title="BV18.Purpose of loan taken and their Status",x = "",y="No of Loan Availed",fill = "Purpose")


#Location vs Percentage Charge Off

prob_df <- aggregate(loandata$loan_status_Bool, by=list(Category=loandata$addr_state), FUN=sum)
current <- aggregate(loandata$loan_status_Current, by=list(Category=loandata$addr_state), FUN=sum)
prob_df$current <- current$x
paid <- aggregate(loandata$loan_status_Paid, by=list(Category=loandata$addr_state), FUN=sum)
prob_df$paid <- paid$x
prob_df$total <- prob_df$x + prob_df$current + prob_df$paid
prob_df$prob_chargedOff <- round(prob_df$x / prob_df$total,2)

ggplot(data=prob_df, aes(x=Category, y=prob_chargedOff, group=1)) + 
  geom_line(colour="red", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white")+
  labs(title="Location vs Percentage Charge Off",x="Location",y="Percentage Of Charged Off")

#Purpose of Loan vs Percentage Charge Off

prob_df_purpose <- aggregate(loandata$loan_status_Bool, by=list(Category=loandata$purpose), FUN=sum)
current <- aggregate(loandata$loan_status_Current, by=list(Category=loandata$purpose), FUN=sum)
prob_df_purpose$current <- current$x
paid <- aggregate(loandata$loan_status_Paid, by=list(Category=loandata$purpose), FUN=sum)
prob_df_purpose$paid <- paid$x
prob_df_purpose$total <- prob_df_purpose$x + prob_df_purpose$current + prob_df_purpose$paid
prob_df_purpose$prob_chargedOff <- round(prob_df_purpose$x / prob_df_purpose$total,2)


ggplot(data=prob_df_purpose, aes(x=Category, y=prob_chargedOff, group=1)) + 
  geom_line(colour="red", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white") + 
  labs(title="Purpose vs Probability Charge Off",x="Purpose",y="Probability Of Charged Off")

#Grade/Subgrade vs Percentage Charge Off

prob_df_grade <- aggregate(loandata$loan_status_Bool, by=list(Category=loandata$grade), FUN=sum)
current <- aggregate(loandata$loan_status_Current, by=list(Category=loandata$grade), FUN=sum)
prob_df_grade$current <- current$x
paid <- aggregate(loandata$loan_status_Paid, by=list(Category=loandata$grade), FUN=sum)
prob_df_grade$paid <- paid$x
prob_df_grade$total <- prob_df_grade$x + prob_df_grade$current + prob_df_grade$paid
prob_df_grade$prob_chargedOff <- round(prob_df_grade$x / prob_df_grade$total,2)


ggplot(data=prob_df_grade, aes(x=Category, y=prob_chargedOff, group=1)) + 
  geom_line(colour="red", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white") +
  labs(title="Grade vs Probability Charge Off",x="Grade",y="Probability Of Charged Off")

prob_df_sub_grade <- aggregate(loandata$loan_status_Bool, by=list(Category=loandata$sub_grade), FUN=sum)
current <- aggregate(loandata$loan_status_Current, by=list(Category=loandata$sub_grade), FUN=sum)
prob_df_sub_grade$current <- current$x
paid <- aggregate(loandata$loan_status_Paid, by=list(Category=loandata$sub_grade), FUN=sum)
prob_df_sub_grade$paid <- paid$x
prob_df_sub_grade$total <- prob_df_sub_grade$x + prob_df_sub_grade$current + prob_df_sub_grade$paid
prob_df_sub_grade$prob_chargedOff <- round(prob_df_sub_grade$x / prob_df_sub_grade$total,2)


ggplot(data=prob_df_sub_grade, aes(x=Category, y=prob_chargedOff, group=1)) + 
  geom_line(colour="red", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white") +
  labs(title="Subgrade vs Percentage Charge Off",x="Subgrade",y="Percentage Of Charged Off")


#Annual Income Range vs Percentage Charge Off

prob_df_annual_range <- aggregate(loandata$loan_status_Bool, by=list(Category=loandata$ann_inc_bin), FUN=sum)
current <- aggregate(loandata$loan_status_Current, by=list(Category=loandata$ann_inc_bin), FUN=sum)
prob_df_annual_range$current <- current$x
paid <- aggregate(loandata$loan_status_Paid, by=list(Category=loandata$ann_inc_bin), FUN=sum)
prob_df_annual_range$paid <- paid$x
prob_df_annual_range$total <- prob_df_annual_range$x + prob_df_annual_range$current + prob_df_annual_range$paid
prob_df_annual_range$prob_chargedOff <- round(prob_df_annual_range$x / prob_df_annual_range$total,2)


ggplot(data=prob_df_annual_range, aes(x=Category, y=prob_chargedOff, group=1)) + 
  geom_line(colour="red", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white") +
  labs(title="Annual Income Range vs Probability Charge Off",x="Annual Income Range",y="Percentage Of Charged Off")


#Interest rate vs Percentage Charge Off

prob_df_int_rate <- aggregate(loandata$loan_status_Bool, by=list(Category=loandata$int_rate), FUN=sum)
current <- aggregate(loandata$loan_status_Current, by=list(Category=loandata$int_rate), FUN=sum)
prob_df_int_rate$current <- current$x
paid <- aggregate(loandata$loan_status_Paid, by=list(Category=loandata$int_rate), FUN=sum)
prob_df_int_rate$paid <- paid$x
prob_df_int_rate$total <- prob_df_int_rate$x + prob_df_int_rate$current + prob_df_int_rate$paid
prob_df_int_rate$prob_chargedOff <- round(prob_df_int_rate$x / prob_df_int_rate$total,2)


ggplot(data=prob_df_int_rate, aes(x=Category, y=prob_chargedOff, group=1)) + 
  geom_line(colour="red", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white") +
  labs(title="Interest rate vs Percentage Charge Off",x="Interest Rate",y="Percentage Of Charged Off")


#Interest rate Range vs Percentage Charge Off

prob_df_int_rate_bin <- aggregate(loandata$loan_status_Bool, by=list(Category=loandata$int_rate_bin), FUN=sum)
current <- aggregate(loandata$loan_status_Current, by=list(Category=loandata$int_rate_bin), FUN=sum)
prob_df_int_rate_bin$current <- current$x
paid <- aggregate(loandata$loan_status_Paid, by=list(Category=loandata$int_rate_bin), FUN=sum)
prob_df_int_rate_bin$paid <- paid$x
prob_df_int_rate_bin$total <- prob_df_int_rate_bin$x + prob_df_int_rate_bin$current + prob_df_int_rate_bin$paid
prob_df_int_rate_bin$prob_chargedOff <- round(prob_df_int_rate_bin$x / prob_df_int_rate_bin$total,2)


ggplot(data=prob_df_int_rate_bin, aes(x=Category, y=prob_chargedOff, group=1)) + 
  geom_line(colour="red", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white") +
  labs(title="Interest rate Range vs Percentage Charge Off",x="Interest Rate Range",y="Percentage Of Charged Off")


#Employment Age vs Percentage Charge Off

prob_df_emp_length <- aggregate(loandata$loan_status_Bool, by=list(Category=loandata$emp_length), FUN=sum)
current <- aggregate(loandata$loan_status_Current, by=list(Category=loandata$emp_length), FUN=sum)
prob_df_emp_length$current <- current$x
paid <- aggregate(loandata$loan_status_Paid, by=list(Category=loandata$emp_length), FUN=sum)
prob_df_emp_length$paid <- paid$x
prob_df_emp_length$total <- prob_df_emp_length$x + prob_df_emp_length$current + prob_df_emp_length$paid
prob_df_emp_length$prob_chargedOff <- round(prob_df_emp_length$x / prob_df_emp_length$total,2)


ggplot(data=prob_df_emp_length, aes(x=Category, y=prob_chargedOff, group=1)) + 
  geom_line(colour="red", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white") +
  labs(title="Employment Length vs Percentage Charge Off",x="Employee Length",y="Percentage Of Charged Off")

#Loan amount vs Interaste rate

loan_status_q <- filter(loandata, loan_status == "Charged Off")

p <- ggplot(loan_status_q, mapping = aes(x = loan_amnt, y = int_rate))  
p + geom_jitter() +  # using geom_jitter to avoid overplotting of points
  geom_smooth(span = 0.95)

