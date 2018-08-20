# INVESTMENT CASE STUDY - SPARK FUNDS
# CASE STUDY GROUP MEMBERS - KARTIK, KEERTHIKA, RAHUL, VIVEKA

setwd("C:\\Users\\Sir 'M'\\Desktop\\Upgrad\\Assignment\\Investment Case Study\\Inputs")

#Loading the required packages
library(dplyr)
library(stringr)
library(tidyr)
##--------------------------------------------------------------------
#Checkpoint 1: Data Cleaning 1
#Load the companies and rounds data into two data frames and name them companies and rounds2 
#respectively
companies <- read.delim("companies.txt",header = T,dec = ".",sep = "\t",stringsAsFactors = F)
rounds2 <- read.csv("rounds2.csv",header = T,stringsAsFactors = F)
##--------------------------------------------------------------------
#Table-1.1
#How many unique companies are present in rounds2?
no_unique_companies_rounds2 <- nrow(distinct(rounds2,tolower(company_permalink)))
no_unique_companies_rounds2

#How many unique companies are present in companies?
no_unique_companies_companies <- nrow(distinct(companies,permalink))
no_unique_companies_companies

#Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
distinct(rounds2,tolower(company_permalink)) %in% distinct(companies,permalink)

#Merge the two data frames so that all variables (columns)in the companies frame are added 
#to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame?

companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")
##--------------------------------------------------------------------
#Checkpoint 2: Funding Type Analysis 
#Average funding amount of venture type
#Table-2.1

funding_groups <- group_by(master_frame,funding_round_type)
summarise(funding_groups,mean(raised_amount_usd,na.rm = T))
##--------------------------------------------------------------------
#Checkpoint 3: Country Analysis
#For the chosen investment type, make a data frame named top9 with the top nine countries 
#(based on the total investment amount each country has received)

#Grouping on country code
country_groups <- group_by(master_frame,country_code)

#Filter the data with the venture type
venture_country_groups <- filter(country_groups,funding_round_type == "venture")

#Summarise the data country wise for the total investment
country_total_venture <- summarise(venture_country_groups, total_investment_venture = sum(raised_amount_usd, na.rm = TRUE))

#Arrange the data in the decreasing order based on investment amount
country_total_venture_desc <- arrange(country_total_venture,desc(total_investment_venture))

#Store the top 9 countries in the df top9
Top_9 <- country_total_venture_desc[-which(country_total_venture_desc$country_code == ""),][1:9,] #exclude the Blank country_code
Top_9
##--------------------------------------------------------------------
# Checkpoint 4: Sector Analysis 1
# Extract the primary sector of each category list from the category_list column

master_frame_separated  <-separate(master_frame,category_list,into=c("Primary_Sector"),sep="\\|",remove = FALSE)

# Read mapping file into R
mapping <- read.csv("mapping.csv")

# converting from wide data format to long format
mapping_long <- gather(mapping,main_sector,my_val,Automotive...Sports:Social..Finance..Analytics..Advertising)
mapping_long <- mapping_long[!(mapping_long$my_val == 0),]
mapping_long <- mapping_long[,-3]
View(mapping_long)

# Data cleaning of mapping file - Start

# 0 is replaced with na in the main_category names in excel. So replacing na wherever 0 is found in main_category names
mapping_long$category_list <- str_replace_all(mapping_long$category_list,"0","na")

# bringing back main category values to original form as R replaces certain characters ('/', ',') with dots (.)
mapping_long$main_sector <-  str_replace(mapping_long$main_sector,"Automotive...Sports","Automotive & Sports")
mapping_long$main_sector <-  str_replace(mapping_long$main_sector,"Cleantech...Semiconductors","Cleantech / Semiconductors") 
mapping_long$main_sector <-  str_replace(mapping_long$main_sector,"News..Search.and.Messaging","News, Search and Messaging") 
mapping_long$main_sector <-  str_replace(mapping_long$main_sector,"Social..Finance..Analytics..Advertising","Social, Finance, Analytics, Advertising") 

# In the above procedure 0 in Enterprise 2.0 gets replaced by na which is wrong. So replacing back the original string
mapping_long$category_list <- str_replace_all(mapping_long$category_list,"Enterprise 2.na","Enterprise 2.0")

# Data cleaning of mapping file - End

# Creating Final Master Frame - merging with the master_frame with primary sector as separate column

final_master_frame <- merge(master_frame_separated,mapping_long,by.x = "Primary_Sector", by.y = "category_list")
final_master_frame <- subset(final_master_frame, main_sector != "Blanks") #removing Blanks because it is not among Eight main sectors
View(final_master_frame)

##--------------------------------------------------------------------

# Checkpoint 5: Sector Analysis 2

# Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of 
#funding type FT falling within the 5-15 million USD range. The three data frames should contain:

# Creating separate data frames with required columns Total number (count) of investments and Total amount invested in each main sector in a separate column

# D1 for Country Code USA
D1 <- filter(final_master_frame,country_code=="USA",funding_round_type=="venture",raised_amount_usd>=5000000,raised_amount_usd<=15000000)
D1_sector_groups <- group_by(D1,main_sector)
D1_invsetment_details <- summarise(D1_sector_groups,total_no_of_investments=n(),total_amount_invested=sum(raised_amount_usd,na.rm = T))
D1 <- merge(D1,D1_invsetment_details,by="main_sector") 
# View(D1) # quick visual check

# D2 for Country Code GBR
D2 <- filter(final_master_frame,country_code=="GBR",funding_round_type=="venture",raised_amount_usd>=5000000,raised_amount_usd<=15000000)
D2_sector_groups <- group_by(D2,main_sector)
D2_invsetment_details <- summarise(D2_sector_groups,total_no_of_investments=n(),total_amount_invested=sum(raised_amount_usd,na.rm = T))
D2 <- merge(D2,D2_invsetment_details,by="main_sector") 
# View(D2)  # quick visual check

# D3 for Country Code IND
D3 <- filter(final_master_frame,country_code=="IND",funding_round_type=="venture",raised_amount_usd>=5000000,raised_amount_usd<=15000000)
D3_sector_groups <- group_by(D3,main_sector)
D3_invsetment_details <- summarise(D3_sector_groups,total_no_of_investments=n(),total_amount_invested=sum(raised_amount_usd,na.rm = T))
D3 <- merge(D3,D3_invsetment_details,by="main_sector") 
# View(D3)  # quick visual check
##--------------------------------------------------------------------
#Table 5.1 : Sector-wise Investment Analysis

# 1. Total number of investments (count)
# Answers to point 1 of Table 5.1
nrow(D1) #No. of investments for Country Code USA

nrow(D2) #No. of investments for Country Code GBR

nrow(D3) #No. of investments for Country Code INR
##--------------------------------------------------------------------
# 2. Total amount of investment (USD)
# Answers to point 2 of Table 5.1
sum(D1$raised_amount_usd) #Amt. invested for Country Code USA

sum(D2$raised_amount_usd) #Amt. invested for Country Code GBR

sum(D3$raised_amount_usd) #Amt. invested for Country Code INR
##--------------------------------------------------------------------
#3. Top Three Sector names (no. of investment-wise) with No. of investments
# Answers to point 3 to 8 of Table 5.1

# D1 for country code USA
sector_groups_D1 <- group_by(D1,main_sector)
sector_investment_count_D1 <- summarise(sector_groups_D1,investment_count = n())
top_sector_D1 <- arrange(sector_investment_count_D1,desc(investment_count))[1:3,]
top_sector_D1 # To get top three sectors (investment count wise) along with number of investments for country code = USA

# D2 for country code GBR
sector_groups_D2 <- group_by(D2,main_sector)
sector_investment_count_D2 <- summarise(sector_groups_D2,investment_count = n())
top_sector_D2 <- arrange(sector_investment_count_D2,desc(investment_count))[1:3,]
top_sector_D2 # To get top three sectors (investment count wise) along with number of investments for country code = GBR

# D3 for country code IND
sector_groups_D3 <- group_by(D3,main_sector)
sector_investment_count_D3 <- summarise(sector_groups_D3,investment_count = n())
top_sector_D3 <- arrange(sector_investment_count_D3,desc(investment_count))[1:3,]
top_sector_D3 # To get top three sectors (investment count wise) along with number of investments for country code = INR

##--------------------------------------------------------------------
# 4.Which company receive highest investment in Top sector (count-wise) in D1, D2 and D3 each?
# Answers to point 9 of Table 5.1
##reading Top sector name for country code USA
top_sector_name_D1 <- as.character(top_sector_D1[1,1]) 

# creating a subset of only Top Sector data
top_sector_details_D1 <- subset(D1,main_sector==top_sector_name_D1) 

# sorting the data by Total investment and creating meaningful column names
top_sector_inv.by.companies_D1 <- setNames(aggregate(top_sector_details_D1$raised_amount_usd,by=list(top_sector_details_D1$name), FUN = sum), c("Company_Name", "Total_Inv_Amt"))

# Extracting Top company name and the amount of investment received
arrange(top_sector_inv.by.companies_D1, desc(Total_Inv_Amt))[1,1:2]

##reading Top sector name for country code GBR
top_sector_name_D2 <- as.character(top_sector_D2[1,1]) 

# creating a subset of only Top Sector data
top_sector_details_D2 <- subset(D2,main_sector==top_sector_name_D2) 

# sorting the data by Total investment and creating meaningful column names
top_sector_inv.by.companies_D2 <- setNames(aggregate(top_sector_details_D2$raised_amount_usd,by=list(top_sector_details_D2$name), FUN = sum), c("Company_Name", "Total_Inv_Amt"))

# Extracting Top company name and the amount of investment received
arrange(top_sector_inv.by.companies_D2, desc(Total_Inv_Amt))[1,1:2]

##reading Top sector name for country code IND
top_sector_name_D3 <- as.character(top_sector_D3[1,1]) 

# creating a subset of only Top Sector data
top_sector_details_D3 <- subset(D3,main_sector==top_sector_name_D3) 

# sorting the data by Total investment and creating meaningful column names
top_sector_inv.by.companies_D3 <- setNames(aggregate(top_sector_details_D3$raised_amount_usd,by=list(top_sector_details_D3$name), FUN = sum), c("Company_Name", "Total_Inv_Amt"))

# Extracting Top company name and the amount of investment received
arrange(top_sector_inv.by.companies_D3, desc(Total_Inv_Amt))[1,1:2]
##--------------------------------------------------------------------

# 5.Which company receive highest investment in Second sector (count-wise) in D1, D2 and D3 each?
# Answers to point 10 of Table 5.1

##reading Second top sector name for country code USA
sec_sector_name_D1 <- as.character(top_sector_D1[2,1]) 

# creating a subset of only Top Sector data
sec_sector_details_D1 <- subset(D1,main_sector==sec_sector_name_D1) 

# sorting the data by Total investment and creating meaningful column names
sec_sector_inv.by.companies_D1 <- setNames(aggregate(sec_sector_details_D1$raised_amount_usd,by=list(sec_sector_details_D1$name), FUN = sum), c("Company_Name", "Total_Inv_Amt"))

# Extracting Top company name and the amount of investment received
arrange(sec_sector_inv.by.companies_D1, desc(Total_Inv_Amt))[1,1:2]

##reading Second top sector for country code GBR
sec_sector_name_D2 <- as.character(top_sector_D2[2,1]) 

# creating a subset of only Top Sector data
sec_sector_details_D2 <- subset(D2,main_sector==sec_sector_name_D2) 

# sorting the data by Total investment and creating meaningful column names
sec_sector_inv.by.companies_D2 <- setNames(aggregate(sec_sector_details_D2$raised_amount_usd,by=list(sec_sector_details_D2$name), FUN = sum), c("Company_Name", "Total_Inv_Amt"))

# Extracting Top company name and the amount of investment received
arrange(sec_sector_inv.by.companies_D2, desc(Total_Inv_Amt))[1,1:2]

##reading Second top sector for country code IND
sec_sector_name_D3 <- as.character(top_sector_D3[2,1]) 

# creating a subset of only Top Sector data
sec_sector_details_D3 <- subset(D3,main_sector==sec_sector_name_D3) 

# sorting the data by Total investment and creating meaningful column names
sec_sector_inv.by.companies_D3 <- setNames(aggregate(sec_sector_details_D3$raised_amount_usd,by=list(sec_sector_details_D3$name), FUN = sum), c("Company_Name", "Total_Inv_Amt"))

# Extracting Top company name and the amount of investment received
arrange(sec_sector_inv.by.companies_D3, desc(Total_Inv_Amt))[1,1:2]

##----------------END OF CODE------------------