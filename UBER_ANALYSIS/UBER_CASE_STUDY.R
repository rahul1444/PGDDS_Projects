#load the required packages 
library(xlsx)


# Read csv File 
uberData <- read.csv("Uber_Request_Data.csv",stringsAsFactors = FALSE)

#Check Structure of Data 
str(uberData)

#Clean Data
format_one <- as.POSIXct(uberData$Request.timestamp,format="%d/%m/%Y %H:%M")
format_two <- as.POSIXct(uberData$Request.timestamp,format="%d-%m-%Y %H:%M:%OS")

# Combine both while keeping their ranks
format_one[is.na(format_one)] <- format_two[!is.na(format_two)] 

# Put it back in your dataframe
uberData$RequestDate <- format_one

uberData$RequestOnlyDate <- as.numeric(format(uberData$RequestDate , format = "%d"))
uberData$RequestOnlyHour <- as.numeric(format(uberData$RequestDate , format = "%H"))

RequestOnlyHour <- c(0:23)

#define time slots
Time_Slot1 <- c("Pre_Morning","Morning","Day_Time","Evening","Late_Night")

#crate vector which time comes under which slots
times <- c(4,6,7,5,2)
Time_Slot <- rep(Time_Slot1,times)
new_frame <- data.frame(Time_Slot,RequestOnlyHour)

#merge both data frames
uberData <- merge(uberData,new_frame,by="RequestOnlyHour",all.x=TRUE)

#change column postions
uberData <- uberData[,c(2,3,4,5,6,7,8,1,9,10)]

uberData$canceledOrNoCars <- ifelse(uberData$Status == "No Cars Available" | uberData$Status == "Cancelled",1,0)
uberData$Canceled <- ifelse(uberData$Status == "Cancelled",1,0)
uberData$NoCars <- ifelse(uberData$Status == "No Cars Available",1,0)
uberData$Completed <- ifelse(uberData$Status == "Trip Completed",1,0)

write.xlsx(uberData, "uberData.xlsx")


#problem 1. Large number of service requests got cancelled during the Morning Time slot
#Subset the Morning time slot data for analysis
Problem_df <- subset(uberData,uberData$Time_Slot=="Morning")

#Number of trips cancelled for the Morning time slot
total_trip_cancel <- length(which(Problem_df$Status=="Cancelled"))
total_trip_cancel

#Number of trips cancelled from airport for Morning
airport_trip_cancel <- length(which((Problem_df$Pickup.point=="Airport") & (Problem_df$Status == "Cancelled")))
airport_trip_cancel

# Number of trips cancelled from city for Morning
city_trip_cancel <- length(which((Problem_df$Pickup.point=="City") & (Problem_df$Status == "Cancelled")))
city_trip_cancel

# Percentage of trips cancelled from city out of total trips cancelled during morning
percent_trip_cancel_city <- (city_trip_cancel/total_trip_cancel*100)
percent_trip_cancel_city

# Percentage of trips cancelled from airport out of total trips cancelled during Morning
percent_trip_cancel_airport <- (airport_trip_cancel/total_trip_cancel*100)
percent_trip_cancel_airport

# Number of trips requested from city to airport during morning rush
demand_trip_request_city <- length(which(Problem_df$Pickup.point=="City"))
demand_trip_request_city

#Number of trips completed from city to airport during morning rush
demand_trip_city_completed <- length(which((Problem_df$Pickup.point=="City")& (Problem_df$Status=="Trip Completed")))
demand_trip_city_completed


#problem2
#subset the data for Evening rush from dataframe for analysis
Problem2_df <- subset(subset(uberData,uberData$Time_Slot=="Evening"))
#plot the bar graph with status of requests on x-axis and count in y-axis for evening rush time slot
# Show the request from different pickup points in different colors

# No of service requests with no cars available for evening rush time slot
total_nocar_available <- length(which(Problem2_df$Status=="No Cars Available"))
total_nocar_available

# No of  service requests with no cars available from airport during evening rush
airport_nocar_available <- length(which((Problem2_df$Pickup.point=="Airport") & (Problem2_df$Status == "No Cars Available")))
airport_nocar_available

# No of service requests with no cars availablefrom city during evening rush
city_nocar_available <- length(which((Problem2_df$Pickup.point=="City") & (Problem2_df$Status == "No Cars Available")))
city_nocar_available

# Percentage of no cars available status from city out of total no cars available during evening rush
percent_city_nocar <- (city_nocar_available/total_nocar_available*100)
percent_city_nocar

# Percentage of no cars available status from airport out of total no cars available during evening rush
percent_airport_nocar <- (airport_nocar_available/total_nocar_available*100)
percent_airport_nocar

#No of service requests from airport to city during evening rush
demand_nocar_request_airport <- length(which(Problem2_df$Pickup.point=="Airport"))
demand_nocar_request_airport

#No of trips completed from airport to city during evening rush
demand_nocar_request_airport_completed <- length(which((Problem2_df$Pickup.point=="Airport") & (Problem2_df$Status=="Trip Completed")))
demand_nocar_request_airport_completed
