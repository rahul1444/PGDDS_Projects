# Load essential libraries
library(stringr)
library(dplyr)
library(MASS)
library(car)
library(ggplot2)
library(plotrix)

# load the media company data
carPrice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)
str(carPrice)
nrow(carPrice)
ncol(carPrice)
View(carPrice)

################ Data Cleaning (DC) ################

#DC1. Removing columns that only contain NA using ColSums and is.na
#..which checks for number of NAs in a column and remove them
carPrice <- carPrice[,colSums(is.na(carPrice)) != nrow(carPrice)]
View(carPrice)
ncol(carPrice) # No. of columns 26 only

#DC2. Check for duplicates in id and member column to identify which
#..can be used for identifying total unique applicants

length(carPrice$car_ID)
length(unique(carPrice$car_ID)) == length(carPrice$car_ID) #no duplicates in id column
length(which(is.na(carPrice$car_ID))) # no NAs in id column

carPrice[duplicated(carPrice[,3:26]),] #check duplicate values

summary(as.factor(carPrice$CarName))

#check and remove outliers

checkOuliers <- function(dt, var) {  # function to check any variables have outliers or not with mean , percentage of outliers
  var_name <- eval(substitute(var),eval(dt))
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
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  y[x < (qnt[1] - H)] <- caps[1]
  y[x > (qnt[2] + H)] <- caps[2]
  y
}

checkOuliers(carPrice,highwaympg)
carPrice$highwaympg <- remove_outliers(carPrice$highwaympg)

checkOuliers(carPrice,citympg)
carPrice$citympg <- remove_outliers(carPrice$citympg)

checkOuliers(carPrice,enginesize)
carPrice$enginesize <- remove_outliers(carPrice$enginesize)

checkOuliers(carPrice,carlength)
carPrice$carlength <- remove_outliers(carPrice$carlength)

checkOuliers(carPrice,carwidth)
carPrice$carwidth <- remove_outliers(carPrice$carwidth)

checkOuliers(carPrice,carheight)

checkOuliers(carPrice,highwaympg)

checkOuliers(carPrice,boreratio)

checkOuliers(carPrice,stroke)
carPrice$stroke <- remove_outliers(carPrice$stroke)

checkOuliers(carPrice,compressionratio)
carPrice$compressionratio <- remove_outliers(carPrice$compressionratio)

checkOuliers(carPrice,peakrpm)
carPrice$peakrpm <- remove_outliers(carPrice$peakrpm)

# either columns can be used for analysis as no duplicates and no NAs

#DC3. Company name spitted from car name as we required only comapany name

#Split CarName to get Company Name
CompanyNames <- str_split_fixed(carPrice$CarName, " ", 2)
carPrice$Company <- CompanyNames[,1]

#DC4. Dummy variables creation :- Catogorical columns changed to integer 

#check levels of fueltype
summary(as.factor(carPrice$fueltype))
carPrice$fueltype_Diesel <- as.factor(carPrice$fueltype)

#convertFuel type to integer
levels(carPrice$fueltype_Diesel) <- c(1,0)
carPrice$fueltype_Diesel <- as.numeric(levels(carPrice$fueltype_Diesel))[carPrice$fueltype_Diesel]

#check levels of aspiration
summary(as.factor(carPrice$aspiration))
carPrice$aspiration_std <- as.factor(carPrice$aspiration)

#aspiration type to integer
levels(carPrice$aspiration_std) <- c(1,0)
carPrice$aspiration_std <- as.numeric(levels(carPrice$aspiration_std))[carPrice$aspiration_std]

#check levels of doorNumber
summary(as.factor(carPrice$doornumber))
carPrice$doornumber_Four <- as.factor(carPrice$doornumber)

#doorNumber type to integer
levels(carPrice$doornumber_Four) <- c(1,0)
carPrice$doornumber_Four <- as.numeric(levels(carPrice$doornumber_Four))[carPrice$doornumber_Four]

#check levels of carbody
summary(as.factor(carPrice$carbody))
carPrice$carbody <- as.factor(carPrice$carbody)

#carbody type to integer
carbody <- model.matrix(~carPrice$carbody,data = carPrice)
carbody <- subset(carbody,select = -1)
colnames(carbody) <- c("carbodyhardtop", "carbodyhatchback","carbodysedan","carbodywagon") #convert lavel names
carPrice <- cbind(carPrice, carbody)

#check levels of companyName
summary(as.factor(carPrice$Company))
carPrice<-within(carPrice, Company[Company=="porcshce"] <- 'porsche') #Some of company name is not properly speeled , converting them to proper company name
carPrice<-within(carPrice, Company[Company=="vokswagen"] <- 'volkswagen')
carPrice<-within(carPrice, Company[Company=="vw"] <- 'volkswagen')
carPrice<-within(carPrice, Company[Company=="toyouta"] <- 'toyota')
carPrice<-within(carPrice, Company[Company=="maxda"] <- 'mazda')
carPrice<-within(carPrice, Company[Company=="Nissan"] <- 'nissan')
carPrice$Company <- as.factor(carPrice$Company)

#companyName type to integer
Company <- model.matrix(~carPrice$Company,data = carPrice)
Company <- subset(Company,select = -1)
colnames(Company) <- c("audi", "bmw", "buick", "chevrolet", "dodge", "honda", "isuzu", "jaguar", "mazda", "mercury", "mitsubishi","nissan", "peugeot", "plymouth", "porsche", "renault", "saab", "subaru", "toyota","volkswagen","volvo")
carPrice <- cbind(carPrice, Company)

#check levels of driveWheel
summary(as.factor(carPrice$drivewheel))
carPrice$drivewheel <- as.factor(carPrice$drivewheel)

#driveWheel type to integer
drivewheel <- model.matrix(~carPrice$drivewheel,data = carPrice)
drivewheel <- subset(drivewheel,select = -1)
colnames(drivewheel) <- c("drivewheelfwd", "drivewheelrwd")
carPrice <- cbind(carPrice, drivewheel)

#check levels of engineLocation
summary(as.factor(carPrice$enginelocation))
carPrice$enginelocation_front <- as.factor(carPrice$enginelocation)

#engineLocation type to integer
levels(carPrice$enginelocation_front) <- c(1,0)
carPrice$enginelocation_front <- as.numeric(carPrice$enginelocation_front)


#check levels of engineType
summary(as.factor(carPrice$enginetype))
carPrice$enginetype <- as.factor(carPrice$enginetype)

#engineType type to integer
enginetype <- model.matrix(~carPrice$enginetype,data = carPrice)
enginetype <- subset(enginetype,select = -1)
colnames(enginetype) <- c("engineType_dohcv","engineType_l","engineType_ohc","engineType_ohcf","engineType_ohcv","engineType_rotor")
carPrice <- cbind(carPrice, enginetype)

#check levels of cylinderType
summary(as.factor(carPrice$cylindernumber))
carPrice$cylindernumber <- as.factor(carPrice$cylindernumber)

#cylinderType type to integer
cylindernumber <- model.matrix(~carPrice$cylindernumber,data = carPrice)
cylindernumber <- subset(cylindernumber,select = -1)
colnames(cylindernumber) <- c("cylinderNumber_five","cylinderNumbere_four","cylinderNumber_six","cylinderNumber_three","cylinderNumber_twelve","cylinderNumber_two")
carPrice <- cbind(carPrice, cylindernumber)


#check levels of fuelSystem
summary(as.factor(carPrice$fuelsystem))
carPrice$fuelsystem <- as.factor(carPrice$fuelsystem)

#fuelSystem type to integer
fuelsystem <- model.matrix(~carPrice$fuelsystem,data = carPrice)
fuelsystem <- subset(fuelsystem,select = -1)
colnames(fuelsystem) <- c("fuelsystem_2bbl","fuelsystem_4bbl","fuelsystem_idi","fuelsystem_mfi","fuelsystem_mpfi","fuelsystem_spdi","fuelsystem_spfi")
carPrice <- cbind(carPrice, fuelsystem)

#check levels of symboling
summary(as.factor(carPrice$symboling))
carPrice$symboling <- as.factor(carPrice$symboling)

#fuelSystem type to integer
symboling <- model.matrix(~carPrice$symboling,data = carPrice)
symboling <- subset(symboling,select = -1)
colnames(symboling) <- c("symbolingminus1","symboling0","symboling1","symboling2","symboling3")
carPrice <- cbind(carPrice, symboling)

#remove unwanted rows like Id, derived company name ,and 
#other categorical columns which are converted to dummy values int
carPrice <- subset(carPrice,select = c(-1,-2,-4,-5,-6,-7,-8,-9,-15,-16,-18,-27))

#remove ID and move company name to first
carPrice <- carPrice%>%dplyr::select(price, everything())

#DATA ANALYSIS

# Scatter Plots
ggplot(carPrice, aes(enginesize, price)) + geom_point() + geom_smooth()
ggplot(carPrice, aes(horsepower, price)) + geom_point() + geom_smooth()
ggplot(carPrice, aes(highwaympg, price)) + geom_point() + geom_smooth()
ggplot(carPrice, aes(curbweight, price)) + geom_point() + geom_smooth()


# Derived Metrics #by above we graph we able to see highway and enginesize maximum values is between two points so 
# i take log of both for as new varaible for further analysis
carPrice$cityVshighwayMpg <- round(carPrice$citympg/carPrice$highwaympg, digits = 2)
carPrice$logOfEngineSize <- round(log(carPrice$enginesize), digits = 2) 
carPrice$logOfHorsePower <- round(log(carPrice$horsepower), digits = 2)


summary(carPrice)

#seperate training and testing data
set.seed(100)

#trainindicescarPrice = sample(1:nrow(carPrice), 0.7*nrow(carPrice))
#trainingData = carPrice[trainindicescarPrice,]
#testData = carPrice[-trainindicescarPrice,]

#Since observation count is very less only 205 it not usefull to distribute training data and testing data 
#we have to use all as training data.

trainingData = carPrice

#Design models


#model1.1 first model with all parameters except CarName
str(trainingData)
model_1<-lm(price~.-CarName, data=trainingData)
summary(model_1)

#Multiple R-squared:  0.9682,	Adjusted R-squared:  0.9526 

#model1.2 now run AIC which clean all non significant varaibles 
model_2<-stepAIC(model_1,direction = "both")
summary(model_2)
vif(model_2)

#Multiple R-squared:  0.9659,	Adjusted R-squared:  0.9565 

#model1.3 remove logOfHorsePower 
model_3<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
              enginesize + compressionratio + horsepower + fueltype_Diesel + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              bmw + chevrolet + dodge + honda + isuzu + jaguar + mazda + 
              mercury + mitsubishi + nissan + plymouth + porsche + renault + 
              subaru + toyota + volkswagen + volvo + enginelocation_front + 
              engineType_dohcv + engineType_l + engineType_ohcv + engineType_rotor + 
              cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
              fuelsystem_2bbl + fuelsystem_mpfi + symboling0 + symboling1 + 
              logOfEngineSize, data = trainingData)
summary(model_3)
vif(model_3)

#Multiple R-squared:  0.9651,	Adjusted R-squared:  0.9558 

#model1.4 remove symboling1
model_4<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
              enginesize + compressionratio + horsepower + fueltype_Diesel + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              bmw + chevrolet + dodge + honda + isuzu + jaguar + mazda + 
              mercury + mitsubishi + nissan + plymouth + porsche + renault + 
              subaru + toyota + volkswagen + volvo + enginelocation_front + 
              engineType_dohcv + engineType_l + engineType_ohcv + engineType_rotor + 
              cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
              fuelsystem_2bbl + fuelsystem_mpfi + symboling0 + 
              logOfEngineSize, data = trainingData)
summary(model_4)
vif(model_4)

#Multiple R-squared:  0.9648,	Adjusted R-squared:  0.9556 

#model1.5 remove jaguar
model_5<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
              enginesize + compressionratio + horsepower + fueltype_Diesel + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              bmw + chevrolet + dodge + honda + isuzu + mazda + 
              mercury + mitsubishi + nissan + plymouth + porsche + renault + 
              subaru + toyota + volkswagen + volvo + enginelocation_front + 
              engineType_dohcv + engineType_l + engineType_ohcv + engineType_rotor + 
              cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
              fuelsystem_2bbl + fuelsystem_mpfi + symboling0 + 
              logOfEngineSize, data = trainingData)
summary(model_5)
vif(model_5)

#Multiple R-squared:  0.9644,	Adjusted R-squared:  0.9555 

#model1.6 remove symboling0
model_6<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
              enginesize + compressionratio + horsepower + fueltype_Diesel + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              bmw + chevrolet + dodge + honda + isuzu + mazda + 
              mercury + mitsubishi + nissan + plymouth + porsche + renault + 
              subaru + toyota + volkswagen + volvo + enginelocation_front + 
              engineType_dohcv + engineType_l + engineType_ohcv + engineType_rotor + 
              cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
              fuelsystem_2bbl + fuelsystem_mpfi + 
              logOfEngineSize, data = trainingData)
summary(model_6)
vif(model_6)

#Multiple R-squared:  0.9642,	Adjusted R-squared:  0.9555 

#model1.7 remove fuelsystem_mpfi
model_7<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
              enginesize + compressionratio + horsepower + fueltype_Diesel + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              bmw + chevrolet + dodge + honda + isuzu + mazda + 
              mercury + mitsubishi + nissan + plymouth + porsche + renault + 
              subaru + toyota + volkswagen + volvo + enginelocation_front + 
              engineType_dohcv + engineType_l + engineType_ohcv + engineType_rotor + 
              cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
              fuelsystem_2bbl + 
              logOfEngineSize, data = trainingData)
summary(model_7)
vif(model_7)

#Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9553 

#model1.8 remove porsche
model_8<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
              enginesize + compressionratio + horsepower + fueltype_Diesel + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              bmw + chevrolet + dodge + honda + isuzu + mazda + 
              mercury + mitsubishi + nissan + plymouth + renault + 
              subaru + toyota + volkswagen + volvo + enginelocation_front + 
              engineType_dohcv + engineType_l + engineType_ohcv + engineType_rotor + 
              cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
              fuelsystem_2bbl + 
              logOfEngineSize, data = trainingData)
summary(model_8)
vif(model_8)

#Multiple R-squared:  0.963,	Adjusted R-squared:  0.9546


#model1.9 remove enginesize
model_9<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
              compressionratio + horsepower + fueltype_Diesel + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              bmw + chevrolet + dodge + honda + isuzu + mazda + 
              mercury + mitsubishi + nissan + plymouth + renault + 
              subaru + toyota + volkswagen + volvo + enginelocation_front + 
              engineType_dohcv + engineType_l + engineType_ohcv + engineType_rotor + 
              cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
              fuelsystem_2bbl + logOfEngineSize , data = trainingData)
summary(model_9)
vif(model_9)

#Multiple R-squared:  0.9587,	Adjusted R-squared:  0.9495 

#model1.10 remove carbodyhardtop
model_10<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + carbodysedan + carbodywagon + 
               bmw + chevrolet + dodge + honda + isuzu + mazda + 
               mercury + mitsubishi + nissan + plymouth + renault + 
               subaru + toyota + volkswagen + volvo + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_ohcv + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl + logOfEngineSize , data = trainingData)
summary(model_10)
vif(model_10)

#Multiple R-squared:  0.9583,	Adjusted R-squared:  0.9494 

#model1.11 remove carbodysedan
model_11<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + carbodywagon + 
               bmw + chevrolet + dodge + honda + isuzu + mazda + 
               mercury + mitsubishi + nissan + plymouth + renault + 
               subaru + toyota + volkswagen + volvo + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_ohcv + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl + logOfEngineSize , data = trainingData)
summary(model_11)
vif(model_11)

#Multiple R-squared:  0.9575,	Adjusted R-squared:  0.9487 

#model1.12 remove carbodywagon
model_12<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + chevrolet + dodge + honda + isuzu + mazda + 
               mercury + mitsubishi + nissan + plymouth + renault + 
               subaru + toyota + volkswagen + volvo + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_ohcv + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl + logOfEngineSize , data = trainingData)
summary(model_12)
vif(model_12)

#Multiple R-squared:  0.9574,	Adjusted R-squared:  0.9488 

#model1.13 remove logOfEngineSize
model_13<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + chevrolet + dodge + honda + isuzu + mazda + 
               mercury + mitsubishi + nissan + plymouth + renault + 
               subaru + toyota + volkswagen + volvo + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_ohcv + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_13)
vif(model_13)

#Multiple R-squared:  0.9568,	Adjusted R-squared:  0.9485 

#model1.14 remove engineType_ohcv
model_14<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + chevrolet + dodge + honda + isuzu + mazda + 
               mercury + mitsubishi + nissan + plymouth + renault + 
               subaru + toyota + volkswagen + volvo + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_14)
vif(model_14)

#Multiple R-squared:  0.9543,	Adjusted R-squared:  0.9458 

#model1.15 remove carwidth
model_15<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + chevrolet + dodge + honda + isuzu + mazda + 
               mercury + mitsubishi + nissan + plymouth + renault + 
               subaru + toyota + volkswagen + volvo + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_15)
vif(model_15)

#Multiple R-squared:  0.9525,	Adjusted R-squared:  0.944

#model1.16 remove renault
model_16<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + chevrolet + dodge + honda + isuzu + mazda + 
               mercury + mitsubishi + nissan + plymouth + 
               subaru + toyota + volkswagen + volvo + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_16)
vif(model_16)

#Multiple R-squared:  0.9507,	Adjusted R-squared:  0.9422 

#model1.17 remove honda
model_17<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + chevrolet + dodge + isuzu + mazda + 
               mercury + mitsubishi + nissan + plymouth + 
               subaru + toyota + volkswagen + volvo + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_17)
vif(model_17)

#Multiple R-squared:  0.9478,	Adjusted R-squared:  0.9391 

#model1.18 remove volkswagen
model_18<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + chevrolet + dodge + isuzu + mazda + 
               mercury + mitsubishi + nissan + plymouth + 
               subaru + toyota + volvo + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_18)
vif(model_18)

#Multiple R-squared:  0.947,	Adjusted R-squared:  0.9385 

#model1.19 remove fuelsystem_spdi
model_19<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + chevrolet + dodge + isuzu + mazda + 
               mercury + mitsubishi + nissan + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_19)
vif(model_19)

#Multiple R-squared:  0.9464,	Adjusted R-squared:  0.9383 

#model1.20 remove mazda
model_20<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + chevrolet + dodge + isuzu + 
               mercury + mitsubishi + nissan + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_20)
vif(model_20)

#Multiple R-squared:  0.9449,	Adjusted R-squared:  0.9368 

#model1.21 remove mercury
model_21<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + chevrolet + dodge + isuzu + mitsubishi + nissan + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_21)
vif(model_21)

#Multiple R-squared:  0.9439,	Adjusted R-squared:  0.9361 

#model1.22 remove chevrolet
model_22<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + dodge + isuzu + mitsubishi + nissan + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_22)
vif(model_22)

#Multiple R-squared:  0.9429,	Adjusted R-squared:  0.9353 

#model1.23 remove isuzu
model_23<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + carbodyhatchback + 
               bmw + dodge + mitsubishi + nissan + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_23)
vif(model_23)

#Multiple R-squared:  0.9418,	Adjusted R-squared:  0.9344 

#model1.24 remove carbodyhatchback
model_24<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + 
               bmw + dodge + mitsubishi + nissan + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_24)
vif(model_24)

#Multiple R-squared:  0.9406,	Adjusted R-squared:  0.9334 

#model1.25 remove nissan
model_25<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + 
               bmw + dodge + mitsubishi + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six + 
               fuelsystem_2bbl , data = trainingData)
summary(model_25)
vif(model_25)

#Multiple R-squared:  0.939,	Adjusted R-squared:  0.932 

#model1.26 remove fuelsystem_2bbl
model_26<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + horsepower + fueltype_Diesel + 
               bmw + dodge + mitsubishi + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_26)
vif(model_26)

#Multiple R-squared:  0.9369,	Adjusted R-squared:  0.9301

#model1.27 remove horsepower
model_27<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + fueltype_Diesel + 
               bmw + dodge + mitsubishi + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_dohcv + engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_27)
vif(model_27)

#Multiple R-squared:  0.9347,	Adjusted R-squared:  0.9279

#model1.28 remove engineType_dohcv
model_28<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + fueltype_Diesel + 
               bmw + dodge + mitsubishi + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_28)
vif(model_28)

#Multiple R-squared:  0.9343,	Adjusted R-squared:  0.928

#model1.29 remove fueltype_Diesel
model_29<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               compressionratio + 
               bmw + dodge + mitsubishi + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_29)
vif(model_29)

#Multiple R-squared:  0.9324,	Adjusted R-squared:  0.9262

#model1.30 remove compressionratio
model_30<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + bmw + dodge + mitsubishi + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_30)
vif(model_30)

#Multiple R-squared:  0.9322,	Adjusted R-squared:  0.9265

#model1.31 remove carlength
model_31<-lm(formula = price ~ wheelbase + carheight + curbweight + bmw + dodge + mitsubishi + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_31)
vif(model_31)

#Multiple R-squared:  0.9297,	Adjusted R-squared:  0.9242

#model1.32 remove dodge
model_32<-lm(formula = price ~ wheelbase + carheight + curbweight + bmw + mitsubishi + plymouth + 
               subaru + toyota + enginelocation_front + 
               engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_32)
vif(model_32)

#Multiple R-squared:  0.9279,	Adjusted R-squared:  0.9226 

#model1.33 remove plymouth
model_33<-lm(formula = price ~ wheelbase + carheight + curbweight + bmw + mitsubishi + 
               subaru + toyota + enginelocation_front + 
               engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_33)
vif(model_33)

#Multiple R-squared:  0.9261,	Adjusted R-squared:  0.921 

#model1.34 remove subaru
model_34<-lm(formula = price ~ wheelbase + carheight + curbweight + bmw + mitsubishi + 
               toyota + enginelocation_front + 
               engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_34)
vif(model_34)

#Multiple R-squared:  0.9241,	Adjusted R-squared:  0.9194

#model1.35 remove mitsubishi
model_35<-lm(formula = price ~ wheelbase + carheight + curbweight + bmw + 
               toyota + enginelocation_front + 
               engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_35)
vif(model_35)

#Multiple R-squared:  0.9199,	Adjusted R-squared:  0.9154

#model1.36 remove toyota
model_36<-lm(formula = price ~ wheelbase + carheight + curbweight + bmw + enginelocation_front + 
               engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_36)
vif(model_36)

#Multiple R-squared:  0.9156,	Adjusted R-squared:  0.9112 

#model1.37 remove carheight
model_37<-lm(formula = price ~ wheelbase + curbweight + bmw + enginelocation_front + 
               engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_37)
vif(model_37)

#Multiple R-squared:  0.9109,	Adjusted R-squared:  0.9067

#model1.38 remove wheelbase
model_38<-lm(formula = price ~ curbweight + bmw + enginelocation_front + 
               engineType_l + engineType_rotor + 
               cylinderNumber_five + cylinderNumbere_four + cylinderNumber_six , data = trainingData)
summary(model_38)
vif(model_38)

#Multiple R-squared:  0.9084,	Adjusted R-squared:  0.9047

# model_38 will be our final model as pr value for all remaining varibale is very significant 
# And on removing any value from data model causing huge decrease in R square 


#Let's predict the price of car by using model_38

Predict_1 <- predict(model_38,trainingData[,-2])
trainingData$testprice <- Predict_1
trainingData <- trainingData%>%dplyr::select(testprice, everything()) # move testprice to 1st column

trainingData$error <-  trainingData$price - trainingData$testprice #find error between actual price and predicted price
trainingData <- trainingData%>%dplyr::select(error, everything()) #move error to first column

#line graph between carName and price of cars
ggplot(trainingData, aes(x = CarName, y = price)) + geom_line(aes(group = 1))

#line graph between carName and testprice of cars
ggplot(trainingData, aes(x = CarName, y = testprice)) + geom_line(aes(group = 1))

#line graph between carName and error of cars
ggplot(trainingData, aes(x = CarName, y = error)) + geom_line(aes(group = 1))


#line graph between carName and testprice and Price of cars
ggplot(trainingData, aes(CarName, price)) + geom_line(aes(colour = "Actula Price",group = 1 )) + geom_line(aes(x = CarName, y = testprice, colour="Predicted Price",group = 1))

#In above graph we able to see paatern of car price for actulat and predicted is same 

r <- cor(trainingData$price,trainingData$testprice)
r
rSquared <- cor(trainingData$price,trainingData$testprice)^2
rSquared

#R-square calculated by model_38 and by formula on predicted price is 
#approxamitely same So, we conclude above model is good for car price prediction .

#Variables which are used for prediction is 
#1) curbweight
#2) bmw
#3) enginelocation_front
#4) engineType_l
#5) engineType_rotor
#6) cylinderNumber_five
#7) cylinderNumbere_four
#6) cylinderNumber_six

#with Adjusted R-squared:  0.9047

#Code execution completed
