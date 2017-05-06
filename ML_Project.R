setwd("/home/ec2-user/.ssh")
library(lubridate)
library(dplyr)
library(plyr)
library(ggplot2)

taxi1 = read.csv("green_tripdata_2015-01.csv", header = FALSE,skip=1)
taxi2 = read.csv("green_tripdata_2015-02.csv", header = FALSE,skip=1)
taxi3 = read.csv("green_tripdata_2015-03.csv", header = FALSE,skip=1)
taxi4 = read.csv("green_tripdata_2015-04.csv", header = FALSE,skip=1)
taxi5 = read.csv("green_tripdata_2015-05.csv", header = FALSE,skip=1)
taxi6 = read.csv("green_tripdata_2015-06.csv", header = FALSE,skip=1)

taxi7 = read.csv("green_tripdata_2015-07.csv")
taxi8 = read.csv("green_tripdata_2015-08.csv")
taxi9 = read.csv("green_tripdata_2015-09.csv")
taxi10 = read.csv("green_tripdata_2015-10.csv")
taxi11 = read.csv("green_tripdata_2015-11.csv")  
taxi12 = read.csv("green_tripdata_2015-12.csv")  
Taxi.data.raw = rbind(taxi1[,1:21],taxi2[,1:21],taxi3[,1:21],taxi4[,1:21],taxi5[,1:21],taxi6[,1:21])
Taxi.data.raw2 = rbind(taxi7,taxi8,taxi9,taxi10,taxi11,taxi12)
colnames(Taxi.data.raw ) <- colnames(Taxi.data.raw2)
Taxi.data.raw <- rbind(Taxi.data.raw,Taxi.data.raw2)



Taxi.data.raw$lpep_pickup_datetime <- as.POSIXct(Taxi.data.raw$lpep_pickup_datetime,format='%Y-%m-%d %H:%M:%S')
Taxi.data.raw$Lpep_dropoff_datetime <- as.POSIXct(Taxi.data.raw$Lpep_dropoff_datetime,format='%Y-%m-%d %H:%M:%S')

## Calculate the duration and avg speed variables
Taxi.data.raw$duration <- floor(as.double(Taxi.data.raw$Lpep_dropoff_datetime-Taxi.data.raw$lpep_pickup_datetime)/60.0)
#Taxi.data.raw$percent  <- Taxi.data.raw$Tip_amount/Taxi.data.raw$Fare_amount * 100.0
Taxi.data.raw$speed    <- Taxi.data.raw$Trip_distance/Taxi.data.raw$duration *60


#Taxi.data.raw <-  read.csv("green_tripdata_2015-09.csv")
Taxi.data.raw <- Taxi.data.raw[!Taxi.data.raw$Total_amount<=0,]
sum(Taxi.data.raw[Taxi.data.raw$Payment_type==2, "Tip_amount"]) #Only 2 cash transactions that occur that have a tip indicated. 
Taxi.data.raw <- Taxi.data.raw[!Taxi.data.raw$Fare_amount==0, ] 
Taxi.data.raw <- Taxi.data.raw[!Taxi.data.raw$Payment_type==2, ] #We don't know the tip for cash transactions
Taxi.data.raw$Total_amount= Taxi.data.raw$Total_amount- Taxi.data.raw$Tip_amount
##### 1. The Tip percentage variable is created  ########
Taxi.data.raw[,"Tip_percent"] <- Taxi.data.raw$Tip_amount/(Taxi.data.raw$Total_amount)

training.validation.df <- function(Taxi.data.raw) {
  
  # Taxi.data.raw$lpep_pickup_datetime <- 
  #   as.POSIXct(Taxi.data.raw$lpep_pickup_datetime,
  #              format='%Y-%m-%d %H:%M:%S')
  # 
  # # Changes the pickup datetime into POSIXct which allows for easier
  # # calculations and extractions 
  # 
  # Taxi.data.raw$Lpep_dropoff_datetime <- 
  #   as.POSIXct(Taxi.data.raw$Lpep_dropoff_datetime,
  #              format='%Y-%m-%d %H:%M:%S')
  # Changes the pickup datetime into POSIXct which allows for easier
  # calculations and extractions 
  
  #Taxi.data.raw$trip_time_minutes <- 
  # as.numeric(round((Taxi.data.raw$Lpep_dropoff_datetime 
  #                   -Taxi.data.raw$lpep_pickup_datetime) / 60))
  
  # Calculates the length of the trip in minutes by subtracting
  # the dropoff datetime by pickup datetime. Rounds up a 
  # minute when 30 seconds or more. 
  
  Taxi.data.raw$Lpep_dropoff_hour <- 
    as.POSIXlt(Taxi.data.raw$Lpep_dropoff_datetime)$hour
  # Creates a column for the hour of the drop off of a taxi
  
  Taxi.data.raw$lpep_pickup_hour <- 
    as.POSIXlt(Taxi.data.raw$lpep_pickup_datetime)$hour
  # Creates a column for the hour of the pick up of a taxi 
  
  Taxi.data.raw$dayofweek <- 
    as.factor(weekdays(Taxi.data.raw$Lpep_dropoff_datetime))
  
  # Creates a variable dayofweek. This has which day of week the
  # taxi trip had ended (Mon. -Sun.)
  
  Taxi.data.raw$Tolls.present <- sapply(as.numeric(
    Taxi.data.raw$Tolls_amount),
    function(x) {ifelse(0 < x, 1, 0 )})
  
  # Creates a variable Tolls.present. Tolls.present indicates whether
  # the trip had contained a Toll payment.
  
  # Taxi.data.raw$neg.zero.fares1 <- sapply(as.numeric
  #                                         (Taxi.data.raw$Fare_amount), 
  #                                         function(x) {ifelse(0 < x, 1, 0)})
  #Taxi.data.raw["neg.zero.fares"] <- ifelse(Taxi.data.feat$Total_amount<=0,1,0)
  
  # Changes the negative fair amount to zero. This was done becuase the 
  # tip amount is still considered to be zero in this case. 
  
  Taxi.data.raw$no.payment.type <- sapply(Taxi.data.raw$Payment_type, 
                                          function(x){ ifelse(x == 3 | x == 4, 1, 0)})
  # If the payment type is 3 or 4 it is indicated by 1 in the
  # no.payment.type. This is beucase there is now tip and no payemnt 
  # notice or clue
  
  Taxi.data.raw$Trip_type <- sapply(Taxi.data.raw$Trip_type, function(x){
    ifelse( is.na(x)== TRUE, 
            "other", Taxi.data.raw$Trip_type)})
  # Changes the NA values of trip type to other so to not to lose data and 
  # can still show inside rather than removing the rows 
  
  Taxi.data.raw$dayparting <- 
    sapply(as.POSIXlt(Taxi.data.raw$lpep_pickup_datetime)$hour,  function(x){
      ifelse(x>=0 & x<=5, "Early.Morn", 
             ifelse(x >=6  & x<= 11, "Morning",
                    ifelse (x>=12 & x<= 17, "Afternoon", 
                            ifelse(x>= 18 & x <= 23, "Evening", NA))))})
  
  # Bins the hours into Early Morning, Morning, Afternoon, and Evening. This is seperated by 0-24
  # 
  # Total_fare <- as.data.frame(as.numeric(Taxi.data.raw$Tip_amount)
  #                             + as.numeric(Taxi.data.raw$Total_amount))
  
  # Creates the Total fare which Tip amount plus total amount
  # added together. 
  
  
  # Taxi.data.raw[,"tip.percentage"] <- (Taxi.data.raw$Tip_amount/Taxi.data.raw$Total_amount)
  #Taxi.data.raw[,"Tip_percent"] <- ifelse(Taxi.data.raw$Total_amount> 0,Taxi.data.raw$Tip_amount/(Taxi.data.raw$Total_amount-Taxi.data.raw$Tip_amount), 0)
  
  # Creates the tip.percentage by dividing teh tip amount by 
  # the total fare(calculated above)
  
  #Taxi.data.raw$tip.percentage <- sapply(Taxi.data.raw$tip.percentage,
  #                                      function(x) ifelse( x <= 0 | is.nan(x)==TRUE, 0, Taxi.data.raw$tip.percentage))
  
  # If the tip percentage is negative inputs a 0 beucase no tip
  # likely did not occur here. However, I want the model to learn
  # the pattern of these situations. 
  
  return(Taxi.data.raw)
  # returns the data frame 
}

Taxi.data.feat<- training.validation.df(Taxi.data.raw)
#Taxi.data.feat$month <- as.factor(month(Taxi.data.feat$lpep_pickup_datetime))

green_adm <- filter(Taxi.data.feat, RateCodeID %in% c(1,2,6), Payment_type %in% c(1,2))
Taxi.data.feat<-filter(green_adm,Fare_amount<200,Total_amount<1000,Tolls_amount<30,Fare_amount>0,Trip_distance<50,
                    Trip_distance>0.02,duration<120,duration>=1,speed>0,speed<100)

cols = c("VendorID", "Store_and_fwd_flag", "RateCodeID", "Payment_type","Trip_type","Lpep_dropoff_hour","lpep_pickup_hour","Tolls.present","no.payment.type","dayparting")  
Taxi.data.feat[cols] <- lapply(Taxi.data.feat[cols], factor) 
lapply(Taxi.data.feat,class)   
response <- "Tip_percent"
predictors = c("VendorID", 
               "Store_and_fwd_flag", "RateCodeID", "Passenger_count", "Trip_distance", 
               "Fare_amount", "Extra", "MTA_tax", "Tolls_amount", 
               "improvement_surcharge", "Total_amount", "Payment_type", 
               "Trip_type",  "Lpep_dropoff_hour", "lpep_pickup_hour",
               "dayofweek", "Tolls.present",  "no.payment.type", 
               "dayparting","duration","speed")
# Taxi.data.feat[cols] <- lapply(Taxi.data.feat[cols], factor) 
# lapply(Taxi.data.feat,class)   

# train= Taxi.data.feat[Taxi.data.feat$month!= 11| Taxi.data.feat$month!= 12,]
# valid= Taxi.data.feat[Taxi.data.feat$month== 11,]
# test= Taxi.data.feat[Taxi.data.feat$month== 12,]
# training.data.h2o = as.h2o(as.data.frame(train[which(colnames(Taxi.data.feat) %in% c(response, predictors))]), destination_frame = "training_data")
# valid.data.h2o= as.h2o(as.data.frame(valid[which(colnames(Taxi.data.feat) %in% c(response, predictors))]), destination_frame = "validation_data")
# test.data.h2o= as.h2o( as.h2o(as.data.frame(test[which(colnames(Taxi.data.feat) %in% c(response, predictors))]), destination_frame = "test_data"))

library(h2o)
h2o = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = "128G", nthreads = -1)

set.seed(12345)
test.data = Taxi.data.feat[sample(1:nrow(Taxi.data.feat), .1*nrow(Taxi.data.feat), replace= FALSE),which(colnames(Taxi.data.feat) %in% c(predictors, response))]
filtered_data= Taxi.data.feat[-as.numeric(rownames(test.data)),]
set.seed(12345)
filtered_data= filtered_data[sample(1:nrow(filtered_data), .5*nrow(filtered_data), replace= FALSE),]
training.data.h2o = as.h2o(filtered_data, destination_frame = "training_data")
test.data.h2o = as.h2o(test.data, destination_frame = 'testing_data')

x=Taxi.data.feat[,which(colnames(Taxi.data.feat) %in% c(response, predictors))]
training.data.h2o = as.h2o(x, destination_frame = "training_data")
dim(training.data.h2o)
#Training and testing frame
taxi.split <- h2o.splitFrame(training.data.h2o, ratios = c(0.5,.1), seed = 12345)
train <- taxi.split[[1]]
test <-  taxi.split[[2]]

lmodel <- h2o.gbm(x= predictors, y= response, training_frame = train, nfolds = 5, ntrees = 100)
#h2o.saveModel(lmodel, path=getwd(),force = TRUE)
summary(lmodel)
lmodel_perf <- h2o.performance(lmodel, test)
lmodel_perf

predicted <- h2o.predict(lmodel, test)
predicted <- as.data.frame(predicted)


lmodel <- h2o.glm(x= predictors, y= response, training_frame = training.data.h2o , validation_frame= valid.data.h2o)
