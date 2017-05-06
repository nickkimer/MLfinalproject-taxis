setwd("~/ML Final Project")
library(dplyr)
library(plyr)
library(ggplot2)
####################################################################
###################  Predictive model  #############################
####################################################################
# Overview 
# 1. The Tip percentage variable is created 
# 2. Feature engineering variables 
#   (list: "trip_time_minutes" "Lpep_dropoff_hour" "lpep_pickup_hour"  "dayofweek"        
#     "Tolls.present"     "neg.zero.fares"    "no.payment.type"   "dayparting"       
#     "tip.percentage"   )
# 
# 3. Variables are removed from the data frame the is believed not to add value this includes:
#    - Longitude and latitude drop off and pick up columns(total of 4 columns)
#
# 4. Implement the deep learning model
#   4a) create a training set and testing set
#   4b) Model tunning(finding the best parameters for the model in a given space)
#   4c) testing the model in the testing set 

Taxi.data.raw <-  read.csv("green_tripdata_2015-09.csv")
Taxi.data.raw <- Taxi.data.raw[!Taxi.data.raw$Total_amount<=0,]
sum(Taxi.data.raw[Taxi.data.raw$Payment_type==2, "Tip_amount"]) #Only 2 cash transactions that occur that have a tip indicated. 
Taxi.data.raw <- Taxi.data.raw[!Taxi.data.raw$Fare_amount==0, ] 
Taxi.data.raw <- Taxi.data.raw[!Taxi.data.raw$Payment_type==2, ] #We don't know the tip for cash transactions
Taxi.data.raw$Total_amount= Taxi.data.raw$Total_amount- Taxi.data.raw$Tip_amount
##### 1. The Tip percentage variable is created  ########

#Total_fare <- as.data.frame(as.numeric(Taxi.data.raw$Tip_amount)
#                           + as.numeric(Taxi.data.raw$Total_amount))

# creates the Total fare which Tip amount plus total amount
# added together. 

# Taxi.data.raw$tip.percentage <- Taxi.data.raw$Tip_amount/Total_fare

Taxi.data.raw[,"Tip_percent"] <- Taxi.data.raw$Tip_amount/(Taxi.data.raw$Total_amount)

# Creates the tip.percentage by dividing teh tip amount by 
# the total fare(calculated above)


### EDA
hist(Taxi.data.raw$Tip_percent)


no_zero_neg_fare= Taxi.data.raw[!Taxi.data.raw$Total_amount<=0,]

less_5_pct_tip= no_zero_neg_fare[no_zero_neg_fare$Tip_percent<=.05, ]
gr_5_pct_tip= no_zero_neg_fare[no_zero_neg_fare$Tip_percent>.05, ]
hist(less_5_pct_tip$Tip_percent, xlim = c(0.01,0.06), ylim= c(0,2000))
hist(gr_5_pct_tip$Tip_percent, ylim= c(0,5000))
# 
# no_zero_neg_fare.feat= Taxi.data.feat[!Taxi.data.feat$Total_amount<=0,]
# plot(Taxi.data.feat$dayofweek, Taxi.data.feat$Tip_percent)
# 
# sum(no_zero_neg_fare.feat[no_zero_neg_fare.feat$Total_amount<1,"Total_amount"])
# trip_distance_agg_by_dow <- ddply(no_zero_neg_fare.feat, .(no_zero_neg_fare.feat$dayofweek), summarize,  Mean=mean(Tip_percent), Median=median(Tip_percent), total_spent= sum(Total_amount))
# trip_distance_agg_by_dow <- ddply(no_zero_neg_fare.feat, .(no_zero_neg_fare.feat$lpep_pickup_hour), summarize,  Mean=mean(Tip_percent), Median=median(Tip_percent), total_spent= sum(Total_amount), total_transactions= length(Total_amount))
# par(mfrow=c(2,1))
# plot(trip_distance_agg_by_dow$`no_zero_neg_fare.feat$lpep_pickup_hour`, trip_distance_agg_by_dow$total_transactions)
# plot(trip_distance_agg_by_dow$`no_zero_neg_fare.feat$lpep_pickup_hour`, trip_distance_agg_by_dow$Mean)

### 2. Feature engineering variables #########
# The following function creates a data frame with added features
# from feature engineering. The Validation data frame should be put
# through this function inorder to have the same vairables.

training.validation.df <- function(Taxi.data.raw) {
  
  Taxi.data.raw$lpep_pickup_datetime <- 
    as.POSIXct(Taxi.data.raw$lpep_pickup_datetime,
               format='%Y-%m-%d %H:%M:%S')
  
  # Changes the pickup datetime into POSIXct which allows for easier
  # calculations and extractions 
  
  Taxi.data.raw$Lpep_dropoff_datetime <- 
    as.POSIXct(Taxi.data.raw$Lpep_dropoff_datetime,
               format='%Y-%m-%d %H:%M:%S')
  # Changes the pickup datetime into POSIXct which allows for easier
  # calculations and extractions 
  
  Taxi.data.raw$trip_time_minutes <- 
    as.numeric(round((Taxi.data.raw$Lpep_dropoff_datetime 
                      -Taxi.data.raw$lpep_pickup_datetime) / 60))
  
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
plot(Taxi.data.feat$Total_amount, Taxi.data.feat$Tip_amount)
# initialize h2o server
library(h2o)
h2o = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = "128G", nthreads = -1)
response <- "Tip_percent"
#dput(colnames(Taxi.data.feat))
predictors = c("VendorID", 
               "Store_and_fwd_flag", "RateCodeID", "Passenger_count", "Trip_distance", 
               "Fare_amount", "Extra", "MTA_tax", "Tolls_amount", 
               "improvement_surcharge", "Total_amount", "Payment_type", 
               "Trip_type", "trip_time_minutes",  "Lpep_dropoff_hour", "lpep_pickup_hour",
               "dayofweek", "Tolls.present",  "no.payment.type", 
               "dayparting")

lapply(Taxi.data.feat[which(colnames(Taxi.data.feat) %in% c(response, predictors))], class)

cols = c("VendorID", "Store_and_fwd_flag", "RateCodeID", "Payment_type","Trip_type","Lpep_dropoff_hour","lpep_pickup_hour","Tolls.present","no.payment.type","dayparting")  
Taxi.data.feat[cols] <- lapply(Taxi.data.feat[cols], factor) 
lapply(Taxi.data.feat,class)   

training.data.h2o = as.h2o(as.data.frame(Taxi.data.feat[which(colnames(Taxi.data.feat) %in% c(response, predictors))]), destination_frame = "training_data")

#Training and testing frame
taxi.split <- h2o.splitFrame(training.data.h2o, ratios = c(0.8), seed = 12345)
train <- taxi.split[[1]]
test <-  taxi.split[[2]]

lmodel <- h2o.gbm(x= predictors, y= response, training_frame = train, nfolds = 5)
#h2o.saveModel(lmodel, path=getwd(),force = TRUE)
summary(lmodel)
lmodel_perf <- h2o.performance(lmodel, test)
lmodel_perf

predicted <- h2o.predict(lmodel, test)
predicted <- as.data.frame(predicted)

actual <- as.data.frame(test$Tip_percent)
act_pred=cbind(actual,predicted)
act_pred=cbind(as.data.frame(test$Total_amount),act_pred)

ggplot(act_pred, aes(x = actual, y = predicted)) +
  geom_abline(lty = "dashed", col = "red") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  labs(
    x = "Actual",
    y = "Predicted ",
    title = "Predicted vs. Actual Tip Percentage"
  )

#######################################

# Tuning the Deep learning model (finding the best paraemters)
hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=list(c(20,20),c(60,60),c(35,35,35),c(25,25,25,25)),
  input_dropout_ratio=c(0,0.05),
  l1=seq(0,1e-4,1e-6),
  l2=seq(0,1e-4,1e-6),
  loss = "Huber"
)
hyper_params

# The follwing indicates the hyperparamters going to be tested for the 
# deep learing model. The loss function is huber(less senstive to 
# outliers and is used for regression). L1 and L2 are regularizations.
# (This prevents overfitting the data).

## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
search.criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 200, max_models = 100, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)

# Indicates search parameters through the space. Indicats a random search. 
# The stopping rounds tells how many times the model can be below the
# stopping tolerance before it is dropped. 


predictors <- c("VendorID",  "Store_and_fwd_flag", "RateCodeID", "Passenger_count","Trip_distance", 
                "Fare_amount", "Extra", "MTA_tax", "Tolls_amount", "improvement_surcharge","Total_amount",
                "Payment_type", "Trip_type", "lpep_pickup_hour", "Lpep_dropoff_hour", "trip_time_minutes", "dayofweek", "Tolls.present",
                "neg.zero.fares","no.payment.type", "dayparting","sq_amt")

# Predictors fo the data frame 

response <- c("tip.percentage")
# reponse variable to the data frame 

dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "dl.random",
  training_frame=training.data.h2o,
  nfolds= 5,
  x= predictors, 
  y=response,
  epochs=3,
  stopping_metric="MSE",
  stopping_tolerance=1e-2,       
  stopping_rounds=2,
  hyper_params = hyper_params,
  search_criteria = search.criteria
)       
# runs a grid search over the space using MSE as the stopping metric

taxi.grid <- h2o.getGrid("dl.random",sort_by="MSE",decreasing=FALSE)
# taxi.grid contains a grid of the deep learning model sorting in 
# increasing order of the MSE

deeplearning_best_model <- h2o.getModel(taxi.grid@model_ids[[1]]) 
# Takes the model with the lowest MSE 

h2odeeplearning_best_params <- best_model@allparameters
# Gets  the parameters of the best model 

# above are the best paramters of the model for teh activation, hidden
# input_dropout ratio for the given space. 


###  4c) testing the model in the testing set
test.data.h2o = as.h2o(as.data.frame(test.data), destination_frame = 'testing.data')

h2o.tip.predictions = h2o.predict(deeplearning_best_model, test.data.h2o)
# uses the deep learning model to predic the amount of tip 

h2o.tip.predictions = as.data.frame(h2o.tip.predictions)$predict
# converts teh h2o data frame to a R dataframe(which is more 
# assessible)


perf_deep_complete <- h2o.performance(deeplearning_best_model, newdata = test.data.h2o)
# gets the performance of the model 

perf_deep_complete
# shows performance of the model

# Performance of the model:(if model must be re-run numbers could change slightly)
# MSE :  5.109213e-05
# RMSE : 0.007147876
# MAE : 0.001404323
# RMSLE : 0.006606055
# Mean Residual Deviance: 1.340357e-06

h2o.shutdown()
# stops the h2o.server(recommend doing this to avoid random errors
# with other functions)
