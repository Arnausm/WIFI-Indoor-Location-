## INDOOR LOCATIONING. WIFI ##
## Arnau Simó Muñoz ##


## I did two models: randomforest from package "randomForest"
## and KNN from package "FNN". The best results came from rf.
## I did, also, rf per building.

#Libraries
library(dplyr)
library(reshape2)
library(FNN)
library(randomForest)

pacman::p_load("plotly","esquisse","reshape2","class","spatstat",
               "dplyr","tidyr","RMySQL", "caret","e1071","FNN",
               "ggplot2","padr","zoo","randomForest","gridExtra","grid","car")

training_data <- read.csv("C:/Users/Arnau/Documents/Task Wi-Fi/trainingData.csv", stringsAsFactors = FALSE)
validation_data <- read.csv("C:/Users/Arnau/Documents/Task Wi-Fi/validationData.csv", stringsAsFactors = FALSE)

#### PREPROCESSING ####

## Remove duplicate rows
training_data <- distinct(training_data)

## Find out WAPs in training with no information, value = 100
null_WAP_training <- which(apply(training_data[,1:520], 2, function(x) mean(x)) == 100)

training_data <- training_data[,-c(null_WAP_training)] # wAPS training

## Find out WAPs in validation with no information, value = 100
null_WAP_validation <- which(apply(validation_data[,1:520], 2, function(x) mean(x)) == 100)

validation_data <- validation_data[,-c(null_WAP_validation)] # wAPS validation

## Replace values 100

  # Values = 100 are replaced for a value of -105
  # Value = 100 means that don't receive any information

# Training
trainWaps <- training_data[,1:465]

trainWaps[trainWaps == 100] <- -105

training_data[,1:465] <- trainWaps[,1:465] # Replace modified values

# validation
validationwaps <- validation_data[,1:367]

validationwaps[validationwaps == 100] <- -105

validation_data[,1:367] <- validationwaps[,1:367]

## Converting data types

# Training 
training_data$FLOOR <- as.factor(training_data$FLOOR)
training_data$BUILDINGID <- as.factor(training_data$BUILDINGID)
training_data$RELATIVEPOSITION <- as.factor(training_data$RELATIVEPOSITION)
training_data$SPACEID <- as.factor(training_data$SPACEID)
training_data$USERID <- as.factor(training_data$USERID)
training_data$PHONEID <- as.factor(training_data$PHONEID)

# Validation
validation_data$FLOOR <- as.factor(validation_data$FLOOR)
validation_data$BUILDINGID <- as.factor(validation_data$BUILDINGID)
validation_data$RELATIVEPOSITION <- as.factor(validation_data$RELATIVEPOSITION)
validation_data$SPACEID <- as.factor(validation_data$SPACEID)
validation_data$USERID <- as.factor(validation_data$USERID)
validation_data$PHONEID <- as.factor(validation_data$PHONEID)

#### Histogram waps ####

# All waps
plot(validation_data$LONGITUDE, validation_data$LATITUDE)

plot(training_data$LONGITUDE, training_data$LATITUDE)

valors <- c()

for(i in 1:465){
  valors <- c(valors,trainWaps[,i])
}

hist(valors[which(valors != -105)])

## WAPs with signal

## Remove WAPs that doesn't work in both parts

trainwapn <- names(trainWaps) # WAPs training
valiwapn <- names(validationwaps) # WAPs validation

Waps <- intersect(trainwapn,valiwapn) # WAPs matched in both parts

trainWaps <- training_data[,c(Waps)]
traininfo <- training_data[,466:474] # columns with other information (no WAPs)

training_data_new <- cbind(trainWaps,traininfo)

# Validation

validationwaps <- validation_data[,c(Waps)]
validationinfo <- validation_data[,368:376] # columns with other information (no WAPs)

validation_data_new <- cbind(validationwaps,validationinfo)

# No waps

info <- names(traininfo)


#### Creation of variables for the models ####

## Waps ##

Waps <- names(training_data_new[1:312])

## Waps + building ## 

g <- training_data_new %>% select(c(1:312),BUILDINGID)
Wapsnbuilding <- names(g)
rm(g)

## Waps + building + floor ##

g <- training_data_new %>% select(c(1:312),BUILDINGID,FLOOR)
Wapsnbuildingnfloor <- names(g)
rm(g)

## Waps + building + floor + latitud ##

g <- training_data_new %>% select(c(1:312),BUILDINGID,FLOOR,LATITUDE)
Wapsnbuildingnfloornlatitude <- names(g)
rm(g)

## Data.frame created to storage the results of models ##

model_results = data.frame()[1:1111, ]


#### PLOTS Group_by building ####

a <- training_data_new %>%
    filter(apply(training_data_new[,Waps],1, function(x) any(x > -60)))
a
rm(a)

b0 <- which(training_data_new$BUILDINGID == 0)
b1 <- which(training_data_new$BUILDINGID == 1)
b2 <- which(training_data_new$BUILDINGID == 2)


plot(x = training_data_new[b0,]$LONGITUDE, y = training_data_new[b0,]$LATITUDE)
plot(x = training_data_new[b1,]$LONGITUDE, y = training_data_new[b1,]$LATITUDE)
plot(x = training_data_new[b2,]$LONGITUDE, y = training_data_new[b2,]$LATITUDE)

## PLOTS Group_by floor

F0 <- which(training_data_new$FLOOR == 0)
F1 <- which(training_data_new$FLOOR == 1)
F2 <- which(training_data_new$FLOOR == 2)


plot(x = training_data_new[b0,]$LONGITUDE, y = training_data_new[b0,]$LATITUDE)
plot(x = training_data_new[b1,]$LONGITUDE, y = training_data_new[b1,]$LATITUDE)
plot(x = training_data_new[b2,]$LONGITUDE, y = training_data_new[b2,]$LATITUDE)


# Remove columns for model

training_data_new$SPACEID <- NULL
training_data_new$RELATIVEPOSITION <- NULL
training_data_new$USERID <- NULL
training_data_new$PHONEID <- NULL
training_data_new$TIMESTAMP <- NULL

validation_data_new$SPACEID <- NULL
validation_data_new$RELATIVEPOSITION <- NULL
validation_data_new$USERID <- NULL
validation_data_new$PHONEID <- NULL
validation_data_new$TIMESTAMP <- NULL


#### Modelling ####

#### Random Forest Model ####

## Building

set.seed(450)
inTraining <- createDataPartition(training_data_new$BUILDINGID, p = 0.7, list = FALSE)
training <- training_data_new[inTraining,]
testing <- training_data_new[-inTraining,]

# Function to find the best "mtry" 

# bestmtry_rf<-tuneRF(training_data_new[Waps], 
#                     training_data_new$BUILDINGID,
#                     ntreeTry = 100, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)

# Random forest. mtry = 17
system.time(Building_model_rf <- randomForest(y=training_data_new$BUILDINGID,
                                   x=training_data_new[Waps],
                                   importance=T,
                                   method="rf",
                                   ntree=100,
                                   mtry=17))

# Prediction training
Building_pred_training_rf <- predict(Building_model_rf,training)

# Prediction testing and confusion matrix
Building_pred_testing_rf <- predict(Building_model_rf,testing)

    # Evaluate the obtained values
confusionMatrix(Building_pred_testing_rf, testing$BUILDINGID)

# Prediction validation
Building_pred_validation_rf <- predict(Building_model_rf,validation_data_new)

    # Evaluate the obtained values
confusionMatrix(Building_pred_validation_rf, validation_data_new$BUILDINGID)

# Add the values of predictions in a new column
model_results$BUILDINGID_pred_rf <- Building_pred_validation_rf


## Floor 

# Data partition

set.seed(450)
inTraining <- createDataPartition(training_data_new$FLOOR, p = 0.7, list = FALSE)
training <- training_data_new[inTraining,]
testing <- training_data_new[-inTraining,]

# Function to find the best "mtry" 

# bestmtry_rf<-tuneRF(training_data_new[Wapsnbuilding], 
#                     training_data_new$FLOOR,
#                     ntreeTry = 100, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)

# Random forest. mtry = 34

system.time(Floor_model_rf <- randomForest(y=training_data_new$FLOOR,
                                   x=training_data_new[Wapsnbuilding],
                                   importance=T,
                                   method="rf",
                                   ntree=100,
                                   mtry=34))

# Prediction training
Floor_pred_training_rf <- predict(Floor_model_rf,training)

# Prediction testing and confusion matrix
Floor_pred_testing_rf <- predict(Floor_model_rf,testing)

  # Evaluate the obtained values
  confusionMatrix(Floor_pred_testing_rf, testing$FLOOR)

# Prediction validation
Floor_pred_validation_rf <- predict(Floor_model_rf,validation_data_new)

  # Evaluate the obtained values
  confusionMatrix(Floor_pred_validation_rf, validation_data_new$FLOOR)

# Add the values of predictions in a new column
model_results$FLOOR_pred_rf <- Floor_pred_validation_rf


## LATITUDE

set.seed(450)
inTraining <- createDataPartition(training_data_new$LATITUDE, p = 0.7, list = FALSE)
training <- training_data_new[inTraining,]
testing <- training_data_new[-inTraining,]

# Function to find the best "mtry"

# bestmtry_rf<-tuneRF(training_data_new[Wapsnbuildingnfloor], 
#                     training_data_new$LATITUDE,
#                     ntreeTry = 100, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)

# Random forest. mtry = 104

system.time(Latitude_model_rf <- randomForest(y=training_data_new$LATITUDE,
                                        x=training_data_new[Wapsnbuildingnfloor],
                                        importance=T,
                                        method="rf",
                                        ntree=100,
                                        mtry=40))

# Prediction training
Latitude_pred_training_rf <- predict(Latitude_model_rf,training)

# Prediction testing and confusion matrix
Latitude_pred_testing_rf <- predict(Latitude_model_rf,testing)

  # Evaluate obtained values
postResample(Latitude_pred_testing_rf,testing$LATITUDE)

# Prediction validation
Latitude_pred_validation_rf <- predict(Latitude_model_rf,validation_data_new)

  # Evaluate obtained values
postResample(Latitude_pred_validation_rf,validation_data_new$LATITUDE)

# Add the values of predictions in a new column
model_results$LATITUDE_pred_rf <- Latitude_pred_validation_rf

# Histogram errors latitude RF

hist(Latitude_pred_validation_rf - validation_data_new$LATITUDE,prob=T,breaks = 50)
lines(density(Latitude_pred_validation_rf - validation_data_new$LATITUDE))

errors_latitude_rf <- Latitude_pred_validation_rf - validation_data_new$LATITUDE
errors_latitude_rf <- as.data.frame(errors_latitude_rf)

ggplot(errors_latitude_rf,aes(x=errors_latitude_rf, fill = "red")) +
  geom_histogram(aes(x=errors_latitude_rf,y=..density..),bins = 100) +
  geom_density() + ylab("") + xlab("Latitude Errors RF") +
  ggtitle("Error Distribution in Latitude")


## LONGITUDE

set.seed(450)
inTraining <- createDataPartition(training_data_new$LONGITUDE, p = 0.7, list = FALSE)
training <- training_data_new[inTraining,]
testing <- training_data_new[-inTraining,]

# Function to find the best "mtry"

# bestmtry_rf<-tuneRF(training_data_new[Wapsnbuildingnfloornlatitude], 
#                     training_data_new$LATITUDE,
#                     ntreeTry = 100, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)

# Random forest. mtry = 104

system.time(Longitude_model_rf <- randomForest(y=training_data_new$LONGITUDE,
                                              x=training_data_new[Wapsnbuildingnfloornlatitude],
                                              importance=T,
                                              method="rf",
                                              ntree=100,
                                              mtry=40))

# Prediction training
Longitude_pred_training_rf <- predict(Longitude_model_rf,training)

# Prediction testing and confusion matrix
Longitude_pred_testing_rf <- predict(Longitude_model_rf,testing)

  # Evaluate the obtained values
postResample(Longitude_pred_testing_rf,testing$LONGITUDE)

# Prediction validation
Longitude_pred_validation_rf <- predict(Longitude_model_rf,validation_data_new)

  # Evaluate obtained values
postResample(Longitude_pred_validation_rf,validation_data_new$LONGITUDE)

# Add the values of predictions in a new column
model_results$LONGITUDE_pred_rf <- Longitude_pred_validation_rf

# Histogram errors latitude RF

hist(Longitude_pred_validation_rf - validation_data_new$LONGITUDE,prob=T,breaks = 50)
lines(density(Longitude_pred_validation_rf - validation_data_new$LONGITUDE))

errors_longitude_rf <- Longitude_pred_validation_rf - validation_data_new$LONGITUDE
errors_longitude_rf <- as.data.frame(errors_longitude_rf)
ggplot(errors_longitude_rf,aes(x=errors_longitude_rf, fill = "red")) +
  geom_histogram(aes(x=errors_longitude_rf,y=..density..),bins = 100) +
  geom_density() + ylab("") + xlab("Longitude Errors RF") +
  ggtitle("Error Distribution in longitude")


#### KNN Model ####

## BUILDING ##

# Preprocess

# Calculate the pre-process parameters from the dataset

preprocessParams_training <- preProcess(training_data_new[Waps], method=c("center", "scale"))

# Transform the waps using the parameters
preprocess_waps_training <- predict(preprocessParams_training, training_data_new[Waps])
preprocess_waps_validation <- predict(preprocessParams_training, validation_data_new[Waps])

# Complete datasetV
training_data_new_preprocess <- cbind(preprocess_waps_training,training_data_new[info[1:4]])
validation_data_new_preprocess <- cbind(preprocess_waps_validation,validation_data_new[info[1:4]])

# Dummify BUILINGID

DummiesBuilding_training <- dummify(training_data_new_preprocess$BUILDINGID)

training_data_new_preprocess <- cbind(training_data_new_preprocess,DummiesBuilding_training)

names(training_data_new_preprocess)[317:319] = c("B0", "B1", "B2") # Change name of columns


# Validation

DummiesBuilding_validation <- dummify(validation_data_new_preprocess$BUILDINGID)

validation_data_new_preprocess <- cbind(validation_data_new_preprocess,DummiesBuilding_validation)

names(validation_data_new_preprocess)[317:319] = c("B0", "B1", "B2") # Change name of columns


# Training and testing 

set.seed(450)
inTraining_preprocess <- createDataPartition(training_data_new_preprocess$BUILDINGID,
                                             p = 0.7,
                                             list = FALSE)
training_preprocess <- training_data_new_preprocess[inTraining_preprocess,] 
testing_preprocess <- training_data_new_preprocess[-inTraining_preprocess,]

# Modelling

system.time(building_pred_testing_knn <- knn(train = training_preprocess[Waps],
                                             test = testing_preprocess[Waps],
                                             cl = training_preprocess$BUILDINGID,
                                             k = 5))

  # Evaluate obtained values
confusionMatrix(building_pred_testing_knn, testing_preprocess$BUILDINGID)

# Prediction validation
system.time(building_pred_validation_knn <- knn(train = training_preprocess[Waps],
                                                test = validation_data_new_preprocess[Waps],
                                                cl = training_preprocess$BUILDINGID,
                                                k = 5))
  # Evaluate obtained values
confusionMatrix(building_pred_validation_knn, validation_data_new_preprocess$BUILDINGID)

# Add the values of predictions in a new column
model_results$BUILDINGID_pred_knn <- building_pred_validation_knn


## FLOOR

# Preprocess
training_data_new_preprocess$BUILDINGID <- NULL
validation_data_new_preprocess$BUILDINGID <- NULL

# Dummify FLOOR

DummiesFloor_training <- dummify(training_data_new_preprocess$FLOOR)

training_data_new_preprocess <- cbind(training_data_new_preprocess,DummiesFloor_training)

names(training_data_new_preprocess)[319:323] = c("F0","F1","F2","F3","F4")

# Validation

DummiesFloor_validation <- dummify(validation_data_new_preprocess$FLOOR)

validation_data_new_preprocess <- cbind(validation_data_new_preprocess,DummiesFloor_validation)

names(validation_data_new_preprocess)[319:323] = c("F0","F1","F2","F3","F4")

# Training and testing 

set.seed(450)
inTraining_preprocess <- createDataPartition(training_data_new_preprocess$FLOOR,
                                             p = 0.7,
                                             list = FALSE)
training_preprocess <- training_data_new_preprocess[inTraining_preprocess,] 
testing_preprocess <- training_data_new_preprocess[-inTraining_preprocess,]

# Choose variables of the model

g <- training_data_new_preprocess %>% select(c(1:312),"B0","B1","B2")
Wapsndummies <- names(g)
rm(g)

# Modelling

system.time(floor_pred_testing_knn <- knn(train = training_preprocess[Wapsndummies],
                                          test = testing_preprocess[Wapsndummies],
                                          cl = training_preprocess$FLOOR,
                                          k = 5))
  # Evaluate obtained values
confusionMatrix(floor_pred_testing_knn, testing_preprocess$FLOOR)

# Prediction validation
system.time(floor_pred_validation_knn <- knn(train = training_preprocess[Wapsndummies],
                                             test = validation_data_new_preprocess[Wapsndummies],
                                             cl = training_preprocess$FLOOR,
                                             k = 5))
  # Evaluate obtained values
confusionMatrix(floor_pred_validation_knn,validation_data_new_preprocess$FLOOR)

# Add the values of predictions in a new column
model_results$FLOOR_pred_knn <- floor_pred_validation_knn


## LATITUDE

training_data_new_preprocess$FLOOR <- NULL
validation_data_new_preprocess$FLOOR <- NULL

# Training and testing 

set.seed(450)
inTraining_preprocess <- createDataPartition(training_data_new_preprocess$LATITUDE,
                                             p = 0.7,
                                             list = FALSE)
training_preprocess <- training_data_new_preprocess[inTraining_preprocess,] 
testing_preprocess <- training_data_new_preprocess[-inTraining_preprocess,]

# Election variables of the model

g <- training_data_new_preprocess %>% select(c(1:312),c(315:322))
Wapsndummies <- names(g)
rm(g)

# Modelling

system.time(latitude_pred_testing_knn <- knn.reg(train = training_preprocess[Wapsndummies],
                                                 test = testing_preprocess[Wapsndummies],
                                                 y = training_preprocess$LATITUDE,
                                                 k = 5))

  # Evaluate obtained values
postResample(latitude_pred_testing_knn$pred, testing_preprocess$LATITUDE)

# hist(latitude_pred_testing_knn$pred - testing_preprocess$LATITUDE,prob=T,breaks = 50)
# lines(density(latitude_pred_testing_knn$pred - testing_preprocess$LATITUDE))
# 
# errors <- latitude_pred_testing_knn$pred - testing_preprocess$LATITUDE
# errors <- as.data.frame(errors)
# ggplot(errors,aes(x=errors,fill="red")) + geom_histogram(aes(x=errors,y=..density..),bins = 50) +
#   geom_density()

# Prediction validation
system.time(latitude_pred_validation_knn <- knn.reg(train = training_preprocess[Wapsndummies],
                                                    test = validation_data_new_preprocess[Wapsndummies],
                                                    y = training_preprocess$LATITUDE,
                                                    k = 5))

  # Evaluate obtained values
postResample(latitude_pred_validation_knn$pred,validation_data_new_preprocess$LATITUDE)

# Add the values of predictions in a new column
model_results$LATITUDE_pred_knn <- latitude_pred_validation_knn$pred

# Distribution errors

hist(latitude_pred_validation_knn$pred - validation_data_new$LATITUDE,prob=T,breaks = 50)
lines(density(latitude_pred_validation_knn$pred - validation_data_new$LATITUDE))

errors_latitude_knn <- latitude_pred_validation_knn$pred - validation_data_new$LATITUDE
errors_latitude_knn <- as.data.frame(errors_latitude_knn)
ggplot(errors_latitude_knn,aes(x=errors_latitude_knn, fill = "red")) +
  geom_histogram(aes(x=errors_latitude_knn,y=..density..),bins = 100) +
  geom_density() + xlab("Errors latitude KNN") + ggtitle("Error distribution Latitude")


## LONGITUDE

# Preprocess

g <- training_data_new_preprocess %>% select(c(1:312),LATITUDE)
Wapsnlatitude <- names(g)
rm(g)

# Calculate the pre-process parameters from the dataset

preprocessParams_training <- preProcess(training_data_new[Wapsnlatitude], method=c("center", "scale"))

# Transform the waps using the parameters
preprocess_waps_training <- predict(preprocessParams_training, training_data_new[Wapsnlatitude])
preprocess_waps_validation <- predict(preprocessParams_training, validation_data_new[Wapsnlatitude])

# Complete datasetV
training_data_new_preprocess <- cbind(preprocess_waps_training,training_data_new[info[c(1,3:4)]])
validation_data_new_preprocess <- cbind(preprocess_waps_validation,validation_data_new[info[c(1,3:4)]])

## Preprocess training

# Dummify BUILINGID

DummiesBuilding_training <- dummify(training_data_new_preprocess$BUILDINGID)

training_data_new_preprocess <- cbind(training_data_new_preprocess,DummiesBuilding_training)

names(training_data_new_preprocess)[317:319] = c("B0", "B1", "B2")


# Dummify FLOOR

DummiesFloor_training <- dummify(training_data_new_preprocess$FLOOR)

training_data_new_preprocess <- cbind(training_data_new_preprocess,DummiesFloor_training)

names(training_data_new_preprocess)[320:324] = c("F0","F1","F2","F3","F4")

# Eliminate useless columnn

training_data_new_preprocess$BUILDINGID <- NULL
training_data_new_preprocess$FLOOR <- NULL


## Preprocess validation

# Dummify BUILINGID

DummiesBuilding_validation <- dummify(validation_data_new_preprocess$BUILDINGID)

validation_data_new_preprocess <- cbind(validation_data_new_preprocess,DummiesBuilding_validation)

names(validation_data_new_preprocess)[317:319] = c("B0", "B1", "B2")


# Dummify FLOOR

DummiesFloor_validation <- dummify(validation_data_new_preprocess$FLOOR)

validation_data_new_preprocess <- cbind(validation_data_new_preprocess,DummiesFloor_validation)

names(validation_data_new_preprocess)[320:324] = c("F0","F1","F2","F3","F4")

# Eliminate useless columnn

validation_data_new_preprocess$BUILDINGID <- NULL
validation_data_new_preprocess$FLOOR <- NULL


## Training and testing 

set.seed(450)
inTraining_preprocess <- createDataPartition(training_data_new_preprocess$LONGITUDE,
                                             p = 0.7,
                                             list = FALSE)
training_preprocess <- training_data_new_preprocess[inTraining_preprocess,] 
testing_preprocess <- training_data_new_preprocess[-inTraining_preprocess,]

# Election variables of the model

g <- training_data_new_preprocess %>% select(c(1:313),c(315:322))
Wapsndummies <- names(g)
rm(g)

# Modelling

system.time(longitude_pred_testing_knn <- knn.reg(train = training_preprocess[Wapsndummies],
                                                  test = testing_preprocess[Wapsndummies],
                                                  y = training_preprocess$LONGITUDE,
                                                  k = 5))
  # Evaluate obtained values
postResample(longitude_pred_testing_knn$pred, testing_preprocess$LONGITUDE)

# Prediction validation
system.time(longitude_pred_validation_knn <- knn.reg(train = training_preprocess[Wapsndummies],
                                                    test = validation_data_new_preprocess[Wapsndummies],
                                                    y = training_preprocess$LONGITUDE,
                                                    k = 5))
  # Evaluate obtained values
postResample(longitude_pred_validation_knn$pred,validation_data_new_preprocess$LONGITUDE)

# Add the values of predictions in a new column
model_results$LONGITUDE_pred_knn <- longitude_pred_validation_knn$pred

# Historigram errors

hist(longitude_pred_validation_knn$pred - validation_data_new$LONGITUDE,prob=T,breaks = 50)
lines(density(longitude_pred_validation_knn$pred - validation_data_new$LONGITUDE))

errors_longitude_knn <- longitude_pred_validation_knn$pred - validation_data_new$LONGITUDE
errors_longitude_knn <- as.data.frame(errors_longitude_knn)
ggplot(errors_longitude_knn,aes(x=errors_longitude_knn, fill = "red")) +
  geom_histogram(aes(x=errors_longitude_knn,y=..density..),bins = 100) +
  geom_density() + xlab("Errors longitude KNN") + ylab("") + 
  ggtitle("Error distribution Longitude")


## Add columns of information of validation data

model_results$BUILDINGID <- validation_data_new$BUILDINGID
model_results$FLOOR <- validation_data_new$FLOOR
model_results$LONGITUDE <- validation_data_new$LONGITUDE
model_results$LATITUDE <- validation_data_new$LATITUDE

#### PLOTS ####

## Plots Building

ggplot(data = model_results) +
  aes(x = BUILDINGID, fill = BUILDINGID_pred_knn) +
  geom_bar(position = "fill") + theme_minimal() +
  xlab("") + ylab("") + ggtitle("Building Prediction KNN")

ggplot(data = model_results) +
  aes(x = BUILDINGID, fill = BUILDINGID_pred_rf) +
  geom_bar(position = "fill") + theme_minimal() +
  xlab("") + ylab("") + ggtitle("Building Prediction RF")

## Plots Floor

ggplot(data = model_results) +
  aes(x = FLOOR, fill = FLOOR_pred_knn) +
  geom_bar(position = "fill") + theme_minimal() +
  xlab("") + ylab("") + ggtitle("Floor Prediction KNN")

ggplot(data = model_results) +
  aes(x = FLOOR, fill = FLOOR_pred_rf) +
  geom_bar(position = "fill") + theme_minimal() +
  xlab("") + ylab("") + ggtitle("Floor Prediction RF")


ggplot(data = model_results,
       aes(x = model_results$LONGITUDE_pred_knn,
           y = model_results$LATITUDE_pred_knn)) +
      geom_point(color = "red")

ggplot(data = model_results,
       aes(x = model_results$LONGITUDE,
           y = model_results$LATITUDE)) +
  geom_point(color = "green")

# Plots generals

ggplot() + geom_point(aes(x= model_results$LATITUDE, y= model_results$LONGITUDE_pred_knn),color= "red") +
  geom_point(aes(x= model_results$LATITUDE,y= model_results$LONGITUDE),color= "black") +
  ylab("Prediction Longitude") + xlab("Latitude") +
  ggtitle("Prediction Longitude KNN vs Real Values")

ggplot() + geom_point(aes(x= model_results$LATITUDE, y= model_results$LONGITUDE_pred_rf),color= "blue") +
  geom_point(aes(x= model_results$LATITUDE,y= model_results$LONGITUDE),color= "black") +
  ylab("Prediction Longitude") + xlab("Latitude") +
  ggtitle("Prediction Longitude RF vs Real Values")

# Plot 3D

plot_ly(model_results, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~BUILDINGID) %>%
  add_markers() #checking distribution of sample1


#### Model per building. ####

## Create training and testing ##

set.seed(450)
inTraining <- createDataPartition(training_data_new$BUILDINGID, p = 0.7, list = FALSE)
training <- training_data_new[inTraining,]
testing <- training_data_new[-inTraining,]

## Building ##

system.time(building_model_rf <- randomForest(x = training_data_new[Waps],
                                              y = training_data_new$BUILDINGID,
                                              method = "rf",
                                              ntree = 100,
                                              mtry = 17,
                                              importance = T))
# Prediction training
building_pred_training_rf <- predict(building_model_rf,training)

# Prediction testing
building_pred_testing_rf <- predict(building_model_rf,testing)

  # Evaluate obtained results
confusionMatrix(building_pred_testing_rf,testing$BUILDINGID)

# Prediction validation
Building_pred_validation_rf <- predict(building_model_rf,validation_data_new)

  # Evaluate obtained results
confusionMatrix(Building_pred_validation_rf,validation_data_new$BUILDINGID)

# Add Values
validation_data_new$BUILDINGID <- Building_pred_validation_rf
 
model_results$BUILDINGID_pred <- Building_pred_validation_rf
      # datafram with results

## FLOOR ##

#### Prediction floor building 0 ####

# Creation dataset only floor 0

training_building_0 <- which(training_data_new$BUILDINGID == 0)
validation_building_0 <- which(validation_data_new$BUILDINGID == 0)

training_building_0 <- training_data_new[training_building_0,]
validation_building_0 <- validation_data_new[validation_building_0,]

# Drop level 4 (not needed, no values)

training_building_0$FLOOR <- droplevels.factor(training_building_0$FLOOR)

levels(training_building_0$FLOOR)

validation_building_0$FLOOR <- droplevels.factor(validation_building_0$FLOOR)

levels(validation_building_0$FLOOR)

# Training and testing #

set.seed(450)
inTraining <- createDataPartition(training_building_0$FLOOR,p = 0.7, list = FALSE)
training <- training_building_0[inTraining,]
testing <- training_building_0[-inTraining,]

## mtry

# bestmtry_rf<-tuneRF(training_building_0[Wapsnbuilding],
#                     training_building_0$FLOOR,
#                     ntreeTry = 100, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)

## Building ##
system.time(floor_model_rf_0 <- randomForest(x = training_building_0[Wapsnbuilding],
                                              y = training_building_0$FLOOR,
                                              method = "rf",
                                              ntree = 100,
                                              mtry = 17,
                                              importance = T))

# Prediction training
floor_pred_training_rf_0 <- predict(floor_model_rf_0,training)

# Prediction testing
floor_pred_testing_rf_0 <- predict(floor_model_rf_0,testing)

 # Evaluate obtained values
confusionMatrix(floor_pred_testing_rf_0,testing$FLOOR)

# Prediction validation
floor_pred_validation_rf_0 <- predict(floor_model_rf_0,validation_building_0)

  # Evaluate obtained values
confusionMatrix(floor_pred_validation_rf_0,validation_building_0$FLOOR)

# Add values of predictions
validation_building_0$FLOOR <- floor_pred_validation_rf_0
# model_results$FLOOR_pred <- floor_pred_validation_rf


#### Latitude building 0 ####

# Training and testing #

set.seed(450)
inTraining <- createDataPartition(training_building_0$LATITUDE,p = 0.7, list = FALSE)
training <- training_building_0[inTraining,]
testing <- training_building_0[-inTraining,]

## mtry

# bestmtry_rf<-tuneRF(training_building_0[Wapsnbuildingnfloor],
#                     training_building_0$LATITUDE,
#                     ntreeTry = 100, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)


## latitude ##

system.time(latitude_model_rf_0 <- randomForest(x = training_building_0[Wapsnbuildingnfloor],
                                           y = training_building_0$LATITUDE,
                                           method = "rf",
                                           ntree = 100,
                                           mtry = 52,
                                           importance = T))

# Prediction training
latitude_pred_training_rf <- predict(latitude_model_rf_0,training)

# Prediction testing
latitude_pred_testing_rf <- predict(latitude_model_rf_0,testing)

  # Evaluate obtained results
postResample(latitude_pred_testing_rf,testing$FLOOR)

# Prediction validation
latitude_pred_validation_rf <- predict(latitude_model_rf_0,validation_building_0)

  # Evaluate obtained results
postResample(latitude_pred_validation_rf,validation_building_0$LATITUDE)

# Add the values of predictions in a new column
validation_building_0$LATITUDE <- latitude_pred_validation_rf

#### Longitude building 0 ####

# Training and testing #

set.seed(450)
inTraining <- createDataPartition(training_building_0$LONGITUDE,p = 0.7, list = FALSE)
training <- training_building_0[inTraining,]
testing <- training_building_0[-inTraining,]

## mtry

# bestmtry_rf<-tuneRF(training_building_0[Wapsnbuildingnfloornlatitude],
#                     training_building_0$LONGITUDE,
#                     ntreeTry = 100, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)


## latitude ##

system.time(longitude_model_rf_0 <- randomForest(x = training_building_0[Wapsnbuildingnfloornlatitude],
                                                y = training_building_0$LONGITUDE,
                                                method = "rf",
                                                ntree = 100,
                                                mtry = 105,
                                                importance = T))

# Prediction training
longitude_pred_training_rf_0 <- predict(longitude_model_rf_0,training)

# Prediction testing
longitude_pred_testing_rf_0 <- predict(longitude_model_rf_0,testing)

  # Evaluate obtained values
postResample(longitude_pred_testing_rf_0,testing$LONGITUDE)

# Prediction validation
longitude_pred_validation_rf_0 <- predict(longitude_model_rf_0,validation_building_0)

  # Evaluate obtained values
postResample(longitude_pred_validation_rf_0,validation_building_0$LONGITUDE)

# Add the values of predictions in a new column
validation_building_0$LONGITUDE <- longitude_pred_validation_rf_0

#### Prediction floor building 1 ####

# Creation dataset only floor 1
training_building_1 <- which(training_data_new$BUILDINGID == 1)
validation_building_1 <- which(validation_data_new$BUILDINGID == 1)

training_building_1 <- training_data_new[training_building_1,]
validation_building_1 <- validation_data_new[validation_building_1,]

# Drop level 4 (not needed, no values)
training_building_1$FLOOR <- droplevels.factor(training_building_1$FLOOR)

levels(training_building_1$FLOOR)

validation_building_1$FLOOR <- droplevels.factor(validation_building_1$FLOOR)

levels(validation_building_1$FLOOR)

# Training and testig #
set.seed(450)
inTraining <- createDataPartition(training_building_1$FLOOR,p = 0.7, list = FALSE)
training <- training_building_1[inTraining,]
testing <- training_building_1[-inTraining,]

## mtry

# bestmtry_rf<-tuneRF(training_building_1[Wapsnbuilding],
#                     training_building_1$FLOOR,
#                     ntreeTry = 100, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)


## Building ##

system.time(floor_model_rf_1 <- randomForest(x = training_building_1[Wapsnbuilding],
                                           y = training_building_1$FLOOR,
                                           method = "rf",
                                           ntree = 100,
                                           mtry = 34,
                                           importance = T))

# Prediction training
floor_pred_training_rf_1 <- predict(floor_model_rf_1,training)

# Prediction testing
floor_pred_testing_rf_1 <- predict(floor_model_rf_1,testing)

  # Evaluate obtained values
confusionMatrix(floor_pred_testing_rf_1,testing$FLOOR)

# Prediction validation
floor_pred_validation_rf_1 <- predict(floor_model_rf_1,validation_building_1)

  # Evaluate obtained values
confusionMatrix(floor_pred_validation_rf_1,validation_building_1$FLOOR)

# Add values of predictions in a new columns
validation_building_1$FLOOR <- floor_pred_validation_rf_1
# model_results$FLOOR_pred <- floor_pred_validation_rf

#### Latitude building 1 ####

# Training and testing #

set.seed(450)
inTraining <- createDataPartition(training_building_1$LATITUDE,p = 0.7, list = FALSE)
training <- training_building_1[inTraining,]
testing <- training_building_1[-inTraining,]

## mtry

# bestmtry_rf<-tuneRF(training_building_1[Wapsnbuildingnfloornlatitude],
#                     training_building_1$LONGITUDE,
#                     ntreeTry = 100, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)


## latitude ##

system.time(latitude_model_rf_1 <- randomForest(x = training_building_1[Wapsnbuildingnfloor],
                                                y = training_building_1$LATITUDE,
                                                method = "rf",
                                                ntree = 100,
                                                mtry = 105,
                                                importance = T))

# Prediction training
latitude_pred_training_rf_1 <- predict(latitude_model_rf_1,training)

# Prediction testing
latitude_pred_testing_rf_1 <- predict(latitude_model_rf_1,testing)

  # Evaluate obtained values
postResample(latitude_pred_testing_rf_1,testing$LATITUDE)

# Prediction validation
latitude_pred_validation_rf_1 <- predict(latitude_model_rf_1,validation_building_1)

  # Evaluate obtained values
postResample(latitude_pred_validation_rf_1,validation_building_1$LATITUDE)

# Add values of predictions in a new column
validation_building_1$LATITUDE <- latitude_pred_validation_rf_1

#### Longitude building 1 ####

# Training and testing #

set.seed(450)
inTraining <- createDataPartition(training_building_1$LONGITUDE,p = 0.7, list = FALSE)
training <- training_building_1[inTraining,]
testing <- training_building_1[-inTraining,]

## mtry

# bestmtry_rf<-tuneRF(training_building_1[Wapsnbuildingnfloornlatitude],
#                     training_building_1$LONGITUDE,
#                     ntreeTry = 105, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)


## latitude ##

system.time(longitude_model_rf_1 <- randomForest(x = training_building_1[Wapsnbuildingnfloornlatitude],
                                                y = training_building_1$LONGITUDE,
                                                method = "rf",
                                                ntree = 100,
                                                mtry = 105,
                                                importance = T))

# Prediction training
longitude_pred_training_rf_1 <- predict(longitude_model_rf_1,training)

# Prediction testing
longitude_pred_testing_rf_1 <- predict(longitude_model_rf_1,testing)

  # Evaluate obtained values
postResample(longitude_pred_testing_rf_1,testing$LONGITUDE)

# Prediction validation
longitude_pred_validation_rf_1 <- predict(longitude_model_rf_1,validation_building_1)

  # Evaluate obtained values
postResample(longitude_pred_validation_rf_1,validation_building_1$LONGITUDE)

# Add values of predictions in a new column
validation_building_1$LONGITUDE <- latitude_pred_validation_rf_1

#### Prediction floor building 2 ####

# Creation dataset only floor 2

training_building_2 <- which(training_data_new$BUILDINGID == 2)
validation_building_2 <- which(validation_data_new$BUILDINGID == 2)

training_building_2 <- training_data_new[training_building_2,]
validation_building_2 <- validation_data_new[validation_building_2,]

# Training and testing #

set.seed(450)
inTraining <- createDataPartition(training_building_2$FLOOR,p = 0.7, list = FALSE)
training <- training_building_2[inTraining,]
testing <- training_building_2[-inTraining,]

## mtry

# bestmtry_rf<-tuneRF(training_building_2[Wapsnbuilding],
#                     training_building_2$FLOOR,
#                     ntreeTry = 100, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)


## Building ##

system.time(floor_model_rf_2 <- randomForest(x = training_building_2[Wapsnbuilding],
                                             y = training_building_2$FLOOR,
                                             method = "rf",
                                             ntree = 100,
                                             mtry = 17,
                                             importance = T))

# Prediction training
floor_pred_training_rf_2 <- predict(floor_model_rf_2,training)

# Prediction testing
floor_pred_testing_rf_2 <- predict(floor_model_rf_2,testing)

  # Evaluate obtained values
confusionMatrix(floor_pred_testing_rf_2,testing$FLOOR)

# Prediction validation
floor_pred_validation_rf_2 <- predict(floor_model_rf_2,validation_building_2)

  # Evaluate obtained values
confusionMatrix(floor_pred_validation_rf_2,validation_building_2$FLOOR)

# Add values of predictions in a new column
validation_building_2$FLOOR <- floor_pred_validation_rf_2
# model_results$FLOOR_pred <- floor_pred_validation_rf

#### Latitude building 2 ####

# Training and testing #

set.seed(450)
inTraining <- createDataPartition(training_building_2$LATITUDE,p = 0.7, list = FALSE)
training <- training_building_2[inTraining,]
testing <- training_building_2[-inTraining,]

## mtry

# bestmtry_rf<-tuneRF(training_building_2[Wapsnbuildingnfloornlatitude],
#                     training_building_2$LONGITUDE,
#                     ntreeTry = 100, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)


## latitude ##

system.time(latitude_model_rf_2 <- randomForest(x = training_building_2[Wapsnbuildingnfloor],
                                                y = training_building_2$LATITUDE,
                                                method = "rf",
                                                ntree = 100,
                                                mtry = 105,
                                                importance = T))

# Prediction training
latitude_pred_training_rf_2 <- predict(latitude_model_rf_2,training)

# Prediction testing
latitude_pred_testing_rf_2 <- predict(latitude_model_rf_2,testing)

  # Evaluate obtained values
postResample(latitude_pred_testing_rf_2,testing$LATITUDE)

# Prediction validation
latitude_pred_validation_rf_2 <- predict(latitude_model_rf_2,validation_building_2)

  # Evaluate obtained values
postResample(latitude_pred_validation_rf_2,validation_building_2$LATITUDE)

# Add values of predictions in a new column
validation_building_2$LATITUDE <- latitude_pred_validation_rf_2

#### Longitude building 2 ####

# Training and testing #

set.seed(450)
inTraining <- createDataPartition(training_building_2$LONGITUDE,p = 0.7, list = FALSE)
training <- training_building_2[inTraining,]
testing <- training_building_2[-inTraining,]

## mtry

# bestmtry_rf<-tuneRF(training_building_2[Wapsnbuildingnfloornlatitude],
#                     training_building_2$LONGITUDE,
#                     ntreeTry = 105, stepFactor = 2,
#                     improve = 0.05, trace = TRUE, plot = T)


## latitude ##
system.time(longitude_model_rf_2 <- randomForest(x = training_building_2[Wapsnbuildingnfloornlatitude],
                                                 y = training_building_2$LONGITUDE,
                                                 method = "rf",
                                                 ntree = 100,
                                                 mtry = 105,
                                                 importance = T))

# Prediction training
longitude_pred_training_rf_2 <- predict(longitude_model_rf_2,training)

# Prediction testing
longitude_pred_testing_rf_2 <- predict(longitude_model_rf_2,testing)

# Evaluate obtained values
postResample(longitude_pred_testing_rf_2,testing$LONGITUDE)

# Prediction validation
longitude_pred_validation_rf_2 <- predict(longitude_model_rf_2,validation_building_2)

# Evaluate obtained values
postResample(longitude_pred_validation_rf_2,validation_building_2$LONGITUDE)

# Add values of predictions in a new column
validation_building_2$LONGITUDE <- latitude_pred_validation_rf_2
