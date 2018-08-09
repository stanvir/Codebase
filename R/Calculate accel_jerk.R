#TO: Calculate FES and other trip parameters
#from a csv file as formatted in google drive
# also a user can select a particular TRIP_ID to
#calculate VSP> FC> FES


#################################################
#############Developer: Shams Tanvir#############
############# ITRE, NCSU#########################
rm(list=ls())
library(ggplot2)

#load the functions needed
#setwd("C:\\Users\\SHAMS\\Desktop\\i2D_Data")
source("FES_functions.R")

rawdat<-read.csv("trip_data_rev_neg_valueX.csv")
rawdat= as.data.frame(rawdat)

#read the csv file from google drive #ARPAE > WebsiteCheck
rawdat <- rawdat[complete.cases(rawdat[,3]),]
rawdat$Acceleration <- rep(NA, length(rawdat[,1]))
rawdat$Jerk <- rep(NA, length(rawdat[,1]))
rawdat$Speed.mph <- numconvert(rawdat$speed) * 2.236936

#rawdat$Slope..m.m. <- numconvert(rawdat$Slope..m.m)
rawdat$TRIP_ID <- as.character(rawdat$trip_history_id)

#Speed=rawdat[,5]

tripidList <- unique(rawdat$TRIP_ID)


coefficient <- NULL


#selectedTripid <- sample(tripidList, 5)
selectedTripid <- tripidList

selected_trip <- rawdat[tripidList %in% selectedTripid,]
selected_trip <- rawdat[rawdat$TRIP_ID %in% selectedTripid, ]
ndat5 <- split(seq(nrow(selected_trip)), selected_trip$TRIP_ID)


for(j in 1:length(ndat5)){
        for(i in 2:length(ndat5[[j]])-1){
                selected_trip$Acceleration[ndat5[[j]][i]] <- 1.466667 *((selected_trip$Speed.mph[ndat5[[j]][i+1]]-
                                                                      selected_trip$Speed.mph[ndat5[[j]][i]]))
        }
}


for(j in 1:length(ndat5)){
  for(i in 2:length(ndat5[[j]])-1){
    selected_trip$Jerk[ndat5[[j]][i]] <- (selected_trip$Acceleration[ndat5[[j]][i+1]]-
                                                    selected_trip$Acceleration[ndat5[[j]][i]])
  }
}

write.csv(selected_trip, file = "Computed accel_Jerk_negX.csv")
