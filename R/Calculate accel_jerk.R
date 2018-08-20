#TO: Calculate FES and other trip parameters
#from a csv file as formatted in google drive
# also a user can select a particular TRIP_ID to
#calculate VSP> FC> FES


#################################################
#############Developer: Shams Tanvir#############
############# ITRE, NCSU#########################
rm(list=ls())
library(ggplot2)

source("R/FES_functions.R")

rawdat<-read.csv("data/trip_data_rev_neg_valueX.csv")
rawdat <- as.data.frame(rawdat)


rawdat <- rawdat[complete.cases(rawdat[,5]),]
rawdat$Acceleration <- rep(NA, length(rawdat[,1]))
rawdat$Jerk <- rep(NA, length(rawdat[,1]))
rawdat$Speed.mph <- numconvert(rawdat$speed) * 2.236936


rawdat$TRIP_ID <- as.character(rawdat$trip_history_id)


tripidList <- unique(rawdat$TRIP_ID)


selectedTripid <- tripidList #it is possible to filter the trips here


selected_trip <- rawdat[rawdat$TRIP_ID %in% selectedTripid, ]
ndat5 <- split(seq(nrow(selected_trip)), selected_trip$TRIP_ID)


for(j in 1:length(ndat5)){
        for(i in 1:length(ndat5[[j]])-1){
                selected_trip$Acceleration[ndat5[[j]][i]] <- 1.466667 *((selected_trip$Speed.mph[ndat5[[j]][i+1]]-
                                                                      selected_trip$Speed.mph[ndat5[[j]][i]]))
        }
}


for(j in 1:length(ndat5)){
  for(i in 1:length(ndat5[[j]])-2){
    selected_trip$Jerk[ndat5[[j]][i]] <- (selected_trip$Acceleration[ndat5[[j]][i+1]]-
                                                    selected_trip$Acceleration[ndat5[[j]][i]])
  }
}

#calculate VSP
selected_trip$VSP_myway <- CalculateVSP(selected_trip$Speed.mph,
                                        selected_trip$Acceleration)

selected_trip$VSP_Livedrive <- 0.8 + 0.75* (selected_trip$VSP_myway) + 0.001* (selected_trip$VSP_myway)^2

#calculate fuel consumption
breaks <- c(-Inf,0,60, Inf)
a <- c(0.3,0.462, 4.39)
b <- c(0, 0.55,0)
range <- cut(selected_trip$VSP_Livedrive, breaks, labels = FALSE)
selected_trip$FC <- a[range]*(selected_trip$VSP_Livedrive)^b[range]

write.csv(selected_trip, file = "data/Computed accel_Jerk_negX.csv")

#plotting to see if the speed and acceleration traces make sense
plottrip <- selected_trip[selected_trip$TRIP_ID %in% selectedTripid[1],]
plottrip$tstamp <- as.character(plottrip$tstamp)
plottrip$tstamp <- strtrim(plottrip$tstamp, 19)
plottrip$tstamp <- strptime(plottrip$tstamp, "%Y-%m-%d %H:%M:%S")



ggplot(plottrip, aes(x= tstamp, y= Speed.mph)) + geom_line()
ggplot(plottrip, aes(x= tstamp, y= Acceleration)) + geom_line()
