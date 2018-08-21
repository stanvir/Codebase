rm(list=ls())

rawdat <- selected_trip

trip_id <- unique(as.character(rawdat$trip_history_id))

percentage <- matrix(0,length(trip_id),13)  #13 is number of variables

#for loop for calculating variables
for (i in 1:length(trip_id)){
  a=trip_id[i]
  sub_data=rawdat[as.character(rawdat$trip_history_id)==a,]
  total_time=as.numeric(dim(sub_data)[1])
  over_40=as.numeric(dim(sub_data[sub_data$Speed.mph>40,])[1])
  over_70=as.numeric(dim(sub_data[sub_data$Speed.mph>70,])[1])
  over_1accel=as.numeric(dim(sub_data[sub_data$Acceleration>1,])[1])
  over_5accel=as.numeric(dim(sub_data[sub_data$Acceleration>5,])[1])
  over_5deccel=as.numeric(dim(sub_data[sub_data$Acceleration<(-5),])[1])
  over_neg_Jerk=as.numeric(dim(sub_data[sub_data$Jerk<0,])[1])
  over_pos_Jerk=as.numeric(dim(sub_data[sub_data$Jerk>0,])[1])
  Jerk_95th=as.numeric(quantile(sub_data$Jerk, .95, na.rm=TRUE))
  Jerk_98th=as.numeric(quantile(sub_data$Jerk, .98, na.rm=TRUE))
  
  totalFCGallon <- as.numeric(sum(sub_data$FC, na.rm = T))/2800
  averageSpeed <- as.numeric(mean(sub_data$Speed.mph, na.rm = T))
  totalDistance <- total_time * (averageSpeed/ 3600) #distance in miles
  standardMPG <-  totalDistance /totalFCGallon
  
  percentage[i,1]=over_40/total_time
  percentage[i,2]=over_70/total_time
  percentage[i,3]=over_1accel/(total_time-1)
  percentage[i,4]=over_5accel/(total_time -1)
  percentage[i,5]=over_5deccel/(total_time -1)
  percentage[i,6]=over_neg_Jerk/(total_time -2)
  percentage[i,7]=over_pos_Jerk/(total_time -2)
  percentage[i,8]=Jerk_95th
  percentage[i,9]=Jerk_98th
  percentage[i,10]= averageSpeed
  percentage[i,11] = totalDistance
  percentage[i,12] = totalFCGallon
  percentage[i,13] = standardMPG
  

}

#Matching with Trip_ID in metadata file and put the computed variables
K=as.data.frame(trip_id)
unique_id=cbind(K,percentage)

colnames(unique_id)<- c("trip_history_id",">40mph",">70mph",">1 accel",
                        ">5 accel",">5 deccel","Neg Jerk","Pos Jerk","95th Jerk","98th Jerk",
                        "averageSpeed", "totalDistance", "totalFCGallon", "standardMPG")

new_data <- unique_id


new_data$averagespeedbin <- cut (new_data$averageSpeed, seq(0,100,10),
                                include.lowest = T)

#need to have quantile loaded in the environment
quantile <- read.csv(file= "data/quantile3.csv", header= T)
quantile <- quantile[,c(1,2,10)]
new_data <- merge(quantile, new_data, by= "averagespeedbin")

FES_calc2 <- function (MPG, ui, li){
        if(MPG > ui) {FES <- 100}
        else if (MPG < li) {FES <- 20}
        else {FES <- 20+ (MPG - li)/(ui- li)*80}
        return(FES)
}

new_data <-  new_data[complete.cases(new_data),]
new_data$FES_recalculated <- mapply(FES_calc2,MPG = new_data$standardMPG,
                      ui = new_data$ui, li = new_data$li)

write.csv(new_data, file = "Metadata with variables_perc.csv")
