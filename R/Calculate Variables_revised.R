rawdat<-read.csv("Computed accel_Jerk_negX.csv")
data<-read.csv("trip_metadata_rev_neg_valueX.csv")
rawdat= as.data.frame(rawdat)
data=as.data.frame(data)
id=unique(rawdat$trip_history_id)
# There are 183 unique trips

trip_id=as.character(id)
percentage=matrix(0,183,9)

#for loop for calculating variables
for (i in 1:183){
  a=trip_id[i]
  sub_data=rawdat[as.character(rawdat$trip_history_id)==a,]
  total_time=as.numeric(dim(sub_data)[1])
  over_40=as.numeric(dim(sub_data[sub_data$Speed.mph>40,])[1])
  over_70=as.numeric(dim(sub_data[sub_data$Speed.mph>70,])[1])
  over_1accel=as.numeric(dim(sub_data[sub_data$Acceleration>1,])[1])
  over_5accel=as.numeric(dim(sub_data[sub_data$Acceleration>5,])[1])
  over_5deccel=as.numeric(dim(sub_data[sub_data$Acceleration<-5,])[1])
  over_neg_Jerk=as.numeric(dim(sub_data[sub_data$Jerk<0,])[1])
  over_pos_Jerk=as.numeric(dim(sub_data[sub_data$Jerk>0,])[1])
  Jerk_95th=as.numeric(quantile(sub_data$Jerk, .95, na.rm=TRUE))
  Jerk_98th=as.numeric(quantile(sub_data$Jerk, .98, na.rm=TRUE))
  percentage[i,1]=over_40/total_time
  percentage[i,2]=over_70/total_time
  percentage[i,3]=over_1accel/total_time
  percentage[i,4]=over_5accel/total_time
  percentage[i,5]=over_5deccel/total_time
  percentage[i,6]=over_neg_Jerk/total_time
  percentage[i,7]=over_pos_Jerk/total_time
  percentage[i,8]=Jerk_95th
  percentage[i,9]=Jerk_98th
}


#Matching with Trip_ID in metadata file and put the computed variables
K=as.data.frame(trip_id)
unique_id=cbind(K,percentage)

colnames(unique_id)<- c("trip_history_id",">40mph",">70mph",">1 accel",
                        ">5 accel",">5 deccel","Neg Jerk","Pos Jerk","95th Jerk","98th Jerk")
new_data=merge(data,unique_id,by="trip_history_id")

write.csv(new_data, file = "Metadata with variables_perc.csv")
