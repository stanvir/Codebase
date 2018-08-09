rawdat<-read.csv("Computed accel_Jerk.csv")
data<-read.csv("Metadata with variables.csv")
rawdat= as.data.frame(rawdat)
data=as.data.frame(data)
id=unique(rawdat$trip_history_id)
# There are 183 unique trips

trip_id=as.character(id)
percentage=matrix(0,184,1)

for (i in 1:184){
  a=trip_id[i]
  sub_data=rawdat[as.character(rawdat$trip_history_id)==a,]
  total_time=as.numeric(dim(sub_data)[1])
  perc_error=as.numeric(dim(sub_data[sub_data$Speed.mph<0,])[1])
  percentage[i,1]=perc_error/total_time

}



K=as.data.frame(trip_id)
unique_id=cbind(K,percentage)

colnames(unique_id)<- c("trip_history_id","%error")
new_data=merge(data,unique_id,by="trip_history_id")

write.csv(new_data, file = "Metadata with variables_error.csv")
