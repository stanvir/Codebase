rawdat<-read.csv("Computed accel_Jerk.csv")
data<-read.csv("trip_metadata.csv")
rawdat= as.data.frame(rawdat)
data=as.data.frame(data)
id=unique(rawdat$trip_history_id)
# There are 183 unique trips

trip_id=as.character(id)
Average=matrix(0,length(id),3)

#for loop for calculating variables
for (i in 1:length(id)){
  a=trip_id[i]
  sub_data=rawdat[as.character(rawdat$trip_history_id)==a,]
  Avg_speed_1=mean(sub_data$Speed.mph)
  Avg_speed_2=mean(sub_data$Speed.mph[sub_data$Speed.mph>0])
  Avg_speed_3=mean(sub_data$Speed.mph[sub_data$Speed.mph!=0])
  
  Average[i,1]=Avg_speed_1
  Average[i,2]=Avg_speed_2
  Average[i,3]=Avg_speed_3
  
}


#Matching with Trip_ID in metadata file and put the computed variables
K=as.data.frame(trip_id)
unique_id=cbind(K,Average)

colnames(unique_id)<- c("trip_history_id","Avg_1","Avg_2","Avg_3")
                        
new_data=merge(data,unique_id,by="trip_history_id")

write.csv(new_data, file = "Average Comparison.csv")
