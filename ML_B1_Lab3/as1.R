set.seed(1234567890)
library(ggplot2)
library(geosphere)
library(readr)
?geosphere
stations <- read_csv("data/stations.csv")
temps <- read_csv("data/temps50k.csv")

weather_predict<-function(date,lat,lon){
  
  h_distance <-300000 # These three values are up to the students
  h_date <-120
  h_time <-5
  
  date <- "2014-08-11" #The date to predict
  a <- 58.397 #Latitude of the point to predict
  b <- 15.578
  times <- c("04:00:00", "06:00:00","08:00:00","10:00:00","12:00:00","14:00:00",
             "16:00:00","18:00:00","20:00:00","22:00:00", "24:00:00")
  temp <- vector(length=length(times))
  
  st <- merge(stations,temps,by="station_number")
  
  st<-st[,c(-3,-6,-7,-8,-12)]
  st$date<-as.Date(st$date)
  date<-as.Date(date)
  st$days_diff<-as.numeric(difftime(st$date,date,units = "days"))
  idx<-which(st$days_diff>=0)
  if(length(idx)!=0){
    st<-st[-idx,]
  }
  
  time1<-strptime(paste(st$time),"%H:%M:%S")
  time2<-strptime(paste(times[1]),"%H:%M:%S")
  time_diff<-as.numeric(abs(difftime(time1,time2,units = "hours")))
  time_diff<-ifelse(time_diff<12,time_diff,24-time_diff)
  
  df<-as.data.frame(time_diff)
  
  for(i in 2:length(times)){
    time1<-strptime(paste(st$time),"%H:%M:%S")
    time2<-strptime(paste(times[i]),"%H:%M:%S")
    time_diff<-as.numeric(abs(difftime(time1,time2,units = "hours")))
    time_diff<-ifelse(time_diff<12,time_diff,24-time_diff)
    tdf<-as.data.frame(time_diff)
    df<-cbind(df,tdf)
  }
  
  names(df)<-times
  st<-cbind(st,df)
  
  # Students’ code here
  ?distHaversine
  distance_kernel<-function(lat,lon){
    k_dist<-distHaversine(st[,3:4],c(lat,lon))
    #x <- seq(0,1000000, 10000)
    #plot(k_dist,exp(-abs(k_dist/h_distance)^2))
    #print(sd(exp(-abs(k_dist/h_distance)^2)))
    #abline(v=2*sd(x))
    print(k_dist)
    exp(-abs(k_dist/h_distance)^2)
  }
  distance_kernel(a,b)
  date_kernel<-function(){
    res=exp(-abs(st$days_diff/h_date)^2)
    #plot(-st$days_diff,exp(-abs(st$days_diff/h_date)^2))
    return(res)
  }
  
  time_kernel<-function(pred){
    res=exp(-abs(as.numeric(st[,pred])/h_time)^2)
    #plot(as.numeric(st[,pred]),exp(-abs(as.numeric(st[,pred])/h_time)^2))
    return(res)
  }
  
  sum_kernel<-function(pred){
    sum_k<-distance_kernel(a,b)+date_kernel()+time_kernel(pred)
    print(sum_k)
    res<-sum(sum_k*st$air_temperature)/sum(sum_k)
    return(res)
  }
  
  mul_kernel<-function(pred){
    sum_k<-distance_kernel(lat,lon)*date_kernel()*time_kernel(pred)
    res<-sum(sum_k*st$air_temperature)/sum(sum_k)
    return(res)
  }
  
  temp<-c()
  for(i in 1:1){
    temp<-c(temp,sum_kernel(times[i]))
  }
  temp
  
  temp_mul<-c()
  for(i in 1:length(times)){
    temp_mul<-c(temp_mul,mul_kernel(times[i]))
  }
  ggplot()+geom_line(aes(x=times,y=temp,group=1),col="blue")+
    geom_line(aes(x=times,y=temp_mul,group=1),col="red")+
    ggtitle(paste(date))
  #plot(temp, type="o",col="blue")
  #plot(temp_mul, type="o",col="red")
}

 #Longitude of the point to predict
weather_predict(date,a,b)


