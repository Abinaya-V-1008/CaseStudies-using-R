setwd("D:/APLLIED STATS/CASE STUDY-II")
Weather_data <- read.csv(unzip("mount-rainier-weather-and-climbing-data.zip", "Rainier_Weather.csv"))
Climbing_data<-read.csv(unzip("mount-rainier-weather-and-climbing-data.zip", "climbing_statistics.csv"))
library(dplyr)
library(ggplot2)
#install.packages("corrplot")
library(corrplot)

head(Weather_data,n=5)
head(Climbing_data)

#formatting the date
Climbing_data$DATE<-as.Date(Climbing_data$Date,"%m/%d/%Y")
Climbing_data$YEAR<-as.numeric(format(Climbing_data$DATE,"%Y"))
Climbing_data$MONTH<-as.numeric(format(Climbing_data$DATE,"%m"))
head(Climbing_data,n=10)

Weather_data$DATE<-as.Date(Weather_data$Date,"%m/%d/%Y")
Weather_data$YEAR<-as.numeric(format(Weather_data$DATE,"%Y"))
Weather_data$MONTH<-as.numeric(format(Weather_data$DATE,"%m"))
head(Weather_data,n=10)

#assigning labels to month variable
monl=c("Jan","Feb","Mar","Apr", "May", "Jun", "July", "Aug", "Sept", "Oct", "Nov","Dec")
Climbing_data$MONTH<-factor(Climbing_data$MONTH,labels=monl)
Weather_data$MONTH<-factor(Weather_data$MONTH,labels=monl)

#plot attempt and success
plot(Climbing_data$Attempted,Climbing_data$Succeeded,col="blue",main="Attempts and Successes",xlab="Attempt",ylab="Success")

obs_success<-Climbing_data%>%filter(Succeeded>60)
obs_success
#weather that day for 70 success
obsuccess_weather<-Weather_data%>%filter(Date=="7/20/2015")
obsuccess_weather

#attempts and success over months boxplots
att<-ggplot(data=Climbing_data,aes(x=as.factor(MONTH),y=Attempted)) + geom_boxplot(fill="purple")+ggtitle("Attempts Over Months")+xlab("Month")+ylab("Number of people who attempted")
att

suc<-ggplot(data=Climbing_data,aes(x=as.factor(MONTH),y=Succeeded)) + geom_boxplot(fill="gold")+ggtitle("Successes Over Months")+xlab("Month")+ylab("Number of people who succeeded")
suc

#attempts and success over years boxplots
att<-ggplot(data=Climbing_data,aes(x=as.factor(YEAR),y=Attempted)) + geom_boxplot(fill="purple")+ggtitle("Attempts Over Years")+xlab("Years")+ylab("Number of people who attempted")
att

suc<-ggplot(data=Climbing_data,aes(x=as.factor(YEAR),y=Succeeded)) + geom_boxplot(fill="gold")+ggtitle("Successes Over Years")+xlab("Years")+ylab("Number of people who succeeded")
suc

#details of success
sucdet<-Climbing_data%>%select("Date","Route","Succeeded","Success.Percentage","YEAR","MONTH")%>%filter(Succeeded>0)
sucdet

#what route was taken by those people who succeeded
way<-rev(sort(table(sucdet$Route)))[1:10]
barplot(way,las=2,col="red",main="Top 10 Routes with highest Successful Climbs",ylab="Successes")

#dev.off()
hist(Weather_data$Battery.Voltage.AVG,col="green")
hist(Weather_data$Temperature.AVG,col="purple")
hist(Weather_data$Relative.Humidity.AVG,col="blue")
hist(Weather_data$Wind.Speed.Daily.AVG,col="red")
hist(Weather_data$Wind.Direction.AVG,col="yellow")
hist(Weather_data$Solare.Radiation.AVG,col="black")

ggplot(Weather_data,aes(x=MONTH,y=Battery.Voltage.AVG)) + geom_boxplot(fill="gold") +ggtitle("Battery Voltage") 
ggplot(Weather_data,aes(x=MONTH,y=Temperature.AVG)) + geom_boxplot(fill="purple") +ggtitle("Temperature") 
ggplot(Weather_data,aes(x=MONTH,y=Wind.Speed.Daily.AVG)) + geom_boxplot(fill="blue") +ggtitle("Wind Speed") 
ggplot(Weather_data,aes(x=MONTH,y=Wind.Direction.AVG)) + geom_boxplot(fill="green") +ggtitle("Wind Direction") 
ggplot(Weather_data,aes(x=MONTH,y=Solare.Radiation.AVG)) + geom_boxplot(fill="orange") +ggtitle("Solar Radiation") 

#corelation
weather<-Weather_data%>%select("Battery.Voltage.AVG", "Temperature.AVG", "Relative.Humidity.AVG", "Wind.Speed.Daily.AVG", "Wind.Direction.AVG", "Solare.Radiation.AVG")
corrplot(cor(weather),type="lower",method="number")

#reg
modelb<-lm(Weather_data$Battery.Voltage.AVG~Weather_data$MONTH)
summary(modelb)

modelt<-lm(Weather_data$Temperature.AVG~Weather_data$MONTH)
summary(modelt)

modelh<-lm(Weather_data$Relative.Humidity.AVG~Weather_data$MONTH)
summary(modelh)

modelws<-lm(Weather_data$Wind.Speed.Daily.AVG~Weather_data$MONTH)
summary(modelws)

modelwd<-lm(Weather_data$Wind.Direction.AVG~Weather_data$MONTH)
summary(modelwd)

modelsr<-lm(Weather_data$Solare.Radiation.AVG~Weather_data$MONTH)
summary(modelsr)


#anova
Battery_Month<-aov(Battery.Voltage.AVG~MONTH,data=Weather_data)
summary(Battery_Month)

Temp_Month<-aov(Temperature.AVG~MONTH,data=Weather_data)
summary(Temp_Month)

Hum_Month<-aov(Relative.Humidity.AVG~MONTH,data=Weather_data)
summary(Hum_Month)

Windspeed_Month<-aov(Wind.Speed.Daily.AVG~MONTH,data=Weather_data)
summary(Windspeed_Month)

Winddir_Month<-aov(Wind.Direction.AVG~MONTH,data=Weather_data)
summary(Winddir_Month)

Sol_Month <- aov(Solare.Radiation.AVG ~ MONTH, data=Weather_data)
summary(Sol_Month)

