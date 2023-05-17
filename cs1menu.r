setwd("D:/APLLIED STATS/CASE STUDY-II")
data<-read.csv("menu.csv")

summary(data)

library(ggplot2)
library(dplyr)
library(stringr)

#1)
mean(data$Calories)

#no of items in each category
grouped0<-data%>%group_by(Category)%>%tally()
View(grouped0)
a0<-c(grouped0$n)
a0
ac<-c(grouped0$Category)
ac

barplot(a0,las=2,names.arg=ac,col="blue",main="No of Items in each category",xlab="Category",ylab="No of Items")

#average calories in each category
grouped1<-data%>%group_by(Category)%>%summarise(Average_Calorie_Content=mean(Calories))%>%ggplot(aes(Category, Average_Calorie_Content)) +
  geom_col(fill = "deepskyblue4") +
  ggtitle("Average Calories in Each Category") +
  labs(y= "Avg. Cal", x = "Category")
grouped1


#2)
grouped<-data%>%group_by(Category)%>%summarise(Total_Calorie_Content=sum(Calories))
View(grouped)

l<-0
a1<-c(grouped$Total_Calorie_Content)
count=1
for(i in grouped$Category)
{
  if(i=="Coffee & Tea")
  {
    cat(i,":",a1[count])
    l=(a1[count])/sum(data$Calories)
    print(l*100)
  }
  if(i=="Beverages")
  {
    cat(i,":",a1[1])
    l=(a1[count])/sum(data$Calories)
    print(l*100)
  }
    
  count=count+1
}


#3)
# Calling str_detect() function
count<-1
str_detect("Premium GrilledS Chicken Classic Sandwich", "Grilled")
g<-0
cr<-0
for (i in data$Category)
{
  if (i=="Chicken & Fish" && str_detect(data$Item[count], "Sandwich"))
  {
    if (str_detect(data$Item[count], "Grilled"))
    {
      
      g<-g+data$Calories[count]+data$Total.Fat[count]+data$Sodium[count]+data$Carbohydrates[count]+data$Dietary.Fiber[count]+data$Protein[count]
    }
    if (str_detect(data$Item[count], "Crispy"))
    {
      
      cr<-cr+data$Calories[count]+data$Total.Fat[count]+data$Sodium[count]+data$Carbohydrates[count]+data$Dietary.Fiber[count]+data$Protein[count]
    }
  }
  count<-count+1
}
print(g)
print(cr)
if (g>cr)
{
  print("Grilled is nutritionous")
}else{
  print("Crispy is nutritionous")
}

#4)
count<-1
white<-0
egg<-0
for (i in data$Category)
{
  if (i=="Breakfast" && str_detect(data$Item[count], "Egg"))
  {
    if (str_detect(data$Item[count], "White"))
    {
      
      white<-white+data$Calories[count]+data$Total.Fat[count]+data$Sodium[count]+data$Carbohydrates[count]+data$Dietary.Fiber[count]+data$Protein[count]
    }else{
      
      egg<-egg+data$Calories[count]+data$Total.Fat[count]+data$Sodium[count]+data$Carbohydrates[count]+data$Dietary.Fiber[count]+data$Protein[count]
    }
  }
  count<-count+1
}
print(white)
print(egg)
if (white>egg)
{
  print("Egg White is nutritionous")
}else{
  print("Egg is nutritionous")
}

#5)
desc_category<-data%>%arrange(desc(Calories))
desc_category
n<-1
calcount<-0
count<-1
desc_category$Calories[1]
desc_category$Calories[2]
n<-0
for(i in desc_category)
{
  print(n)
  calcount=calcount+desc_category$Calories[count]
  calcount
  desc_category$Calories[count]
  n=n+1
  if(calcount>=2000)
  {
    break
  }
  count=count+1
}
print(n)
dailyreq<-desc_category[1:n,]
dailyreq

