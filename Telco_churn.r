setwd("C:/Users/HiapLi/Desktop/Hiapli/Education/Master - Data Science (UM)/Sem1/WQD7004_Programming in Data Science/Assignment")

df<-read.csv("Telco_churn.csv",header=TRUE)

## load library
library(dplyr)
library(tidyverse) 
library(MASS)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)

#study dataframe structure - identified 11rows with totalcharges NA and tenure=0
str(df)
glimpse(df)
summary(df)
sapply(df, function(x) sum(is.na(x)))


## Data Featuring

#remove 11 data as totalcharges=NA
df<- filter(df,!is.na(TotalCharges))

# add new column - tenure group
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
df$tenure_group <- sapply(df$tenure,group_tenure)
df$tenure_group <- as.factor(df$tenure_group)

#Remove customer ID
df$customerID <-NULL



#Exploratory Data Analysis

#basic plotting for analysis
options(repr.plot.width = 6, repr.plot.height = 4)
df %>% group_by(Churn) %>% summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Churn Percent")


#options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(df, aes(x=gender,fill=Churn))+ geom_bar()+ theme_bw()+ggtitle('Gender vs Churn')
ggplot(df, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen vs Churn')
ggplot(df, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Partner vs Churn')
ggplot(df, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Dependents vs Churn')
ggplot(df, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('PhoneService vs Churn')
ggplot(df, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('MultipleLines vs Churn')
ggplot(df, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('InternetService vs Churn')       
ggplot(df, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('OnlineSecurity vs Churn') 
ggplot(df, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('OnlineBackup vs Churn') 
ggplot(df, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('DeviceProtection vs Churn') 
ggplot(df, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('TechSupport vs Churn')
ggplot(df, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('StreamingTV vs Churn')
ggplot(df, aes(x=StreamingMovies,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('StreamingMovies vs Churn')
ggplot(df, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Contract vs Churn')
ggplot(df, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('PaperlessBilling vs Churn')
ggplot(df, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('PaymentMethod vs Churn')
ggplot(df, aes(y= tenure, x = "", fill = Churn)) + geom_boxplot()+theme_bw()+ xlab("")+ggtitle('Tenure vs Churn')
ggplot(df, aes(y= MonthlyCharges, x = "", fill = Churn)) + geom_boxplot()+theme_bw()+ xlab("")+ggtitle('MonthlyCharges vs Churn')
ggplot(df, aes(y= TotalCharges, x = "", fill = Churn)) + geom_boxplot()+theme_bw()+ xlab("")+ggtitle('TotalCharges vs Churn')



#to change the factor to numeric
df$gender <- as.numeric(ifelse(df$gender=='Female',0,1))
df$Partner <- as.numeric(ifelse(df$Partner=='No',0,1))
df$Dependents <- as.numeric(ifelse(df$Dependents=='No',0,1))
df$PhoneService <- as.numeric(ifelse(df$PhoneService=='No',0,1))
df$MultipleLines <- as.numeric(ifelse(df$MultipleLines=='No',0,1))




#add new column
mutate(df, newcol = func(oldcol))

# method 2 
cbind(df,columnname)

#to sort
sort(df$column)
df[order(df$column,decreasing=TRUE),]