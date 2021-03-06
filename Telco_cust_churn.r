setwd("")

df<-read.csv("Telco_churn.csv",header=TRUE)

## load library
library(dplyr)
library(tidyverse) 
library(ggplot)
library(ggcorrplot)

#study dataframe structure - identified 11rows with totalcharges NA and tenure=0
str(df)
glimpse(df)
summary(df)
sapply(df, function(x) sum(is.na(x)))


## Data Pre-processing
#Option 1
#remove 11 data as totalcharges=NA
df<- filter(df,!is.na(TotalCharges))

#Option 2
#fil up 11 missing value in totalcharges by using median 
#using median due to positive skewness
boxplot(df$TotalCharges)
df$TotalCharges[is.na(df$TotalCharges)] <- median(df$TotalCharges,na.rm=TRUE)

#fill up tenure=0 with mean as it is more normally distributed
boxplot(df$tenure)
df$tenure[df$tenure==0] <- mean(df$tenure,na.rm=TRUE)

#Converting all factor categorical variable to factor numeric for later predictive modeling purpose
df$gender <- factor(df$gender, levels=c("Female","Male"), labels=c(0,1))
df$SeniorCitizen <- as.factor(df$SeniorCitizen)
df$Partner <- factor(df$Partner, levels=c("No","Yes"), labels=c(0,1))
df$Dependents <- factor(df$Dependents, levels=c("No","Yes"), labels=c(0,1))
df$PhoneService <- factor(df$PhoneService, levels=c("No","Yes"), labels=c(0,1))
df$MultipleLines <- factor(df$MultipleLines, levels=c("No",'No phone service',"Yes"), labels=c(0,1,2))
df$InternetService <- factor(df$InternetService, levels=c('No',"DSL",'Fiber optic'), labels=c(0,1,2))
df$OnlineSecurity <- factor(df$OnlineSecurity, levels=c("No",'No internet service',"Yes"), labels=c(0,1,2))
df$OnlineBackup <- factor(df$OnlineBackup, levels=c("No",'No internet service',"Yes"), labels=c(0,1,2))
df$DeviceProtection <- factor(df$DeviceProtection, levels=c("No",'No internet service',"Yes"), labels=c(0,1,2))
df$TechSupport <- factor(df$TechSupport, levels=c("No",'No internet service',"Yes"), labels=c(0,1,2))
df$StreamingTV <- factor(df$StreamingTV, levels=c("No",'No internet service',"Yes"), labels=c(0,1,2))
df$StreamingMovies <- factor(df$StreamingMovies, levels=c("No",'No internet service',"Yes"), labels=c(0,1,2))
df$Contract <- factor(df$Contract, levels=c('Month-to-month','One year','Two year'),labels=c(0,1,2))
df$PaperlessBilling <- factor(df$PaperlessBilling, levels=c("No","Yes"), labels=c(0,1))
df$PaymentMethod <- factor(df$PaymentMethod, levels=c("Electronic check","Mailed check",'Bank transfer (automatic)','Credit card (automatic)'), labels=c(1,2,3,4))
df$Churn <- factor(df$Churn, levels=c("No","Yes"), labels=c(0,1))

write.csv(df,'telco_churn_cleaned_data.csv')






#Exploratory Data Analysis (EDA)

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
#Customer Account Info
ggplot(df, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Contract vs Churn')
ggplot(df, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('PaperlessBilling vs Churn')
ggplot(df, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('PaymentMethod vs Churn')
ggplot(df, aes(y= tenure, x = "", fill = Churn)) + geom_boxplot()+theme_bw()+ xlab("")+ggtitle('Tenure vs Churn')
ggplot(df, aes(y= MonthlyCharges, x = "", fill = Churn)) + geom_boxplot()+theme_bw()+ xlab("")+ggtitle('MonthlyCharges vs Churn')
ggplot(df, aes(y= TotalCharges, x = "", fill = Churn)) + geom_boxplot()+theme_bw()+ xlab("")+ggtitle('TotalCharges vs Churn')

ggplot(df, aes(x = tenure, fill = Churn)) + geom_density(alpha = 0.4) + labs(title = "Churn by tenure")
ggplot(df, aes(x = MonthlyCharges, fill = Churn)) + geom_density(alpha = 0.4) + labs(title = "Churn by Monthly Charges")
ggplot(df, aes(x = TotalCharges, fill = Churn)) + geom_density(alpha = 0.4) + labs(title = "Churn by Total Charges")


#Customer Demographic
ggplot(df, aes(x=gender,fill=Churn))+ geom_bar()+ theme_bw()+ggtitle('Gender vs Churn')
ggplot(df, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen vs Churn')
ggplot(df, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Partner vs Churn')
ggplot(df, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Dependents vs Churn')

#Type of services customer signed up 
ggplot(df, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('PhoneService vs Churn')
ggplot(df, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('MultipleLines vs Churn')
ggplot(df, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('InternetService vs Churn')       
ggplot(df, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('OnlineSecurity vs Churn') 
ggplot(df, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('OnlineBackup vs Churn') 
ggplot(df, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('DeviceProtection vs Churn') 
ggplot(df, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('TechSupport vs Churn')
ggplot(df, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('StreamingTV vs Churn')
ggplot(df, aes(x=StreamingMovies,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('StreamingMovies vs Churn')



##Senior Citizen Deep Dive
Senior <- df %>% filter(SeniorCitizen==1)

ggplot(Senior, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen Payment Method vs Churn')
ggplot(Senior, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen Internet Service vs Churn')
ggplot(Senior, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen Paperless Billing vs Churn')
ggplot(Senior, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen Phone Service vs Churn')
ggplot(Senior, aes(y= MonthlyCharges, x = "", fill = Churn)) + geom_boxplot()+theme_bw()+ xlab("")+ggtitle('MonthlyCharges vs Churn')
ggplot(df, aes(x=SeniorCitizen,fill=InternetService))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen vs Internet Service')
ggplot(df, aes(x=SeniorCitizen,fill=PaymentMethod))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen vs Payment Method')
ggplot(df, aes(x=SeniorCitizen,fill=PaperlessBilling))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen vs Paperless Billing')
ggplot(df, aes(x=SeniorCitizen,fill=Contract))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen vs Contract')



##Internet Service Deep Dive
Fibre <- df %>% filter(InternetService=='Fiber optic')
ggplot(Fibre, aes(y= MonthlyCharges, x = "", fill = Churn)) + geom_boxplot()+theme_bw()+ xlab("")+ggtitle('Fibre Optic MonthlyCharges vs Churn')

DSL <- df %>% filter(InternetService=='DSL')
ggplot(DSL, aes(y= MonthlyCharges, x = "", fill = Churn)) + geom_boxplot()+theme_bw()+ xlab("")+ggtitle('DSL MonthlyCharges vs Churn')

No <- df %>% filter(InternetService=='No')
ggplot(No, aes(y= MonthlyCharges, x = "", fill = Churn)) + geom_boxplot()+theme_bw()+ xlab("")+ggtitle('No Internet MonthlyCharges vs Churn')



##Senior Citizen Deep Dive
Senior <- df %>% filter(SeniorCitizen==1)

ggplot(Senior, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen Payment Method vs Churn')
ggplot(Senior, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen Internet Service vs Churn')
ggplot(Senior, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen Paperless Billing vs Churn')
ggplot(Senior, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen Phone Service vs Churn')
ggplot(Senior, aes(y= MonthlyCharges, x = "", fill = Churn)) + geom_boxplot()+theme_bw()+ xlab("")+ggtitle('MonthlyCharges vs Churn')
ggplot(df, aes(x=SeniorCitizen,fill=InternetService))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen vs Internet Service')
ggplot(df, aes(x=SeniorCitizen,fill=PaymentMethod))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen vs Payment Method')
ggplot(df, aes(x=SeniorCitizen,fill=PaperlessBilling))+ geom_bar(position = 'fill')+theme_bw()+ggtitle('Senior Citizen vs Paperless Billing')



# Features engineering
#Remove customer ID - as not using it for modeling and analysis
df$customerID <-NULL

# add attributes - tenure group for analysis purpose
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

