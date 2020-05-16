# Steps:
# Questions:
# Question1 : Why Customer Churns?
# Identify the group of customers with high churn rate
# Question2 : How to reduce the Churn rate?
# Identify the services offered with low churn rate
# Question3 : What are the potential profit with the implementation of customer churn model?
# 

# Import necessary library
library(readr)
library(dplyr)
library(ggplot2)

# Read CSV into R
df <- read.csv(file='C:/Users/W10/Desktop/Uni/WQD 7005 Programming for Data Science/Group project/Telco-Customer-Churn.csv', header=TRUE, sep=',')

# Check the dimension of dataset
dim(df)

## Data Cleaning
# Check number and format of columns
glimpse(df)

 
# Check unique values of all columns
# No duplicate of rows, as all customerID are unique
summary(df)

# Format conversion & label encoding
# Label encoding on categorical data set
df <- df %>% mutate(gender = ifelse(gender=='Male', 0, 1),
                    Partner = ifelse(Partner == 'Yes',0,1),
                    Dependents = ifelse(Dependents == 'Yes',0,1),
                    PhoneService = ifelse(PhoneService == 'Yes',0,1),
                    MultipleLines = ifelse(MultipleLines == 'Yes',0,ifelse(MultipleLines == 'No',1,2)),
                    InternetService = ifelse(InternetService == 'DSL',0,ifelse(InternetService == 'Fiber optic',1,2)),
                    OnlineSecurity = ifelse(OnlineSecurity == 'Yes',0,ifelse(OnlineSecurity == 'No',1,2)),
                    OnlineBackup = ifelse(OnlineBackup == 'Yes',0,ifelse(OnlineBackup == 'No',1,2)),
                    DeviceProtection = ifelse(DeviceProtection == 'Yes',0,ifelse(DeviceProtection == 'No',1,2)),
                    TechSupport = ifelse(TechSupport == 'Yes',0,ifelse(TechSupport == 'No',1,2)),
                    StreamingTV = ifelse(StreamingTV == 'Yes',0,ifelse(StreamingTV == 'No',1,2)),
                    StreamingMovies = ifelse(StreamingMovies == 'Yes',0,ifelse(StreamingMovies == 'No',1,2)),
                    Contract = ifelse(Contract == 'Yes',0,ifelse(Contract == 'No',1,2)),
                    PaperlessBilling = ifelse(PaperlessBilling == 'Yes',0,1),
                    PaymentMethod = ifelse(PaymentMethod == 'Bank transfer (automatic)',0,ifelse(PaymentMethod == 'Credit card (automatic)',1,ifelse(PaymentMethod == 'Electronic check',2,3))),
                    Churn = ifelse(Churn == 'Yes',0,1))

# Reformat the columns
categorical_var <-  c('gender', 'SeniorCitizen', 'Partner', 'Dependents', 'PhoneService', 'MultipleLines', 'InternetService',
                         'OnlineSecurity', 'OnlineBackup', 'DeviceProtection', 'TechSupport', 'StreamingTV', 'StreamingMovies',
                         'Contract', 'PaperlessBilling', 'PaymentMethod', 'Churn')
df[categorical_var] <- lapply(df[categorical_var], factor)

# Missing values
# Check number of columns with missing value
colSums(is.na(df))
Miss_values <- subset(df, is.na(df$TotalCharges))
# Impute the missing values with media,. as only 1 column of data is missing, other columns might contains important information
# As outliers are present in the datasets, we replace the missing values with median values
summary(df$TotalCharges)
df <- df %>% mutate(TotalCharges = replace(TotalCharges, is.na(TotalCharges), median(TotalCharges, na.rm = TRUE)))

# Outliers/Anomaly
Plot1 <- boxplot(df$tenure,
        main = "Customer's Tenure",
        xlab = "Tenure",
        horizontal = T)

# Slightly negative skewed
boxplot(df$MonthlyCharges,
        main = "Monthly Charges on Customer",
        xlab = "Monthly Charges",
        horizontal = T)

# Highly positive skewed
boxplot(df$TotalCharges,
        main = "Total Charges on Customer",
        xlab = "Tenure",
        horizontal = T)

# Data Exploration
# Relationship between continuos variables with churn probability
df %>% ggplot(aes(x=tenure,fill=Churn))+ geom_density(alpha=0.5)+scale_fill_manual(values=c('blue','red'))+labs(title='Tenure density split churn vs non churn' )
df %>% ggplot(aes(x=MonthlyCharges,fill=Churn))+ geom_density(alpha=0.5)+scale_fill_manual(values=c('blue','red'))+labs(title='Monthly Charges density split churn vs non churn' )
df %>% ggplot(aes(x=TotalCharges,fill=Churn))+ geom_density(alpha=0.5)+scale_fill_manual(values=c('blue','red'))+labs(title='Total Charges density split churn vs non churn' )