# Import Library

library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plyr)

# Read CSV into R

df <- read.csv('telco.csv')

# Check the dimension of dataset
dim_desc(df)
names(df)

## Data wrangling 
glimpse(df)

# change characters to Factors

df <- df %>% mutate_if(is.character, as.factor)
df$SeniorCitizen <- as.factor(df$SeniorCitizen)
glimpse(df)

### Replace No phone service & No internet service = No 

df$MultipleLines <- as.factor(mapvalues(df$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))
df$OnlineSecurity <- as.factor(mapvalues(df$OnlineSecurity, 
                                        from=c("No internet service"),
                                        to=c("No")))
df$DeviceProtection <- as.factor(mapvalues(df$DeviceProtection, 
                                        from=c("No internet service"),
                                        to=c("No")))
df$OnlineBackup <- as.factor(mapvalues(df$OnlineBackup, 
                                           from=c("No internet service"),
                                           to=c("No")))
df$TechSupport <- as.factor(mapvalues(df$TechSupport, 
                                        from=c("No internet service"),
                                        to=c("No")))
df$StreamingMovies <- as.factor(mapvalues(df$StreamingMovies, 
                                        from=c("No internet service"),
                                        to=c("No")))
df$StreamingTV <- as.factor(mapvalues(df$StreamingTV, 
                                          from=c("No internet service"),
                                          to=c("No")))
# identify the Type of TotalCharge Data (skewed data -> use median to replace missing value)

ggplot(df, aes(x= TotalCharges)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


#searching for missing data

df %>% map(~ sum(is.na(.)))

#replace missing values with median

df <- df %>% mutate(TotalCharges = replace(TotalCharges, is.na(TotalCharges),median(TotalCharges, na.rm = TRUE)))
sum(is.na(df$TotalCharges))

#Remove customer ID as it doesn’t provide any informations

df <- df %>% select(-customerID)









