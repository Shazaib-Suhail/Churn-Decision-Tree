#Importing Data 
df <- read.csv("\Data\WA_Fn-UseC_-Telco-Customer-Churn.csv", stringsAsFactors = T)

#Understanind the structure of data
str(df)

#Trying to check for missing values
table(df$gender)

#Creating a copy of the Dataframe 
df_new <- df

#Labeling the missing factors
df_new$SeniorCitizen <- factor(df_new$SeniorCitizen, levels = c(0,1), labels = c("No", "Yes"))

#Cleaning the data for better performance. (Reducing the number of factors)
df_new$MultipleLines[df_new$MultipleLines == "No phone service"] <- "No"
df_new$OnlineSecurity[df_new$OnlineSecurity == "No internet service"] <- "No"
df_new$OnlineBackup[df_new$OnlineBackup == "No internet service"] <- "No"
df_new$DeviceProtection[df_new$DeviceProtection == "No internet service"] <- "No"
df_new$TechSupport[df_new$TechSupport == "No internet service"] <- "No"
df_new$StreamingTV[df_new$StreamingTV == "No internet service"] <- "No"
df_new$StreamingMovies[df_new$StreamingMovies == "No internet service"] <- "No"

#Reducing factors for tenure variable
#max(df_new$tenure)
#min(df_new$tenure)
df_new$tenure[(df_new$tenure >= 0 & df_new$tenure <= 12)] <- "0-1 Year" 
df_new$tenure[(df_new$tenure >= 13 & df_new$tenure <= 24)] <- "1-2 Year" 
df_new$tenure[df_new$tenure >= 25 & df_new$tenure <= 36] <- "2-3 Year"
df_new$tenure[df_new$tenure >= 37 & df_new$tenure <= 48] <- "3-4 Year"
df_new$tenure[df_new$tenure >= 49 & df_new$tenure <= 60] <- "4-5 Year"
df_new$tenure[df_new$tenure >= 61 & df_new$tenure <= 72] <- "5-6 Year"
df_new$tenure[df_new$tenure >= 73 & df_new$tenure <= 84] <- "6-7 Year" 

df_new$tenure <- factor(df_new$tenure)

#Updating the factor changes
df_new$MultipleLines <- factor(df_new$MultipleLines)
df_new$OnlineSecurity <- factor(df_new$OnlineSecurity)
df_new$OnlineBackup <- factor(df_new$OnlineBackup)
df_new$DeviceProtection <- factor(df_new$DeviceProtection)
df_new$TechSupport <-factor(df_new$TechSupport)
df_new$StreamingTV <- factor(df_new$StreamingTV)
df_new$StreamingMovies <-factor(df_new$StreamingMovies)

#Attaching the column names for ease of access.
attach(df_new)

library(caret)
library(ggplot2)
library(rpart)

#Setting the random seed to get the same sample when executed again.
set.seed(123)
df_new$Churn = as.factor(df_new$Churn)
df_new <- df_new[-c(1)]

#Sampling for testing and training
sample <- createDataPartition(df_new$Churn, p = .70, list = FALSE) 
train = df_new[sample,]
test = df_new[-sample,]

#Training the model
dt = rpart(Churn ~., data = train, method = "class")
summary(dt)

#Testing the data on the test sample
dt_test <- predict(dt, type = "class", newdata = test[,-24])

#Model Evaluation
confusionMatrix(test$Churn, dt_test)


