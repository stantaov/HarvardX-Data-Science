
#############################################
# Final Capstone Project - Fraud Detection  # 
# Stanislav Taov                            #
# date: "28/12/2020"                        #
#############################################

# installing and loading necessary libraries 
if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) 
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) 
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readr)) 
  install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(outliers)) 
  install.packages("outliers", repos = "http://cran.us.r-project.org")
if(!require(stringr)) 
  install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) 
  install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) 
  install.packages("ROCR", repos = "http://cran.us.r-project.org")
if(!require(pROC)) 
  install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(gbm)) 
  install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(googledrive)) 
  install.packages("googledrive", repos = "https://cloud.r-project.org/")

library(googledrive)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(outliers)
library(stringr)
library(corrplot)
library(caret)
library(ROCR)
library(pROC)
library(gbm) 


# loading the dataset, it will ask for google authorization and will provide a code
# use it to get an access to the dataset
temp <- tempfile(fileext = ".zip")
dl <- drive_download(
  as_id("1PWKrd28N99dIM7CCHPwmlcYHrE9s2soY"), path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())
df <- read_csv(out[1])

#setting seed for the process replication
set.seed(2020)

# displaying data structure
str(df)

# checking missing values
sum(is.na(df))

# table fraud transactions
df %>%
  group_by(type) %>%
  summarise(transactions = n(), fraud = sum(isFraud), percentage=(fraud/transactions)*100) %>%
  knitr::kable()

# plot distribution of steps, message=FALSE, warning=FALSE}
ggplot(df, aes(x=step)) + 
  geom_histogram(fill="steelblue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Steps")

# converting steps into hours
df$hour=df$step %%24

# plot distribution of hours
ggplot(df, aes(x=hour)) + 
  geom_histogram(bins = 24, fill="steelblue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of of The Hour of the Day")


# distribution of hour with fraud
df %>%
  filter(isFraud == 1) %>%
  ggplot(aes(x=hour)) + 
    geom_histogram(bins = 24, fill="steelblue", color = "black") +
    theme_minimal() +
    ggtitle("Distribution of Hour (Fraud)")

# distribution of amount
ggplot(df, aes(x=amount)) + 
  geom_histogram(fill="steelblue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Amount")


# distribution of Amount (Scaled)
ggplot(df, aes(x=amount)) + 
  geom_histogram(fill="steelblue", color = "black") +
  theme_minimal() +
  scale_x_log10() +
  ggtitle("Distribution of Amount (Scaled)")


# Box Plot of Transaction Types
ggplot(df, aes(x=type, y=amount, fill=type)) + 
  geom_boxplot() + 
  theme_minimal() +
  ggtitle("Box Plot of Transaction Types")

# Box Plot of Transaction Type
df %>%
  filter(type == "TRANSFER", isFraud == 1) %>%
  select(type, amount, isFraud) %>%
  ggplot(aes(x=type, y=amount, fill=type)) + 
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Box Plot of Transaction Type (TRANSFER) with fraud")


# Box Plot of Transaction Type (CASH_OUT) with fraud
df %>%
  filter(type == "CASH_OUT", isFraud == 1) %>%
  select(type, amount, isFraud) %>%
  ggplot(aes(x=type, y=amount, fill=type)) + 
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Box Plot of Transaction Type (CASH_OUT) with fraud")


# Distribution of oldbalanceOrg graph
ggplot(df, aes(x=oldbalanceOrg)) + 
  geom_histogram(fill="steelblue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of oldbalanceOrg")

# Distribution of newbalanceOrig graph
ggplot(df, aes(x=newbalanceOrig)) + 
  geom_histogram(fill="steelblue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of newbalanceOrig")

# Distribution of oldbalanceDest graph
ggplot(df, aes(x=oldbalanceDest)) + 
  geom_histogram(fill="steelblue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of oldbalanceDest")

# Distribution of newbalanceDest
ggplot(df, aes(x=newbalanceDest)) + 
  geom_histogram(fill="steelblue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of newbalanceDest")

# outliers
grubbs.test(df$amount)
grubbs.test(df$oldbalanceOrg)
grubbs.test(df$newbalanceOrig)
grubbs.test(df$oldbalanceDest)
grubbs.test(df$newbalanceDest)


# creating new feature nameStart and nameRecives
df <- df %>% 
  mutate(nameStart = str_sub(nameOrig, 1, 1), nameRecives = str_sub(nameDest, 1, 1))


# distribution of nameStart
ggplot(df, aes(x=nameStart, y =(..count..))) + 
  geom_bar(fill="steelblue", color = "black") +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  labs(x = "Customer/Merchants", y = "Frequency",
       title = "Frequency of nameStart (Transaction Sender)")


ggplot(df, aes(x=nameRecives, y =(..count..))) + 
  geom_bar(fill="steelblue", color = "black") +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  labs(x = "Customer/Merchants", y = "Frequency",
       title = "Frequency of nameRecives (Transaction Reciver)")


# distribution of Customer/Merchants
df %>%
  filter(isFraud == 1) %>%
  ggplot(aes(x=nameRecives, y =(..count..))) + 
    geom_bar(fill="steelblue", color = "black") +
    theme_minimal() +
    geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
    labs(x = "Customer/Merchants", y = "Frequency",
         title = "Frequency of nameRecives - Transaction Reciver (Fraud)")

# distribution of Customer/Merchants with fraud
ggplot(df, aes(x=isFlaggedFraud, y =(..count..))) + 
  geom_bar(fill="steelblue", color = "black") +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  labs(x = "Customer/Merchants", y = "Frequency", 
       title = "Frequency of nameRecives (Transaction Reciver)")

df %>%
  filter(amount > 200000) %>%
  summarise(transactions_over_200000 = n()) %>%
  knitr::kable()


# delete columns
df <- df %>%
  select(-c('nameDest', 'nameOrig', 'isFlaggedFraud', 'nameStart'))

# balanceDifSender
df <- df %>% 
  mutate(balanceDifSender = if_else(newbalanceOrig - oldbalanceOrg > 0, 1, if_else(newbalanceOrig - oldbalanceOrg < 0, -1, 0)))

# balanceDifReciver
df <- df %>% 
  mutate(balanceDifReciver = if_else(newbalanceDest - oldbalanceDest > 0, 1, if_else(newbalanceDest - oldbalanceDest < 0, -1, 0)))

#Converting type and nameRecives to factor.
df$type<-as.factor(df$type)
df$nameRecives<-as.factor(df$nameRecives)

# Converting to numerical
df <- df %>%
  mutate_if(is.factor, as.numeric)

# correction matrix
df.cor <- cor(df, method = c("spearman"))
corrplot(df.cor)

# undersample
fraud <- df %>% filter(isFraud == 1) %>% nrow()
not_fraud <- df %>% filter(isFraud == 0) %>% sample_n( fraud*1.3)

# creating undersample df
under_df <- df %>% filter(isFraud == 1) %>% rbind(not_fraud) %>% arrange(step)


# splitting the dataset
train_part <- 0.60
test_part <- 0.30
validation_part <- 0.10

train_size <- floor(train_part * nrow(under_df))
test_size <- floor(test_part * nrow(under_df))
validation_size <- floor(validation_part * nrow(under_df))

train_size <- floor(train_part * nrow(under_df))
test_size <- floor(test_part * nrow(under_df))
validation_size <- floor(validation_part * nrow(under_df))

train_indices    <- sort(sample(seq_len(nrow(under_df)), size=train_size))
not_train_indices <- setdiff(seq_len(nrow(under_df)), train_indices)
validation_indices  <- sort(sample(not_train_indices, size=validation_size))
test_indices <- setdiff(not_train_indices, validation_indices)

train   <- under_df[train_indices, ]
validation <- under_df[validation_indices, ]
test       <- under_df[test_indices, ]


#Decision Tree model
model_dt <- train(as.factor(isFraud) ~ ., 
               data = train, method = "rpart",
               trControl = trainControl(method = "cv", number =5),
                   tuneLength = 10, 
                   parms=list(split='information'))

# Decision Tree model test
results_dt <- predict(model_dt , test)
roc_curve_dt <- roc(test$isFraud ~ as.numeric(results_dt))
roc_curve_dt
plot(roc_curve_dt)

# Random Forest model
model_rf <- train(as.factor(isFraud) ~ ., 
                  data = train, 
                  trControl = trainControl(method = "cv", number =5),
                  importance=TRUE,
                  method="rf",ntree=5)
# Variable importance chart
plot(varImp(model_rf))

# Random Forest model testing
results_rf <- predict(model_rf , test)
roc_curve_rf <- roc(test$isFraud ~ as.numeric(results_rf))
roc_curve_rf
plot(roc_curve_rf)


## XGBoost model
model_xgb <- train(
  as.factor(isFraud) ~ .,  data = train, method = "xgbTree",
  trControl = trainControl("cv", number = 5)
  )

#Variable importance chart
plot(varImp(model_xgb))


# xgboost model test
results_xgb <- predict(model_xgb , test)
roc_curve_xgb <- roc(test$isFraud ~ as.numeric(results_xgb))
roc_curve_xgb
plot(roc_curve_xgb)


# final model test
results <- predict(model_rf , validation)
roc_curve<- roc(validation$isFraud ~ as.numeric(results))
roc_curve
plot(roc_curve)

#Confusion Matrix and Statistics
xtab <- table(results, validation$isFraud)
confusionMatrix(xtab)

