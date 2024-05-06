#' Predicting Telco Churn (Classification Model - Logistic Regression)


setwd("C:/Users/12149/Desktop/Data Analysis Projects/Classification model/")
library(readxl)
d <- read_excel("TelcoChurn.xlsx", sheet="Data")
str(d)
View(d)

# Pre-processing the data

#Factoring
d$gender           <- factor(d$gender)
d$gender           <- relevel(d$gender, "Female")
d$Contract         <- factor(d$Contract)
d$Contract         <- relevel(d$Contract, "Month-to-month")

d$PaymentMethod    <- factor(d$PaymentMethod)
d$PaymentMethod    <- relevel(d$PaymentMethod, "Mailed check")

d$Churn            <- as.factor(d$Churn)

#Binary classification of relevant variables 
d$Churn            <- ifelse(d$Churn=="Yes", 1, 0)
d$PaperlessBilling <- ifelse(d$PaperlessBilling=="Yes", 1, 0)
d$Partner          <- ifelse(d$Partner=="Yes", 1, 0)
d$Dependents       <- ifelse(d$Dependents=="Yes", 1, 0)
d$MultipleLines    <- ifelse(d$MultipleLines=="Yes", 1, 0)
d$OnlineSecurity   <- ifelse(d$OnlineSecurity=="Yes", 1, 0)
d$OnlineBackup     <- ifelse(d$OnlineBackup=="Yes", 1, 0)
d$DeviceProtection <- ifelse(d$DeviceProtection=="Yes", 1, 0)
d$TechSupport      <- ifelse(d$TechSupport=="Yes", 1, 0)
d$StreamingTV      <- ifelse(d$StreamingTV=="Yes", 1, 0)
d$StreamingMovies  <- ifelse(d$StreamingMovies=="Yes", 1, 0)

#' Subset data

phone    <- d[d$PhoneService=="Yes"   & d$InternetService=="No", ]
internet <- d[d$InternetService!="No" & d$PhoneService=="No" , ]
both     <- d[d$PhoneService=="Yes"   & d$InternetService!="No", ]

dim(phone); dim(internet); dim(both)
table(phone$Churn)                    # Unbalanced data
table(internet$Churn)
table(both$Churn)

#' Training/test split

set.seed(1024)
sample_size    <- floor(0.75 * nrow(phone))
train_index    <- sample(seq_len(nrow(phone)), size=sample_size)
train_phone    <- phone[train_index, ]
test_phone     <- phone[-train_index, ]

sample_size    <- floor(0.75 * nrow(internet))
train_index    <- sample(seq_len(nrow(internet)), size=sample_size)
train_internet <- internet[train_index, ]
test_internet  <- internet[-train_index, ]

sample_size    <- floor(0.75 * nrow(both))
train_index    <- sample(seq_len(nrow(both)), size=sample_size)
train_both     <- both[train_index, ]
test_both      <- both[-train_index, ]

#' Logistic models

phone_model <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + 
                     tenure + MultipleLines + Contract + PaperlessBilling + 
                     PaymentMethod + MonthlyCharges, data=train_phone,
                   family=binomial (link="logit"))

internet_model <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + 
                        tenure + OnlineSecurity + OnlineBackup + DeviceProtection + 
                        TechSupport + StreamingTV + StreamingMovies + Contract + 
                        PaperlessBilling + PaymentMethod + MonthlyCharges,
                      data=train_internet, family=binomial (link="logit"))

both_model <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + 
                    tenure + MultipleLines + OnlineSecurity + OnlineBackup + 
                    DeviceProtection + TechSupport + StreamingTV + StreamingMovies + 
                    Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, 
                  data=train_both, family=binomial (link="logit"))

library(stargazer)
stargazer(phone_model, internet_model, both_model, type="text", single.row=TRUE)

# Classification metrics against test data:

library(caret)
library(ROCR)
predlogit <- predict(phone_model, newdata=test_phone, type="response")
predlogit <- ifelse(predlogit>0.5, 1, 0)
length(predlogit)
length(test_phone$Churn)
table(test_phone$Churn, predlogit)

cm <- confusionMatrix(as.factor(predlogit), reference=test_phone$Churn)
cm$byClass['Recall']                           # 1.00
cm$byClass['Precision']                        # 1.00
cm$byClass['F1']                               # 0.96

# pr <- predict(predlogit, test_phone$Churn)
# prf <- performance(pr, measure = "tpr", x.measure = "fpr")
# plot(prf)                                                 
# auc <- performance(pr, measure = "auc")
# auc@y.values[[1]]

predlogit <- predict(internet_model, newdata=test_internet, type="response")
predlogit <- ifelse(predlogit>0.5, 1, 0)
table(test_internet$Churn, predlogit)
cm <-confusionMatrix(as.factor(predlogit), reference=test_internet$Churn)
cm$byClass['Recall']                           # 0.92
cm$byClass['Precision']                        # 0.82
cm$byClass['F1']                               # 0.87

predlogit <-predict(both_model, newdata=test_both, type="response")
predlogit <- ifelse(predlogit>0.5, 1, 0)
table(test_both$Churn, predlogit)
cm <-confusionMatrix(as.factor(predlogit), reference=test_both$Churn)
cm$byClass['Recall']                           # 0.84
cm$byClass['Precision']                        # 0.82
cm$byClass['F1']                               # 0.83

# Good fit on the test dataset suggests no overfitting of the data.
# The phone only model has the best metrics and higher classification accuracy 
# of the three models, followed by the internet only and both model has the 
# least classification accuracy. 
