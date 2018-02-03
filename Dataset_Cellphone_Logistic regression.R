rm(list=ls())

## Import Data
setwd ("D:/BA Course/GL/Predictive modelling/GA")
getwd()

#install.packages("mlbench")
#install.packages("Hmisc")

library(mlbench)
library(Hmisc)

Data_full <- read.csv("Dataset_Cellphone.csv", header = TRUE)
View(Data_full)
summary(Data_full)
str(Data_full)
attach(Data_full)

#Converting Churn,ContractRenewal & DataPlan as factor variables.
Data_full$Churn <- factor(Data_full$Churn)
Data_full$ContractRenewal<- factor(Data_full$ContractRenewal)
Data_full$DataPlan <- factor(Data_full$DataPlan)
str(Data_full)
View(Data_full)

## Creating the Dev and holdout (testing) data
# 70% of the sample size
smp_size <- floor(0.70 * nrow(Data_full))
# set the seed to make your partition reproductible
set.seed(123)
set.seed(200)
train_ind <- sample(seq_len(nrow(Data_full)), size = smp_size)
Data.dev <- Data_full[train_ind, ]
Data.test <- Data_full[-train_ind, ]

c(nrow(Data_full), nrow(Data.dev), nrow(Data.test))

View(Data.dev)
str(Data.dev)
attach(Data.dev)

View(Data.test)
str(Data.test)
attach(Data.test)

#Scaling the data
Data.devscaled = scale(Data.dev[4:11])
View(Data.devscaled)
Data.dev = cbind(Data.dev[1:3],Data.devscaled)
View(Data.dev)
str(Data.dev)
summary(Data.dev)

#Exploratory analysis
library(corrplot)
M<-cor(Data.dev[4:11])
corrplot(M, method="number")

#Logistic regression model
fit1 <- glm(Churn~., data=Data.dev, family=binomial(link='logit'))
summary(fit1)

#fit2
fit2 <- glm(Churn~ ContractRenewal+DataPlan+(DataUsage/MonthlyCharge)+AccountWeeks
            +CustServCalls+(DayMins/MonthlyCharge)+ OverageFee
            +(RoamMins/MonthlyCharge)+ DayCalls,
            data=Data.dev, family=binomial(link='logit'))
summary(fit2)

#Log likelihood Ratio test

#install.packages("lmtest")
library(lmtest)
lrtest(fit2)

#Psuedo R-square

#install.packages("pscl")
library(pscl)
pR2(fit2)

# interpreting the summary of the model

#Prediction on development sample
probabilities <- predict(fit2, Data.dev[,2:11], type='response')
predictions <- ifelse(probabilities > 0.5,'1','0')
Predicted = data.frame(ActualChurn = Data.dev$Churn, PredictedChurn = predictions)
write.csv(Predicted,"Predicted.csv")
conf<-table(predictions, Data.dev$Churn)
print(conf)

#confusion matrix
TP = conf[1,1]
FN = conf[1,2]
FP = conf[2,1]
TN = conf[2,2]

#Accuracy
Acc = (TP+TN)/(TP+TN+FP+FN)
Acc
#Precision
Prec = TP/(TP+FP)
Prec
#Recall
Recall = TP/(TP+FN)
Recall

#Prediction on Holdout sample
probabilities <- predict(fit2, Data.test[,2:11], type='response')
predictions <- ifelse(probabilities > 0.5,'1','0')
Predictedtest = data.frame(ActualChurn = Data.test$Churn, PredictedChurn = predictions)
write.csv(Predictedtest,"PredictedTest.csv")
conf1<-table(predictions, Data.test$Churn)
print(conf1)

#confusion matrix
TP = conf1[1,1]
FN = conf1[1,2]
FP = conf1[2,1]
TN = conf1[2,2]

#Accuracy
Acc = (TP+TN)/(TP+TN+FP+FN)
Acc
#Precision
Prec = TP/(TP+FP)
Prec
#Recall
Recall = TP/(TP+FN)
Recall

#ROC Plot
??roc.plot
#install.packages("sandwich")
#install.packages("Deducer")
library(sandwich)
library(Deducer)
rocplot(fit1, diag = TRUE, AUC = TRUE, pred.prob.labels = TRUE, prob.label.digits = 3)

#Odds ratio
#Finding Odds
odds<-exp(coef(fit2))
write.csv(odds,"odds.csv")