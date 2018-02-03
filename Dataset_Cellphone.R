rm(list=ls())

##Importing Data
setwd ("D:/BA Course/GL/Predictive modelling/GA")
getwd()

Data_full <- read.csv("Dataset_Cellphone.csv", header = TRUE)
View(Data_full)
str(Data_full)

#Converting Churn,ContractRenewal & DataPlan as factor variables

Data_full$Churn <- factor(Data_full$Churn)
Data_full$ContractRenewal<- factor(Data_full$ContractRenewal)
Data_full$DataPlan <- factor(Data_full$DataPlan)
str(Data_full)
summary(Data_full)
View(Data_full)
attach(Data_full)

## Installing Libraries

#install.packages("mlbench")
#install.packages("Hmisc")

library(mlbench)
library(Hmisc)

## Exploratory data analysis

# Correlation matrix
library(corrplot)
M<-cor(Data_full[4:11])
corrplot(M, method="number")

# Validating Normality of data  - Qnorm plot
par(mfrow= c(2,4))
qqnorm(AccountWeeks, main =  "AccountWeeks ")
qqnorm(DataUsage, main =  "DataUsage")
qqnorm(CustServCalls, main =  "CustServCalls")
qqnorm(DayMins, main =   "DayMins")
qqnorm(DayCalls, main =   "DayCalls")
qqnorm(MonthlyCharge, main =   "MonthlyCharge")
qqnorm(OverageFee, main =   "OverageFee")
qqnorm(RoamMins, main =   "RoamMins")

title("Qnorm Plot -  Normality Check - Cellphone company", outer=TRUE)

#checking skewness of continuous variables
#(for normal distribution skewness is zero)
#install.packages("moments")
library(moments)
skewness(AccountWeeks)
skewness(DataUsage)
skewness(CustServCalls)
skewness(DayMins)
skewness(DayCalls)
skewness(MonthlyCharge)
skewness(OverageFee)
skewness(RoamMins)

# Normality analysis via Histogram
par(mfrow= c(2,4))
hist(AccountWeeks, main ="AccountWeeks",xlab="") 
hist(DataUsage, main = "DataUsage",xlab="")
hist(CustServCalls, main="CustServCalls",xlab="")
hist(DayMins, main ="DayMins",xlab="")
hist(DayCalls, main = "DayCalls",xlab="")
hist(MonthlyCharge, main = "MonthlyCharge",xlab="")
hist(OverageFee, main = "OverageFee",xlab="")
hist(RoamMins, main = "RoamMins",xlab="")
title("Histogram to check data distribution - Cellphone company", outer=TRUE)

#Outlier checking using Boxplot

par(mfrow=c(2,4))
p1 = boxplot(AccountWeeks, main = "AccountWeeks", col = "orange")
p2 = boxplot(DataUsage, main = "DataUsage",  col = "orange")
p4 = boxplot(CustServCalls, main = " CustServCalls", col = "orange")
p5 = boxplot(DayMins, main = "DayMins", col = "orange")
p3 = boxplot(DayCalls, main = "DayCalls",  col = "orange")
p6 = boxplot(MonthlyCharge, main = "MonthlyCharge", col = "orange")
p6 = boxplot(OverageFee, main = "OverageFee",  col = "orange")
p6 = boxplot(RoamMins, main = "RoamMins",  col = "orange")
title("Boxplot Analysis - Cellphone company", outer=TRUE)

## Creating the Dev and holdout (Validating) data
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
#Data.devscaled = scale(Data.dev[4:11])
#View(Data.devscaled)
#Data.dev = cbind(Data.dev[1:3],Data.devscaled)
#View(Data.dev)
#str(Data.dev)
#summary(Data.dev)

#Logistic regression model

fit1 <- glm(Churn~., data=Data.dev, family=binomial(link='logit'))
summary(fit1)

#fit2
fit2 <- glm(Churn~ ContractRenewal+DataPlan+(DataUsage/MonthlyCharge)+AccountWeeks
            +(CustServCalls/MonthlyCharge)+(DayMins/MonthlyCharge)
            +(RoamMins/MonthlyCharge),data=Data.dev, family=binomial(link='logit'))

summary(fit2)

#Check for Variable inflation factor
library(car)
vif(fit2)

#Log likelihood Ratio test - model(fit2)

#install.packages("lmtest")
library(lmtest)
lrtest(fit2)

#Psuedo R-square - model(fit2)

#install.packages("pscl")
library(pscl)
pR2(fit2)

#Prediction on development sample with fit2
probabilities <- predict(fit2, Data.dev[,2:11], type='response')
predictionDev <- ifelse(probabilities > 0.5,'1','0')
Predicted = data.frame(ActualChurn = Data.dev$Churn, PredictedChurn = predictionDev)
write.csv(Predicted,"Predicted.csv")

#confusion matrix
conf<-table(predictions, Data.dev$Churn)
print(conf)

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

## ROC Plot
#install.packages("plotROC")
library(plotROC)
plotROC(Data.dev$Churn, predictionDev)

#Prediction on Holdout sample
probabilities <- predict(fit2, Data.test[,2:11], type='response')
predictionTest <- ifelse(probabilities > 0.5,'1','0')
Predictedtest = data.frame(ActualChurn=Data.test$Churn,PredictedChurn=predictionsTest)
write.csv(Predictedtest,"PredictedTest.csv")

#confusion matrix
conf1<-table(predictions, Data.test$Churn)
print(conf1)

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

##ROC Plot:

#install.packages("sandwich")
#install.packages("Deducer")
library(MASS)
library(sandwich)
library(Deducer)

# For fit1
rocplot(fit1, diag = TRUE, AUC = TRUE, pred.prob.labels = TRUE, prob.label.digits = 3)

#For fit2
rocplot(fit2, diag = TRUE, AUC = TRUE, pred.prob.labels = TRUE, prob.label.digits = 3)

#Odds ratio
#Finding Odds
odds<-exp(coef(fit2))
write.csv(odds,"odds.csv")



