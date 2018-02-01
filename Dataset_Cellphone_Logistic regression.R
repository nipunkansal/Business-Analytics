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

#Exploratory analysis


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
View(Data_full)
str(Data_full)
attach(Data_full)

View(Data.dev)
str(Data.dev)
attach(Data.dev)

View(Data.test)
str(Data.test)
attach(Data.test)

#Logistic regression model
fit1 <- glm(Churn~., data=Data.dev, family=binomial(link='logit'))
summary(fit1)

#Log likelihood Ratio test

#install.packages("lmtest")
library(lmtest)
lrtest(fit1)

#Psuedo R-square

#install.packages("pscl")
library(pscl)
pR2(fit1)

# interpreting the summary of the model

#Prediction
probabilities <- predict(fit1, Data.dev[,2:11], type='response')
predictions <- ifelse(probabilities > 0.5,'1','0')
data.frame(ActualChurn = Data.dev$Churn, PredictedChurn = predictions)
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

#ROC Plot
??roc.plot
#install.packages("sandwich")
#install.packages("Deducer")
library(sandwich)
library(Deducer)
rocplot(fit1, diag = TRUE, AUC = TRUE, pred.prob.labels = TRUE, prob.label.digits = 3)

#Odds ratio
#Finding Odds
odds<-exp(coef(fit1))
write.csv(odds,"odds.csv")



