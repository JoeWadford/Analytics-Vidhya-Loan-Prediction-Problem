setwd("C:/Users/joewa/OneDrive/Documents/MBA/MBAD 6211/5- Hackathon")
library("data.table")
test <- fread("test_Y3wMUE5_7gLdaTN.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
test_mv <- fread("test_Y3wMUE5_7gLdaTN.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
nrow(test) 
summary(test)

library("caret")
library("bnclassify")
library("evtree")
library("FNN")
library("mice")
library("nnet")
library("adabag")
library("nodeHarvest")
library("randomForest")
library("tree")

#Gender Married Education  
test$"Gender"[test$"Gender"=="Male"] <- 1
test$"Gender"[test$"Gender"=="Female"] <- 0
test$"Married"[test$"Married"=="Yes"] <- 1
test$"Married"[test$"Married"=="No"] <- 0
test$"Dependents"[test$"Dependents"=="3+"] <- 1
test$"Dependents"[test$"Dependents"=="2"] <- 1
test$"Dependents"[test$"Dependents"=="1"] <- 1
test$"Education"[test$"Education"=="Graduate"] <- 18
test$"Education"[test$"Education"=="Not Graduate"] <- 16
test$"Self_Employed"[test$"Self_Employed"=="Yes"] <- 1
test$"Self_Employed"[test$"Self_Employed"=="No"] <- 0
test$"Self_Employed"[is.na(test$"Self_Employed")] <- 0
test$"Total_Income"<- (test$"ApplicantIncome"+test$"CoapplicantIncome")
test$"Urban"[test$"Property_Area"=="Urban"] <- 1
test$"Rural"[test$"Property_Area"=="Rural"] <- 1
test$loan2inc <- test$Total_Income / test$LoanAmount
test$"Urban"[is.na(test$"Urban")] <- 0
test$"Rural"[is.na(test$"Rural")] <- 0

test$ApplicantIncome <- test$ApplicantIncome +1
test$CoapplicantIncome <- test$CoapplicantIncome + 1
test$loan2appinc <- (test$ApplicantIncome +1) / test$LoanAmount
test$lloanappinc <- log(test$loan2appinc)
test$terminc2amt <- test$loan2appinc * test$Loan_Amount_Term
test$lTotal_Income <- log(test$Total_Income)
test$lApplicantIncome <- log(test$ApplicantIncome)
test$lCoapplicantIncome <- log(test$CoapplicantIncome)
test$lLoanAmount <- log(test$LoanAmount)
test$lloaninc <- log(test$loan2inc)


test$Gender <- as.factor(test$Gender)
test$Married <- as.factor(test$Married)
test$Education <- as.factor(test$Education)
test$Self_Employed <- as.factor(test$Self_Employed)
test$Urban <- as.factor(test$Urban)
test$Rural <- as.factor(test$Rural)
test$Dependents <- as.factor(test$Dependents)

test_mv$"Gender"[test_mv$"Gender"=="Male"] <- 1
test_mv$"Gender"[test_mv$"Gender"=="Female"] <- 0
test_mv$"Married"[test_mv$"Married"=="Yes"] <- 1
test_mv$"Married"[test_mv$"Married"=="No"] <- 0
test_mv$"Dependents"[test_mv$"Dependents"=="3+"] <- 1
test_mv$"Dependents"[test_mv$"Dependents"=="2"] <- 1
test_mv$"Dependents"[test_mv$"Dependents"=="1"] <- 1
test_mv$"Education"[test_mv$"Education"=="Graduate"] <- 18
test_mv$"Education"[test_mv$"Education"=="Not Graduate"] <- 16
test_mv$"Self_Employed"[test_mv$"Self_Employed"=="Yes"] <- 1
test_mv$"Self_Employed"[test_mv$"Self_Employed"=="No"] <- 0
test_mv$"Self_Employed"[is.na(test_mv$"Self_Employed")] <- 0
test_mv$"Total_Income"<- (test_mv$"ApplicantIncome"+test_mv$"CoapplicantIncome")
test_mv$"Urban"[test_mv$"Property_Area"=="Urban"] <- 1
test_mv$"Rural"[test_mv$"Property_Area"=="Rural"] <- 1
test_mv$loan2inc <- test_mv$Total_Income / test_mv$LoanAmount
test_mv$"Urban"[is.na(test_mv$"Urban")] <- 0
test_mv$"Rural"[is.na(test_mv$"Rural")] <- 0

test_mv$ApplicantIncome <- test_mv$ApplicantIncome +1
test_mv$CoapplicantIncome <- test_mv$CoapplicantIncome + 1
test_mv$loan2appinc <- (test_mv$ApplicantIncome +1) / test_mv$LoanAmount
test_mv$lloanappinc <- log(test_mv$loan2appinc)
test_mv$terminc2amt <- test_mv$loan2appinc * test_mv$Loan_Amount_Term
test_mv$lTotal_Income <- log(test_mv$Total_Income)
test_mv$lApplicantIncome <- log(test_mv$ApplicantIncome)
test_mv$lCoapplicantIncome <- log(test_mv$CoapplicantIncome)
test_mv$lLoanAmount <- log(test_mv$LoanAmount)
test_mv$lloaninc <- log(test_mv$loan2inc)


test_mv$Gender <- as.factor(test_mv$Gender)
test_mv$Married <- as.factor(test_mv$Married)
test_mv$Education <- as.factor(test_mv$Education)
test_mv$Self_Employed <- as.factor(test_mv$Self_Employed)
test_mv$Urban <- as.factor(test_mv$Urban)
test_mv$Rural <- as.factor(test_mv$Rural)
test_mv$Dependents <- as.factor(test_mv$Dependents)

summary(test)

