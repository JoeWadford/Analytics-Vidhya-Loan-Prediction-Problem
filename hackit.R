setwd("C:/Users/joewa/OneDrive/Documents/MBA/MBAD 6211/5- Hackathon")
teams <- fread("train_u6lujuX_CVtuZ9i.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
teams_mv <- fread("train_u6lujuX_CVtuZ9i.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
nrow(teams) #614
summary(teams)

library("caret")
library("bnclassify")
library("data.table")
library("evtree")
library("FNN")
library("mice")
library("nnet")
library("adabag")
library("nodeHarvest")
library("randomForest")
library("tree")
library("deepnet")
library("gbm")
library("rJava")
library("VGAM")
library("wsrf")

  #Gender Married Education  
teams$"Gender"[teams$"Gender"=="Male"] <- 1
teams$"Gender"[teams$"Gender"=="Female"] <- 0
teams$"Married"[teams$"Married"=="Yes"] <- 1
teams$"Married"[teams$"Married"=="No"] <- 0
teams$"Dependents"[teams$"Dependents"=="3+"] <- 1
teams$"Dependents"[teams$"Dependents"=="2"] <- 1
teams$"Dependents"[teams$"Dependents"=="1"] <- 1
teams$"Education"[teams$"Education"=="Graduate"] <- 18
teams$"Education"[teams$"Education"=="Not Graduate"] <- 16
teams$"Self_Employed"[teams$"Self_Employed"=="Yes"] <- 1
teams$"Self_Employed"[teams$"Self_Employed"=="No"] <- 0
teams$"Total_Income"<- (teams$"ApplicantIncome"+teams$"CoapplicantIncome")
teams$"Urban"[teams$"Property_Area"=="Urban"] <- 1
teams$"Rural"[teams$"Property_Area"=="Rural"] <- 1

teams_mv$"Gender"[teams_mv$"Gender"=="Male"] <- 1
teams_mv_mv$"Gender"[teams_mv$"Gender"=="Female"] <- 0
teams_mv_mv$"Married"[teams_mv$"Married"=="Yes"] <- 1
teams_mv_mv$"Married"[teams_mv$"Married"=="No"] <- 0
teams_mv_mv$"Dependents"[teams_mv$"Dependents"=="3+"] <- 1
teams_mv$"Dependents"[teams_mv$"Dependents"=="2"] <- 1
teams_mv$"Dependents"[teams_mv$"Dependents"=="1"] <- 1
teams_mv$"Education"[teams_mv$"Education"=="Graduate"] <- 18
teams_mv$"Education"[teams_mv$"Education"=="Not Graduate"] <- 16
teams_mv$"Self_Employed"[teams_mv$"Self_Employed"=="Yes"] <- 1
teams_mv$"Self_Employed"[teams_mv$"Self_Employed"=="No"] <- 0
teams_mv$"Total_Income"<- (teams_mv$"ApplicantIncome"+teams_mv$"CoapplicantIncome")
teams_mv$"Urban"[teams_mv$"Property_Area"=="Urban"] <- 1
teams_mv$"Rural"[teams_mv$"Property_Area"=="Rural"] <- 1


teams$ApplicantIncome <- teams$ApplicantIncome +1
teams$CoapplicantIncome <- teams$CoapplicantIncome +1
teams$loan2inc <- teams$Total_Income / teams$LoanAmount
teams$loan2appinc <- (teams$ApplicantIncome + 1) / teams$LoanAmount
teams$lloanappinc <- log(teams$loan2appinc)
teams$terminc2amt <- teams$loan2appinc * teams$Loan_Amount_Term
teams$lloaninc <- log(teams$loan2inc)
teams$lLoanAmount <- log(teams$LoanAmount)
teams$lApplicantIncome <- log(teams$ApplicantIncome)
teams$lCoapplicantIncome <- log(teams$CoapplicantIncome)
teams$lTotal_Income <- log(teams$Total_Income)

teams_mv$ApplicantIncome <- teams_mv$ApplicantIncome +1
teams_mv$CoapplicantIncome <- teams_mv$CoapplicantIncome +1
teams_mv$loan2inc <- teams_mv$Total_Income / teams_mv$LoanAmount
teams_mv$loan2appinc <- (teams_mv$ApplicantIncome + 1) / teams_mv$LoanAmount
teams_mv$lloanappinc <- log(teams_mv$loan2appinc)
teams_mv$terminc2amt <- teams_mv$loan2appinc * teams_mv$Loan_Amount_Term
teams_mv$lloaninc <- log(teams_mv$loan2inc)
teams_mv$lLoanAmount <- log(teams_mv$LoanAmount)
teams_mv$lApplicantIncome <- log(teams_mv$ApplicantIncome)
teams_mv$lCoapplicantIncome <- log(teams_mv$CoapplicantIncome)
teams_mv$lTotal_Income <- log(teams_mv$Total_Income)

teams$"Urban"[is.na(teams$"Urban")] <- 0
teams$"Rural"[is.na(teams$"Rural")] <- 0
teams$"Loan_Status"[teams$"Loan_Status"=="Y"] <- 1
teams$"Loan_Status"[teams$"Loan_Status"=="N"] <- 0

teams_mv$"Urban"[is.na(teams_mv$"Urban")] <- 0
teams_mv$"Rural"[is.na(teams_mv$"Rural")] <- 0
teams_mv$"Loan_Status"[teams_mv$"Loan_Status"=="Y"] <- 1
teams_mv$"Loan_Status"[teams_mv$"Loan_Status"=="N"] <- 0

teams$Gender <- as.factor(teams$Gender)
teams$Married <- as.factor(teams$Married)
teams$Dependents <- as.factor(teams$Dependents)
teams$Self_Employed <- as.factor(teams$Self_Employed)
teams$Urban <- as.factor(teams$Urban)
teams$Rural <- as.factor(teams$Rural)
teams$Loan_Status <- as.factor(teams$Loan_Status)
teams$Education <- as.numeric(teams$Education)

teams_mv$Gender <- as.factor(teams_mv$Gender)
teams_mv$Married <- as.factor(teams_mv$Married)
teams_mv$Dependents <- as.factor(teams_mv$Dependents)
teams_mv$Self_Employed <- as.factor(teams_mv$Self_Employed)
teams_mv$Urban <- as.factor(teams_mv$Urban)
teams_mv$Rural <- as.factor(teams_mv$Rural)
teams_mv$Loan_Status <- as.factor(teams_mv$Loan_Status)
teams_mv$Education <- as.numeric(teams_mv$Education)


teams_mv$"Property_Area" <- NULL
teams_mv$Loan_ID <- NULL
test_mv$Loan_ID <- NULL
test_mv$"Property_Area" <- NULL
teams_mv$lTotal_Income <- NULL
test_mv$lTotal_Income <- NULL
teams_mv$lApplicantIncome <- NULL
test_mv$lApplicantIncome <- NULL
teams_mv$lCoapplicantIncome <- NULL
test_mv$lCoapplicantIncome <- NULL
teams_mv$lloan2appinc <- NULL
test_mv$lloan2appinc <- NULL
teams_mv$lloan2inc <- NULL
test_mv$lloan2inc <- NULL
teams_mv$lloaninc <- NULL
test_mv$lloaninc <- NULL
teams_mv$lLoanAmount <- NULL
test_mv$lLoanAmount <- NULL
teams_mv$Loan_Status <- NULL
teams_mv$Gender <- NULL
test_mv$Gender <- NULL
teams_mv$Dependents <- NULL
test_mv$Dependents <- NULL
teams_mv$lloanappinc <- NULL
test_mv$lloanappinc <- NULL
teams_mv$Married <- NULL
test_mv$Married <- NULL
teams_mv$loan2appinc <- NULL
test_mv$loan2appinc <- NULL
teams_mv$Rural <- NULL
teams_mv$Urban <- NULL
test_mv$Rural <- NULL
test_mv$Urban <- NULL
teams_mv$Self_Employed <- NULL
test_mv$Self_Employed <- NULL
teams_mv$Loan_Amount_Term <- NULL
test_mv$Loan_Amount_Term <- NULL
teams_mv$terminc2amt <- NULL
test_mv$terminc2amt <- NULL
teams_mv$Education <- NULL
test_mv$Education <- NULL

teams$Loan_ID <- NULL
teams$Property_Area <- NULL

summary(teams)
glmcredit_history <- glm(Married~., family = binomial(link = "logit"), data = teams)
summary(glmcredit_history)
#Education Self_Employed ApplicantIncome LoanAmount Loan_Amount_Term Credit_History 
#Loan_Status Urban Rural loan2inc loan2appinc lloanappinc terminc2amt lloaninc lLoanAmount
#lApplicantIncome lTotal_Income


teams_mv1 <- mice(teams_mv, m=200, method="pmm") 

teams_mv1 <- complete(teams_mv1, 1)

test_mv1 <- mice(test_mv, m=200, method="pmm") 

test_mv1 <- complete(test_mv1, 1)

teams_comp$Credit_History <- teams_mv1$Credit_History
test_comp$Credit_History <- test_mv1$Credit_History

teams_comp <- mice(teams, m=200, method="pmm")
test_comp <- mice(test, m=200, method="pmm")
teams_comp <- complete(teams_comp, 1)
test_comp <- complete(test_comp, 1)

teams_comp$Loan_ID <- NULL
test_comp$Loan_ID <- NULL
teams_comp$Property_Area <- NULL
test_comp$Property_Area <- NULL

glmloanstatus <- glm(Loan_Status~., family = binomial(link = "logit"), data = teams_comp)
summary(glmloanstatus)

str(train_comp)
summary(teams_comp)
summary(test_comp)

teams_comp$Self_Employed <- NULL
test_comp$Self_Employed <- NULL
test_comp$Loan_Status <- NULL
teams_comp$Dependents <- NULL
test_comp$Dependents <- NULL
teams_comp$ApplicantIncome <- NULL
test_comp$ApplicantIncome <- NULL
teams_comp$lApplicantIncome <- NULL
test_comp$lApplicantIncome <- NULL
teams_comp$Total_Income <- NULL
test_comp$Total_Income <- NULL
teams_comp$CoapplicantIncome <- teams_comp$CoapplicantIncome - 1
test_comp$CoapplicantIncome <- test_comp$CoapplicantIncome - 1
teams_comp$LoanAmount <- NULL
test_comp$LoanAmount <- NULL
teams_comp$lCoapplicantIncome <- NULL
test_comp$lCoapplicantIncome <- NULL
teams_comp$loan2inc <- NULL
test_comp$loan2inc <- NULL
teams_comp$lloaninc <- NULL
test_comp$lloaninc <- NULL
teams_comp$Gender <- NULL
test_comp$Gender <- NULL
teams_comp$loan2appinc <- NULL
test_comp$loan2appinc <- NULL
teams_comp$Loan_Amount_Term <- NULL
test_comp$Loan_Amount_Term <- NULL
teams_comp$terminc2amt <- NULL
test_comp$terminc2amt <- NULL

model <- train(Loan_Status~ ., data= teams_comp, method = "rf")

summary(model)
str(model)

predicted_values <- predict(model, test_comp, type = "prob")
summary(predicted_values)
predict <- ifelse(predicted_values >= .5, "Y", "N")
summary(predict)

test_comp$Loan_Status <- predict

test_out <- test_comp[,c("Loan_Status")]

write.csv(test_out, "test12.csv", row.names = F)

