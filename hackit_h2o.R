setwd("C:/Users/joewa/OneDrive/Documents/MBA/MBAD 6211/5- Hackathon")
library("data.table")
teams <- fread("train_u6lujuX_CVtuZ9i.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
nrow(teams) #614
summary(teams)

teams$Loan_ID <- NULL
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
library("deepnet")
library("gbm")
library("rJava")

  #Gender Married Education  
teams$"Gender"[teams$"Gender"=="Male"] <- 1
teams$"Gender"[teams$"Gender"=="Female"] <- 0
teams$"Married"[teams$"Married"=="Yes"] <- 1
teams$"Married"[teams$"Married"=="No"] <- 0
teams$"Dependents"[teams$"Dependents"=="3+"] <- 1
teams$"Dependents"[teams$"Dependents"=="2"] <- 1
teams$"Dependents"[teams$"Dependents"=="1"] <- 1
teams$"Education"[teams$"Education"=="Graduate"] <- 1
teams$"Education"[teams$"Education"=="Not Graduate"] <- 0
teams$"Self_Employed"[teams$"Self_Employed"=="Yes"] <- 1
teams$"Self_Employed"[teams$"Self_Employed"=="No"] <- 0
teams$"Self_Employed"[is.na(teams$"Self_Employed")] <- 0
teams$"Total_Income"<- (teams$"ApplicantIncome"+teams$"CoapplicantIncome")
teams$"Urban"[teams$"Property_Area"=="Urban"] <- 1
teams$"Rural"[teams$"Property_Area"=="Rural"] <- 1
teams$"Property_Area" <- NULL

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



teams$"Urban"[is.na(teams$"Urban")] <- 0
teams$"Rural"[is.na(teams$"Rural")] <- 0


teams$"Loan_Status"[teams$"Loan_Status"=="Y"] <- 1
teams$"Loan_Status"[teams$"Loan_Status"=="N"] <- 0



teams$Gender <- as.factor(teams$Gender)
teams$Married <- as.factor(teams$Married)
teams$Dependents <- as.factor(teams$Dependents)
teams$Education <- as.factor(teams$Education)
teams$Self_Employed <- as.factor(teams$Self_Employed)
teams$Urban <- as.factor(teams$Urban)
teams$Rural <- as.factor(teams$Rural)
teams$Loan_Status <- as.factor(teams$Loan_Status)

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wolpert/7/R")


library("h2o")
h2o.init()
train_h2o <- as.h2o(teams)
train_comp <- h2o.glrm(train_h2o, k = 5) 

train_comp <- h2o.predict(train_comp)

test_comp <- mice(test) 

test_comp <- complete(test_comp, 1)

str(train_comp)
summary(train_comp)
summary(test_comp)

model <- train(Loan_Status~., data= train_comp, method = "rf")
model <- glm(Loan_Status~ .,
             data = train_comp, family = binomial(link = "logit"), maxit=10000) 
summary(model)
str(model)

predicted_values <- predict(model, test_comp, type = "response")
summary(predicted_values)
predict <- ifelse(predicted_values == 1, "Y", "N")
summary(predict)

test_comp$Loan_Status <- predict

test_out <- test_comp[,c("Loan_Status")]

write.csv(test_out, "test10.csv", row.names = F)

