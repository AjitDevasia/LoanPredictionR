#Loading Libraries
library(tidyverse)
library(dplyr)
library(DataExplorer)
library(data.table)
library(ggplot2)
library(vcd)
library(rpart)
library(mice)
library(randomForest)

#Read dataset into R
lp<-read.csv("train.csv")

#Object size and structure of Dataset
object.size(lp)
#79 KB
str(lp)
#Checking for NAs across the Dataset
lp %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))



#There are 22, 14 and 50 NAs in LoanAmount, Loan_Amount_Term and Credit History

#Data Cleaning , Transformation
prop.table(table(lp$Gender))
lp$Gender <- as.character(lp$Gender)
lp$Gender <-ifelse(lp$Gender=="",NA,lp$Gender)
lp$Gender <- as.factor(lp$Gender)
lp1 <- lp
lp$Gender <- ifelse(is.na(lp$Gender),2,lp$Gender)
prop.table(table(lp$Gender))
# Since a small no. of missing values were present, the missing values got converted to Males

# Its a 81:18 split between Male:Female. Hence data heavily influenced by Males. 1% NA values to be dealt with..
prop.table(table(lp$Married))
lp$Married <- as.character(lp$Married)
lp$Married <- ifelse(lp$Married =="",NA,lp$Married)       
prop.table(table(lp$Married))
lp$Married <- as.factor(lp$Married)
ggplot(data=lp, mapping = aes(x= lp$Married, fill = lp$Married,na.rm=TRUE)) +
  geom_bar()
str(lp$Married)
lp$Married<-ifelse(is.na(lp$Married),2,lp$Married)
#65% Married Vs 34% Unmarried

prop.table(table(lp$Dependents))
lp$Dependents <- as.character(lp$Dependents)
lp$Dependents <-ifelse(lp$Dependents=="",NA,lp$Dependents)
lp$Dependents <- as.factor(lp$Dependents)
ggplot(data=lp, mapping = aes(x= lp$Dependents, fill = lp$Dependents,na.rm=TRUE))+
  geom_bar()
str(lp$Dependents)
lp$Dependents<-ifelse(is.na(lp$Dependents),1,lp$Dependents)
#57% have zero dependents, with 8.5% with 3+ Dependents

prop.table(table(lp$Education))
str(lp$Education)
#78% Graduates as compared to only 21% Non Grads

prop.table(table(lp$Self_Employed, exclude = NULL))
lp$Self_Employed <- as.character(lp$Self_Employed)
lp$Self_Employed <-ifelse(lp$Self_Employed=="",NA,lp$Self_Employed)
lp$Self_Employed <- as.factor(lp$Self_Employed)
ggplot(data=lp, mapping = aes(x= Self_Employed, fill = Self_Employed,na.rm=TRUE))+
  geom_bar()
str(lp$Self_Employed)
lp$Self_Employed<-ifelse(is.na(lp$Self_Employed),1,lp$Self_Employed)
#81% are not self-employed, meaning they are salaried professionals

ggplot(data=lp, mapping = aes(x= lp$ApplicantIncome, fill = lp$ApplicantIncome))+
  geom_histogram(binwidth = 1000)
#Most Applicant incomes are below 25,000. Detecting outliers with boxplot
boxplot(lp$ApplicantIncome,main="Applicant Income")
# Function to impute outliers in Datasets

fun <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
lp$ApplicantIncome <- fun(lp$ApplicantIncome)

ggplot(data=lp, mapping = aes(x= lp$CoapplicantIncome, fill = lp$CoapplicantIncome))+
  geom_histogram()
anyNA(lp$CoapplicantIncome)
#Maximum Co-Applicants without any income. Very few outliers hence no need to impute

ggplot(data=lp, mapping = aes(x= lp$LoanAmount, fill = lp$LoanAmount))+
  geom_histogram()
#Imputing NAs by prediction for LoanAmount

fit <-  rpart(LoanAmount~ Loan_ID + ApplicantIncome + CoapplicantIncome,
              data = lp[!is.na(lp$LoanAmount),],
              method = "anova")
lp$LoanAmount[is.na(lp$LoanAmount)] <-
  predict(fit, lp[is.na(lp$LoanAmount),])
anyNA(lp$LoanAmount)

#Imputing NAs by prediction for LoanTerm
fit1 <-  rpart(Loan_Amount_Term~ Loan_ID + ApplicantIncome + CoapplicantIncome,
              data = lp[!is.na(lp$Loan_Amount_Term),],
              method = "anova")
lp$Loan_Amount_Term[is.na(lp$Loan_Amount_Term)] <-
  predict(fit1, lp[is.na(lp$Loan_Amount_Term),])
lp$Loan_Amount_Term <- as.factor(lp$Loan_Amount_Term)
ggplot(data=lp, mapping = aes(x= Loan_Amount_Term, fill = Loan_Amount_Term)) +
  geom_bar()
#Maximum no. of cases have loan amount term as 360 close to 85%

prop.table(table(lp$Credit_History,exclude = NULL))
str(lp$Credit_History)

fit2 <-  rpart(Credit_History ~ Loan_ID + ApplicantIncome + CoapplicantIncome,
               data = lp[!is.na(lp$Credit_History),],
               method = "anova")
lp$Credit_History[is.na(lp$Credit_History)] <-
  predict(fit2, lp[is.na(lp$Credit_History),])
lp$Credit_History<- as.factor(lp$Credit_History)
ggplot(data=lp, mapping = aes(x= Credit_History, fill = Credit_History)) +
  geom_bar()

#Credit history meets guidelines 77% of cases

prop.table(table(lp$Loan_Status))
lp$Loan_Status <- as.factor(lp$Loan_Status)

#Building randomforest classification model
lp$Loan_ID <- as.character(lp$Loan_ID)
lp2<- select(lp,-c(Loan_ID))
lp_randomforest <- randomForest(Loan_Status~.,data = lp2)
lp_randomforest
lp_test <- read.csv("test.csv")


