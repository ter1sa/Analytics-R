library(data.table)
library(randomForest)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library(lessR)
library(tidyverse)

#Part A: Data Exploration and Preparation
#1a.
#import csv dataset
setwd("~/Documents/NTU/Y2S2/BC2407/AY21S2 BC2407 CBA")
data1<-fread("data01.csv")

#check the class types of all the variables
for(var in colnames(data1)){
  print(var)
  print(class(data1[[var]]))
}

#all the variables with class type integer are categorical except for ID, age and EF
#make the class types of all those variables categorical
for(var in colnames(data1)){
  if(class(data1[[var]]) == "integer" & var != "ID" & var != "age" & var != "EF")
    data1[[var]] <- as.factor(data1[[var]])
}

#1b.
#the derivation group is the train set to train the dataset using machine learning and the validation group is the test set to test the model that the train set has trained to ensure that there is no overfitting.
#it is reflected in the group variable. group = 1 is the derivation group and group = 2 is the validation group.

#1c.
NA_table <- data.frame(colSums(is.na(data1)))
NA_table <- data.table(NA_table, keep.rownames=TRUE)
i = 1
for(var in colnames(data1)){
  NA_table$Data.Type[i] = class(data1[[var]])
  i = i + 1
}
NA_table = NA_table[,c(1,3,2)]
setnames(NA_table, "rn", "Variable.Name")
setnames(NA_table, "colSums.is.na.data1..", "NA.Count")
NA_table = NA_table[c(NA_table$NA.Count!=0)]
View(NA_table)

#1d.
#separate into continuous and categorical data for data exploration (exclude group and ID)
contVar <- data1
for(var in colnames(contVar)){
  if(class(contVar[[var]]) == "factor"){
    contVar[[var]]=NULL
  }
}
contVar[, ID:=NULL]
summary(contVar)
#anomalies: #BMI, Urine output, Leucocyte, Platelets, Neutrophils, Basophils, Lymphocyte, PT, INR, NT-proBNP, Creatine kinase, Creatinine, Urea nitrogen, glucose


#Univariate: Continuous Variables

#distribution of continuous variables 
par(mfrow=c(2,4))
boxplot(data1$BMI, main = "BMI")
boxplot(data1$`Urine output`, main = "Urine output")
boxplot(data1$Leucocyte, main = "Leucocyte")
boxplot(data1$Platelets, main = "Platelets")
boxplot(data1$Neutrophils, main = "Neutrophils")
boxplot(data1$Basophils, main = "heart rate")
boxplot(data1$Lymphocyte, main = "Lymphocyte")
boxplot(data1$PT, main = "PT")
par(mfrow=c(2,3))
boxplot(data1$INR, main = "INR")
boxplot(data1$`Diastolic blood pressure`, main = "Diastolic blood pressure")
boxplot(data1$`Systolic blood pressure`, main = "Systolic blood pressure")
boxplot(data1$Creatinine, main = "Creatinine")
boxplot(data1$`Urea nitrogen`, main = "Urea nitrogen")
boxplot(data1$glucose, main = "glucose")
par(mfrow=c(1,1))


#line plot
plot1 = ggplot(data = na.omit(data1), aes(x = BMI, geom = "blank")) + 
  geom_line(aes(y = ..density..), stat = "density") +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, colour="black", fill="blue") +
  labs(title="BMI") + 
  geom_vline(aes(xintercept=mean(BMI)), color="red", linetype="dashed", size=1)

plot2 = ggplot(data = na.omit(data1), aes(x = `Urine output`, geom = "blank")) + 
  geom_line(aes(y = ..density..), stat = "density") +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, colour="black", fill="blue") +
  labs(title="Urine output") + 
  geom_vline(aes(xintercept=mean(`Urine output`)), color="red", linetype="dashed", size=1)

plot3 = ggplot(data = na.omit(data1), aes(x = Leucocyte, geom = "blank")) + 
  geom_line(aes(y = ..density..), stat = "density") +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, colour="black", fill="blue") +
  labs(title="Leucocyte") + 
  geom_vline(aes(xintercept=mean(Leucocyte)), color="red", linetype="dashed", size=1)

plot4 = ggplot(data = na.omit(data1), aes(x = Platelets, geom = "blank")) + 
  geom_line(aes(y = ..density..), stat = "density") +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, colour="black", fill="blue") +
  labs(title="Platelets") + 
  geom_vline(aes(xintercept=mean(Platelets)), color="red", linetype="dashed", size=1)

grid.arrange(plot1, plot2, plot3, plot4)

#too many variables for correlation plot
#correlation of continuous variables in an decreasing order
z <- cor(na.omit(contVar))
z[lower.tri(z,diag=TRUE)] = NA
z = as.data.frame(as.table(z))
z = z[order(-abs(z$Freq)),]
View(z)


#Bivariate: Continuous Variables

plot1 = ggplot(data = na.omit(data1), aes(y = BMI, fill = outcome)) + geom_boxplot(show.legend = FALSE) + labs(title="BMI against outcome") + facet_grid(.~outcome)

plot2 = ggplot(data = na.omit(data1), aes(y = `Urea nitrogen`, fill = outcome)) + geom_boxplot(show.legend = FALSE) + labs(title="Urea nitrogen against outcome") + facet_grid(.~outcome)

plot3 = ggplot(data = na.omit(data1), aes(y = `heart rate`, fill = outcome)) + geom_boxplot(show.legend = FALSE) + labs(title="Heart rate against outcome") + facet_grid(.~outcome)

grid.arrange(plot1, plot2, plot3, nrow = 1)

#Univariate: Categorical Variables

catVar <- data1
for(var in colnames(catVar)){
  if(class(catVar[[var]]) != "factor"){
    catVar[[var]]=NULL
  }
}
catVar[, group:=NULL]
summary(catVar)

plot1 <- ggplot(na.omit(data1), aes(x = outcome, fill = outcome)) + geom_bar()
plot2 <- ggplot(na.omit(data1), aes(x = gendera, fill = gendera)) + geom_bar()
plot3 <- ggplot(na.omit(data1), aes(x = hypertensive, fill = hypertensive)) + geom_bar()
plot4 <- ggplot(na.omit(data1), aes(x = atrialfibrillation, fill = atrialfibrillation)) + geom_bar()
plot5 <- ggplot(na.omit(data1), aes(x = `CHD with no MI`, fill = `CHD with no MI`)) + geom_bar()
plot6 <- ggplot(na.omit(data1), aes(x = diabetes, fill = diabetes)) + geom_bar()
plot7 <- ggplot(na.omit(data1), aes(x = deficiencyanemias, fill = deficiencyanemias)) + geom_bar()
plot8 <- ggplot(na.omit(data1), aes(x = depression, fill = depression)) + geom_bar()
plot9 <- ggplot(na.omit(data1), aes(x = Hyperlipemia, fill = Hyperlipemia)) + geom_bar()
plot10 <- ggplot(na.omit(data1), aes(x = `Renal failure`, fill = `Renal failure`)) + geom_bar()
plot11 <- ggplot(na.omit(data1), aes(x = COPD, fill = COPD)) + geom_bar()

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, plot11, nrow = 6, ncol = 2)


#Bivariate: Categorical Variables
plot1 <- ggplot(na.omit(data1), aes(x = outcome, fill = gendera)) + geom_bar(position = position_dodge(preserve = "single"))
plot2 <- ggplot(na.omit(data1), aes(x = outcome, fill = hypertensive)) + geom_bar(position = position_dodge(preserve = "single"))
plot3 <- ggplot(na.omit(data1), aes(x = outcome, fill = atrialfibrillation)) + geom_bar(position = position_dodge(preserve = "single"))
plot4 <- ggplot(na.omit(data1), aes(x = outcome, fill = `CHD with no MI`)) + geom_bar(position = position_dodge(preserve = "single"))
plot5 <- ggplot(na.omit(data1), aes(x = outcome, fill = diabetes)) + geom_bar(position = position_dodge(preserve = "single"))
plot6 <- ggplot(na.omit(data1), aes(x = outcome, fill = deficiencyanemias)) + geom_bar(position = position_dodge(preserve = "single"))
plot7 <- ggplot(na.omit(data1), aes(x = outcome, fill = depression)) + geom_bar(position = position_dodge(preserve = "single"))
plot8 <- ggplot(na.omit(data1), aes(x = outcome, fill = Hyperlipemia)) + geom_bar(position = position_dodge(preserve = "single"))
plot9 <- ggplot(na.omit(data1), aes(x = outcome, fill = `Renal failure`)) + geom_bar(position = position_dodge(preserve = "single"))
plot10 <- ggplot(na.omit(data1), aes(x = outcome, fill = COPD)) + geom_bar(position = position_dodge(preserve = "single"))
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, nrow = 5, ncol = 2)



#2a.
#copy of data1
data2 <- data1

#replace all missing values for continuous variables with median
for(var in colnames(data2)){
  if(class(data2[[var]]) != "factor"){
    data2[[var]][is.na(data2[[var]])] <- median(data2[[var]], na.rm = T)
  }
}

#replace all missing values for categorical variables with mode
mode <- function(x) {
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}
for(var in colnames(data2)){
  if(class(data2[[var]]) == "factor"){
    data2[[var]][is.na(data2[[var]])] <- mode(data2[[var]])
  }
}

#check that data2 has no missing value
sum(is.na(data2))


#2b.
#produce a trainset
trainset <- data2[group == 1]

#remove "group" and "ID"
trainset[, c("group", "ID"):=NULL]

#show proportion of died vs alive
PieChart(outcome, data = trainset, hole = 0)


#2c.
#produce a testset
testset <- data2[group == 2]

#remove "group" and "ID"
testset[, c("group", "ID"):=NULL]

#show proportion of died vs alive
PieChart(outcome, data = testset, hole = 0)










#Part B: Analytics and Insight
set.seed(22)

#change all variable names with spaces to that without spaces for Random Forest
names(trainset) <- make.names(names(trainset), unique=TRUE)


#3.
#In multivariable analysis, age, admission systolic blood pressure, admission BUN, admission serum sodium, admission heart rate, nonblack race, and the presence of COPD were independent predictors of in-hospital death, and the outcome variable mortality

gwtg_train <- trainset[,c("outcome", "Systolic.blood.pressure", "Urea.nitrogen", "Blood.sodium", "age", "heart.rate", "COPD")]


#assigning points to each variable
gwtg_train$Systolic.blood.pressure.pts <- cut(gwtg_train$Systolic.blood.pressure, breaks=c(50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,max(gwtg_train$Systolic.blood.pressure)), labels=c(28,26,24,23,21,19,17,15,13,11,9,8,6,4,2,0), include.lowest = TRUE)

gwtg_train$Urea.nitrogen.pts <- cut(gwtg_train$Urea.nitrogen, breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,max(gwtg_train$Urea.nitrogen)), labels=c(0,2,4,6,8,9,11,13,15,17,19,21,23,25,27,28), include.lowest = TRUE)

gwtg_train$Blood.sodium.pts <- cut(gwtg_train$Blood.sodium, breaks=c(0,131,132,133,134,135,136,137,138,139,max(gwtg_train$Blood.sodium)), labels=c(4,3,3,3,2,2,2,1,1,0), include.lowest = TRUE)

gwtg_train$age.pts <- cut(gwtg_train$age, breaks=c(0,20,30,40,50,60,70,80,90,100,110,max(gwtg_train$age)), labels=c(0,3,6,8,11,14,17,19,22,25,28), include.lowest = TRUE)

gwtg_train$heart.rate.pts <- cut(gwtg_train$heart.rate, breaks=c(0,80,85,90,95,100,105,max(gwtg_train$Blood.sodium)), labels=c(0,1,3,4,5,6,8), include.lowest = TRUE)

gwtg_train$COPD.pts <- cut(as.numeric(gwtg_train$COPD), breaks=c(0,1,2), labels=c(0,2), include.lowest = TRUE)

#3 points are assigned to all race values as ICU patients in the data set are assumed to be non-black
gwtg_train$race.pts <- 3

for(var in colnames(gwtg_train)){
  if(var!="outcome")
    gwtg_train[[var]] <- as.numeric(gwtg_train[[var]])
}

#tabulate risk scores
gwtg_train$total_score <- gwtg_train$Systolic.blood.pressure.pts + gwtg_train$Urea.nitrogen.pts + gwtg_train$Blood.sodium.pts + gwtg_train$age.pts + gwtg_train$heart.rate.pts + gwtg_train$COPD.pts + gwtg_train$race.pts

#predicted outcomes using risk scores (>=79 means died => predict outcome = 1)
gwtg_train$predicted.outcome <- as.factor(cut(gwtg_train$total_score, breaks = c(0,79,max(gwtg_train$total_score)), labels = c(0,1), include.lowest = TRUE))

#confusion matrix
gwtg_train_cm <- table(trainset$outcome, gwtg_train$predicted.outcome)
gwtg_train_cm


#4
#predictors for outcome and the outcome variable
nomo_train <- trainset[,c("outcome", "Anion.gap", "Lactic.acid", "Blood.calcium", "Urea.nitrogen", "Renal.failure", "Diastolic.blood.pressure")]

#calculating points for Anion.gap
#equation: points = 40/9*(Anion.gap) - 560/9
nomo_train$Anion.gap.pts <-  ((40/9)*(nomo_train$Anion.gap) - (560/9))

#calculating points for Lactic.acid
#equation: points = 11.25*(Lactic.acid) - 16.875
nomo_train$Lactic.acid.pts <-  (11.25*(nomo_train$Lactic.acid) - 16.875)

#calculating points for Blood.calcium
#equation: points = -20.625*(Blood.calcium) + 226.875
nomo_train$Blood.calcium.pts <-  (-20.625*(nomo_train$Blood.calcium) + 226.875)

#calculating points for Urea.nitrogen
#equation: points =  19/36*(Urea.nitrogen)
nomo_train$Urea.nitrogen.pts <-  ((19/36)*(nomo_train$Urea.nitrogen))

#calculating points for Renal.failure
#categorical: 1 (Yes) = 0, 0 (No) = 34.5
for(i in 1:nrow(nomo_train)){
  if(nomo_train$Renal.failure[i] == 1){
    nomo_train$Renal.failure.pts[i] = 0
  }
  else if(nomo_train$Renal.failure[i] == 0){
    nomo_train$Renal.failure.pts[i] = 34.5
  }
}

#calculating points for diastolic blood pressure
#equation:  points = -10/9(Diastolic.blood.pressure) + 1100/9
nomo_train$Diastolic.blood.pressure.pts <-  ((-10/9)*(nomo_train$Diastolic.blood.pressure) + (1100/9))

for(var in colnames(nomo_train)){
  if(var!="outcome")
    nomo_train[[var]] <- as.numeric(nomo_train[[var]])
}

#calculating Nomogram Score
nomo_train$total_score <- (nomo_train$Anion.gap.pts + nomo_train$Lactic.acid.pts + nomo_train$Blood.calcium.pts + nomo_train$Urea.nitrogen.pts + nomo_train$Renal.failure.pts + nomo_train$Diastolic.blood.pressure.pts)

#predicted outcome
for (i in 1:nrow(nomo_train)){
  if(nomo_train$total_score[i] >= 205){
    nomo_train$predicted.outcome[i] = 1
  }
  else{
    nomo_train$predicted.outcome[i] = 0
  }
}

#confusion matrix
nomo_train_cm <- table(trainset$outcome, nomo_train$predicted.outcome)
nomo_train_cm


#5

#create an empty table for trainset errors
error_table_train = data.table("Model" = c("Logistic Reg(BE)", "Random Forest", "GWTG", "Nomogram"), "FPR" = rep(0, 4), "FNR" = rep(0, 4), "Err" = rep(0, 4))

#GWTG
#adding FPR, FNR and Err into table
error_table_train[Model=="GWTG"]$FPR <- gwtg_train_cm[1,2]/sum(gwtg_train_cm[1,])
error_table_train[Model=="GWTG"]$FNR <- gwtg_train_cm[2,1]/sum(gwtg_train_cm[2,])
error_table_train[Model=="GWTG"]$Err <- (gwtg_train_cm[1,2] +gwtg_train_cm[2,1])/nrow(trainset)
error_table_train

#Nomogram
#adding FPR, FNR and Err into table
error_table_train[Model=="Nomogram"]$FPR <- nomo_train_cm[1,2]/sum(nomo_train_cm[1,])
error_table_train[Model=="Nomogram"]$FNR <- nomo_train_cm[2,1]/sum(nomo_train_cm[2,])
error_table_train[Model=="Nomogram"]$Err <- (nomo_train_cm[1,2] +nomo_train_cm[2,1])/nrow(trainset)
error_table_train



# Logistic Regression Model
set.seed(22)

#train the logistic regression model
logR_train <- glm(outcome ~ ., family = binomial, trainset)
summary(logR_train)

#backward elimination
logR_train_be = step(logR_train) 
formula(logR_train_be)
summary(logR_train_be)

#predicted outcome
threshold <- 0.5
logR_train_predict <- predict(logR_train_be, type = "response", newdata = trainset)
logR_train_yhat <- ifelse(logR_train_predict > threshold, 1, 0)

#confusion matrix
logR_train_cm <- table(trainset$outcome, logR_train_yhat)
logR_train_cm

#adding FPR, FNR and Err into table
error_table_train[Model=="Logistic Reg(BE)"]$FPR <- logR_train_cm[1,2]/sum(logR_train_cm[1,])
error_table_train[Model=="Logistic Reg(BE)"]$FNR <- logR_train_cm[2,1]/sum(logR_train_cm[2,])
error_table_train[Model=="Logistic Reg(BE)"]$Err <- (logR_train_cm[1,2] +logR_train_cm[2,1])/nrow(trainset)
error_table_train



# Random Forest Model
set.seed(22)
rf_train <- randomForest(outcome ~ . , data=trainset, importance=T)

#predicted outcome
rf_train_yhat <- predict(rf_train, newdata = trainset)

#confusion matrix
rf_train_cm <- table(trainset$outcome, rf_train_yhat)
rf_train_cm

#adding FPR, FNR and Err into table
error_table_train[Model=="Random Forest"]$FPR <- rf_train_cm[1,2]/sum(rf_train_cm[1,])
error_table_train[Model=="Random Forest"]$FNR <- rf_train_cm[2,1]/sum(rf_train_cm[2,])
error_table_train[Model=="Random Forest"]$Err <- (rf_train_cm[1,2] +rf_train_cm[2,1])/nrow(trainset)
View(error_table_train)







#6.

error_table_test = data.table("Model" = c("Logistic Reg(BE)", "Random Forest", "GWTG", "Nomogram"), "FPR" = rep(0, 4), "FNR" = rep(0, 4), "Err" = rep(0, 4))

#GWTG
names(testset) <- make.names(names(testset), unique=TRUE)

gwtg_test <- testset[,c("outcome", "Systolic.blood.pressure", "Urea.nitrogen", "Blood.sodium", "age", "heart.rate", "COPD")]

gwtg_test$BP.pts <- cut(gwtg_test$Systolic.blood.pressure, breaks=c(50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,max(gwtg_test$Systolic.blood.pressure)), labels=c(28,26,24,23,21,19,17,15,13,11,9,8,6,4,2,0), include.lowest = TRUE)

gwtg_test$BUN.pts <- cut(gwtg_test$Urea.nitrogen, breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,max(gwtg_test$Urea.nitrogen)), labels=c(0,2,4,6,8,9,11,13,15,17,19,21,23,25,27,28), include.lowest = TRUE)

gwtg_test$Sodium.pts <- cut(gwtg_test$Blood.sodium, breaks=c(0,131,132,133,134,135,136,137,138,139,max(gwtg_test$Blood.sodium)), labels=c(4,3,3,3,2,2,2,1,1,0), include.lowest = TRUE)

gwtg_test$age.pts <- cut(gwtg_test$age, breaks=c(0,20,30,40,50,60,70,80,90,100,110,max(gwtg_test$age)), labels=c(0,3,6,8,11,14,17,19,22,25,28), include.lowest = TRUE)

gwtg_test$hr.pts <- cut(gwtg_test$heart.rate, breaks=c(0,80,85,90,95,100,105,max(gwtg_test$Blood.sodium)), labels=c(0,1,3,4,5,6,8), include.lowest = TRUE)

gwtg_test$race.pts <- 3

gwtg_test$COPD.pts <- cut(as.numeric(gwtg_test$COPD), breaks=c(0,1,2), labels=c(0,2), include.lowest = TRUE)

for(var in colnames(gwtg_test)){
  if(var!="outcome")
    gwtg_test[[var]] <- as.numeric(gwtg_test[[var]])
}

gwtg_test$hf_score <- gwtg_test$BP.pts + gwtg_test$BUN.pts + gwtg_test$Sodium.pts + gwtg_test$age.pts + gwtg_test$hr.pts + gwtg_test$COPD.pts + gwtg_test$race.pts

gwtg_test$predict.outcome <- as.factor(cut(gwtg_test$hf_score, breaks = c(0,79,max(gwtg_test$hf_score)), labels = c(0,1), include.lowest = TRUE))

gwtg_test_cm <- table(testset$outcome, gwtg_test$predict.outcome)
gwtg_test_cm

error_table_test[Model=="GWTG"]$FPR <- gwtg_test_cm[1,2]/sum(gwtg_test_cm[1,])
error_table_test[Model=="GWTG"]$FNR <- gwtg_test_cm[2,1]/sum(gwtg_test_cm[2,])
error_table_test[Model=="GWTG"]$Err <- (gwtg_test_cm[1,2] +gwtg_test_cm[2,1])/nrow(testset)
error_table_test



#nomogram
nomo_test <- testset[,c("outcome", "Anion.gap", "Lactic.acid", "Blood.calcium", "Urea.nitrogen", "Renal.failure", "Diastolic.blood.pressure")]
nomo_test$Anion.gap.pts <-  ((40/9)*(nomo_test$Anion.gap) - (560/9))
nomo_test$Lactic.acid.pts <-  (11.25*(nomo_test$Lactic.acid) - 16.875)
nomo_test$Blood.calcium.pts <-  (-20.625*(nomo_test$Blood.calcium) + 226.875)
nomo_test$Urea.nitrogen.pts <-  ((19/36)*(nomo_test$Urea.nitrogen))
for(i in 1:nrow(nomo_test)){
  if(nomo_test$Renal.failure[i] == 1){
    nomo_test$Renal.failure.pts[i] = 0
  }
  else if(nomo_test$Renal.failure[i] == 0){
    nomo_test$Renal.failure.pts[i] = 34.5
  }
}
nomo_test$Diastolic.blood.pressure.pts <-  ((-10/9)*(nomo_test$Diastolic.blood.pressure) + (1100/9))

for(var in colnames(nomo_test)){
  if(var!="outcome")
    nomo_test[[var]] <- as.numeric(nomo_test[[var]])
}

nomo_test$total_score <- (nomo_test$Anion.gap.pts + nomo_test$Lactic.acid.pts + nomo_test$Blood.calcium.pts + nomo_test$Urea.nitrogen.pts + nomo_test$Renal.failure.pts + nomo_test$Diastolic.blood.pressure.pts)

for (i in 1:nrow(nomo_test)){
  if(nomo_test$total_score[i] >= 205){
    nomo_test$predicted.outcome[i] = 1
  }
  else{
    nomo_test$predicted.outcome[i] = 0
  }
}

#confusion matrix
nomo_test_cm <- table(testset$outcome, nomo_test$predicted.outcome)
nomo_test_cm


#adding FPR, FNR and Err into table
error_table_test[Model=="Nomogram"]$FPR <- nomo_test_cm[1,2]/sum(nomo_test_cm[1,])
error_table_test[Model=="Nomogram"]$FNR <- nomo_test_cm[2,1]/sum(nomo_test_cm[2,])
error_table_test[Model=="Nomogram"]$Err <- (nomo_test_cm[1,2] +nomo_test_cm[2,1])/nrow(testset)
error_table_test
error_table_test


# Logistic Regression Model
threshold <- 0.5
logR_test_predict <- predict(logR_train_be, type = "response", newdata = testset)
logR_test_yhat <- ifelse(logR_test_predict > threshold, 1, 0)
logR_test_cm <- table(testset$outcome, logR_test_yhat)
logR_test_cm
error_table_test[Model=="Logistic Reg(BE)"]$FPR <- logR_test_cm[1,2]/sum(logR_test_cm[1,])
error_table_test[Model=="Logistic Reg(BE)"]$FNR <- logR_test_cm[2,1]/sum(logR_test_cm[2,])
error_table_test[Model=="Logistic Reg(BE)"]$Err <- (logR_test_cm[1,2] +logR_test_cm[2,1])/nrow(testset)
error_table_test


# Random Forest Model
set.seed(22)
rf_test_yhat <- predict(rf_train, newdata = testset)
rf_test_cm <- table(testset$outcome, rf_test_yhat)
rf_test_cm
error_table_test[Model=="Random Forest"]$FPR <- rf_test_cm[1,2]/sum(rf_test_cm[1,])
error_table_test[Model=="Random Forest"]$FNR <- rf_test_cm[2,1]/sum(rf_test_cm[2,])
error_table_test[Model=="Random Forest"]$Err <- (rf_test_cm[1,2] +rf_test_cm[2,1])/nrow(testset)
View(error_table_test)



#7.

error_table_test1 = data.table("Model" = c("Logistic Reg(BE)", "Random Forest"), "FPR" = rep(0, 2), "FNR" = rep(0, 2), "Err" = rep(0, 2))

# Random sample from majority class outcome = 0 and combine with Default = 1 to form new trainset -----
majority <- trainset[outcome == 0]
minority <- trainset[outcome == 1]

# Randomly sample the row numbers to be in trainset. Same sample size as minority cases.
set.seed(22)
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority))

# Subset the original trainset based on randomly chosen row numbers. 
majority.chosen <- majority[chosen]

# Combine two data tables by appending the rows 
trainset.bal <- rbind(majority.chosen, minority) 
summary(trainset.bal)

## Check trainset is balanced.
#show proportion of died vs alive
PieChart(outcome, data = trainset.bal, hole = 0)

# Logistic Regression Model
set.seed(22)
logR_train1 <- glm(outcome ~ ., family = binomial, trainset.bal)
summary(logR_train1)
logR_train_be1 = step(logR_train1) 
formula(logR_train_be1)
summary(logR_train_be1)

threshold <- 0.5
logR_test_predict1 <- predict(logR_train_be1, type = "response", newdata = testset)
logR_test_yhat1 <- ifelse(logR_test_predict1 > threshold, 1, 0)
logR_test_cm1 <- table(testset$outcome, logR_test_yhat1)
logR_test_cm1
error_table_test1[Model=="Logistic Reg(BE)"]$FPR <- logR_test_cm1[1,2]/sum(logR_test_cm1[1,])
error_table_test1[Model=="Logistic Reg(BE)"]$FNR <- logR_test_cm1[2,1]/sum(logR_test_cm1[2,])
error_table_test1[Model=="Logistic Reg(BE)"]$Err <- (logR_test_cm1[1,2] +logR_test_cm1[2,1])/nrow(testset)
error_table_test1

# Random Forest Model
set.seed(22)
rf_train1 <- randomForest(outcome ~ . , data=trainset.bal, importance=T)
rf_train1

rf_test_yhat1 <- predict(rf_train1, newdata = testset)
rf_test_cm1 <- table(testset$outcome, rf_test_yhat1)
rf_test_cm1
error_table_test1[Model=="Random Forest"]$FPR <- rf_test_cm1[1,2]/sum(rf_test_cm1[1,])
error_table_test1[Model=="Random Forest"]$FNR <- rf_test_cm1[2,1]/sum(rf_test_cm1[2,])
error_table_test1[Model=="Random Forest"]$Err <- (rf_test_cm1[1,2] +rf_test_cm1[2,1])/nrow(testset)
View(error_table_test1)


#8.

error_table_test2 = data.table("Model" = c("RF VarImpt into Logistic Reg"), "FPR" = 0, "FNR" = 0, "Err" = 0)

#arrange the variance importance from random forest in decreasing order to and get the first 20 variables
var.impt <- as.data.frame(importance(rf_train1))
var.impt <- data.frame(Names = rownames(var.impt), MeanDecreaseAccuracy = var.impt$MeanDecreaseAccuracy)
top20.var.impt <- var.impt[order(var.impt$MeanDecreaseAccuracy, decreasing = T),][1:20,]
top20.var.impt

#include the top 20 variable importance from balanced trainset
logR_trainset <- trainset.bal %>% select(top20.var.impt$Names, outcome)

#logistic regression without backward elimination
logR_train_20 <- glm(outcome ~ ., family = binomial, logR_trainset)
summary(logR_train_20)

#predicted outcome
threshold <- 0.5
logR_test_predict_20 <- predict(logR_train_20, type = "response", newdata = testset)
logR_test_yhat_20 <- ifelse(logR_test_predict_20 > threshold, 1, 0)
logR_test_cm_20 <- table(testset$outcome, logR_test_yhat_20)
logR_test_cm_20

#testset results
error_table_test2[Model=="RF VarImpt into Logistic Reg"]$FPR <- logR_test_cm_20[1,2]/sum(logR_test_cm_20[1,])
error_table_test2[Model=="RF VarImpt into Logistic Reg"]$FNR <- logR_test_cm_20[2,1]/sum(logR_test_cm_20[2,])
error_table_test2[Model=="RF VarImpt into Logistic Reg"]$Err <- (logR_test_cm_20[1,2] +logR_test_cm_20[2,1])/nrow(testset)

#append table
View(rbind(error_table_test1, error_table_test2))

