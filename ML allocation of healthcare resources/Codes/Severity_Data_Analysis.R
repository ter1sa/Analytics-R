#Data Analysis for Severity Dataset

setwd("/Users/chanyixuan/Documents/NTU/Y2S2/BC2407 Analytics II/Group Project/BC2407_Sem4_Team8/Data Sets")

library(data.table)
library(nnet)
library(caTools)
library(randomForest)
library(neuralnet)
library(NeuralNetTools)


# DATA PREPARATION -------------------------------------------------------------

severity_data <- fread("Severity_Data_Cleaned.csv", stringsAsFactors = T)

severity_data$Bodypain <- as.factor(severity_data$Bodypain)
severity_data$Runny_nose <- as.factor(severity_data$Runny_nose)
severity_data$Difficulty_in_breathing <- as.factor(severity_data$Difficulty_in_breathing)
severity_data$Nasal_congestion <- as.factor(severity_data$Nasal_congestion)
severity_data$Sore_throat <- as.factor(severity_data$Sore_throat)

summary(severity_data)

#Split into 70-30 Train-Test set
set.seed(2004)
train <- sample.split(Y = severity_data$Severity, SplitRatio = 0.7)
sev_train <- subset(severity_data, train == T)
sev_test <- subset(severity_data, train == F)

# MULTINOMIAL LOGISTIC REGRESSION MODEL ----------------------------------------

set.seed(2004)

#Creating Results Table
#Mild Error: supposed to be mild, but wrongly classified as moderate/severe
#Moderate Error: supposed to be moderate, but wrongly classified as mild/severe
#Severe Error: supposed to be severe, but wrongly classified as mild/moderate
sev_logreg_results_tab <- matrix(data = NA, nrow=2, ncol=4)
rownames(sev_logreg_results_tab) <- c('Trainset', 'Testset')
colnames(sev_logreg_results_tab) <- c('Mild Error', 'Moderate Error','Severe Error', 'Total Error')


#Setting Reference Level (since there's more than 2 categories in y)
sev_train$Severity <- relevel(sev_train$Severity, ref = "Mild")

#Generating full logreg model
#source: https://www.r-bloggers.com/2020/05/multinomial-logistic-regression-with-r/
sev_logreg_full <- multinom(Severity ~ ., data = sev_train)
summary(sev_logreg_full)

#Generating logreg model with backward elimination
sev_logreg_wBE <- step(sev_logreg_full,direction="backward",trace=FALSE)
summary(sev_logreg_wBE)

#Generating t-value to find variable importance
sev_logreg_tval <- t(data.frame(pt(abs(summary(sev_logreg_wBE)$coefficients / summary(sev_logreg_wBE)$standard.errors), df=nrow(sev_train)-10, lower=FALSE)))
sev_logreg_tval <- data.frame(cbind(sev_logreg_tval, sev_logreg_tval[,1]+sev_logreg_tval[,2]))
sev_logreg_varimpt <- sev_logreg_tval[order(sev_logreg_tval$V3),]

#Generating trainset error matrix
sev_logreg_train_predict <- predict(sev_logreg_wBE, newdata =sev_train, type = 'class')
sev_logreg_train_cmatrix <- table(sev_train$Severity, sev_logreg_train_predict)

#Adding trainset errors into table
sev_logreg_results_tab[1,1] <- sum(sev_logreg_train_cmatrix[1,2:3])/sum(sev_logreg_train_cmatrix[1,])
sev_logreg_results_tab[1,2] <- sum(sev_logreg_train_cmatrix[2,c(1,3)])/sum(sev_logreg_train_cmatrix[2,])
sev_logreg_results_tab[1,3] <- sum(sev_logreg_train_cmatrix[3,1:2])/sum(sev_logreg_train_cmatrix[3,])
sev_logreg_results_tab[1,4] <- (sum(sev_logreg_train_cmatrix[1,2:3]) + sum(sev_logreg_train_cmatrix[2,c(1,3)]) + sum(sev_logreg_train_cmatrix[3,1:2]))/nrow(sev_train)

#Generating testset error matrix
sev_logreg_test_predict <- predict(sev_logreg_wBE, newdata =sev_test, type = 'class')
sev_logreg_test_cmatrix <- table(sev_test$Severity, sev_logreg_test_predict)

#Adding testset errors into table
sev_logreg_results_tab[2,1] <- sum(sev_logreg_test_cmatrix[1,2:3])/sum(sev_logreg_test_cmatrix[1,])
sev_logreg_results_tab[2,2] <- sum(sev_logreg_test_cmatrix[2,c(1,3)])/sum(sev_logreg_test_cmatrix[2,])
sev_logreg_results_tab[2,3] <- sum(sev_logreg_test_cmatrix[3,1:2])/sum(sev_logreg_test_cmatrix[3,])
sev_logreg_results_tab[2,4] <- (sum(sev_logreg_test_cmatrix[1,2:3]) + sum(sev_logreg_test_cmatrix[2,c(1,3)]) + sum(sev_logreg_test_cmatrix[3,1:2]))/nrow(sev_test)


# RANDOM FOREST MODEL ----------------------------------------------------------

set.seed(2004)

#Creating Results Table
#Mild Error: supposed to be mild, but wrongly classified as moderate/severe
#Moderate Error: supposed to be moderate, but wrongly classified as mild/severe
#Severe Error: supposed to be severe, but wrongly classified as mild/moderate
sev_rf_results_tab <- matrix(data = NA, nrow=2, ncol=4)
rownames(sev_rf_results_tab) <- c('Trainset', 'Testset')
colnames(sev_rf_results_tab) <- c('Mild Error', 'Moderate Error','Severe Error', 'Total Error')

#Generating Random Forest Model
severity_rf <- randomForest(Severity ~ . , data = sev_train, 
                            na.action = na.omit, 
                            importance = T)

#Generating variable importance using mean decrease accuracy
severity_rf_varimpt <- severity_rf$importance[order(-severity_rf$importance[,4]),]
severity_rf_varimpt

#Checking if OOB error has stabilised when default ntree=500
plot(severity_rf $err.rate[,1])
#plot shows OOB error has indeed stabilised after ntree=250

#Generating trainset confusion matrix for Random Forest model
sev_rf_train_predict <- predict(severity_rf, newdata=sev_train, type = 'class')
sev_rf_train_cmatrix <- table(sev_train$Severity, sev_rf_train_predict)

#Adding trainset errors into table
sev_rf_results_tab[1,1] <- sum(sev_rf_train_cmatrix[1,2:3])/sum(sev_rf_train_cmatrix[1,])
sev_rf_results_tab[1,2] <- sum(sev_rf_train_cmatrix[2,c(1,3)])/sum(sev_rf_train_cmatrix[2,])
sev_rf_results_tab[1,3] <- sum(sev_rf_train_cmatrix[3,1:2])/sum(sev_rf_train_cmatrix[3,])
sev_rf_results_tab[1,4] <- (sum(sev_rf_train_cmatrix[1,2:3]) + sum(sev_rf_train_cmatrix[2,c(1,3)]) + sum(sev_rf_train_cmatrix[3,1:2]))/nrow(sev_train)

#Generating testset confusion matrix for Random Forest model
sev_rf_test_predict <- predict(severity_rf, newdata=sev_test, type = 'class')
sev_rf_test_cmatrix <- table(sev_test$Severity, sev_rf_test_predict)

#Adding testset errors into table
sev_rf_results_tab[2,1] <- sum(sev_rf_test_cmatrix[1,2:3])/sum(sev_rf_test_cmatrix[1,])
sev_rf_results_tab[2,2] <- sum(sev_rf_test_cmatrix[2,c(1,3)])/sum(sev_rf_test_cmatrix[2,])
sev_rf_results_tab[2,3] <- sum(sev_rf_test_cmatrix[3,1:2])/sum(sev_rf_test_cmatrix[3,])
sev_rf_results_tab[2,4] <- (sum(sev_rf_test_cmatrix[1,2:3]) + sum(sev_rf_test_cmatrix[2,c(1,3)]) + sum(sev_rf_test_cmatrix[3,1:2]))/nrow(sev_test)



# NEURAL NETWORK MODEL ---------------------------------------------------------

set.seed(2004)

#Neural Network-specific data processing:
#convert all factor to numeric (since neural network only takes in quantitative variables)
  #Trainset:
sev_train_nn <- sev_train
summary(sev_train_nn)

sev_train_nn$Gender <- as.character(sev_train_nn$Gender)
for (i in 1:nrow(sev_train_nn)){
  if (sev_train_nn$Gender[i] =="Male"){
    sev_train_nn$Gender[i] = 0
  }
  else if (sev_train_nn$Gender[i] =="Female"){
    sev_train_nn$Gender[i] = 1
  }
  else if (sev_train_nn$Gender[i] =="Transgender"){
    sev_train_nn$Gender[i] = 2
  }
}
sev_train_nn$Gender <- as.numeric(sev_train_nn$Gender)
sev_train_nn$Bodypain <- as.numeric(as.character(sev_train_nn$Bodypain))
sev_train_nn$Runny_nose <- as.numeric(as.character(sev_train_nn$Runny_nose))
sev_train_nn$Difficulty_in_breathing <- as.numeric(as.character(sev_train_nn$Difficulty_in_breathing))
sev_train_nn$Nasal_congestion <- as.numeric(as.character(sev_train_nn$Nasal_congestion))
sev_train_nn$Sore_throat <- as.numeric(as.character(sev_train_nn$Sore_throat))

sev_train_nn$Contact_with_covid_patient <- as.character(sev_train_nn$Contact_with_covid_patient)
for (i in 1:nrow(sev_train_nn)){
  if (sev_train_nn$Contact_with_covid_patient[i] =="No"){
    sev_train_nn$Contact_with_covid_patient[i] = 0
  }
  else if (sev_train_nn$Contact_with_covid_patient[i] =="Yes"){
    sev_train_nn$Contact_with_covid_patient[i] = 1
  }
  else if (sev_train_nn$Contact_with_covid_patient[i] =="Not known"){
    sev_train_nn$Contact_with_covid_patient[i] = 2
  }
}
sev_train_nn$Contact_with_covid_patient <- as.numeric(sev_train_nn$Contact_with_covid_patient)

summary(sev_train_nn)

  #Testset:
sev_test_nn <- sev_test
summary(sev_test_nn)

sev_test_nn$Gender <- as.character(sev_test_nn$Gender)
for (i in 1:nrow(sev_test_nn)){
  if (sev_test_nn$Gender[i] =="Male"){
    sev_test_nn$Gender[i] = 0
  }
  else if (sev_test_nn$Gender[i] =="Female"){
    sev_test_nn$Gender[i] = 1
  }
  else if (sev_test_nn$Gender[i] =="Transgender"){
    sev_test_nn$Gender[i] = 2
  }
}
sev_test_nn$Gender <- as.numeric(sev_test_nn$Gender)
sev_test_nn$Bodypain <- as.numeric(as.character(sev_test_nn$Bodypain))
sev_test_nn$Runny_nose <- as.numeric(as.character(sev_test_nn$Runny_nose))
sev_test_nn$Difficulty_in_breathing <- as.numeric(as.character(sev_test_nn$Difficulty_in_breathing))
sev_test_nn$Nasal_congestion <- as.numeric(as.character(sev_test_nn$Nasal_congestion))
sev_test_nn$Sore_throat <- as.numeric(as.character(sev_test_nn$Sore_throat))

sev_test_nn$Contact_with_covid_patient <- as.character(sev_test_nn$Contact_with_covid_patient)
for (i in 1:nrow(sev_test_nn)){
  if (sev_test_nn$Contact_with_covid_patient[i] =="No"){
    sev_test_nn$Contact_with_covid_patient[i] = 0
  }
  else if (sev_test_nn$Contact_with_covid_patient[i] =="Yes"){
    sev_test_nn$Contact_with_covid_patient[i] = 1
  }
  else if (sev_test_nn$Contact_with_covid_patient[i] =="Not known"){
    sev_test_nn$Contact_with_covid_patient[i] = 2
  }
}
sev_test_nn$Contact_with_covid_patient <- as.numeric(sev_test_nn$Contact_with_covid_patient)

summary(sev_test_nn)


#Creating Results Table
#Mild Error: supposed to be mild, but wrongly classified as moderate/severe
#Moderate Error: supposed to be moderate, but wrongly classified as mild/severe
#Severe Error: supposed to be severe, but wrongly classified as mild/moderate
sev_nn_results_tab <- matrix(data = NA, nrow=4, ncol=4)
rownames(sev_nn_results_tab) <- c('Trainset (1 layer)', 'Testset (1 layer)', 'Trainset (2 layers)', 'Testset (2 layers)')
colnames(sev_nn_results_tab) <- c('Mild Error', 'Moderate Error','Severe Error', 'Total Error')


#Generating 1 Hidden Layer Neural Network model
#number of neurons in the hidden layer should be 2/3 size of input layer + size of output layer
#source: https://medium.com/geekculture/introduction-to-neural-network-2f8b8221fbd3
sev_nn_1ly <- neuralnet(Severity ~ ., data = sev_train_nn, hidden = round((2/3)*(ncol(sev_test)-1) + 3), err.fct="ce", linear.output=FALSE)
plot(sev_nn_1ly)

#Generating trainset confusion matrix for 1 Hidden Layer Neural Network model
sev_nn_1ly_train_predict <- predict(sev_nn_1ly, newdata=sev_train_nn, type = 'class')
sev_nn_1ly_train_predict_df <- data.frame(sev_nn_1ly_train_predict)
for (i in 1: nrow(sev_nn_1ly_train_predict_df)){
  if (sev_nn_1ly_train_predict_df[i,1] >= 0.5){
    sev_nn_1ly_train_predict_df$outcome[i] = 'Mild'
  }
  else if (sev_nn_1ly_train_predict_df[i,2] >= 0.5){
    sev_nn_1ly_train_predict_df$outcome[i] = 'Moderate'
  }
  else if (sev_nn_1ly_train_predict_df[i,3] >= 0.5){
    sev_nn_1ly_train_predict_df$outcome[i] = 'Severe'
  }
}
sev_nn_1ly_train_cmatrix <- table(as.factor(sev_train_nn$Severity), as.factor(sev_nn_1ly_train_predict_df$outcome))

#Adding trainset errors into table
sev_nn_results_tab[1,1] <- sum(sev_nn_1ly_train_cmatrix[1,2:3])/sum(sev_nn_1ly_train_cmatrix[1,])
sev_nn_results_tab[1,2] <- sum(sev_nn_1ly_train_cmatrix[2,c(1,3)])/sum(sev_nn_1ly_train_cmatrix[2,])
sev_nn_results_tab[1,3] <- sum(sev_nn_1ly_train_cmatrix[3,1:2])/sum(sev_nn_1ly_train_cmatrix[3,])
sev_nn_results_tab[1,4] <- (sum(sev_nn_1ly_train_cmatrix[1,2:3]) + sum(sev_nn_1ly_train_cmatrix[2,c(1,3)]) + sum(sev_nn_1ly_train_cmatrix[3,1:2]))/nrow(sev_train_nn)

#Generating testset confusion matrix for 1 Hidden Layer Neural Network model
sev_nn_1ly_test_predict <- predict(sev_nn_1ly, newdata=sev_test_nn, type = 'class')
sev_nn_1ly_test_predict_df <- data.frame(sev_nn_1ly_test_predict)
for (i in 1: nrow(sev_nn_1ly_test_predict_df)){
  if (sev_nn_1ly_test_predict_df[i,1] >= 0.5){
    sev_nn_1ly_test_predict_df$outcome[i] = 'Mild'
  }
  else if (sev_nn_1ly_test_predict_df[i,2] >= 0.5){
    sev_nn_1ly_test_predict_df$outcome[i] = 'Moderate'
  }
  else if (sev_nn_1ly_test_predict_df[i,3] >= 0.5){
    sev_nn_1ly_test_predict_df$outcome[i] = 'Severe'
  }
}
sev_nn_1ly_test_cmatrix <- table(as.factor(sev_test_nn$Severity), as.factor(sev_nn_1ly_test_predict_df$outcome))

#Adding trainset errors into table
sev_nn_results_tab[2,1] <- sum(sev_nn_1ly_test_cmatrix[1,2:3])/sum(sev_nn_1ly_test_cmatrix[1,])
sev_nn_results_tab[2,2] <- sum(sev_nn_1ly_test_cmatrix[2,c(1,3)])/sum(sev_nn_1ly_test_cmatrix[2,])
sev_nn_results_tab[2,3] <- sum(sev_nn_1ly_test_cmatrix[3,1:2])/sum(sev_nn_1ly_test_cmatrix[3,])
sev_nn_results_tab[2,4] <- (sum(sev_nn_1ly_test_cmatrix[1,2:3]) + sum(sev_nn_1ly_test_cmatrix[2,c(1,3)]) + sum(sev_nn_1ly_test_cmatrix[3,1:2]))/nrow(sev_test_nn)


#Generating 2 Hidden Layer Neural Network model
#number of neurons in the hidden layer should be 2/3 size of input layer + size of output layer
#source: https://medium.com/geekculture/introduction-to-neural-network-2f8b8221fbd3
sev_nn_2ly <- neuralnet(Severity ~ ., data = sev_train_nn, hidden = c((round((2/3)*(ncol(sev_test)-1) + 3)),(round((2/3)*(ncol(sev_test)-1) + 3))) , err.fct="ce", linear.output=FALSE)
plot(sev_nn_2ly)

#Generating trainset confusion matrix for 2 Hidden Layer Neural Network model
sev_nn_2ly_train_predict <- predict(sev_nn_2ly, newdata=sev_train_nn, type = 'class')
sev_nn_2ly_train_predict_df <- data.frame(sev_nn_2ly_train_predict)
for (i in 1: nrow(sev_nn_2ly_train_predict_df)){
  if (sev_nn_2ly_train_predict_df[i,1] >= 0.5){
    sev_nn_2ly_train_predict_df$outcome[i] = 'Mild'
  }
  else if (sev_nn_2ly_train_predict_df[i,2] >= 0.5){
    sev_nn_2ly_train_predict_df$outcome[i] = 'Moderate'
  }
  else if (sev_nn_2ly_train_predict_df[i,3] >= 0.5){
    sev_nn_2ly_train_predict_df$outcome[i] = 'Severe'
  }
}
sev_nn_2ly_train_cmatrix <- table(as.factor(sev_train_nn$Severity), as.factor(sev_nn_2ly_train_predict_df$outcome))

#Adding trainset errors into table
sev_nn_results_tab[3,1] <- sum(sev_nn_2ly_train_cmatrix[1,2:3])/sum(sev_nn_2ly_train_cmatrix[1,])
sev_nn_results_tab[3,2] <- sum(sev_nn_2ly_train_cmatrix[2,c(1,3)])/sum(sev_nn_2ly_train_cmatrix[2,])
sev_nn_results_tab[3,3] <- sum(sev_nn_2ly_train_cmatrix[3,1:2])/sum(sev_nn_2ly_train_cmatrix[3,])
sev_nn_results_tab[3,4] <- (sum(sev_nn_2ly_train_cmatrix[1,2:3]) + sum(sev_nn_2ly_train_cmatrix[2,c(1,3)]) + sum(sev_nn_2ly_train_cmatrix[3,1:2]))/nrow(sev_train_nn)

#Generating testset confusion matrix for 2 Hidden Layer Neural Network model
sev_nn_2ly_test_predict <- predict(sev_nn_2ly, newdata=sev_test_nn, type = 'class')
sev_nn_2ly_test_predict_df <- data.frame(sev_nn_2ly_test_predict)
for (i in 1: nrow(sev_nn_2ly_test_predict_df)){
  if (sev_nn_2ly_test_predict_df[i,1] >= 0.5){
    sev_nn_2ly_test_predict_df$outcome[i] = 'Mild'
  }
  else if (sev_nn_2ly_test_predict_df[i,2] >= 0.5){
    sev_nn_2ly_test_predict_df$outcome[i] = 'Moderate'
  }
  else if (sev_nn_2ly_test_predict_df[i,3] >= 0.5){
    sev_nn_2ly_test_predict_df$outcome[i] = 'Severe'
  }
}
sev_nn_2ly_test_cmatrix <- table(as.factor(sev_test_nn$Severity), as.factor(sev_nn_2ly_test_predict_df$outcome))

#Adding testset errors into table
sev_nn_results_tab[4,1] <- sum(sev_nn_2ly_test_cmatrix[1,2:3])/sum(sev_nn_2ly_test_cmatrix[1,])
sev_nn_results_tab[4,2] <- sum(sev_nn_2ly_test_cmatrix[2,c(1,3)])/sum(sev_nn_2ly_test_cmatrix[2,])
sev_nn_results_tab[4,3] <- sum(sev_nn_2ly_test_cmatrix[3,1:2])/sum(sev_nn_2ly_test_cmatrix[3,])
sev_nn_results_tab[4,4] <- (sum(sev_nn_2ly_test_cmatrix[1,2:3]) + sum(sev_nn_2ly_test_cmatrix[2,c(1,3)]) + sum(sev_nn_2ly_test_cmatrix[3,1:2]))/nrow(sev_test_nn)

#since 1 Hidden Level Neural Network produced a model with a lower total error, it would be adopted over the 2 Hidden Layer model. 


# OVERALL ERROR TABLE ----------------------------------------------------------

#Creating overall error table
sev_error_tab <- matrix(data = NA, nrow=6, ncol=4)
rownames(sev_error_tab) <- c('Logistic Regression Train', 'Logistic Regression Test', 'Random Forest Train', 'Random Forest Test', 'Neural Network (1 lvl) Train', 'Neural Network (1 lvl) Test')
colnames(sev_error_tab) <- c('Mild Error', 'Moderate Error','Severe Error', 'Total Error')

#Adding errors from the 3 models into the table
sev_error_tab[1:2,]<- sev_logreg_results_tab
sev_error_tab[3:4,]<- sev_rf_results_tab
sev_error_tab[5:6,]<- sev_nn_results_tab[1:2,]



# RANDOM CODES------
severity_cart <- rpart(Severity ~ ., data = severity_data, method = 'class', control = rpart.control(minsplit = 10, cp = 0))

rpart.plot(severity_cart, box.palette = list("green","yellow","red"))

severity_mild <- with(severity_data, severity_data[(Severity== "Mild"), ])
severity_moderate <- with(severity_data, severity_data[(Severity== "Moderate"), ])
severity_severe <- with(severity_data, severity_data[(Severity== "Severe"), ])

summary(severity_mild)
summary(severity_moderate)
summary(severity_severe)

