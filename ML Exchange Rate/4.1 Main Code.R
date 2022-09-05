# ==============================================================================================================
library(readxl)
library(data.table)
library(ggplot2)
library(plotly)
library(zoo)
library(corrplot)
library(corrr)
library(car)
library(caTools)
library(rpart)
library(rpart.plot)
library(Metrics)

#Set working directory
setwd("")

#Function that outputs the train-test split RMSE, which we can use to evaluate the different models
#Run the whole function first in order to use it later on
traintesterror <- function(x, data1) {
  set.seed(1234)
  
  train <- sample.split(Y = data1$`Exchange rate LCU:US$ (av)`, SplitRatio = 0.7)
  trainset <- subset(data1, train == T)
  testset <- subset(data1, train == F)
  
  temp <- update(x, .~., data = trainset)
  
  train.RMSE <- sqrt(mean(residuals(temp)^2))
  
  predict.test <- predict(temp, newdata = testset)
  testset.residuals <- testset$`Exchange rate LCU:US$ (av)` - predict.test
  # Testset Errors
  test.RMSE <- sqrt(mean(testset.residuals^2))
  
  print(sprintf("The trainset RMSE is %f while the testset RMSE is %f", train.RMSE, test.RMSE))
}

# ==============================================================================================================

#CANADA
#input xlsx as data table
canada.dt <- read_excel("3.1 Canada.xlsx")
setDT(canada.dt)

#DATA PREPARATION
#check for values that does not make sense
canada.dt$`Growth in average wages (LCU; % pa)`[1] #"-"
canada.dt$`Private sector credit/GDP`[75] #NA

#set those values as NA
canada.dt <- read_excel("3.1 Canada.xlsx", na = c("NA", "-"))
setDT(canada.dt)

#convert the class type of quarterly dates from character to date
class(canada.dt$Date) #character
canada.dt$Date = as.yearqtr(canada.dt$Date)
canada.dt$Date = as.Date(canada.dt$Date)
class(canada.dt$Date) #date

#remove all data after 2021Q2 as they are forecasted data and not accurate
canada.dt = canada.dt[1:114]

#remove date as it is not numeric
canada1.dt <- canada.dt[,-1]

#we are using average period instead of end period, thus, we will remove `Exchange rate LCU:$ (end-period)` and "Short term interest rate (%; end-period)"
#we will remove `M1 Money supply (LCU)` and `Stock of money M2 (LCU)` as they are not in USD
#we will remove `Export volume of goods and services (% change pa)` and `Import volume of goods and services (% change pa)` as they are repeated
canada1.dt = canada1.dt[,-c('Exchange rate LCU:$ (end-period)', 'M1 Money supply (LCU)', "Short term interest rate (%; end-period)", 'Stock of money M2 (LCU)', 'Export volume of goods and services (% change pa)', 'Import volume of goods and services (% change pa)')]

#check for any negative values that are unlikely to be negative (e.g. money supply, stock of money, nominal GDP and exchange rate)
canada1.dt[`M1 Money supply (US$)` < 0, .N]
canada1.dt[`Stock of money M2 (US$)` < 0, .N]
canada1.dt[`Nominal GDP (US$)` < 0, .N]
canada1.dt[`Exchange rate LCU:US$ (av)` < 0, .N]

#all 0

#check through the summary to make sure the data make sense
summary(canada1.dt)

#check for number of NA values
sum(is.na(canada1.dt)) #116

#check which columns have NA values
for(var in colnames(canada1.dt)){
  print(var)
  print(sum(is.na(canada1.dt[[var]])))
}

#Export these rows, drop the NA values, and check for correlation with response variable to see if these rows can be dropped
for(var in colnames(canada1.dt)){
  try <- data.table(canada1.dt$`Exchange rate LCU:US$ (av)`, canada1.dt[[var]])
  try <- na.omit(try)
  if(cor(try)[2] < 0.1 && cor(try)[2] > -0.1){
    print(var);
    print(cor(try)[2])
  }
}
#"Private consumption (real % change pa)" -0.0253189
#"Gross fixed investment (% real change pa)" -0.04618729
#"Budget balance (% of GDP)" 0.04624708
#"Consumer prices (% change pa; av)" 0.03643502

#creating data table for cart before dropping NA values
canada2.dt <- canada1.dt

#drop them
canada1.dt <- canada1.dt[, -c("Private consumption (real % change pa)", "Gross fixed investment (% real change pa)", "Budget balance (% of GDP)", "Consumer prices (% change pa; av)")]


#check NA percentage for each column
for(var in colnames(canada1.dt)){
  print(var)
  print(sum(is.na(canada1.dt[[var]]))/114)
}
#"Growth in average wages (LCU; % pa)" 0.3157895
#"Average real wages (% change pa)" 0.3157895
#"Private sector credit/GDP" 0.3859649
#very high >30%

#drop them instead of replacing the rest of NA values with the respective mean of each column
canada1.dt <- canada1.dt[, -c("Growth in average wages (LCU; % pa)", "Average real wages (% change pa)", "Private sector credit/GDP")]

sum(is.na(canada1.dt)) #0

#check for correlation again
for(var in colnames(canada1.dt)){
  try <- data.table(canada1.dt$`Exchange rate LCU:US$ (av)`, canada1.dt[[var]])
  if(cor(try)[2] < 0.1 && cor(try)[2] > -0.1){
    print(var);
    print(cor(try)[2])
  }
}

#LINEAR REGRESSION
c <- lm(`Exchange rate LCU:US$ (av)` ~ .,data = canada1.dt) #Create a model using ALL variables
traintesterror(c,canada1.dt)
#Then remove based on multicollinearity
vif(c)
names(which(vif(c) == max(vif(c))))
max(vif(c))
c.vif <- lm(`Exchange rate LCU:US$ (av)` ~ .-`Stock of money M2 (US$)`, data = canada1.dt)
vif(c.vif)
names(which(vif(c.vif) == max(vif(c.vif))))
max(vif(c.vif))
c.vif <- update(c.vif, .~.-`Goods: imports (US$)`)
vif(c.vif)
names(which(vif(c.vif) == max(vif(c.vif))))
max(vif(c.vif))
c.vif <- update(c.vif, .~.-`Lending interest rate (%)`)
vif(c.vif)
names(which(vif(c.vif) == max(vif(c.vif))))
max(vif(c.vif))
c.vif <- update(c.vif, .~.-`Nominal GDP (US$)`)
vif(c.vif)
names(which(vif(c.vif) == max(vif(c.vif))))
max(vif(c.vif))
c.vif <- update(c.vif, .~.-`Deposit interest rate (%)`)
vif(c.vif)
names(which(vif(c.vif) == max(vif(c.vif))))
max(vif(c.vif))
c.vif <- update(c.vif, .~.-`Long-term bond yield (%)`)
vif(c.vif)
names(which(vif(c.vif) == max(vif(c.vif))))
max(vif(c.vif)) 
c.vif <- update(c.vif, .~.-`M1 Money supply (US$)`)
vif(c.vif)
names(which(vif(c.vif) == max(vif(c.vif))))
max(vif(c.vif)) #max vif = 4.393569 < 10 , no need to remove further
summary(c.vif)
traintesterror(c.vif,canada1.dt)
#Then use AIC
c.vif.aic <- step(c.vif)
summary(c.vif.aic) #AIC did not remove any variables after VIF
traintesterror(c.vif.aic,canada1.dt)
par(mfrow = c(2,2))
plot(c.vif.aic)
par(mfrow = c(1,1))


#CART
#split the train and test sets
set.seed(1234)
trainc <- sample.split(Y = canada2.dt$`Exchange rate LCU:US$ (av)`, SplitRatio = 0.7)
trainsetc <- subset(canada2.dt, trainc == T)
testsetc <- subset(canada2.dt, trainc == F)
#create a cart model using the trainset
cartc1 <- rpart(`Exchange rate LCU:US$ (av)` ~ ., data = trainsetc, method = 'anova', control = rpart.control(minsplit = 2, cp = 0)) #create a CART model using all variables
rpart.plot(cartc1, nn= T, main = "Maximal Tree in Canada")
print(cartc1)
printcp(cartc1)
plotcp(cartc1, main = "Subtrees in Canada")
CVerror.cap <- cartc1$cptable[which.min(cartc1$cptable[,"xerror"]), "xerror"] + cartc1$cptable[which.min(cartc1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (cartc1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(cartc1$cptable[i,1] * cartc1$cptable[i-1,1]), 1)
cartc2 <- prune(cartc1, cp = cp.opt)
print(cartc2)
printcp(cartc2, digits = 3)
rpart.plot(cartc2, nn = T, main = "Optimal Tree for Canada Exchange Rates")
#find rmse value for testset data using trainset model
predictc <- predict(cartc2, newdata = testsetc)
predictc
rmse(testsetc$`Exchange rate LCU:US$ (av)`, predictc) #test error = 0.08927858
summary(cartc2)
# M1 Money supply (US$)                     Long-term bond yield (%) 
# 16                                        15 
# Stock of money M2 (US$)                   Goods: exports (US$) 
# 14                                        13 
# Goods: imports (US$)                      Nominal GDP (US$) 
# 12                                        11 
# Public debt  (% of GDP)                   Deposit interest rate (%) 
# 6                                         3 
# Gross fixed investment (% real change pa) Budget balance (% of GDP) 
# 3                                         2 
# Lending interest rate (%)                 Unemployment rate (%) 
# 1                                         1 
# Short term interest rate (%; average)     Private consumption (real % change pa) 
# 1                                         1 
cartc2$variable.importance
# M1 Money supply (US$)                     Long-term bond yield (%) 
# 1.69434873                                1.57397202 
# Stock of money M2 (US$)                   Goods: exports (US$) 
# 1.52110151                                1.35535030 
# Goods: imports (US$)                      Nominal GDP (US$) 
# 1.28381379                                1.22552619 
# Public debt  (% of GDP)                   Deposit interest rate (%) 
# 0.69543073                                0.35788558 
# Gross fixed investment (% real change pa) Budget balance (% of GDP) 
# 0.32734256                                0.24912526 
# Lending interest rate (%)                 Unemployment rate (%) 
# 0.14803019                                0.13470401 
# Short term interest rate (%; average)     Private consumption (real % change pa) 
# 0.11866110                                0.06465458 


#Determining the degree of fluctuation
#first finding the prediction of our exchange rates using our cart model
predictionusingcart = predict(cartc2, newdata = canada2.dt)
#adding in the predictions to the datatable
canada2.dt[,`:=` (prediction = predictionusingcart)]
View(canada2.dt)

#creating a list of the percentage changes
percentagechange = c()
for(var in 2:nrow(canada2.dt)){
  change = abs((canada2.dt[,21][var]-canada2.dt[,21][var-1])/canada2.dt[,21][var-1] * 100)
  percentagechange[var] = change
}
#add the percentage change to the datatable
canada2.dt[,`:=` (percentagedifference = percentagechange)]
#finding the average percentage change across the years
mean(unlist(canada2.dt$percentagedifference[2:nrow(canada2.dt)]))
#2.543745




# ==============================================================================================================

#NETHERLANDS
#input xlsx as data table
netherlands.dt <- read_excel("3.2 Netherlands.xlsx")
setDT(netherlands.dt)

#DATA PREPARATION
#check for values that does not make sense
netherlands.dt$`Money market interest rate (%; end-period)`[1] #"-"
netherlands.dt$`Private sector credit/GDP`[75] #NA

#set those values as NA
netherlands.dt <- read_excel("3.2 Netherlands.xlsx", na = c("NA", "–"))
setDT(netherlands.dt)

#convert the class type of quarterly dates from character to date
class(netherlands.dt$Date) #character
netherlands.dt$Date = as.yearqtr(netherlands.dt$Date)
netherlands.dt$Date = as.Date(netherlands.dt$Date)
class(netherlands.dt$Date) #date

#remove all data after 2021Q2 as they are forecasted data and not accurate
netherlands.dt = netherlands.dt[1:114]

#remove date as it is not numeric
netherlands1.dt <- netherlands.dt[,-1]

#we are using average period instead of end period, thus, we will remove `Exchange rate LCU:$ (end-period)` and `Money market interest rate (%; end-period)`
#we will remove `M1 Money supply (LCU)` and `Stock of money M2 (LCU)` as they are not in USD
#we will remove `Export volume of goods and services (% change pa)` and `Import volume of goods and services (% change pa)` as they are repeated
netherlands1.dt = netherlands1.dt[,-c('Exchange rate LCU:$ (end-period)', 'Money market interest rate (%; end-period)', 'M1 Money supply (LCU)', 'Stock of money M2 (LCU)', 'Export volume of goods and services (% change pa)', 'Import volume of goods and services (% change pa)')]

#check for any negative values that are unlikely to be negative (e.g. money supply, stock of money, nominal GDP and exchange rate)
netherlands1.dt[`M1 Money supply (US$)` < 0, .N]
netherlands1.dt[`Stock of money M2 (US$)` < 0, .N]
netherlands1.dt[`Nominal GDP (US$)` < 0, .N]
netherlands1.dt[`Exchange rate LCU:US$ (av)` < 0, .N]

#all 0

#check through the summary to make sure the data make sense
summary(netherlands1.dt)

#check for number of NA values
sum(is.na(netherlands1.dt)) #182

#creating data table for cart before dropping NA values
netherlands2.dt <- netherlands1.dt

#check which columns have NA values
for(var in colnames(netherlands1.dt)){
  print(var)
  print(sum(is.na(netherlands1.dt[[var]])))
}

#Export these rows, drop the NA values, and check for correlation with response variable to see if these rows can be dropped
for(var in colnames(netherlands1.dt)){
  try <- data.table(netherlands1.dt$`Exchange rate LCU:US$ (av)`, netherlands1.dt[[var]])
  try <- na.omit(try)
  if(cor(try)[2] < 0.1 && cor(try)[2] > -0.1){
    print(var);
    print(cor(try)[2])
  }
}
#"Gross fixed investment (% real change pa)" 0.05237584
#"Long-term bond yield (%)" 0.07641348
#"Average real wages (% change pa)" 0.08773895
#"Private sector credit/GDP" 0.00793913

#drop them
netherlands1.dt <- netherlands1.dt[, -c("Gross fixed investment (% real change pa)", "Long-term bond yield (%)", "Average real wages (% change pa)", "Private sector credit/GDP")]

#check NA percentage for each column
for(var in colnames(netherlands1.dt)){
  print(var)
  print(sum(is.na(netherlands1.dt[[var]]))/114)
}
#"Lending interest rate (%)" 0.3508772
#"Deposit interest rate (%)" 0.3508772
#very high >30%
#"Budget balance (% of GDP)" 0.2192982
#"Public debt  (% of GDP)" 0.254386
#"Goods: exports (US$)" 0.01754386
#"Goods: imports (US$)" 0.01754386
#quite low <30%

#drop them instead of replacing the rest of NA values with the respective mean of each column
netherlands1.dt <- netherlands1.dt[, -c("Lending interest rate (%)", "Deposit interest rate (%)")]

#replace the rest of NA values with the respective mean of each column, don't drop those columns as their correlation > 0.1
for(var in colnames(netherlands1.dt)){
  netherlands1.dt[[var]][is.na(netherlands1.dt[[var]])] <- mean(netherlands1.dt[[var]], na.rm = TRUE)
}

sum(is.na(netherlands1.dt)) #0

#check for correlation again
for(var in colnames(netherlands1.dt)){
  try <- data.table(netherlands1.dt$`Exchange rate LCU:US$ (av)`, netherlands1.dt[[var]])
  if(cor(try)[2] < 0.1 && cor(try)[2] > -0.1){
    print(var);
    print(cor(try)[2])
  }
}


#LINEAR REGRESSION
n <- lm(`Exchange rate LCU:US$ (av)` ~ .,data = netherlands1.dt) #Create a model using ALL variables
traintesterror(n,netherlands1.dt)
#Then remove based on multicollinearity
vif(n)
names(which(vif(n) == max(vif(n))))
max(vif(n))
n.vif <- lm(`Exchange rate LCU:US$ (av)` ~ .-`Goods: exports (US$)`, data = netherlands1.dt)
vif(n.vif)
names(which(vif(n.vif) == max(vif(n.vif))))
max(vif(n.vif))
n.vif <- update(n.vif, .~.-`Stock of money M2 (US$)`)
vif(n.vif)
names(which(vif(n.vif) == max(vif(n.vif))))
max(vif(n.vif))
n.vif <- update(n.vif, .~.-`Nominal GDP (US$)`)
vif(n.vif)
names(which(vif(n.vif) == max(vif(n.vif))))
max(vif(n.vif))
n.vif <- update(n.vif, .~.-`M1 Money supply (US$)`)
vif(n.vif)
names(which(vif(n.vif) == max(vif(n.vif))))
max(vif(n.vif)) #max vif = 5.631253 < 10, no need to remove further
summary(n.vif)
traintesterror(n.vif,netherlands1.dt)
#Then use AIC
n.vif.aic <- step(n.vif) #AIC here did not remove any variables, all variables contributed a significant amount of information
summary(n.vif.aic)
traintesterror(n.vif.aic,netherlands1.dt)
par(mfrow = c(2,2))
plot(n.vif.aic)
par(mfrow = c(1,1))


#CART
#split the train and test sets
set.seed(1234)
trainn <- sample.split(Y = netherlands2.dt$`Exchange rate LCU:US$ (av)`, SplitRatio = 0.7)
trainsetn <- subset(netherlands2.dt, trainn == T)
testsetn <- subset(netherlands2.dt, trainn == F)
#create a cart model using the trainset
cartn1 <- rpart(`Exchange rate LCU:US$ (av)` ~ ., data = trainsetn, method = 'anova', control = rpart.control(minsplit = 2, cp = 0)) #create a CART model using all variables
rpart.plot(cartn1, nn= T, main = "Maximal Tree in Netherlands")
print(cartn1)
printcp(cartn1)
plotcp(cartn1, main = "Subtrees in Netherlands")
CVerror.cap <- cartn1$cptable[which.min(cartn1$cptable[,"xerror"]), "xerror"] + cartn1$cptable[which.min(cartn1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (cartn1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(cartn1$cptable[i,1] * cartn1$cptable[i-1,1]), 1)
cartn2 <- prune(cartn1, cp = cp.opt)
print(cartn2)
printcp(cartn2, digits = 3)
print(cartn2)
rpart.plot(cartn2, nn = T, main = "Optimal Tree for Netherlands Exchange Rates")
#find rmse value for testset data using trainset model
predictn <- predict(cartn2, newdata = testsetn)
predictn
rmse(testsetn$`Exchange rate LCU:US$ (av)`, predictn)
summary(cartn2)
# Consumer prices (% change pa; av)       Growth in average wages (LCU; % pa) Long-term bond yield (%)                Unemployment rate (%) 
# 15                                      12                                  11                                      11 
# Nominal GDP (US$)                       Goods: exports (US$)                Goods: imports (US$)                    M1 Money supply (US$)
# 6                                       6                                   6                                       6 
# Stock of money M2 (US$)                 Lending interest rate (%)           Money market interest rate (%; average) Deposit interest rate (%) 
# 6                                       6                                   6                                       5
# Private consumption (real % change pa)  Average real wages (% change pa)    Real GDP (% change pa)  
# 3                                       1                                   1 
cartn2$variable.importance
# Consumer prices (% change pa; av)       Growth in average wages (LCU; % pa)     Long-term bond yield (%)                Unemployment rate (%) 
# 0.47960510                              0.37005717                              0.35634484                              0.35514462 
# Nominal GDP (US$)                       Goods: exports (US$)                    Goods: imports (US$)                    M1 Money supply (US$) 
# 0.19985587                              0.19186163                              0.19186163                              0.19186163 
# Stock of money M2 (US$)                 Lending interest rate (%)               Money market interest rate (%; average) Deposit interest rate (%) 
# 0.19186163                              0.17929417                              0.17929417                              0.15240004 
# Private consumption (real % change pa)  Average real wages (% change pa)        Real GDP (% change pa) 
# 0.09687788                              0.04025635                              0.03019226 


#Determining the degree of fluctuation
#first finding the prediction of our exchange rates using our cart model
predictionusingcart = predict(cartn2, newdata = netherlands2.dt)
#adding in the predictions to the datatable
netherlands2.dt[,`:=` (prediction = predictionusingcart)]
View(netherlands2.dt)

#creating a list of the percentage changes
percentagechange = c()
for(var in 2:nrow(netherlands2.dt)){
  change  = abs((netherlands2.dt[,21][var]-netherlands2.dt[,21][var-1])/netherlands2.dt[,21][var-1] * 100)
  percentagechange[var] = change
}

#add the percentage change to the datatable
netherlands2.dt[,`:=` (percentagedifference = percentagechange)]
#finding the average percentage change across the years
mean(unlist(netherlands2.dt$percentagedifference[2:nrow(netherlands2.dt)])) #1.766449




# ==============================================================================================================

#RUSSIA
#input xlsx as data table
russia.dt <- read_excel("3.3 Russia.xlsx")
setDT(russia.dt)

#DATA PREPARATION
#check for values that does not make sense
russia.dt$`Export volume of goods and services (% change pa)`[1] #"-"
russia.dt$`Private sector credit/GDP`[75] #NA

#set those values as NA
russia.dt <- read_excel("3.3 Russia.xlsx", na = c("NA", "–"))
setDT(russia.dt)

#convert the class type of quarterly dates from character to date
class(russia.dt$Date) #character
russia.dt$Date = as.yearqtr(russia.dt$Date)
russia.dt$Date = as.Date(russia.dt$Date)
class(russia.dt$Date) #date

#remove all data after 2021Q2 as they are forecasted data and not accurate
russia.dt = russia.dt[1:114]

#remove date as it is not numeric
russia1.dt <- russia.dt[,-1]

#we are using average period instead of end period, thus, we will remove `Exchange rate LCU:$ (end-period)` and `Money market interest rate (%; end-period)`
#we will remove `M1 Money supply (LCU)` and `Stock of money M2 (LCU)` as they are not in USD
#we will remove `Export volume of goods and services (% change pa)` and `Import volume of goods and services (% change pa)` as they are repeated
russia1.dt = russia1.dt[,c('Exchange rate LCU:$ (end-period)', 'Money market interest rate (%; end-period)', 'M1 Money supply (LCU)', 'Stock of money M2 (LCU)', 'Export volume of goods and services (% change pa)', 'Import volume of goods and services (% change pa)'):=NULL]

#check for any negative values that are unlikely to be negative (e.g. money supply, stock of money, nominal GDP and exchange rate)
russia1.dt[`M1 Money supply (US$)` < 0, .N]
russia1.dt[`Stock of money M2 (US$)` < 0, .N]
russia1.dt[`Nominal GDP (US$)` < 0, .N]
russia1.dt[`Exchange rate LCU:US$ (av)` < 0, .N]
#all 0

#check through the summary to make sure the data make sense
summary(russia1.dt)

#check for number of NA values in russia1
sum(is.na(russia1.dt)) #267


#check which columns have NA values
for(var in colnames(russia1.dt)){
  print(var)
  print(sum(is.na(russia1.dt[[var]])))
}

#create table for CART
russia2.dt <- russia1.dt

#Export these rows, drop the NA values, and check for correlation with response variable to see if these rows can be dropped
for(var in colnames(russia1.dt)){
  try <- data.table(russia1.dt$`Exchange rate LCU:US$ (av)`, russia1.dt[[var]])
  try <- na.omit(try)
  if(cor(try)[2] < 0.1 && cor(try)[2] > -0.1){
    print(var);
    print(cor(try)[2])
  }
}
#"Gross fixed investment (% real change pa)" -0.05930858
#"Money market interest rate (%; average)" 0.02729184
#"Average real wages (% change pa)" 0.06137211
#"Private sector credit/GDP" 0.02424513

#drop them
russia1.dt <- russia1.dt[, -c("Gross fixed investment (% real change pa)", "Money market interest rate (%; average)", "Average real wages (% change pa)", "Private sector credit/GDP")]

#check NA percentage for each column
for(var in colnames(russia1.dt)){
  print(var)
  print(sum(is.na(russia1.dt[[var]]))/114)
}
#"Public debt  (% of GDP)" 0.3508772
#very high >30%

#drop them
russia1.dt <- russia1.dt[, -c("Public debt  (% of GDP)")]


#replace the rest of NA values with the respective mean of each column, don't drop those columns as their correlation > 0.1
for(var in colnames(russia1.dt)){
  russia1.dt[[var]][is.na(russia1.dt[[var]])] <- mean(russia1.dt[[var]], na.rm = TRUE)
}

sum(is.na(russia1.dt)) #0

#check for correlation again
for(var in colnames(russia1.dt)){
  try <- data.table(russia1.dt$`Exchange rate LCU:US$ (av)`, russia1.dt[[var]])
  if(cor(try)[2] < 0.1 && cor(try)[2] > -0.1){
    print(var);
    print(cor(try)[2])
  }
}


#LINEAR REGRESSION
r <- lm(`Exchange rate LCU:US$ (av)` ~ .,data = russia1.dt) #Create a model using ALL variables
summary(r)
traintesterror(r,russia1.dt)
#Then remove based on multicollinearity
vif(r)
names(which(vif(r) == max(vif(r))))
max(vif(r))
r.vif <- lm(`Exchange rate LCU:US$ (av)` ~ .-`Lending interest rate (%)`, data = russia1.dt)
vif(r.vif)
names(which(vif(r.vif) == max(vif(r.vif))))
max(vif(r.vif))
r.vif <- update(r.vif, .~.-`Growth in average wages (LCU; % pa)`)
vif(r.vif)
names(which(vif(r.vif) == max(vif(r.vif))))
max(vif(r.vif))
r.vif <- update(r.vif, .~.-`M1 Money supply (US$)`)
vif(r.vif)
names(which(vif(r.vif) == max(vif(r.vif))))
max(vif(r.vif))
r.vif <- update(r.vif, .~.-`Goods: imports (US$)`)
vif(r.vif)
names(which(vif(r.vif) == max(vif(r.vif))))
max(vif(r.vif))
r.vif <- update(r.vif, .~.-`Nominal GDP (US$)`)
vif(r.vif)
names(which(vif(r.vif) == max(vif(r.vif))))
max(vif(r.vif))
r.vif <- update(r.vif, .~.-`Stock of money M2 (US$)`)
vif(r.vif)
names(which(vif(r.vif) == max(vif(r.vif))))
max(vif(r.vif)) #max vif = 4.094483 < 10, no need to remove further
summary(r.vif)
traintesterror(r.vif,russia1.dt)
#Then use AIC
r.vif.aic <- step(r.vif)
summary(r.vif.aic)
traintesterror(r.vif.aic,russia1.dt)
par(mfrow = c(2,2))
plot(r.vif.aic)
par(mfrow = c(1,1))


#CART
#split the train and test sets
set.seed(1234)
trainr <- sample.split(Y = russia2.dt$`Exchange rate LCU:US$ (av)`, SplitRatio = 0.7)
trainsetr <- subset(russia2.dt, trainr == T)
testsetr <- subset(russia2.dt, trainr == F)
#create a cart model using the trainset
cartr1 <- rpart(`Exchange rate LCU:US$ (av)` ~ ., data = trainsetr, method = 'anova', control = rpart.control(minsplit = 2, cp = 0)) #create a CART model using all variables
rpart.plot(cartr1, nn= T, main = "Maximal Tree in Russia")
print(cartr1)
printcp(cartr1)
plotcp(cartr1, main = "Subtrees in Russia")
CVerror.cap <- cartr1$cptable[which.min(cartr1$cptable[,"xerror"]), "xerror"] + cartr1$cptable[which.min(cartr1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (cartr1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(cartr1$cptable[i,1] * cartr1$cptable[i-1,1]), 1)
cartr2 <- prune(cartr1, cp = cp.opt)
print(cartr2)
printcp(cartr2, digits = 3)
rpart.plot(cartr2, nn = T, main = "Optimal Tree for Russia exchange Rates")
#find rmse value for testset data using trainset model
predictr <- predict(cartr2, newdata = testsetr)
predictr
rmse(testsetr$`Exchange rate LCU:US$ (av)`, predictr) #11.97997
summary(cartr2)
# Unemployment rate (%)                     Nominal GDP (US$) 
# 16                                        15 
# Growth in average wages (LCU; % pa)       M1 Money supply (US$) 
# 12                                        9 
# Consumer prices (% change pa; av)         Stock of money M2 (US$) 
# 9                                         8 
# Public debt  (% of GDP)                   Goods: exports (US$) 
# 6                                         5 
# Goods: imports (US$)                      Private consumption (real % change pa) 
# 4                                         4 
# Deposit interest rate (%)                 Lending interest rate (%) 
# 3                                         3 
# Budget balance (% of GDP)                 Gross fixed investment (% real change pa) 
# 2                                         2 
# Real GDP (% change pa) 
# 2 
cartr2$variable.importance
# Unemployment rate (%)                     Nominal GDP (US$) 
# 20528.480                                 18849.826 
# Growth in average wages (LCU; % pa)       M1 Money supply (US$) 
# 14543.091                                 11818.931 
# Consumer prices (% change pa; av)         Stock of money M2 (US$) 
# 11227.985                                 10046.091 
# Public debt  (% of GDP)                   Goods: exports (US$) 
# 7812.106                                  6249.685 
# Goods: imports (US$)                      Private consumption (real % change pa) 
# 5468.474                                  4687.263 
# Deposit interest rate (%)                 Lending interest rate (%) 
# 4102.682                                  3471.500 
# Budget balance (% of GDP)                 Gross fixed investment (% real change pa) 
# 1893.545                                  1893.545 
# Real GDP (% change pa) 
# 1893.545 


#Determining the degree of fluctuation
#first finding the prediction of our exchange rates using our cart model
predictionusingcart = predict(cartr2, newdata = russia2.dt)
#adding in the predictions to the datatable
russia2.dt[,`:=` (prediction = predictionusingcart)]
View(russia2.dt)

#creating a list of the percentage changes
percentagechange = c()
for(var in 2:nrow(russia2.dt)){
  change  = abs((russia2.dt[,21][var]-russia2.dt[,21][var-1])/russia2.dt[,21][var-1] * 100)
  percentagechange[var] = change
}

#add the percentage change to the datatable
russia2.dt[,`:=` (percentagedifference = percentagechange)]
#finding the average percentage change across the years
mean(unlist(russia2.dt$percentagedifference[2:nrow(russia2.dt)]))
#11.52225



# ==============================================================================================================

#SWEDEN
#input xlsx as data table
sweden.dt <- read_excel("3.4 Sweden.xlsx")
setDT(sweden.dt)

#DATA PREPARATION
#check for values that does not make sense
sweden.dt$`Export volume of goods and services (% change pa)`[1] #"-"
sweden.dt$`Private sector credit/GDP`[75] #NA

#set those values as NA
sweden.dt <- read_excel("3.4 Sweden.xlsx", na = c("NA", "–"))
setDT(sweden.dt)

#convert the class type of quarterly dates from character to date
class(sweden.dt$Date) #character
sweden.dt$Date = as.yearqtr(sweden.dt$Date)
sweden.dt$Date = as.Date(sweden.dt$Date)
class(sweden.dt$Date) #date

#remove all data after 2021Q2 as they are forecasted data and not accurate
sweden.dt = sweden.dt[1:114]

#remove date as it is not numeric
sweden1.dt <- sweden.dt[,-1]

#we are using average period instead of end period, thus, we will remove `Exchange rate LCU:$ (end-period)` and `Money market interest rate (%; end-period)`
#we will remove `M1 Money supply (LCU)` and `Stock of money M2 (LCU)` as they are not in USD
#we will remove `Export volume of goods and services (% change pa)` and `Import volume of goods and services (% change pa)` as they are repeated
sweden1.dt = sweden1.dt[, -c('Exchange rate LCU:$ (end-period)', 'Money market interest rate (%; end-period)', 'M1 Money supply (LCU)', 'Stock of money M2 (LCU)', 'Export volume of goods and services (% change pa)', 'Import volume of goods and services (% change pa)')]

#check for any negative values that are unlikely to be negative (e.g. money supply, stock of money, nominal GDP and exchange rate)
sweden1.dt[`M1 Money supply (US$)` < 0, .N]
sweden1.dt[`Stock of money M2 (US$)` < 0, .N]
sweden1.dt[`Nominal GDP (US$)` < 0, .N]
sweden1.dt[`Exchange rate LCU:US$ (av)` < 0, .N]
#all 0

#check through the summary to make sure the data make sense
summary(sweden1.dt)

#check for number of NA values
sum(is.na(sweden1.dt)) #108


#check which columns have NA values
for(var in colnames(sweden1.dt)){
  print(var)
  print(sum(is.na(sweden1.dt[[var]])))
}

#create table for cart
sweden2.dt <- sweden1.dt

#Export these rows, drop the NA values, and check for correlation with response variable to see if these rows can be dropped
for(var in colnames(sweden1.dt)){
  try <- data.table(sweden1.dt$`Exchange rate LCU:US$ (av)`, sweden1.dt[[var]])
  try <- na.omit(try)
  if(cor(try)[2] < 0.1 && cor(try)[2] > -0.1){
    print(var);
    print(cor(try)[2])
  }
}
#"M1 Money supply (US$)" -0.05371186
#"Real GDP (% change pa)" -0.07236359
#"Public debt  (% of GDP)" 0.08604862
#"Private sector credit/GDP" -0.03888641

#drop them
sweden1.dt <- sweden1.dt[, -c("M1 Money supply (US$)", "Real GDP (% change pa)", "Public debt  (% of GDP)", "Private sector credit/GDP")]


#check NA percentage for each column
for(var in colnames(sweden1.dt)){
  print(var)
  print(sum(is.na(sweden1.dt[[var]]))/114)
}
#quite low

#replace the rest of NA values with the respective mean of each column, don't drop those columns as their correlation > 0.1
for(var in colnames(sweden1.dt)){
  sweden1.dt[[var]][is.na(sweden1.dt[[var]])] <- mean(sweden1.dt[[var]], na.rm = TRUE)
}

sum(is.na(sweden1.dt)) #0

#check for correlation again
for(var in colnames(sweden1.dt)){
  try <- data.table(sweden1.dt$`Exchange rate LCU:US$ (av)`, sweden1.dt[[var]])
  if(cor(try)[2] < 0.1 && cor(try)[2] > -0.1){
    print(var);
    print(cor(try)[2])
  }
}


#LINEAR REGRESSION
s <- lm(`Exchange rate LCU:US$ (av)` ~ .,data = sweden1.dt) #Create a model using ALL variables
summary(s)
traintesterror(s,sweden1.dt)
#Then remove based on multicollinearity
vif(s)
names(which(vif(s) == max(vif(s))))
max(vif(s))
s.vif <- lm(`Exchange rate LCU:US$ (av)` ~ .-`Average real wages (% change pa)`, data = sweden1.dt)
vif(s.vif)
names(which(vif(s.vif) == max(vif(s.vif))))
max(vif(s.vif))
s.vif <- update(s.vif, .~.-`Money market interest rate (%; average)`)
vif(s.vif)
names(which(vif(s.vif) == max(vif(s.vif))))
max(vif(s.vif))
s.vif <- update(s.vif, .~.-`Goods: imports (US$)`)
vif(s.vif)
names(which(vif(s.vif) == max(vif(s.vif))))
max(vif(s.vif))
s.vif <- update(s.vif, .~.-`Nominal GDP (US$)`)
vif(s.vif)
names(which(vif(s.vif) == max(vif(s.vif))))
max(vif(s.vif))
s.vif <- update(s.vif, .~.-`Lending interest rate (%)`)
vif(s.vif)
names(which(vif(s.vif) == max(vif(s.vif))))
max(vif(s.vif))
s.vif <- update(s.vif, .~.-`Long-term bond yield (%)`)
vif(s.vif)
names(which(vif(s.vif) == max(vif(s.vif))))
max(vif(s.vif)) #max vif = 5.65947 < 10, no need to remove further
summary(s.vif)
traintesterror(s.vif,sweden1.dt)
#Then use AIC
s.vif.aic <- step(s.vif)
summary(s.vif.aic)
traintesterror(s.vif.aic,sweden1.dt) #The trainset RMSE is 0.508453 while the testset RMSE is 0.439762
par(mfrow = c(2,2))
plot(s.vif.aic)
par(mfrow = c(1,1))



#CART
#split the train and test sets
set.seed(1234)
trains <- sample.split(Y = sweden2.dt$`Exchange rate LCU:US$ (av)`, SplitRatio = 0.7)
trainsets <- subset(sweden2.dt, trains == T)
testsets <- subset(sweden2.dt, trains == F)
#create a cart model using the trainset
carts1 <- rpart(`Exchange rate LCU:US$ (av)` ~ ., data = trainsets, method = 'anova', control = rpart.control(minsplit = 2, cp = 0)) #create a CART model using all variables
rpart.plot(carts1, nn= T, main = "Maximal Tree in Sweden")
print(carts1)
printcp(carts1)
plotcp(carts1, main = "Subtrees in Sweden")
CVerror.cap <- carts1$cptable[which.min(carts1$cptable[,"xerror"]), "xerror"] + carts1$cptable[which.min(carts1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (carts1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(carts1$cptable[i,1] * carts1$cptable[i-1,1]), 1)
carts2 <- prune(carts1, cp = cp.opt)
print(carts2)
printcp(carts2, digits = 3)
print(carts2)
rpart.plot(carts2, nn = T, main = "Optimal Tree for Sweden Exchange Rates")
#find rmse value for testset data using trainset model
predicts <- predict(carts2, newdata = testsets)
predicts
rmse(testsets$`Exchange rate LCU:US$ (av)`, predicts)
summary(carts2)
# Long-term bond yield (%)                  Unemployment rate (%) 
# 12                                        12 
# Deposit interest rate (%)                 Money market interest rate (%; average) 
# 12                                        12 
# Lending interest rate (%)                 Nominal GDP (US$) 
# 11                                        8 
# M1 Money supply (US$)                     Public debt  (% of GDP) 
# 8                                         4 
# Goods: exports (US$)                      Goods: imports (US$) 
# 4                                         4 
# Stock of money M2 (US$)                   Average real wages (% change pa) 
# 4                                         4 
# Private consumption (real % change pa)    Budget balance (% of GDP) 
# 2                                         1 
# Real GDP (% change pa)                    Gross fixed investment (% real change pa) 
# 1                                         1          
carts2$variable.importance
# Long-term bond yield (%)                  Unemployment rate (%) 
# 39.201832                                 37.243352 
# Deposit interest rate (%)                 Money market interest rate (%; average) 
# 36.764835                                 36.764835 
# Lending interest rate (%)                 Nominal GDP (US$) 
# 33.966101                                 25.836386 
# M1 Money supply (US$)                     Public debt  (% of GDP) 
# 25.181126                                 14.350192 
# Goods: exports (US$)                      Goods: imports (US$) 
# 14.342600                                 14.321849 
# Stock of money M2 (US$)                   Average real wages (% change pa) 
# 14.301098                                 11.820244 
# Private consumption (real % change pa)    Budget balance (% of GDP) 
# 7.655821                                  2.992709 
# Real GDP (% change pa)                    Gross fixed investment (% real change pa) 
# 2.974292                                  1.827748


#Determining the degree of fluctuation
#first finding the prediction of our exchange rates using our cart model
predictionusingcart = predict(carts2, newdata = sweden2.dt)
#adding in the predictions to the datatable
sweden2.dt[,`:=` (prediction = predictionusingcart)]
View(sweden2.dt)

#creating a list of the percentage changes
percentagechange = c()
for(var in 2:nrow(sweden2.dt)){
  change  = abs((sweden2.dt[,21][var]-sweden2.dt[,21][var-1])/sweden2.dt[,21][var-1] * 100)
  percentagechange[var] = change
}

#add the percentage change to the datatable
sweden2.dt[,`:=` (percentagedifference = percentagechange)]
#finding the average percentage change across the years
mean(unlist(sweden2.dt$percentagedifference[2:nrow(sweden2.dt)])) #2.804483




# ==============================================================================================================

#UK
#input xlsx as data table
uk.dt <- read_excel("3.5 UK.xlsx")
setDT(uk.dt)

#DATA PREPARATION
#check for values that does not make sense
uk.dt$`Lending interest rate (%)`[1] #"-"
uk.dt$`Private sector credit/GDP`[75] #NA

#set those values as NA
uk.dt <- read_excel("3.5 UK.xlsx", na = c("NA", "–"))
setDT(uk.dt)

#convert the class type of quarterly dates from character to date
class(uk.dt$Date) #character
uk.dt$Date = as.yearqtr(uk.dt$Date)
uk.dt$Date = as.Date(uk.dt$Date)
class(uk.dt$Date) #date

#remove all data after 2021Q2 as they are forecasted data and not accurate
uk.dt = uk.dt[1:114]

#remove date as it is not numeric
uk1.dt <- uk.dt[,-1]

#we are using average period instead of end period, thus, we will remove `Exchange rate LCU:$ (end-period)` and `Money market interest rate (%; end-period)`
#we will remove `M1 Money supply (LCU)` and `Stock of money M2 (LCU)` as they are not in USD
#we will remove `Export volume of goods and services (% change pa)` and `Import volume of goods and services (% change pa)` as they are repeated
uk1.dt = uk1.dt[,-c('Exchange rate LCU:$ (end-period)', 'Money market interest rate (%; end-period)', 'M1 Money supply (LCU)', 'Stock of money M2 (LCU)', 'Export volume of goods and services (% change pa)', 'Import volume of goods and services (% change pa)')]

#check for any negative values that are unlikely to be negative (e.g. money supply, stock of money, nominal GDP and exchange rate)
uk1.dt[`M1 Money supply (US$)` < 0, .N]
uk1.dt[`Stock of money M2 (US$)` < 0, .N]
uk1.dt[`Nominal GDP (US$)` < 0, .N]
uk1.dt[`Exchange rate LCU:US$ (av)` < 0, .N]

#all 0

#check through the summary to make sure the data make sense
summary(uk1.dt)

#check for number of NA values
sum(is.na(uk1.dt)) #61

#check which columns have NA values
for(var in colnames(uk1.dt)){
  print(var)
  print(sum(is.na(uk1.dt[[var]])))
}

#creating data table for cart
uk2.dt <- uk1.dt

#Export these rows, drop the NA values, and check for correlation with response variable to see if these rows can be dropped
for(var in colnames(uk1.dt)){
  try <- data.table(uk1.dt$`Exchange rate LCU:US$ (av)`, uk1.dt[[var]])
  try <- na.omit(try)
  if(cor(try)[2] < 0.1 && cor(try)[2] > -0.1){
    print(var);
    print(cor(try)[2])
  }
}
#"Nominal GDP (US$)" -0.02706139
#"Goods: exports (US$)" -0.002364276
#"Goods: imports (US$)" 0.0357962
#"Private sector credit/GDP" -9.810515e-05

#drop them
uk1.dt <- uk1.dt[, -c("Nominal GDP (US$)", "Goods: exports (US$)", "Goods: imports (US$)", "Private sector credit/GDP")]


#check NA percentage for each column
for(var in colnames(uk1.dt)){
  print(var)
  print(sum(is.na(uk1.dt[[var]]))/114)
}
#quite low

#replace the rest of NA values with the respective mean of each column, don't drop those columns as their correlation > 0.1
for(var in colnames(uk1.dt)){
  uk1.dt[[var]][is.na(uk1.dt[[var]])] <- mean(uk1.dt[[var]], na.rm = TRUE)
}

sum(is.na(uk1.dt)) #0

#check for correlation again
for(var in colnames(uk1.dt)){
  try <- data.table(uk1.dt$`Exchange rate LCU:US$ (av)`, uk1.dt[[var]])
  if(cor(try)[2] < 0.1 && cor(try)[2] > -0.1){
    print(var);
    print(cor(try)[2])
  }
}


#LINEAR REGRESSION
u <- lm(`Exchange rate LCU:US$ (av)` ~ .,data = uk1.dt) #Create a model using ALL variables
summary(u)
traintesterror(u,uk1.dt)
#Then remove based on multicollinearity
vif(u)
names(which(vif(u) == max(vif(u))))
max(vif(u))
u.vif <- lm(`Exchange rate LCU:US$ (av)` ~ .-`M1 Money supply (US$)`, data = uk1.dt)
vif(u.vif)
names(which(vif(u.vif) == max(vif(u.vif))))
max(vif(u.vif))
u.vif <- update(u.vif, .~.-`Money market interest rate (%; average)`)
vif(u.vif)
names(which(vif(u.vif) == max(vif(u.vif))))
max(vif(u.vif))
u.vif <- update(u.vif, .~.-`Average real wages (% change pa)`)
vif(u.vif)
names(which(vif(u.vif) == max(vif(u.vif))))
max(vif(u.vif))
u.vif <- update(u.vif, .~.-`Lending interest rate (%)`)
vif(u.vif)
names(which(vif(u.vif) == max(vif(u.vif))))
max(vif(u.vif))
u.vif <- update(u.vif, .~.-`Real GDP (% change pa)`)
vif(u.vif)
names(which(vif(u.vif) == max(vif(u.vif))))
max(vif(u.vif))
u.vif <- update(u.vif, .~.-`Long-term bond yield (%)`)
vif(u.vif)
names(which(vif(u.vif) == max(vif(u.vif))))
max(vif(u.vif)) #max vif = 6.380917 < 10, no need to remove further
summary(u.vif)
traintesterror(u.vif,uk1.dt)
#Then use AIC
u.vif.aic <- step(u.vif)
summary(u.vif.aic)
traintesterror(u.vif.aic,uk1.dt)
par(mfrow = c(2,2))
plot(u.vif.aic)
par(mfrow = c(1,1))



#CART
#split the train and test sets
set.seed(1234)
trainu <- sample.split(Y = uk2.dt$`Exchange rate LCU:US$ (av)`, SplitRatio = 0.7)
trainsetu <- subset(uk2.dt, trainu == T)
testsetu <- subset(uk2.dt, trainu == F)
#create a cart model using the trainset
cartu1 <- rpart(`Exchange rate LCU:US$ (av)` ~ ., data = trainsetu, method = 'anova', control = rpart.control(minsplit = 2, cp = 0)) #create a CART model using all variables
rpart.plot(cartu1, nn= T, main = "Maximal Tree in UK")
print(cartu1)
printcp(cartu1)
plotcp(cartu1, main = "Subtrees in UK")
CVerror.cap <- cartu1$cptable[which.min(cartu1$cptable[,"xerror"]), "xerror"] + cartu1$cptable[which.min(cartu1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (cartu1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(cartu1$cptable[i,1] * cartu1$cptable[i-1,1]), 1)
cartu2 <- prune(cartu1, cp = cp.opt)
print(cartu2)
printcp(cartu2, digits = 3)
print(cartu2)
rpart.plot(cartu2, nn = T, main = "Optimal Tree for UK Exchange Rates")
#find rmse value for testset data using trainset model
predicts <- predict(cartu2, newdata = testsetu)
predicts
rmse(testsetu$`Exchange rate LCU:US$ (av)`, predicts) #0.03243715
summary(cartu2)
# Long-term bond yield (%)                Deposit interest rate (%)               Public debt  (% of GDP) 
# 21                                      19                                      14 
# M1 Money supply (US$)                   Money market interest rate (%; average) Unemployment rate (%) 
# 10                                      10                                      10 
# Growth in average wages (LCU; % pa)     Lending interest rate (%)               Goods: exports (US$) 
# 4                                       4                                       2 
# Goods: imports (US$)                    Nominal GDP (US$)                       Stock of money M2 (US$) 
# 2                                       2                                       2                        
cartu2$variable.importance
# Long-term bond yield (%)                Deposit interest rate (%)               Public debt  (% of GDP) 
# 0.41296363                              0.37924321                              0.28231526 
# M1 Money supply (US$)                   Money market interest rate (%; average) Unemployment rate (%) 
# 0.20893662                              0.20786781                              0.20565048 
# Growth in average wages (LCU; % pa)     Lending interest rate (%)               Goods: exports (US$) 
# 0.07666478                              0.07076749                              0.03895238 
# Goods: imports (US$)                    Nominal GDP (US$)                       Stock of money M2 (US$) 
# 0.03895238                              0.03895238                              0.03756122                    


#Determining the degree of fluctuation
#first finding the prediction of our exchange rates using our cart model
predictionusingcart = predict(cartu2, newdata = uk2.dt)
#adding in the predictions to the datatable
uk2.dt[,`:=` (prediction = predictionusingcart)]
View(uk2.dt)

#creating a list of the percentage changes
percentagechange = c()
for(var in 2:nrow(uk2.dt)){
  change  = abs((uk2.dt[,21][var]-uk2.dt[,21][var-1])/uk2.dt[,21][var-1] * 100)
  percentagechange[var] = change
}

#add the percentage change to the datatable
uk2.dt[,`:=` (percentagedifference = percentagechange)]
#finding the average percentage change across the years
mean(unlist(uk2.dt$percentagedifference[2:nrow(uk2.dt)])) #0.8351119



