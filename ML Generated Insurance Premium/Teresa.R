setwd("~/Documents/NTU/Y2S1/BC2406/AY21S1 CBA")

library(data.table)
library(ggplot2)
library(plotly)
library(rpart)
library(rpart.plot)
library(caTools)
library(Metrics)
library(car)

premium.dt<-fread("premium2.csv")

#check for any inconsistencies
#View(premium.dt)
summary(premium.dt)

#ensure no null values
sum(is.na(premium.dt))

#ensure no duplicated values
sum(duplicated(premium.dt)==TRUE)

#1. Create the BMI variable based on CDC definition
# ==============================================================================================================
#Create a continuous BMI column
#BMI Formula: weight (kg) / [height (m)]^2
#weight is in kg and height is in cm, need to change height to m
premium.dt[, BMI := Weight/((Height/100)^2)]

#Create a categorical BMI column
#If your BMI is less than 18.5, it falls within the underweight range.
#If your BMI is 18.5 to 24.9, it falls within the normal or Healthy Weight range.
#If your BMI is 25.0 to 29.9, it falls within the overweight range.
#If your BMI is 30.0 or higher, it falls within the obese range.
premium.dt[, Weight_status := cut(premium.dt$BMI, breaks=c(0, 18.5, 25.0, 30.0, Inf), labels=c(0, 1, 2, 3))]

#verify that the BMI column is done correctly

# ==============================================================================================================

#2. There are many categorical variables with integer coded values (e.g. Diabetes, HighBloodPressure, Transplant...etc.) Is it necessary to convert them to factor datatype in R?
# ==============================================================================================================
#check the class types of all the variables
for(var in colnames(premium.dt)){
  print(var)
  print(class(premium.dt[[var]]))
  print("")
}
#all except BMI are of class type integer
#Diabetes, HighBloodPressure, Transplant, ChronicDisease, Allergy, CancerInFamily, and Gender are not supposed to have integer class type and should be categorical instead as they only have two possible values, 1 (Present) or 0 (Absent)
#It is necessary to convert them to factor datatype in R. The datatype affects data exploration and machine learning.
for(var in c("Diabetes", "HighBloodPressure", "Transplant", "ChronicDisease", "Allergy", "CancerInFamily", "Gender")){
  premium.dt[[var]] <- as.factor(premium.dt[[var]])
}
#check the class types of all the variables again
for(var in colnames(premium.dt)){
  print(var)
  print(class(premium.dt[[var]]))
  print("")
}

#change the numerals to the appropriate strings in a new column for data exploration later
premium.dt[Diabetes == 0, Diabetes2:='Absence']
premium.dt[Diabetes == 1, Diabetes2:='Presence']
premium.dt[HighBloodPressure == 0, HighBloodPressure2:='Absence']
premium.dt[HighBloodPressure == 1, HighBloodPressure2:='Presence']
premium.dt[Transplant == 0, Transplant2:='No']
premium.dt[Transplant == 1, Transplant2:='Yes']
premium.dt[ChronicDisease == 0, ChronicDisease2:='No']
premium.dt[ChronicDisease == 1, ChronicDisease2:='Yes']
premium.dt[Allergy == 0, Allergy2:='No']
premium.dt[Allergy == 1, Allergy2:='Yes']
premium.dt[CancerInFamily == 0, CancerInFamily2:='No']
premium.dt[CancerInFamily == 1, CancerInFamily2:='Yes']
premium.dt[Gender == 0, Gender2:='Female']
premium.dt[Gender == 1, Gender2:='Male']
premium.dt[Weight_status == 0, Weight_status2:='Underweight']
premium.dt[Weight_status == 1, Weight_status2:='Normal']
premium.dt[Weight_status == 2, Weight_status2:='Overweight']
premium.dt[Weight_status == 3, Weight_status2:='Obese']
premium.dt$Weight_status2 <- factor(premium.dt$Weight_status2, levels = c("Underweight", "Normal", "Overweight", "Obese"))


# ==============================================================================================================

#3. Explore the data and report on your key findings.
# ==============================================================================================================
#Explore each variables

#Continuous variables premium, Age, Weight, Height, NumMajorSurgeries, BMI
#boxplot and histogram together with density line

#Premium
summary(premium.dt$Premium)
#histogram together with density line
premium = ggplot(data = premium.dt, aes(x = Premium, geom = 'blank')) + 
  geom_line(aes(y = ..density..), stat = 'density') +
  geom_histogram(aes(y = ..density..), binwidth = 50, alpha = 0.5, colour="black", fill="blue") +
  labs(title="Premium") + 
  geom_vline(aes(xintercept=mean(Premium)), color="red", linetype="dashed", size=1)
ggplotly(premium)
#boxplot
boxplot(premium.dt$Premium, main="Premium")

#Age
summary(premium.dt$Age)
#histogram together with density line
age = ggplot(data = premium.dt, aes(x = Age, geom = 'blank')) + 
  geom_line(aes(y = ..density..), stat = 'density') +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, colour="black", fill="blue") +
  labs(title="Age") + 
  geom_vline(aes(xintercept=mean(Age)), color="red", linetype="dashed", size=1)
age
#boxplot
boxplot(premium.dt$Age, main="Age")
#scatter plot with Premium
cor(premium.dt[,c('Age','Premium')])
ggplot(data = premium.dt[,c('Age','Premium')], aes(x = Age, y = Premium)) + geom_smooth(method='loess', formula= y~x) + geom_point(alpha = 0.5) + ggtitle("Premium against Age")

#Weight
summary(premium.dt$Weight)
#histogram together with density line
weight = ggplot(data = premium.dt, aes(x = Weight, geom = 'blank')) + 
  geom_line(aes(y = ..density..), stat = 'density') +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, colour="black", fill="blue") +
  labs(title="Weight") + 
  geom_vline(aes(xintercept=mean(Weight)), color="red", linetype="dashed", size=1)
weight
#boxplot
boxplot(premium.dt$Weight, main="Weight")
#scatter plot with Premium
cor(premium.dt[,c('Weight','Premium')])
ggplot(data = premium.dt[,c('Weight','Premium')], aes(x = Weight, y = Premium)) + geom_smooth(method='loess', formula= y~x) + geom_point(alpha = 0.5) + ggtitle("Premium against Weight")

#Height
summary(premium.dt$Height)
#histogram together with density line
height = ggplot(data = premium.dt, aes(x = Height, geom = 'blank')) + 
  geom_line(aes(y = ..density..), stat = 'density') +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, colour="black", fill="blue") +
  labs(title="Height") + 
  geom_vline(aes(xintercept=mean(Height)), color="red", linetype="dashed", size=1)
height
#boxplot
boxplot(premium.dt$Height, main="Height")
#scatter plot with Premium
cor(premium.dt[,c('Height','Premium')])
ggplot(data = premium.dt[,c('Height','Premium')], aes(x = Height, y = Premium)) + geom_smooth(method='loess', formula= y~x) + geom_point(alpha = 0.5) + ggtitle("Premium against Height")

#NumMajorSurgeries
summary(premium.dt$NumMajorSurgeries)
#histogram together with density line
numMajorSurgeries = ggplot(data = premium.dt, aes(x = NumMajorSurgeries, geom = 'blank')) + 
  geom_line(aes(y = ..density..), stat = 'density') +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, colour="black", fill="blue") +
  labs(title="NumMajorSurgeries") + 
  geom_vline(aes(xintercept=mean(NumMajorSurgeries)), color="red", linetype="dashed", size=1)
numMajorSurgeries
#boxplot
boxplot(premium.dt$NumMajorSurgeries, main="NumMajorSurgeries")
#scatter plot with Premium
cor(premium.dt[,c('NumMajorSurgeries','Premium')])
ggplot(data = premium.dt[,c('NumMajorSurgeries','Premium')], aes(x = NumMajorSurgeries, y = Premium)) + geom_smooth(method='loess', formula= y~x) + geom_point(alpha = 0.5) + ggtitle("Premium against NumMajorSurgeries")
#bar plot
numMajorSurgeries = ggplot(data = premium.dt, aes(x = as.factor(NumMajorSurgeries), fill = as.factor(NumMajorSurgeries))) + geom_bar(show.legend = FALSE) + labs(title="NumMajorSurgeries") + geom_text(stat = 'count', aes(label=after_stat(count)), vjust = -1) +xlab("NumMajorSurgeries")
numMajorSurgeries
#scatter plot with Premium with jittered x
plot(jitter(premium.dt$NumMajorSurgeries), premium.dt$Premium, ylab = "Premium", xlab = "NumMajorSurgeries")
#boxplot with Premium
ggplot(data = premium.dt, aes(y = Premium, fill = as.factor(NumMajorSurgeries))) + geom_boxplot(show.legend = FALSE) + labs(title="Premium against NumMajorSurgeries") + facet_grid(.~as.factor(NumMajorSurgeries))
num0 <- subset(premium.dt$Premium,premium.dt$NumMajorSurgeries == 0)
summary(num0)
num1 <- subset(premium.dt$Premium,premium.dt$NumMajorSurgeries == 1)
summary(num1)
num2 <- subset(premium.dt$Premium,premium.dt$NumMajorSurgeries == 2)
summary(num2)
num3 <- subset(premium.dt$Premium,premium.dt$NumMajorSurgeries == 3)
summary(num3)

#BMI
summary(premium.dt$BMI)
#histogram together with density line
bmi = ggplot(data = premium.dt, aes(x = BMI, geom = 'blank')) + 
  geom_line(aes(y = ..density..), stat = 'density') +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, colour="black", fill="blue") +
  labs(title="BMI") + 
  geom_vline(aes(xintercept=mean(BMI)), color="red", linetype="dashed", size=1)
bmi
#boxplot
boxplot(premium.dt$BMI, main="BMI")
#scatter plot with Premium
cor(premium.dt[,c('BMI','Premium')])
ggplot(data = premium.dt[,c('BMI','Premium')], aes(x = BMI, y = Premium)) + geom_smooth(method='loess', formula= y~x) + geom_point(alpha = 0.5) + ggtitle("Premium against BMI")


#plot Premium against all the continuous variables
premium.dt.melt <- melt(premium.dt, id = "Premium", measure = c("Age", "Weight", "Height", "NumMajorSurgeries", "BMI"))
ggplot(premium.dt.melt, aes(value, Premium, colour = variable)) + geom_point() + facet_wrap(facets = vars(variable), scales = "free_x")

#Categorical variables Diabetes, HighBloodPressure, Transplant, ChronicDisease, Allergy, CancerInFamily, Gender, and BMI

#Diabetes
#bar plot
diabetes = ggplot(data = premium.dt, aes(x = Diabetes2, fill = Diabetes2)) + geom_bar(show.legend = FALSE) + labs(title="Diabetes") + geom_text(stat = 'count', aes(label=after_stat(count)), vjust = -1) + xlab("Diabetes")
diabetes
#scatter plot with Premium with jittered x
plot(jitter(as.numeric(premium.dt$Diabetes)), premium.dt$Premium, ylab = "Premium", xlab = "Diabetes")
#boxplot with Premium
ggplot(data = premium.dt, aes(y = Premium, fill = Diabetes2)) + geom_boxplot(show.legend = FALSE) + labs(title="Premium against Diabetes") + facet_grid(.~Diabetes2)
num0 <- subset(premium.dt$Premium,premium.dt$Diabetes == 0)
summary(num0)
num1 <- subset(premium.dt$Premium,premium.dt$Diabetes == 1)
summary(num1)

#HighBloodPressure
#bar plot
highBloodPressure = ggplot(data = premium.dt, aes(x = HighBloodPressure2, fill = HighBloodPressure2)) + geom_bar(show.legend = FALSE) + labs(title="HighBloodPressure") + geom_text(stat = 'count', aes(label=after_stat(count)), vjust = -1) + xlab("HighBloodPressure")
highBloodPressure
#scatter plot with Premium with jittered x
plot(jitter(as.numeric(premium.dt$HighBloodPressure)), premium.dt$Premium, ylab = "Premium", xlab = "HighBloodPressure")
#boxplot with Premium
ggplot(data = premium.dt, aes(y = Premium, fill = HighBloodPressure2)) + geom_boxplot(show.legend = FALSE) + labs(title="Premium against HighBloodPressure") + facet_grid(.~HighBloodPressure2)
num0 <- subset(premium.dt$Premium,premium.dt$HighBloodPressure == 0)
summary(num0)
num1 <- subset(premium.dt$Premium,premium.dt$HighBloodPressure == 1)
summary(num1)

#Transplant
#bar plot
transplant = ggplot(data = premium.dt, aes(x = Transplant2, fill = Transplant2)) + geom_bar(show.legend = FALSE) + labs(title="Transplant") + geom_text(stat = 'count', aes(label=after_stat(count)), vjust = -1) + xlab("Transplant")
transplant
#scatter plot with Premium with jittered x
plot(jitter(as.numeric(premium.dt$Transplant)), premium.dt$Premium, ylab = "Premium", xlab = "Transplant")
#boxplot with Premium
ggplot(data = premium.dt, aes(y = Premium, fill = Transplant2)) + geom_boxplot(show.legend = FALSE) + labs(title="Premium against Transplant") + facet_grid(.~Transplant2)
num0 <- subset(premium.dt$Premium,premium.dt$Transplant == 0)
summary(num0)
num1 <- subset(premium.dt$Premium,premium.dt$Transplant == 1)
summary(num1)

#ChronicDisease
#bar plot
chronicDisease = ggplot(data = premium.dt, aes(x = ChronicDisease2, fill = ChronicDisease2)) + geom_bar(show.legend = FALSE) + labs(title="ChronicDisease") + geom_text(stat = 'count', aes(label=after_stat(count)), vjust = -1) + xlab("ChronicDisease")
chronicDisease
#scatter plot with Premium with jittered x
plot(jitter(as.numeric(premium.dt$ChronicDisease)), premium.dt$Premium, ylab = "Premium", xlab = "ChronicDisease")
#boxplot with Premium
ggplot(data = premium.dt, aes(y = Premium, fill = ChronicDisease2)) + geom_boxplot(show.legend = FALSE) + labs(title="Premium against ChronicDisease") + facet_grid(.~ChronicDisease2)
num0 <- subset(premium.dt$Premium,premium.dt$ChronicDisease == 0)
summary(num0)
num1 <- subset(premium.dt$Premium,premium.dt$ChronicDisease == 1)
summary(num1)

#Allergy
#bar plot
allergy = ggplot(data = premium.dt, aes(x = Allergy2, fill = Allergy2)) + geom_bar(show.legend = FALSE) + labs(title="Allergy") + geom_text(stat = 'count', aes(label=after_stat(count)), vjust = -1) + xlab("Allergy")
allergy
#scatter plot with Premium with jittered x
plot(jitter(as.numeric(premium.dt$Allergy)), premium.dt$Premium, ylab = "Premium", xlab = "Allergy")
#boxplot with Premium
ggplot(data = premium.dt, aes(y = Premium, fill = Allergy2)) + geom_boxplot(show.legend = FALSE) + labs(title="Premium against Allergy") + facet_grid(.~Allergy2)
num0 <- subset(premium.dt$Premium,premium.dt$Allergy == 0)
summary(num0)
num1 <- subset(premium.dt$Premium,premium.dt$Allergy == 1)
summary(num1)

#CancerInFamily
#bar plot
cancerInFamily = ggplot(data = premium.dt, aes(x = CancerInFamily2, fill = CancerInFamily2)) + geom_bar(show.legend = FALSE) + labs(title="CancerInFamily") + geom_text(stat = 'count', aes(label=after_stat(count)), vjust = -1) + xlab("CancerInFamily")
cancerInFamily
#scatter plot with Premium with jittered x
plot(jitter(as.numeric(premium.dt$CancerInFamily)), premium.dt$Premium, ylab = "Premium", xlab = "CancerInFamily")
#boxplot with Premium
ggplot(data = premium.dt, aes(y = Premium, fill = CancerInFamily)) + geom_boxplot(show.legend = FALSE) + labs(title="Premium against CancerInFamily") + facet_grid(.~CancerInFamily)
num0 <- subset(premium.dt$Premium,premium.dt$CancerInFamily == 0)
summary(num0)
num1 <- subset(premium.dt$Premium,premium.dt$CancerInFamily == 1)
summary(num1)

#Gender
#bar plot
gender = ggplot(data = premium.dt, aes(x = Gender2, fill = Gender2)) + geom_bar(show.legend = FALSE) + labs(title="Gender") + geom_text(stat = 'count', aes(label=after_stat(count)), vjust = -1) + xlab("Gender")
gender
#scatter plot with Premium with jittered x
plot(jitter(as.numeric(premium.dt$Gender)), premium.dt$Premium, ylab = "Premium", xlab = "Gender")
#boxplot with Premium
ggplot(data = premium.dt, aes(y = Premium, fill = Gender2)) + geom_boxplot(show.legend = FALSE) + labs(title="Premium against Gender") + facet_grid(.~Gender2)
num0 <- subset(premium.dt$Premium,premium.dt$Gender == 0)
summary(num0)
num1 <- subset(premium.dt$Premium,premium.dt$Gender == 1)
summary(num1)

#Weight_status
#bar plot
weight_status = ggplot(data = premium.dt, aes(x = Weight_status2, fill = Weight_status2)) + geom_bar(show.legend = FALSE) + labs(title="Weight_status") + geom_text(stat = 'count', aes(label=after_stat(count)), vjust = -1) + xlab("Weight_status")
weight_status
#scatter plot with Premium with jittered x
plot(jitter(as.numeric(premium.dt$Weight_status)), premium.dt$Premium, ylab = "Premium", xlab = "Weight_status")
#boxplot with Premium
ggplot(data = premium.dt, aes(y = Premium, fill = Weight_status2)) + geom_boxplot(show.legend = FALSE) + labs(title="Premium against Weight_status") + facet_grid(.~Weight_status2)
num0 <- subset(premium.dt$Premium,premium.dt$Weight_status == 0)
summary(num0)
num1 <- subset(premium.dt$Premium,premium.dt$Weight_status == 1)
summary(num1)
num2 <- subset(premium.dt$Premium,premium.dt$Weight_status == 2)
summary(num2)
num3 <- subset(premium.dt$Premium,premium.dt$Weight_status == 3)
summary(num3)

# ==============================================================================================================
#4. Using 1 SE optimal CART and one other technique learnt in this course:
#a. What is the 10-fold cross validation RMSE and number of splits in the 1SE Optimal CART?
# ==============================================================================================================
#drop the extra datasets before  machine learning
premium.dt <- premium.dt[, -(15:22)]

#apply CART on the entire dataset first
cart1 <- rpart(Premium ~ ., data = premium.dt, method = "anova", control = rpart.control(minsplit = 2, cp = 0))

#plot the tree model
#rpart.plot(cart1, nn = T, main = "Maximal Tree")
print(cart1)

#check the cp values of all the nodes and plot the chart
printcp(cart1)
plotcp(cart1, main = "Prune Sequence CV errors")

#Extract the Optimal Tree via code instead of eye power
#Compute min CV error + 1SE in maximal tree cart1
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
#using the CP values of tree 9 and 10
cp1 = sqrt(1.0940e-02*1.0047e-02 )
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
#cp1 = cp.opt

# Prune to optimal CART tree
cart2 <- prune(cart1, cp = cp.opt)
rpart.plot(cart2, nn =T, main = "Optimal Tree")
print(cart2)
printcp(cart2)
dataset.RMSE <- sqrt(0.21310 * 97526)
dataset.RMSE
RMSE.cart2 <- sqrt(mean(residuals(cart2)^2))  
RMSE.cart2
CV.RMSE <- sqrt(0.24530 * 97526)
CV.RMSE

# Variable importance
cart2$variable.importance
scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt


set.seed(123)

#split the train and test sets
train <- sample.split(Y = premium.dt$Premium, SplitRatio = 0.7)
trainset <- subset(premium.dt, train == T)
testset <- subset(premium.dt, train == F)

#create a cart model using the trainset
cart1 <- rpart(Premium ~ ., data = trainset, method = "anova", control = rpart.control(minsplit = 2, cp = 0))

#plot the tree model
#rpart.plot(cart1, nn = T, main = "Maximal Tree")
print(cart1)

#check the cp values of all the nodes and plot the chart
printcp(cart1)
plotcp(cart1, main = "Prune Sequence CV errors")

#Extract the Optimal Tree via code instead of eye power
#Compute min CV error + 1SE in maximal tree cart1
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
#using the CP values of tree 4 and 5
cp1 = sqrt(2.8276e-02 * 1.9063e-02)
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
#cp1 = cp.opt

# Prune to optimal CART tree
cart2 <- prune(cart1, cp = cp.opt)
rpart.plot(cart2, nn =T, main = "Optimal Tree")
print(cart2)
printcp(cart2)
trainset.RMSE <- sqrt(0.31867 * 98094)
CV.RMSE <- sqrt(0.36936 * 98094)

#find RMSE value for testset data using trainset model
p <- predict(cart2, newdata = testset)
p
rmse(testset$Premium, p) #test error = 157.5959

# Variable importance
cart2$variable.importance
summary(cart2)



#Linear Regression
m1 <- lm(Premium ~., data = premium.dt)

#Using train test split for linear regression
set.seed(123)
train <- sample.split(Y = premium.dt$Premium, SplitRatio = 0.7)
trainset <- subset(premium.dt, train == T)
testset <- subset(premium.dt, train == F)

#ensure both sets are around the same
summary(trainset$Premium)
summary(testset$Premium)

#develop a model on trainset
m2 <- lm(Premium ~., data = trainset)
summary(m2)
## The significant variables are Age, Transplant, ChronicDisease, CancerInFamily, NumMajorSurgeries and Weight_status

RMSE.train1 <- sqrt(mean(residuals(m2)^2))
RMSE.train1 #187.5841
summary(abs(residuals(m2)))

#Apply Model from trainset to predict on testset
predict.test1 <- predict(m2, newdata = testset)
testset.error1 <- testset$Premium - predict.test1
#testset errors
RMSE.test1 <- sqrt(mean(testset.error1^2))
RMSE.test1 #183.3648
summary(abs(testset.error1))

#check for multicollinearity
#Then remove based on multicollinearity
vif(m1)
max(vif(m1))
m1.vif <- update(m1, .~.-BMI)
#as expected as BMI is calculated from height and weight

vif(m1.vif)
max(vif(m1.vif)) #max vif = 5.244917 < 10 , no need to remove further

#do it again for the new model after vif
#develop a model on trainset
summary(m1.vif)
## The significant variables are Age, Transplant, ChronicDisease, CancerInFamily and NumMajorSurgeries

RMSE.train2 <- sqrt(mean(residuals(m1.vif)^2))
RMSE.train2 #185.5352
summary(abs(residuals(m1.vif)))

#Apply Model from trainset to predict on testset
predict.test2 <- predict(m1.vif, newdata = testset)
testset.error2 <- testset$Premium - predict.test2

#testset errors
RMSE.test2 <- sqrt(mean(testset.error2^2))
RMSE.test2 #179.1065
summary(abs(testset.error2))

#Then use AIC
m1.vif.aic <- step(m1.vif)

#do it again for the new model after aic
#develop a model on trainset
summary(m1.vif.aic)
## The significant variables are Age, Transplant, ChronicDisease, CancerInFamily and NumMajorSurgeries

RMSE.train3 <- sqrt(mean(residuals(m1.vif.aic)^2))
RMSE.train3 #185.6709
summary(abs(residuals(m1.vif.aic)))

#Apply Model from trainset to predict on testset
predict.test3 <- predict(m1.vif.aic, newdata = testset)
testset.error3 <- testset$Premium - predict.test3

#testset errors
RMSE.test3 <- sqrt(mean(testset.error3^2))
RMSE.test3 #179.2729
summary(abs(testset.error3))

#Model Diagnostics Plots
par(mfrow = c(2,2))
plot(m1.vif.aic)
par(mfrow = c(1,1))
#top left chart indicates that (the line is fairly straight and close to 0) assumptions 1 & 2 are true
#top right chart indicates that (most of the data points fall on the dotted line) assumption 2 is true
#bottom left chart indicates that (the points can make a box, and the line has a very mild gradient) assumption 3 is true
#bottom right chart indicates that there is one influential outlier, 110


  



