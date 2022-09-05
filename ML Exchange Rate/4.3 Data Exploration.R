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
library(reshape2)

#Set working directory
setwd("~/Documents/NTU/Y2S1/BC2406/Project")

#Canada
canada.dt <- read_excel("3.1 Canada.xlsx", na = c("NA", "-"))
setDT(canada.dt)
canada.dt$Date = as.yearqtr(canada.dt$Date)
canada.dt$Date = as.Date(canada.dt$Date)
canada.dt = canada.dt[1:114]

#View how exchange rate changes along the years
cexrate.dt <- canada.dt[,c('Date','Exchange rate LCU:US$ (av)')]
plot <- ggplot(data = cexrate.dt, aes(x = Date, y = `Exchange rate LCU:US$ (av)`)) + geom_line() + ggtitle("Exchange rate per Quarter")
plot <- ggplotly(plot)
plot

#Clean
canada1.dt <- canada.dt[,-1]
canada1.dt = canada1.dt[,-c('Exchange rate LCU:$ (end-period)', 'M1 Money supply (LCU)', "Short term interest rate (%; end-period)", 'Stock of money M2 (LCU)', 'Export volume of goods and services (% change pa)', 'Import volume of goods and services (% change pa)')]

#rename the variables so they will be shorter
colnames(canada1.dt)
names <- c("MS", "SM", "Real GDP", "PC", "GFI", "Nominal GDP", "ER", "BB", "PB", "LIR", "DIR", "STIR", "LTBY", "CP", "GAW", "ARW", "UR", "G:E", "G:I", "PSC")
setnames(canada1.dt, names)

#plot exchange rate against the rest of the variables
#1. do it manually
#2. using ggplot
canada1.melt <- melt(canada1.dt, id = "ER", measure = c("MS", "SM", "Real GDP", "PC", "GFI", "Nominal GDP", "BB", "PB", "LIR", "DIR", "STIR", "LTBY", "CP", "GAW", "ARW", "UR", "G:E", "G:I", "PSC"))
ggplotly(ggplot(canada1.melt, aes(ER, value, colour = variable)) + geom_point() + facet_wrap(facets = vars(variable), scales = "free_y"))

#Further Cleaning
canada1.dt <- canada1.dt[, -c("PC", "GFI", "BB", "CP")]
canada1.dt <- canada1.dt[, -c("GAW", "ARW", "PSC")]

#Correlation Matrix
corCanada <- cor(canada1.dt)
View(corCanada)

# Correlation Plot
corrplot(cor(canada1.dt), type = "upper")

#focus on the response variable exchange rate
x <- canada1.dt %>% correlate() %>% focus('ER')
x

x %>% 
  mutate(term = factor(term, levels = term[order(`ER`)])) %>%  
  # Order by correlation strength
  ggplot(aes(x = term, y = `ER`)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Exchange rate") +
  xlab("Variables")


#NETHERLANDS
netherlands.dt <- read_excel("3.2 Netherlands.xlsx", na = c("NA", "-"))
setDT(netherlands.dt)
netherlands.dt$Date = as.yearqtr(netherlands.dt$Date)
netherlands.dt$Date = as.Date(netherlands.dt$Date)
netherlands.dt = netherlands.dt[1:114]

#View how exchange rate changes along the years
cexrate.dt <- netherlands.dt[,c('Date','Exchange rate LCU:US$ (av)')]
plot <- ggplot(data = cexrate.dt, aes(x = Date, y = `Exchange rate LCU:US$ (av)`)) + geom_line() + ggtitle("Exchange rate per Quarter")
plot <- ggplotly(plot)
plot

#clean
netherlands1.dt <- netherlands.dt[,-1]
netherlands1.dt = netherlands1.dt[,-c('Exchange rate LCU:$ (end-period)', 'Money market interest rate (%; end-period)', 'M1 Money supply (LCU)', 'Stock of money M2 (LCU)', 'Export volume of goods and services (% change pa)', 'Import volume of goods and services (% change pa)')]

#rename the variables so they will be shorter
colnames(netherlands1.dt)
names <- c("MS", "SM", "Real GDP", "PC", "GFI", "Nominal GDP", "ER", "BB", "PD", "LIR", "DIR", "MMIR", "LTBY", "CP", "GAW", "ARW", "UR", "G:E", "G:I", "PSC")
setnames(netherlands1.dt, names)

#plot exchange rate against the rest of the variables
#1. do it manually
#2. using ggplot
netherlands1.melt <- melt(netherlands1.dt, id = "ER", measure = c("MS", "SM", "Real GDP", "PC", "GFI", "Nominal GDP", "ER", "BB", "PD", "LIR", "DIR", "MMIR", "LTBY", "CP", "GAW", "ARW", "UR", "G:E", "G:I", "PSC"))
ggplotly(ggplot(netherlands1.melt, aes(ER, value, colour = variable)) + geom_point() + facet_wrap(facets = vars(variable), scales = "free_y"))


#further cleaning
netherlands1.dt <- netherlands1.dt[, -c("GFI", "LTBY", "ARW", "PSC")]
netherlands1.dt <- netherlands1.dt[, -c("LIR", "DIR")]

for(var in colnames(netherlands1.dt)){
  netherlands1.dt[[var]][is.na(netherlands1.dt[[var]])] <- mean(netherlands1.dt[[var]], na.rm = TRUE)
}

#Correlation Matrix
corNetherlands <- cor(netherlands1.dt)
View(corNetherlands)

# Correlation Plot
corrplot(cor(netherlands1.dt), type = "upper")

#focus on the response variable exchange rate
x <- netherlands1.dt %>% correlate() %>% focus('ER')
x

x %>% 
  mutate(term = factor(term, levels = term[order(`ER`)])) %>%  
  # Order by correlation strength
  ggplot(aes(x = term, y = `ER`)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Exchange rate") +
  xlab("Variables")


#RUSSIA
russia.dt <- read_excel("3.3 Russia.xlsx", na = c("NA", "-"))
setDT(russia.dt)
russia.dt$Date = as.yearqtr(russia.dt$Date)
russia.dt$Date = as.Date(russia.dt$Date)
russia.dt = russia.dt[1:114]

#View how exchange rate changes along the years
cexrate.dt <- russia.dt[,c('Date','Exchange rate LCU:US$ (av)')]
plot <- ggplot(data = cexrate.dt, aes(x = Date, y = `Exchange rate LCU:US$ (av)`)) + geom_line() + ggtitle("Exchange rate per Quarter")
plot <- ggplotly(plot)
plot

#clean
russia1.dt <- russia.dt[,-1]
russia1.dt = russia1.dt[,c('Exchange rate LCU:$ (end-period)', 'Money market interest rate (%; end-period)', 'M1 Money supply (LCU)', 'Stock of money M2 (LCU)', 'Export volume of goods and services (% change pa)', 'Import volume of goods and services (% change pa)'):=NULL]

#rename the variables so they will be shorter
colnames(russia1.dt)
names <- c("MS", "SM", "Real GDP", "PC", "GFI", "Nominal GDP", "ER", "BB", "PD", "LIR", "DIR", "MMIR", "LTBY", "CP", "GAW", "ARW", "UR", "G:E", "G:I", "PSC")
setnames(russia1.dt, names)

#plot exchange rate against the rest of the variables
#1. do it manually
#2. using ggplot
russia1.melt <- melt(russia1.dt, id = "ER", measure = c("MS", "SM", "Real GDP", "PC", "GFI", "Nominal GDP", "ER", "BB", "PD", "LIR", "DIR", "MMIR", "LTBY", "CP", "GAW", "ARW", "UR", "G:E", "G:I", "PSC"))
ggplotly(ggplot(russia1.melt, aes(ER, value, colour = variable)) + geom_point() + facet_wrap(facets = vars(variable), scales = "free_y"))


#further cleaning
russia1.dt <- russia1.dt[, -c("GFI", "MMIR", "ARW", "PSC")]
russia1.dt <- russia1.dt[, -c("PD")]

for(var in colnames(russia1.dt)){
  russia1.dt[[var]][is.na(russia1.dt[[var]])] <- mean(russia1.dt[[var]], na.rm = TRUE)
}

#Correlation Matrix
corRussia <- cor(russia1.dt)
View(corRussia)

# Correlation Plot
corrplot(cor(russia1.dt), type = "upper")

#focus on the response variable exchange rate
x <- russia1.dt %>% correlate() %>% focus('ER')
x

x %>% 
  mutate(term = factor(term, levels = term[order(`ER`)])) %>%  
  # Order by correlation strength
  ggplot(aes(x = term, y = `ER`)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Exchange rate") +
  xlab("Variables")


#SWEDEN
sweden.dt <- read_excel("3.4 Sweden.xlsx", na = c("NA", "-"))
setDT(sweden.dt)
sweden.dt$Date = as.yearqtr(sweden.dt$Date)
sweden.dt$Date = as.Date(sweden.dt$Date)
sweden.dt = sweden.dt[1:114]

#View how exchange rate changes along the years
cexrate.dt <- russia.dt[,c('Date','Exchange rate LCU:US$ (av)')]
plot <- ggplot(data = cexrate.dt, aes(x = Date, y = `Exchange rate LCU:US$ (av)`)) + geom_line() + ggtitle("Exchange rate per Quarter")
plot <- ggplotly(plot)
plot

#clean
sweden1.dt <- sweden.dt[,-1]
sweden1.dt = sweden1.dt[, -c('Exchange rate LCU:$ (end-period)', 'Money market interest rate (%; end-period)', 'M1 Money supply (LCU)', 'Stock of money M2 (LCU)', 'Export volume of goods and services (% change pa)', 'Import volume of goods and services (% change pa)')]

#rename the variables so they will be shorter
colnames(sweden1.dt)
names <- c("MS", "SM", "Real GDP", "PC", "GFI", "Nominal GDP", "ER", "BB", "PD", "LIR", "DIR", "MMIR", "LTBY", "CP", "GAW", "ARW", "UR", "G:E", "G:I", "PSC")
setnames(sweden1.dt, names)

#plot exchange rate against the rest of the variables
#1. do it manually
#2. using ggplot
sweden1.melt <- melt(sweden1.dt, id = "ER", measure = c("MS", "SM", "Real GDP", "PC", "GFI", "Nominal GDP", "ER", "BB", "PD", "LIR", "DIR", "MMIR", "LTBY", "CP", "GAW", "ARW", "UR", "G:E", "G:I", "PSC"))
ggplotly(ggplot(sweden1.melt, aes(ER, value, colour = variable)) + geom_point() + facet_wrap(facets = vars(variable), scales = "free_y"))

#further cleaning
sweden1.dt <- sweden1.dt[, -c("MS", "Real GDP", "PD", "PSC")]

for(var in colnames(sweden1.dt)){
  sweden1.dt[[var]][is.na(sweden1.dt[[var]])] <- mean(sweden1.dt[[var]], na.rm = TRUE)
}

#Correlation Matrix
corSweden <- cor(sweden1.dt)
View(corSweden)

# Correlation Plot
corrplot(cor(sweden1.dt), type = "upper")

#focus on the response variable exchange rate
x <- sweden1.dt %>% correlate() %>% focus('ER')
x

x %>% 
  mutate(term = factor(term, levels = term[order(`ER`)])) %>%  
  # Order by correlation strength
  ggplot(aes(x = term, y = `ER`)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Exchange rate") +
  xlab("Variables")



#UK
uk.dt <- read_excel("3.5 UK.xlsx", na = c("NA", "-"))
setDT(uk.dt)
class(uk.dt$Date)
uk.dt$Date = as.yearqtr(uk.dt$Date)
uk.dt$Date = as.Date(uk.dt$Date)
class(uk.dt$Date)
uk.dt = uk.dt[1:114]

#View how exchange rate changes along the years
cexrate.dt <- uk.dt[,c('Date','Exchange rate LCU:US$ (av)')]
plot <- ggplot(data = cexrate.dt, aes(x = Date, y = `Exchange rate LCU:US$ (av)`)) + geom_line() + ggtitle("Exchange rate per Quarter")
plot <- ggplotly(plot)
plot

#clean
uk1.dt <- uk.dt[,-1]
uk1.dt = uk1.dt[,c('Exchange rate LCU:$ (end-period)', 'Money market interest rate (%; end-period)', 'M1 Money supply (LCU)', 'Stock of money M2 (LCU)', 'Export volume of goods and services (% change pa)', 'Import volume of goods and services (% change pa)'):=NULL]

#rename the variables so they will be shorter
colnames(uk1.dt)
names <- c("MS", "SM", "Real GDP", "PC", "GFI", "Nominal GDP", "ER", "BB", "PD", "LIR", "DIR", "MMIR", "LTBY", "CP", "GAW", "ARW", "UR", "G:E", "G:I", "PSC")
setnames(uk1.dt, names)

#plot exchange rate against the rest of the variables
#1. do it manually
ggplot(data = uk1.dt[,c('MS','ER')], aes(x = MS, y = ER)) + geom_point() + ggtitle("ER against MS")
ggplot(data = uk1.dt[,c('SM','ER')], aes(x = SM, y = ER)) + geom_point() + ggtitle("ER against SM")
ggplot(data = uk1.dt[,c('Real GDP','ER')], aes(x = `Real GDP`, y = ER)) + geom_point() + ggtitle("ER against Real GDP")
ggplot(data = uk1.dt[,c('PC','ER')], aes(x = PC, y = ER)) + geom_point() + ggtitle("ER against PC")
ggplot(data = uk1.dt[,c('GFI','ER')], aes(x = GFI, y = ER)) + geom_point() + ggtitle("ER against GFI")
ggplot(data = uk1.dt[,c('Nominal GDP','ER')], aes(x = `Nominal GDP`, y = ER)) + geom_point() + ggtitle("ER against Nominal GDP")
ggplot(data = uk1.dt[,c('BB','ER')], aes(x = BB, y = ER)) + geom_point() + ggtitle("ER against BB")
ggplot(data = uk1.dt[,c('PD','ER')], aes(x = PB, y = ER)) + geom_point() + ggtitle("ER against PD")
ggplot(data = uk1.dt[,c('LIR','ER')], aes(x = LIR, y = ER)) + geom_point() + ggtitle("ER against LIR")
ggplot(data = uk1.dt[,c('DIR','ER')], aes(x = DIR, y = ER)) + geom_point() + ggtitle("ER against DIR")
ggplot(data = uk1.dt[,c('MMIR','ER')], aes(x = MMIR, y = ER)) + geom_point() + ggtitle("ER against MMIR")
ggplot(data = uk1.dt[,c('LTBY','ER')], aes(x = LTBY, y = ER)) + geom_point() + ggtitle("ER against LTBY")
ggplot(data = uk1.dt[,c('CP','ER')], aes(x = CP, y = ER)) + geom_point() + ggtitle("ER against CP")
ggplot(data = uk1.dt[,c('GAW','ER')], aes(x = GAW, y = ER)) + geom_point() + ggtitle("ER against GAW")
ggplot(data = uk1.dt[,c('ARW','ER')], aes(x = ARW, y = ER)) + geom_point() + ggtitle("ER against ARW")
ggplot(data = uk1.dt[,c('UR','ER')], aes(x = UR, y = ER)) + geom_point() + ggtitle("ER against UR")
ggplot(data = uk1.dt[,c('G:E','ER')], aes(x = `G:E`, y = ER)) + geom_point() + ggtitle("ER against G:E")
ggplot(data = uk1.dt[,c('G:I','ER')], aes(x = `G:I`, y = ER)) + geom_point() + ggtitle("ER against G:I")
ggplot(data = uk1.dt[,c('PSC','ER')], aes(x = PSC, y = ER)) + geom_point() + ggtitle("ER against PSC")

#2. using ggplot
uk1.melt <- melt(uk1.dt, id = "ER", measure = c("MS", "SM", "Real GDP", "PC", "GFI", "Nominal GDP", "BB", "PD", "LIR", "DIR", "MMIR", "LTBY", "CP", "GAW", "ARW", "UR", "G:E", "G:I", "PSC"))
ggplotly(ggplot(uk1.melt, aes(ER, value, colour = variable)) + geom_point() + facet_wrap(facets = vars(variable), scales = "free_y"))



#further cleaning
uk1.dt <- uk1.dt[, -c("Nominal GDP", "G:E", "G:I", "PSC")]

for(var in colnames(uk1.dt)){
  uk1.dt[[var]][is.na(uk1.dt[[var]])] <- mean(uk1.dt[[var]], na.rm = TRUE)
}


#Correlation Matrix
corUK <- cor(uk1.dt)
View(corUK)

# Correlation Plot
corrplot(cor(uk1.dt), type = "upper")

#focus on the response variable exchange rate
x <- uk1.dt %>% correlate() %>% focus('ER')
x

x %>% 
  mutate(term = factor(term, levels = term[order(`ER`)])) %>%  
  # Order by correlation strength
  ggplot(aes(x = term, y = `ER`)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Exchange rate") +
  xlab("Variables")

