library(readxl)
library(data.table)
library(dplyr)

ERdata <- read_excel("3.6 Exchange Rate Data.xlsx", sheet = "Sheet1", )
setDT(ERdata)
View(ERdata)
ERdata = ERdata[-(1:40)]

ERdata_omit <- na.omit(ERdata)
View(ERdata_omit)

ERnew = ERdata_omit[ , lapply(.SD, function(v) if(uniqueN(v, na.rm = TRUE) > 1) v)] #removing all constant data
View(ERnew)

for(var in colnames(ERnew)){
  print(var)
  print(sum((ERnew[[var]]==0)))
}

# Remove the following
# [1] "Curacao"
# [1] 11
# [1] "Cyprus"
# [1] 13
# [1] "Estonia"
# [1] 10
# [1] "Faroe Islands"
# [1] 4
# [1] "Guinea"
# [1] 1
# [1] "Greece"
# [1] 20
# [1] "Greenland"
# [1] 4
# [1] "Iraq"
# [1] 1
# [1] "Libya"
# [1] 1
# [1] "Lithuania"
# [1] 6
# [1] "Latvia"
# [1] 7
# [1] "Mauritania"
# [1] 1
# [1] "Peru"
# [1] 1
# [1] "Palau"
# [1] 5
# [1] "Papua New Guinea"
# [1] 1
# [1] "Somalia"
# [1] 12
# [1] "South Sudan"
# [1] 11
# [1] "Slovak Republic"
# [1] 12
# [1] "Slovenia"
# [1] 14
# [1] "Sint Maarten (Dutch part)"
# [1] 11
# [1] "Syrian Arab Republic"
# [1] 3
# [1] "Chad"
# [1] "Turkmenistan"
# [1] 19
# [1] "Tonga"
# [1] "Uzbekistan"
# [1] 12
# [1] "Venezuela, RB"
# [1] 3
# [1] "Virgin Islands (U.S.)"
# [1] 7
# [1] "Kosovo"
# [1] 2
# [1] "Zimbabwe"
# [1] 11

ERnew = ERnew[,-c("Curacao", "Cyprus", "Estonia", "Faroe Islands", "Guinea", "Greece", "Greenland", "Iraq", "Libya", "Lithuania", "Latvia", "Mauritania", "Peru", "Palau", "Papua New Guinea", "Somalia", "South Sudan", "Slovak Republic", "Slovenia", "Sint Maarten (Dutch part)", "Syrian Arab Republic", "Chad", "Turkmenistan", "Tonga", "Uzbekistan", "Venezuela, RB", "Virgin Islands (U.S.)", "Kosovo", "Zimbabwe")]
View(ERnew)

percentagechange = c()
for(i in 2:140){
  for(j in 2:21){
    change  = abs((ERnew[j,..i]-ERnew[j-1,..i])/ERnew[j-1,..i] * 100)
    percentagechange[j] = change
  }
  ERnew[,i] = percentagechange;

    percentagechange = c();
}

meanofeverycolumn = c()
for(i in 2:140){
  meanofeverycolumn[i] = mean(unlist(ERnew[2:21,..i]))
}

meanofeverycolumn

mean(unlist(ERnew[2:21,4]))

plot <- boxplot(meanofeverycolumn,
                main = "Boxplot of Mean Percentage Change (Year on Year) of Different Countries' Exchange Rate",
                ylab = "Percentage Change",
                col = "light blue",
                outline=FALSE) #outline = FALSE removes outliers
plot$stats
#Min % change            : 0.0000281%
#Lower quartile % change : 5.00%
#Median % change         : 5.56%
#Upper quartile % change : 8.36%
#Max % change            : 13.0%
