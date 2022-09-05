#Data Cleaning for Severity Dataset

setwd("/Users/chanyixuan/Documents/NTU/Y2S2/BC2407 Analytics II/Group Project/BC2407_Sem4_Team8/Data Sets")

library(data.table)

severity_data <- fread("Severity_Data.csv", stringsAsFactors = T)

summary(severity_data)

#converting fever temperature from degrees farenheit to degrees celcius
severity_data$fever <- round((severity_data$fever -32) * (5/9),2)

#drop country and infected
severity_data = subset(severity_data, select = -c(Country, Infected))

#converting symptoms to categorical variables
severity_data$Bodypain <- as.factor(severity_data$Bodypain)
severity_data$Runny_nose <- as.factor(severity_data$Runny_nose)
severity_data$Difficulty_in_breathing <- as.factor(severity_data$Difficulty_in_breathing)
severity_data$Nasal_congestion <- as.factor(severity_data$Nasal_congestion)
severity_data$Sore_throat <- as.factor(severity_data$Sore_throat)

#converting Yes/yes in Contact_with_covid_patient to same category
for(i in 1:nrow(severity_data)){
  if(severity_data$Contact_with_covid_patient[i] == "yes"){
    severity_data$Contact_with_covid_patient[i] = "Yes"
  }
  i = i+1
}
severity_data$Contact_with_covid_patient <- droplevels(severity_data$Contact_with_covid_patient, exclude = 'yes')

colnames(severity_data)[3] <- "Temperature"

summary(severity_data)

write.csv(severity_data,"/Users/chanyixuan/Documents/NTU/Y2S2/BC2407 Analytics II/Group Project/Dataset/Severity_Data_Cleaned.csv", row.names = FALSE)
