#==================================
# ICU prediction model cleaning
#==================================
setwd("/Users/chanyixuan/Documents/NTU/Y2S2/BC2407 Analytics II/Group Project/BC2407_Sem4_Team8/Data Sets")

icu.df <- fread('ICU_Data.csv', stringsAsFactors = TRUE)
summary(icu.df) # checking datatype for each variable

# replace all outcomes RIGHT BEFORE admission into ICU as 1 and delete all OTHER rows outcome ==1 
for(i in 1:nrow(icu.df)-1){
  if(i%%5 == 0){
    next
  }
  if(i%%5 == 1){
    if(icu.df$ICU[i:(i+4)] == 1){
      i = i+4
      next
    }
    x = 1
  }
  if(icu.df$ICU[i+1] == 1 & x == 1){
    icu.df$ICU[i] = 2
    x = 0
  }
}
icu.df[, c("PATIENT_VISIT_IDENTIFIER", "WINDOW", "ICU")]


icu.df1 <- icu.df[(icu.df$ICU!=1),]
icu.df1[, c("PATIENT_VISIT_IDENTIFIER", "WINDOW", "ICU")]

icu.df1$ICU[icu.df1$ICU == 2] <- 1
icu.df1[, c("PATIENT_VISIT_IDENTIFIER", "WINDOW", "ICU")]

# DROP disease groupings
icu.df1 <- icu.df1[,`DISEASE GROUPING 1`:=NULL]
icu.df1 <- icu.df1[,`DISEASE GROUPING 2`:=NULL]
icu.df1 <- icu.df1[,`DISEASE GROUPING 3`:=NULL]
icu.df1 <- icu.df1[,`DISEASE GROUPING 4`:=NULL]
icu.df1 <- icu.df1[,`DISEASE GROUPING 5`:=NULL]
icu.df1 <- icu.df1[,`DISEASE GROUPING 6`:=NULL]
icu.df1 <- icu.df1[,`WINDOW`:=NULL]
icu.df1 <- icu.df1[,`OTHER`:=NULL]
icu.df1 <- icu.df1[,`PATIENT_VISIT_IDENTIFIER`:=NULL]
summary(icu.df1)

# Check summary and number of NAs
summary(icu.df1)
sapply(icu.df1, function(x) sum(is.na(x)))
sum(is.na(icu.df1$`HTN`))
sum(is.na(icu.df1$`IMMUNOCOMPROMISED`))

# Replace categorical variable NA with majority
icu.df1$HTN <- ifelse(is.na(icu.df1$HTN),0,icu.df1$HTN)
icu.df1$IMMUNOCOMPROMISED <- ifelse(is.na(icu.df1$IMMUNOCOMPROMISED),0,icu.df1$IMMUNOCOMPROMISED)
table(icu.df1$HTN)
table(icu.df1$IMMUNOCOMPROMISED)


# Extract variables
View(icu.df1)

icu.df2 <- icu.df1[,c("ICU","AGE_ABOVE65","AGE_PERCENTIL","GENDER","HTN","IMMUNOCOMPROMISED","ALBUMIN_MEDIAN",
                      "BE_ARTERIAL_MEDIAN","BE_VENOUS_MEDIAN","BIC_ARTERIAL_MEDIAN","BIC_VENOUS_MEDIAN","BILLIRUBIN_MEDIAN",
                      "BLAST_MEDIAN","CALCIUM_MEDIAN","CREATININ_MEDIAN","FFA_MEDIAN","GGT_MEDIAN","GLUCOSE_MEDIAN","HEMATOCRITE_MEDIAN",
                      "HEMOGLOBIN_MEDIAN","INR_MEDIAN","LACTATE_MEDIAN","LEUKOCYTES_MEDIAN","LINFOCITOS_MEDIAN","NEUTROPHILES_MEDIAN",
                      "P02_ARTERIAL_MEDIAN","P02_VENOUS_MEDIAN","PC02_ARTERIAL_MEDIAN","PC02_VENOUS_MEDIAN","PCR_MEDIAN",
                      "PH_ARTERIAL_MEDIAN","PH_VENOUS_MEDIAN","PLATELETS_MEDIAN","POTASSIUM_MEDIAN","SAT02_ARTERIAL_MEDIAN","SAT02_VENOUS_MEDIAN",
                      "SODIUM_MEDIAN","TGO_MEDIAN","TGP_MEDIAN","TTPA_MEDIAN","UREA_MEDIAN","DIMER_MEDIAN",
                      "BLOODPRESSURE_DIASTOLIC_MEDIAN", "BLOODPRESSURE_SISTOLIC_MEDIAN","HEART_RATE_MEDIAN","RESPIRATORY_RATE_MEDIAN",
                      "TEMPERATURE_MEDIAN","OXYGEN_SATURATION_MEDIAN")]

# Replace missing values using rfImpute
icu.df3<- rfImpute(ICU~.,icu.df2)
View(icu.df3)


# Check summary and number of NAs
summary(icu.df3$ICU)
sapply(icu.df3, function(x) sum(is.na(x)))

write.csv(icu.df3,"/Users/chanyixuan/Documents/NTU/Y2S2/BC2407 Analytics II/Group Project/BC2407_Sem4_Team8/Data Sets/ICU_Data_Cleaned.csv", row.names = FALSE)
