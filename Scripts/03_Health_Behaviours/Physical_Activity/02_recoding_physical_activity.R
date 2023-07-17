
####### Loading and Re-coding data for Vigorous Physical Activity#######
VPA_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Physical_Activity", "01_VigorousPA_4to8.csv"))
head(VPA_4to8)
# re-coding waves 4-8
for (i in 2:6) {
  VPA_4to8[, i] <- ifelse(VPA_4to8[, i] == 1, ">1/week",
    ifelse(VPA_4to8[, i] == 2, "1/week",
      ifelse(VPA_4to8[, i] == 3, "1-3/month",
        ifelse(VPA_4to8[, i] == 4, "hardly ever or never", "NA")
      )
    )
  )
}
head(VPA_4to8)

############# Re-coding data for Moderate Physical Activity############
MPA_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Physical_Activity", "01_ModeratePA_4to8.csv"))
head(MPA_4to8)

# re-coding waves 4-8
for (i in 2:6) {
  MPA_4to8[, i] <- ifelse(MPA_4to8[, i] == 1, ">1/week",
    ifelse(MPA_4to8[, i] == 2, "1/week",
      ifelse(MPA_4to8[, i] == 3, "1-3/month",
        ifelse(MPA_4to8[, i] == 4, "hardly ever or never", "NA")
      )
    )
  )
}
head(MPA_4to8)

############### Re-coding data for Light Physical Activity##############
LPA_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Physical_Activity", "01_LightPA_4to8.csv"))
head(LPA_4to8)

# re-coding waves 4-8
for (i in 2:6) {
  LPA_4to8[, i] <- ifelse(LPA_4to8[, i] == 1, ">1/week",
    ifelse(LPA_4to8[, i] == 2, "1/week",
      ifelse(LPA_4to8[, i] == 3, "1-3/month",
        ifelse(LPA_4to8[, i] == 4, "hardly ever or never", "NA")
      )
    )
  )
}
head(LPA_4to8)
################## 	EXPORT EXTRACTED DATA TO THREE OUTPUT FILES ######## ####################################

write.csv(VPA_4to8, file = "Data/Processed_Data/R_Data/Physical_Activity/02_VigorousPA_4to8_Recoded.csv", row.names = FALSE)

write.csv(MPA_4to8, file = "Data/Processed_Data/R_Data/Physical_Activity/02_ModeratePA_4to8_Recoded.csv", row.names = FALSE)

write.csv(LPA_4to8, file = "Data/Processed_Data/R_Data/Physical_Activity/02_LightPA_4to8_Recoded.csv", row.names = FALSE)
