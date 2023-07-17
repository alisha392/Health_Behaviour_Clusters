
########### Combining files on light, moderate and vigorous activity ###########
VPA_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Physical_Activity", "02_VigorousPA_4to8_Recoded.csv"))
MPA_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Physical_Activity", "02_ModeratePA_4to8_Recoded.csv"))
LPA_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Physical_Activity", "02_LightPA_4to8_Recoded.csv"))

# renaming column names

colnames(VPA_4to8) <- paste("VPA", colnames(VPA_4to8), sep = "_")
names(VPA_4to8)[1] <- paste("idauniq")
head(VPA_4to8)

colnames(MPA_4to8) <- paste("MPA", colnames(MPA_4to8), sep = "_")
names(MPA_4to8)[1] <- paste("idauniq")
head(MPA_4to8)

colnames(LPA_4to8) <- paste("LPA", colnames(LPA_4to8), sep = "_")
names(LPA_4to8)[1] <- paste("idauniq")
head(LPA_4to8)

##### merging (inner join) datasets with light, moderate and vigorous PA data #####

PAdata_4to8 <- list(
  VPA_4to8, MPA_4to8,LPA_4to8
) %>% reduce(inner_join, by = "idauniq")
head(PAdata_4to8)
######### Categorising participants into 4 categories of PA ###################

# 1   =SEDENTARY (light exercise 1–3 times a month, no moderate and vigorous activity)
# 2   =LOW (light, but no vigorous activity at least once a week),
# 3   =MODERATE (moderate activity more than once a week, or vigorous activity between once a week to 1–3 times a month)
# 4   =HIGH (vigorous activity more than once a week)
# -99 = missing


for (k in 4:8) {
  i <- k
  PAdata_4to8 <- PAdata_4to8 %>%
    mutate(Total_activity_for_Wave_k = case_when(
      PAdata_4to8[, paste(c("LPA_Wave", i), collapse = "_")] == "1-3/month" &
        PAdata_4to8[, paste(c("MPA_Wave", i), collapse = "_")] == "hardly ever or never" &
        PAdata_4to8[, paste(c("VPA_Wave", i), collapse = "_")] == "hardly ever or never"
      ~ 1,
      PAdata_4to8[, paste(c("LPA_Wave", i), collapse = "_")] == "1/week" |
        PAdata_4to8[, paste(c("LPA_Wave", i), collapse = "_")] == ">1/week" &
          PAdata_4to8[, paste(c("VPA_Wave", i), collapse = "_")] == "hardly ever or never"
      ~ 2,
      PAdata_4to8[, paste(c("MPA_Wave", i), collapse = "_")] == ">1/week" &
        (PAdata_4to8[, paste(c("VPA_Wave", i), collapse = "_")] != ">1/week")
      ~ 3,
      PAdata_4to8[, paste(c("VPA_Wave", i), collapse = "_")] == ">1/week"
      ~ 4,
      TRUE ~ -99
    ))
  names(PAdata_4to8)[names(PAdata_4to8) == "Total_activity_for_Wave_k"] <- paste(c("W", i, "_PA"), collapse = "")
}
head(PAdata_4to8)
############# Extracting Categorised PA Data for Waves 4 to 8 ###################
Categorised_PA_4to8 <- PAdata_4to8[, c(1, 17, 18, 19, 20, 21)]
head(Categorised_PA_4to8)

write.csv(Categorised_PA_4to8, file = "Data/Processed_Data/R_Data/Physical_Activity/04_Categorised_PA_4to8.csv", row.names = FALSE)

