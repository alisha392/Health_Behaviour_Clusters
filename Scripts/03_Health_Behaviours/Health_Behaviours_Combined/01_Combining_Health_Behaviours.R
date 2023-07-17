
########## 1.1. LOAD PROCESSED DATA FILES ON FOUR HEALTH BEHAVIOURS #################
Smoking_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Smoking", "01_Categorised_Smoking_4to8.csv"),
  header = TRUE
)
Drinking_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Drinking", "02_Categorised_Drinking_4to8.csv"),
  header = TRUE
)
PA_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Physical_Activity", "04_Categorised_PA_4to8.csv"),
  header = TRUE
)
FV_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Fruit_Veg_Intake", "02_Categorised_FruitVeg_4to8.csv"),
  header = TRUE
)

######### 1.3. Merging files to one dataset #########

Wave_4to8 <- list(Smoking_4to8, Drinking_4to8, PA_4to8, FV_4to8) %>%
  reduce(inner_join, by = c("idauniq")) 

### Add longitduinal weights from Wave 8 (which involve weights for those that participated from Wave 4 to Wave 8)
Wave_8_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_8_elsa_data_eul_v2.tab"),
  header = TRUE, sep = "\t"
)
Wave_8_datafile <- Wave_8_datafile %>%
  dplyr::select(idauniq, w8w4lwgt) %>%
  rename(Weights = w8w4lwgt)

Wave_4to8 <- inner_join(Wave_4to8, Wave_8_datafile, by = "idauniq")

Wave_4to8[is.na(Wave_4to8)] <- 0
head(Wave_4to8)

write.csv(Wave_4to8, file = "Data/Processed_Data/R_Data/All_Health_Behaviours/01_All_Health_Behaviours_4to8.csv", row.names = FALSE)
