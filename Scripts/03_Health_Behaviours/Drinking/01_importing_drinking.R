
############## 1.1. LOAD DATA FILES FROM ELSA WAVES 4-8 ############################
# Datafiles for physical activity and alcohol are the same
Wave_4_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_4_elsa_data_v3.tab"),
  header = TRUE, sep = "\t"
)
head(Wave_4_datafile)
Wave_5_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_5_elsa_data_v4.tab"),
  header = TRUE, sep = "\t"
)
head(Wave_5_datafile)
Wave_6_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_6_elsa_data_v2.tab"),
  header = TRUE, sep = "\t"
)
head(Wave_6_datafile)
Wave_7_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_7_elsa_data.tab"),
  header = TRUE, sep = "\t"
)
head(Wave_7_datafile)

Wave_8_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_8_elsa_data_eul_v2.tab"),
  header = TRUE, sep = "\t"
)
head(Wave_8_datafile)


############## 1.2.	EXTRACTING Alcohol DATA FROM WAVES 4-8 ############

# Waves 4-8
# scal7a - Whether respondent had an alcoholic drink in the seven days ending yesterday
# scdrspi - Number of measures of spirit the respondent had last in the last 7 days
# scdrwin - Number of glasses of wine the respondent had last in the last 7 days
# scdrpin - Number of pints of beer the respondent had last in the last 7 days

# Note - variable names are different for Wave 9

Freq_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, scal7a, scdrspi, scdrwin, scdrpin) %>%
  rename(
    Drink_Last_Week_4 = scal7a,
    Spirits_4 = scdrspi, Wine_4 = scdrwin, Beer_4 = scdrpin
  )
head(Freq_4)

Freq_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, scal7a, scdrspi, scdrwin, scdrpin) %>%
  rename(
    Drink_Last_Week_5 = scal7a,
    Spirits_5 = scdrspi, Wine_5 = scdrwin, Beer_5 = scdrpin
  )
head(Freq_5)

Freq_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, scal7a, scdrspi, scdrwin, scdrpin) %>%
  rename(
    Drink_Last_Week_6 = scal7a,
    Spirits_6 = scdrspi, Wine_6 = scdrwin, Beer_6 = scdrpin
  )
head(Freq_6)

Freq_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, scal7a, scdrspi, scdrwin, scdrpin) %>%
  rename(
    Drink_Last_Week_7 = scal7a,
    Spirits_7 = scdrspi, Wine_7 = scdrwin, Beer_7 = scdrpin
  )
head(Freq_7)

Freq_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, scal7a, scdrspi, scdrwin, scdrpin) %>%
  rename(
    Drink_Last_Week_8 = scal7a,
    Spirits_8 = scdrspi, Wine_8 = scdrwin, Beer_8 = scdrpin
  )
head(Freq_8)

Alcohol_4to8 <- list(Freq_4, Freq_5, Freq_6, Freq_7, Freq_8) %>%
  reduce(inner_join, by = c("idauniq"))
head(Alcohol_4to8)

##### Only retaining  information on the ID's that participated from Wave 4 to 9 #####

Merged_4to9_IDs <- read.csv(here("Data","Processed_Data","R_Data","Included_IDs","Merged_4to9_IDs"))

Alcohol_4to8 <- list(
  Alcohol_4to8, Merged_4to9_IDs
) %>% reduce(inner_join, by = "idauniq")


write.csv(Alcohol_4to8, file = here("Data","Processed_Data","R_Data","Drinking","01_Extracted_Drinking_4to8.csv"), row.names = FALSE)
