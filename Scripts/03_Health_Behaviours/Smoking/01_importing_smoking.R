
############## 1.1. LOAD DATA FILES FROM ELSA WAVES 4-9 ############################

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


# Extracting respondents' data on current smoking status in Waves 4-8

Smoking_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, heska) %>%
  rename(W4_Smoke = heska)
head(Smoking_4)

Smoking_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, heska) %>%
  rename(W5_Smoke = heska)
head(Smoking_5)

Smoking_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, HESka) %>%
  rename(W6_Smoke = HESka)
head(Smoking_6)

Smoking_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, HESka) %>%
  rename(W7_Smoke = HESka)
head(Smoking_7)

Smoking_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, heska) %>%
  rename(W8_Smoke = heska)
head(Smoking_8)


################## Changing codes #########

# Original codes for current smoking status  1: YES, 2: NO
# New code for current smoking status  1: YES, 0: NO

ChangeCoding <- function(x) {
  x[2] <- replace(x[2], x[2] == 2, 0)
  return(x)
}
Smoking_4 <- ChangeCoding(Smoking_4)
Smoking_5 <- ChangeCoding(Smoking_5)
Smoking_6 <- ChangeCoding(Smoking_6)
Smoking_7 <- ChangeCoding(Smoking_7)
Smoking_8 <- ChangeCoding(Smoking_8)
################# Combining smoking data from Waves 4-9 ########################

Smoking_4to8 <- list(Smoking_4, Smoking_5, Smoking_6, Smoking_7, Smoking_8) %>%
  reduce(inner_join, by = c("idauniq"))
head(Smoking_4to8)

####### converting all NAs to -99 to standardise missing values
Smoking_4to8[is.na(Smoking_4to8)] <- -99

Smoking_4to8[Smoking_4to8[, ] < 0] <- -99

##### Only retaining  information on the ID's that participated from Wave 4 to 9 #####

Merged_4to9_IDs <- read.csv(here("Data","Processed_Data","R_Data","Included_IDs","Merged_4to9_IDs"))

Smoking_4to8 <- list(
  Smoking_4to8, Merged_4to9_IDs
) %>% reduce(inner_join, by = "idauniq")

write.csv(Smoking_4to8, file = "Data/Processed_Data/R_Data/Smoking/01_Categorised_Smoking_4to8.csv", row.names = FALSE)
