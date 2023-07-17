# Setup Environment -------------------------------------------------------

# installing loading essential packages - only needs to run once
install.packages("pacman")
require(pacman)
pacman::p_load( stringr, here, rio)
options(max.print = 100000)

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

############## 1.2.	EXTRACT PHYSICAL ACTIVITY DATA FROM WAVES 4-8 ###################
## Loading information on vigorous, moderate and light (coded as mild in ELSA files) levels of activity

Physical_Activity_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, heacta, heactb, heactc) %>%
  rename(vg_act_4 = heacta, md_act_4 = heactb, lt_act_4 = heactc)
head(Physical_Activity_4)

Physical_Activity_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, heacta, heactb, heactc) %>%
  rename(vg_act_5 = heacta, md_act_5 = heactb, lt_act_5 = heactc)
head(Physical_Activity_5)

Physical_Activity_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, HeActa, HeActb, HeActc) %>%
  rename(vg_act_6 = HeActa, md_act_6 = HeActb, lt_act_6 = HeActc)
head(Physical_Activity_6)

Physical_Activity_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, HeActa, HeActb, HeActc) %>%
  rename(vg_act_7 = HeActa, md_act_7 = HeActb, lt_act_7 = HeActc)
head(Physical_Activity_7)

Physical_Activity_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, heacta, heactb, heactc) %>%
  rename(vg_act_8 = heacta, md_act_8 = heactb, lt_act_8 = heactc)
head(Physical_Activity_8)


######### 1.3.	MERGE WAVES 4-8 PHYSICAL ACTIVITY DATA  ############################


Physical_Activity_4to8 <- list(Physical_Activity_4, Physical_Activity_5, Physical_Activity_6, Physical_Activity_7, Physical_Activity_8) %>%
  reduce(inner_join, by = c("idauniq"))
head(Physical_Activity_4to8)


##### 1.4. COMBINE WAVE 4-9 PA DATA FOR EACH INDICATOR: vigorous, moderate, light ######

Vigorous_Activity_3to8 <- Physical_Activity_4to8 %>%
  dplyr::select(idauniq, contains("vg", ignore.case = TRUE))
head(Vigorous_Activity_3to8)

Moderate_Activity_3to8 <- Physical_Activity_4to8 %>%
  dplyr::select(idauniq, contains("md", ignore.case = TRUE))
head(Moderate_Activity_3to8)

Light_Activity_3to8 <- Physical_Activity_4to8 %>%
  dplyr::select(idauniq, contains("lt", ignore.case = TRUE))

head(Light_Activity_3to8)

## relabeling columns to represent waves
names(Vigorous_Activity_3to8) <- c(
  "idauniq", "Wave_4", "Wave_5", "Wave_6",
  "Wave_7", "Wave_8"
)
names(Moderate_Activity_3to8) <- c(
  "idauniq", "Wave_4", "Wave_5", "Wave_6",
  "Wave_7", "Wave_8"
)
names(Light_Activity_3to8) <- c(
  "idauniq", "Wave_4", "Wave_5", "Wave_6",
  "Wave_7", "Wave_8"
)

##### Only retaining  information on the ID's that participated from Wave 4 to 9 #####
Merged_4to9_IDs <- read.csv("Data/Processed_Data/R_Data/Included_IDs/Merged_4to9_IDs")

Vigorous_Activity_3to8 <- list(
  Vigorous_Activity_3to8, Merged_4to9_IDs
) %>% reduce(inner_join, by = "idauniq")


Moderate_Activity_3to8 <- list(
  Moderate_Activity_3to8, Merged_4to9_IDs
) %>% reduce(inner_join, by = "idauniq")


Light_Activity_3to8 <- list(
  Light_Activity_3to8, Merged_4to9_IDs
) %>% reduce(inner_join, by = "idauniq")


################## 1.5.	EXPORT EXTRACTED DATA TO THREE OUTPUT FILES ######## ####################################

write.csv(Vigorous_Activity_3to8,
  file = "Data/Processed_Data/R_Data/Physical_Activity/01_VigorousPA_4to8.csv",
  row.names = FALSE
)

write.csv(Moderate_Activity_3to8, file = "Data/Processed_Data/R_Data/Physical_Activity/01_ModeratePA_4to8.csv", row.names = FALSE)

write.csv(Light_Activity_3to8, file = "Data/Processed_Data/R_Data/Physical_Activity/01_LightPA_4to8.csv", row.names = FALSE)
