##### This step aims to merge Waves 4 to 9 to extract the IDs of ELSA participants who
# responded across all Waves 4 to 9
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

Wave_9_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_9_elsa_data_eul_v1.tab"),
                            header = TRUE, sep = "\t"
)
head(Wave_9_datafile)

Merged_4to9_IDs <- list(Wave_4_datafile, Wave_5_datafile, Wave_6_datafile, Wave_7_datafile, Wave_8_datafile,Wave_9_datafile) %>%
  reduce(inner_join, by = c("idauniq")) %>% 
  dplyr::select(idauniq)

head(Merged_4to9_IDs)

# This file records the ID's of participants who were present across all waves 4 to 9
write.csv(Merged_4to9_IDs, file = here("Data","Processed_Data","R_Data","Included_IDs","Merged_4to9_IDs"), row.names = FALSE)


