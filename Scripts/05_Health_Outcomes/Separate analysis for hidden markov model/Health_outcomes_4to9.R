Wave_4_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", 
                                 "wave_4_elsa_data_v3.tab"), header = TRUE, sep = "\t")

Wave_5_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_5_elsa_data_v4.tab"),
                            header = TRUE, sep = "\t"
)

Wave_6_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_6_elsa_data_v2.tab"),
                            header = TRUE, sep = "\t"
)

Wave_7_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_7_elsa_data.tab"),
                            header = TRUE, sep = "\t"
)

Wave_8_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_8_elsa_data_eul_v2.tab"),
                            header = TRUE, sep = "\t"
)


Wave_9_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_9_elsa_data_eul_v1.tab"),
                            header = TRUE, sep = "\t"
)


##### Blood pressure data (wave 4 to 9) #####
# hedimbp :high blood pressure diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawbp :Diagnosed high BP fed forward (1:high BP,<0: missing)
# if hedawbp =1, then hedacbp : whether confirms high BP (1:Yes, 2:No, other values (positive or negative: missing)
# if hedacbp=2, then hedanbp: Reason disputed high BP diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedawbp =1, then hedasbp :whether still has high BP (1:Yes, 2:No, other values (positive or negative: missing)

Blood_Pressure_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedimbp, hedawbp, hedanbp, hedacbp, hedasbp) %>%
  rename(
    Newly_Reported_BP = hedimbp, Fed_Forward_BP = hedawbp, Confirmed_diagnosis_BP = hedacbp, Disputed_diagnosis_BP = hedanbp,
    Still_Has_BP = hedasbp
  ) %>%
  mutate(Blood_Pressure = case_when(
    (Fed_Forward_BP == 1 & Confirmed_diagnosis_BP == 1 & Still_Has_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 & Disputed_diagnosis_BP == 3)) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Blood_Pressure)
head(Blood_Pressure_Data_4)

Blood_Pressure_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedimbp, hedawbp, hedanbp, hedacbp, hedasbp) %>%
  rename(
    Newly_Reported_BP = hedimbp, Fed_Forward_BP = hedawbp, Confirmed_diagnosis_BP = hedacbp, Disputed_diagnosis_BP = hedanbp,
    Still_Has_BP = hedasbp
  ) %>%
  mutate(Blood_Pressure = case_when(
    (Fed_Forward_BP == 1 & Confirmed_diagnosis_BP == 1 & Still_Has_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 & Disputed_diagnosis_BP == 3)) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Blood_Pressure)
head(Blood_Pressure_Data_5)

Blood_Pressure_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedimbp, hedawbp, hedanbp, hedacbp, hedasbp) %>%
  rename(
    Newly_Reported_BP = hedimbp, Fed_Forward_BP = hedawbp, Confirmed_diagnosis_BP = hedacbp, Disputed_diagnosis_BP = hedanbp,
    Still_Has_BP = hedasbp
  ) %>%
  mutate(Blood_Pressure = case_when(
    (Fed_Forward_BP == 1 & Confirmed_diagnosis_BP == 1 & Still_Has_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 & Disputed_diagnosis_BP == 3)) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Blood_Pressure)
head(Blood_Pressure_Data_6)

Blood_Pressure_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedimbp, hedawbp, hedanbp, hedacbp, hedasbp) %>%
  rename(
    Newly_Reported_BP = hedimbp, Fed_Forward_BP = hedawbp, Confirmed_diagnosis_BP = hedacbp, Disputed_diagnosis_BP = hedanbp,
    Still_Has_BP = hedasbp
  ) %>%
  mutate(Blood_Pressure = case_when(
    (Fed_Forward_BP == 1 & Confirmed_diagnosis_BP == 1 & Still_Has_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 & Disputed_diagnosis_BP == 3)) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Blood_Pressure)
head(Blood_Pressure_Data_7)

Blood_Pressure_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedimbp, hedawbp, hedanbp, hedacbp, hedasbp) %>%
  rename(
    Newly_Reported_BP = hedimbp, Fed_Forward_BP = hedawbp, Confirmed_diagnosis_BP = hedacbp, Disputed_diagnosis_BP = hedanbp,
    Still_Has_BP = hedasbp
  ) %>%
  mutate(Blood_Pressure = case_when(
    (Fed_Forward_BP == 1 & Confirmed_diagnosis_BP == 1 & Still_Has_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 & Disputed_diagnosis_BP == 3)) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Blood_Pressure)
head(Blood_Pressure_Data_8)

Blood_Pressure_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedimbp, hedawbp, hedanbp, hedacbp, hedasbp) %>%
  rename(
    Newly_Reported_BP = hedimbp, Fed_Forward_BP = hedawbp, Confirmed_diagnosis_BP = hedacbp, Disputed_diagnosis_BP = hedanbp,
    Still_Has_BP = hedasbp
  ) %>%
  mutate(Blood_Pressure = case_when(
    (Fed_Forward_BP == 1 & Confirmed_diagnosis_BP == 1 & Still_Has_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 & Disputed_diagnosis_BP == 3)) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 1) ~ 1,
    ((Fed_Forward_BP != 1 | Confirmed_diagnosis_BP != 1 | Still_Has_BP != 1) & Newly_Reported_BP == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Blood_Pressure)
head(Blood_Pressure_Data_9)
#### Extracting data on diabetes (wave 4 to 9) ####
# hedimdi :diabetes diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawdi :Diagnosed diabetes fed forward (7:Diabetes,<0: missing)
# if hedawdi =7, then hedacdi : whether confirms diabetes (1:Yes, 2:No, other values (positive or negative: missing)
# if hedacdi=2, then hedandi: Reason disputed diabetes diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# no question for 'still has diabetes'
Diabetes_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedimdi, hedawdi, hedacdi, hedandi) %>%
  rename(Newly_Reported_Diabetes = hedimdi, Fed_Forward_Diabetes = hedawdi, Confirmed_diagnosis_Diabetes = hedacdi, Disputed_diagnosis_Diabetes = hedandi) %>%
  mutate(Diabetes = case_when(
    (Fed_Forward_Diabetes == 7 & Confirmed_diagnosis_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 & Disputed_diagnosis_Diabetes == 3)) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetes)

head(Diabetes_Data_4)

Diabetes_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedimdi, hedawdi, hedacdi, hedandi) %>%
  rename(Newly_Reported_Diabetes = hedimdi, Fed_Forward_Diabetes = hedawdi, Confirmed_diagnosis_Diabetes = hedacdi, Disputed_diagnosis_Diabetes = hedandi) %>%
  mutate(Diabetes = case_when(
    (Fed_Forward_Diabetes == 7 & Confirmed_diagnosis_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 & Disputed_diagnosis_Diabetes == 3)) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetes)

head(Diabetes_Data_5)

Diabetes_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedimdi, hedawdi, hedacdi, hedandi) %>%
  rename(Newly_Reported_Diabetes = hedimdi, Fed_Forward_Diabetes = hedawdi, Confirmed_diagnosis_Diabetes = hedacdi, Disputed_diagnosis_Diabetes = hedandi) %>%
  mutate(Diabetes = case_when(
    (Fed_Forward_Diabetes == 7 & Confirmed_diagnosis_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 & Disputed_diagnosis_Diabetes == 3)) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetes)

head(Diabetes_Data_6)

Diabetes_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedimdi, hedawdi, hedacdi, hedandi) %>%
  rename(Newly_Reported_Diabetes = hedimdi, Fed_Forward_Diabetes = hedawdi, Confirmed_diagnosis_Diabetes = hedacdi, Disputed_diagnosis_Diabetes = hedandi) %>%
  mutate(Diabetes = case_when(
    (Fed_Forward_Diabetes == 7 & Confirmed_diagnosis_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 & Disputed_diagnosis_Diabetes == 3)) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetes)

head(Diabetes_Data_7)

Diabetes_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedimdi, hedawdi, hedacdi, hedandi) %>%
  rename(Newly_Reported_Diabetes = hedimdi, Fed_Forward_Diabetes = hedawdi, Confirmed_diagnosis_Diabetes = hedacdi, Disputed_diagnosis_Diabetes = hedandi) %>%
  mutate(Diabetes = case_when(
    (Fed_Forward_Diabetes == 7 & Confirmed_diagnosis_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 & Disputed_diagnosis_Diabetes == 3)) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetes)

head(Diabetes_Data_8)

Diabetes_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedimdi, hedawdi, hedacdi, hedandi) %>%
  rename(Newly_Reported_Diabetes = hedimdi, Fed_Forward_Diabetes = hedawdi, Confirmed_diagnosis_Diabetes = hedacdi, Disputed_diagnosis_Diabetes = hedandi) %>%
  mutate(Diabetes = case_when(
    (Fed_Forward_Diabetes == 7 & Confirmed_diagnosis_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 & Disputed_diagnosis_Diabetes == 3)) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 1) ~ 1,
    ((Fed_Forward_Diabetes != 7 | Confirmed_diagnosis_Diabetes != 1) & Newly_Reported_Diabetes == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetes)

head(Diabetes_Data_9)

#### Extracting data on cancer (wave 4 to 9) ####
# hedibca :cancer diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwca :Diagnosed cancer fed forward (5:cancer,<0: missing)
# if hedbwca =5, then hedbdca : whether confirms cancer (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdca=2, then hedbmca: Reason disputed cancer diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwca =1, then hedbsca :whether still has cancer (1:Yes, 2:No, other values (positive or negative: missing)
Cancer_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedibca, hedbwca, hedbdca, hedbmca, hedbsca) %>%
  rename(
    Newly_Reported_Cancer = hedibca, Fed_Forward_Cancer = hedbwca, Confirmed_diagnosis_Cancer = hedbdca, Disputed_diagnosis_Cancer = hedbmca,
    Still_Has_Cancer = hedbsca
  ) %>%
  mutate(Cancer = case_when(
    (Fed_Forward_Cancer == 5 & Confirmed_diagnosis_Cancer == 1 & Still_Has_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 & Disputed_diagnosis_Cancer == 3)) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cancer)
head(Cancer_Data_4, 20)

Cancer_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedibca, hedbwca, hedbdca, hedbmca, hedbsca) %>%
  rename(
    Newly_Reported_Cancer = hedibca, Fed_Forward_Cancer = hedbwca, Confirmed_diagnosis_Cancer = hedbdca, Disputed_diagnosis_Cancer = hedbmca,
    Still_Has_Cancer = hedbsca
  ) %>%
  mutate(Cancer = case_when(
    (Fed_Forward_Cancer == 5 & Confirmed_diagnosis_Cancer == 1 & Still_Has_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 & Disputed_diagnosis_Cancer == 3)) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cancer)
head(Cancer_Data_5)

Cancer_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedibca, hedbwca, hedbdca, hedbmca, hedbsca) %>%
  rename(
    Newly_Reported_Cancer = hedibca, Fed_Forward_Cancer = hedbwca, Confirmed_diagnosis_Cancer = hedbdca, Disputed_diagnosis_Cancer = hedbmca,
    Still_Has_Cancer = hedbsca
  ) %>%
  mutate(Cancer = case_when(
    (Fed_Forward_Cancer == 5 & Confirmed_diagnosis_Cancer == 1 & Still_Has_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 & Disputed_diagnosis_Cancer == 3)) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cancer)
head(Cancer_Data_6)

Cancer_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedibca, hedbwca, hedbdca, hedbmca, hedbsca) %>%
  rename(
    Newly_Reported_Cancer = hedibca, Fed_Forward_Cancer = hedbwca, Confirmed_diagnosis_Cancer = hedbdca, Disputed_diagnosis_Cancer = hedbmca,
    Still_Has_Cancer = hedbsca
  ) %>%
  mutate(Cancer = case_when(
    (Fed_Forward_Cancer == 5 & Confirmed_diagnosis_Cancer == 1 & Still_Has_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 & Disputed_diagnosis_Cancer == 3)) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cancer)
head(Cancer_Data_7)

Cancer_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedibca, hedbwca, hedbdca, hedbmca, hedbsca) %>%
  rename(
    Newly_Reported_Cancer = hedibca, Fed_Forward_Cancer = hedbwca, Confirmed_diagnosis_Cancer = hedbdca, Disputed_diagnosis_Cancer = hedbmca,
    Still_Has_Cancer = hedbsca
  ) %>%
  mutate(Cancer = case_when(
    (Fed_Forward_Cancer == 5 & Confirmed_diagnosis_Cancer == 1 & Still_Has_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 & Disputed_diagnosis_Cancer == 3)) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cancer)
head(Cancer_Data_8, 20)

Cancer_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedibca, hedbwca, hedbdca, hedbmca, hedbsca) %>%
  rename(
    Newly_Reported_Cancer = hedibca, Fed_Forward_Cancer = hedbwca, Confirmed_diagnosis_Cancer = hedbdca, Disputed_diagnosis_Cancer = hedbmca,
    Still_Has_Cancer = hedbsca
  ) %>%
  mutate(Cancer = case_when(
    (Fed_Forward_Cancer == 5 & Confirmed_diagnosis_Cancer == 1 & Still_Has_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 & Disputed_diagnosis_Cancer == 3)) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 1) ~ 1,
    ((Fed_Forward_Cancer != 5 | Confirmed_diagnosis_Cancer != 1 | Still_Has_Cancer != 1) & Newly_Reported_Cancer == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cancer)
head(Cancer_Data_9)

#### Extracting data on lung disease (wave 4 to 9) ####
# hediblu :Chronic lung disease diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwlu :Diagnosed Chronic lung disease fed forward (1:Chronic lung disease,<0: missing)
# if hedbwlu =1, then hedbdlu : whether confirms Chronic lung disease (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdlu=2, then hedbmlu: Reason disputed Chronic lung disease diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwlu =1, then hedblu (hedbslu for wave 4) :whether still has Chronic lung disease (1:Yes, 2:No, other values (positive or negative: missing)
Lung_Disease_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hediblu, hedbwlu, hedbdlu, hedbmlu, hedbslu) %>%
  rename(
    Newly_Reported_Lung_Disease = hediblu, Fed_Forward_Lung_Disease = hedbwlu, Confirmed_diagnosis_Lung_Disease = hedbdlu, Disputed_diagnosis_Lung_Disease = hedbmlu,
    Still_Has_Lung_Disease = hedbslu
  ) %>%
  mutate(Lung_Disease = case_when(
    (Fed_Forward_Lung_Disease == 1 & Confirmed_diagnosis_Lung_Disease == 1 & Still_Has_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 & Disputed_diagnosis_Lung_Disease == 3)) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Lung_Disease)

head(Lung_Disease_Data_4)

Lung_Disease_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hediblu, hedbwlu, hedbdlu, hedbmlu, hedblu) %>%
  rename(
    Newly_Reported_Lung_Disease = hediblu, Fed_Forward_Lung_Disease = hedbwlu, Confirmed_diagnosis_Lung_Disease = hedbdlu, Disputed_diagnosis_Lung_Disease = hedbmlu,
    Still_Has_Lung_Disease = hedblu) %>%
  mutate(Lung_Disease = case_when(
    (Fed_Forward_Lung_Disease == 1 & Confirmed_diagnosis_Lung_Disease == 1 & Still_Has_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 & Disputed_diagnosis_Lung_Disease == 3)) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Lung_Disease)

head(Lung_Disease_Data_5)

Lung_Disease_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hediblu, hedbwlu, hedbdlu, hedbmlu, hedblu) %>%
  rename(
    Newly_Reported_Lung_Disease = hediblu, Fed_Forward_Lung_Disease = hedbwlu, Confirmed_diagnosis_Lung_Disease = hedbdlu, Disputed_diagnosis_Lung_Disease = hedbmlu,
    Still_Has_Lung_Disease = hedblu) %>%
  mutate(Lung_Disease = case_when(
    (Fed_Forward_Lung_Disease == 1 & Confirmed_diagnosis_Lung_Disease == 1 & Still_Has_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 & Disputed_diagnosis_Lung_Disease == 3)) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Lung_Disease)

head(Lung_Disease_Data_6)

Lung_Disease_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hediblu, hedbwlu, hedbdlu, hedbmlu, hedblu) %>%
  rename(
    Newly_Reported_Lung_Disease = hediblu, Fed_Forward_Lung_Disease = hedbwlu, Confirmed_diagnosis_Lung_Disease = hedbdlu, Disputed_diagnosis_Lung_Disease = hedbmlu,
    Still_Has_Lung_Disease = hedblu) %>%
  mutate(Lung_Disease = case_when(
    (Fed_Forward_Lung_Disease == 1 & Confirmed_diagnosis_Lung_Disease == 1 & Still_Has_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 & Disputed_diagnosis_Lung_Disease == 3)) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Lung_Disease)

head(Lung_Disease_Data_7)

Lung_Disease_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hediblu, hedbwlu, hedbdlu, hedbmlu, hedblu) %>%
  rename(
    Newly_Reported_Lung_Disease = hediblu, Fed_Forward_Lung_Disease = hedbwlu, Confirmed_diagnosis_Lung_Disease = hedbdlu, Disputed_diagnosis_Lung_Disease = hedbmlu,
    Still_Has_Lung_Disease = hedblu) %>%
  mutate(Lung_Disease = case_when(
    (Fed_Forward_Lung_Disease == 1 & Confirmed_diagnosis_Lung_Disease == 1 & Still_Has_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 & Disputed_diagnosis_Lung_Disease == 3)) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Lung_Disease)

head(Lung_Disease_Data_8)

Lung_Disease_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hediblu, hedbwlu, hedbdlu, hedbmlu, hedblu) %>%
  rename(
    Newly_Reported_Lung_Disease = hediblu, Fed_Forward_Lung_Disease = hedbwlu, Confirmed_diagnosis_Lung_Disease = hedbdlu, Disputed_diagnosis_Lung_Disease = hedbmlu,
    Still_Has_Lung_Disease = hedblu
  ) %>%
  mutate(Lung_Disease = case_when(
    (Fed_Forward_Lung_Disease == 1 & Confirmed_diagnosis_Lung_Disease == 1 & Still_Has_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 & Disputed_diagnosis_Lung_Disease == 3)) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 1) ~ 1,
    ((Fed_Forward_Lung_Disease != 1 | Confirmed_diagnosis_Lung_Disease != 1 | Still_Has_Lung_Disease != 1) & Newly_Reported_Lung_Disease == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Lung_Disease)

head(Lung_Disease_Data_9)

#### Extracting data on stroke (wave 4 to 9) ####
# hedimst : Stroke diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawst :Diagnosed  Stroke fed forward (8: Stroke,<0: missing)
# if hedawst =8, then hedacst : whether confirms  Stroke (1:Yes, 2:No, other values (positive or negative: missing)
# if hedacst=2, then hedanst: Reason disputed  Stroke diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# no question for 'still has stroke'
Stroke_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedimst, hedawst, hedacst, hedanst) %>%
  rename(
    Newly_Reported_Stroke = hedimst, Fed_Forward_Stroke = hedawst, Confirmed_diagnosis_Stroke = hedacst,
    Disputed_diagnosis_Stroke = hedanst
  ) %>%
  mutate(Stroke = case_when(
    (Fed_Forward_Stroke == 8 & Confirmed_diagnosis_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 & Disputed_diagnosis_Stroke == 3)) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Stroke)

head(Stroke_Data_4)

Stroke_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedimst, hedawst, hedacst, hedanst) %>%
  rename(
    Newly_Reported_Stroke = hedimst, Fed_Forward_Stroke = hedawst, Confirmed_diagnosis_Stroke = hedacst,
    Disputed_diagnosis_Stroke = hedanst
  ) %>%
  mutate(Stroke = case_when(
    (Fed_Forward_Stroke == 8 & Confirmed_diagnosis_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 & Disputed_diagnosis_Stroke == 3)) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Stroke)

head(Stroke_Data_5)

Stroke_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedimst, hedawst, hedacst, hedanst) %>%
  rename(
    Newly_Reported_Stroke = hedimst, Fed_Forward_Stroke = hedawst, Confirmed_diagnosis_Stroke = hedacst,
    Disputed_diagnosis_Stroke = hedanst
  ) %>%
  mutate(Stroke = case_when(
    (Fed_Forward_Stroke == 8 & Confirmed_diagnosis_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 & Disputed_diagnosis_Stroke == 3)) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Stroke)

head(Stroke_Data_6)

Stroke_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedimst, hedawst, hedacst, hedanst) %>%
  rename(
    Newly_Reported_Stroke = hedimst, Fed_Forward_Stroke = hedawst, Confirmed_diagnosis_Stroke = hedacst,
    Disputed_diagnosis_Stroke = hedanst
  ) %>%
  mutate(Stroke = case_when(
    (Fed_Forward_Stroke == 8 & Confirmed_diagnosis_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 & Disputed_diagnosis_Stroke == 3)) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Stroke)

head(Stroke_Data_7)

Stroke_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedimst, hedawst, hedacst, hedanst) %>%
  rename(
    Newly_Reported_Stroke = hedimst, Fed_Forward_Stroke = hedawst, Confirmed_diagnosis_Stroke = hedacst,
    Disputed_diagnosis_Stroke = hedanst
  ) %>%
  mutate(Stroke = case_when(
    (Fed_Forward_Stroke == 8 & Confirmed_diagnosis_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 & Disputed_diagnosis_Stroke == 3)) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Stroke)

head(Stroke_Data_8)

Stroke_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedimst, hedawst, hedacst, hedanst) %>%
  rename(
    Newly_Reported_Stroke = hedimst, Fed_Forward_Stroke = hedawst, Confirmed_diagnosis_Stroke = hedacst,
    Disputed_diagnosis_Stroke = hedanst
  ) %>%
  mutate(Stroke = case_when(
    (Fed_Forward_Stroke == 8 & Confirmed_diagnosis_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 & Disputed_diagnosis_Stroke == 3)) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 1) ~ 1,
    ((Fed_Forward_Stroke != 8 | Confirmed_diagnosis_Stroke != 1) & Newly_Reported_Stroke == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Stroke)

head(Stroke_Data_9)

#### Extracting data on osteoporosis (wave 4 to 9) ####
# hedibos :osteoperosis diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwos :Diagnosed osteoperosis fed forward (4:osteoperosis,<0: missing)
# if hedbwos =4, then hedbdos : whether confirms osteoperosis (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdos=2, then hedbmos: Reason disputed osteoperosis diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwos =4, then hedbsos :whether still has osteoperosis (1:Yes, 2:No, other values (positive or negative: missing)
Osteoperosis_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedibos, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Osteoperosis = hedibos, Fed_Forward_Osteoperosis = hedbwos, Confirmed_diagnosis_Osteoperosis = hedbdos, Disputed_diagnosis_Osteoperosis = hedbmos,
    Still_Has_Osteoperosis = hedbsos
  ) %>%
  mutate(Osteoperosis = case_when(
    (Fed_Forward_Osteoperosis == 4 & Confirmed_diagnosis_Osteoperosis == 1 & Still_Has_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 & Disputed_diagnosis_Osteoperosis == 3)) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Osteoperosis)
head(Osteoperosis_Data_4)

Osteoperosis_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedibos, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Osteoperosis = hedibos, Fed_Forward_Osteoperosis = hedbwos, Confirmed_diagnosis_Osteoperosis = hedbdos, Disputed_diagnosis_Osteoperosis = hedbmos,
    Still_Has_Osteoperosis = hedbsos
  ) %>%
  mutate(Osteoperosis = case_when(
    (Fed_Forward_Osteoperosis == 4 & Confirmed_diagnosis_Osteoperosis == 1 & Still_Has_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 & Disputed_diagnosis_Osteoperosis == 3)) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Osteoperosis)
head(Osteoperosis_Data_5)

Osteoperosis_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedibos, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Osteoperosis = hedibos, Fed_Forward_Osteoperosis = hedbwos, Confirmed_diagnosis_Osteoperosis = hedbdos, Disputed_diagnosis_Osteoperosis = hedbmos,
    Still_Has_Osteoperosis = hedbsos
  ) %>%
  mutate(Osteoperosis = case_when(
    (Fed_Forward_Osteoperosis == 4 & Confirmed_diagnosis_Osteoperosis == 1 & Still_Has_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 & Disputed_diagnosis_Osteoperosis == 3)) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Osteoperosis)
head(Osteoperosis_Data_6)

Osteoperosis_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedibos, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Osteoperosis = hedibos, Fed_Forward_Osteoperosis = hedbwos, Confirmed_diagnosis_Osteoperosis = hedbdos, Disputed_diagnosis_Osteoperosis = hedbmos,
    Still_Has_Osteoperosis = hedbsos
  ) %>%
  mutate(Osteoperosis = case_when(
    (Fed_Forward_Osteoperosis == 4 & Confirmed_diagnosis_Osteoperosis == 1 & Still_Has_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 & Disputed_diagnosis_Osteoperosis == 3)) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Osteoperosis)
head(Osteoperosis_Data_7)

Osteoperosis_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedibos, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Osteoperosis = hedibos, Fed_Forward_Osteoperosis = hedbwos, Confirmed_diagnosis_Osteoperosis = hedbdos, Disputed_diagnosis_Osteoperosis = hedbmos,
    Still_Has_Osteoperosis = hedbsos
  ) %>%
  mutate(Osteoperosis = case_when(
    (Fed_Forward_Osteoperosis == 4 & Confirmed_diagnosis_Osteoperosis == 1 & Still_Has_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 & Disputed_diagnosis_Osteoperosis == 3)) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Osteoperosis)
head(Osteoperosis_Data_8)

Osteoperosis_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedibos, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Osteoperosis = hedibos, Fed_Forward_Osteoperosis = hedbwos, Confirmed_diagnosis_Osteoperosis = hedbdos, Disputed_diagnosis_Osteoperosis = hedbmos,
    Still_Has_Osteoperosis = hedbsos
  ) %>%
  mutate(Osteoperosis = case_when(
    (Fed_Forward_Osteoperosis == 4 & Confirmed_diagnosis_Osteoperosis == 1 & Still_Has_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 & Disputed_diagnosis_Osteoperosis == 3)) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 1) ~ 1,
    ((Fed_Forward_Osteoperosis != 4 | Confirmed_diagnosis_Osteoperosis != 1 | Still_Has_Osteoperosis != 1) & Newly_Reported_Osteoperosis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Osteoperosis)
head(Osteoperosis_Data_9)

#### Extracting data on abnormal heart rhythms (wave 4 to 9) ####
# hedimar :abnormal heart rhythm diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawar :Diagnosed abnormal heart rhythm fed forward (6:abnormal heart rhythm,<0: missing)
# if hedawar =6, then hedacar : whether confirms abnormal heart rhythm (1:Yes, 2:No, other values (positive or negative: missing)
# if hedacar=2, then hedanar: Reason disputed abnormal heart rhythm diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedawar =6, then hedasar :whether still has abnormal heart rhythm (1:Yes, 2:No, other values (positive or negative: missing)
Abnormal_Heart_Rhythm_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedimar, hedawar, hedacar, hedanar, hedasar) %>%
  rename(
    Newly_Reported_Abnormal_Heart_Rhythm = hedimar, Fed_Forward_Abnormal_Heart_Rhythm = hedawar, Confirmed_diagnosis_Abnormal_Heart_Rhythm = hedacar, Disputed_diagnosis_Abnormal_Heart_Rhythm = hedanar,
    Still_Has_Abnormal_Heart_Rhythm = hedasar
  ) %>%
  mutate(Abnormal_Heart_Rhythm = case_when(
    (Fed_Forward_Abnormal_Heart_Rhythm == 6 & Confirmed_diagnosis_Abnormal_Heart_Rhythm == 1 & Still_Has_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 & Disputed_diagnosis_Abnormal_Heart_Rhythm == 3)) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Abnormal_Heart_Rhythm)
head(Abnormal_Heart_Rhythm_Data_4)

Abnormal_Heart_Rhythm_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedimar, hedawar, hedacar, hedanar, hedasar) %>%
  rename(
    Newly_Reported_Abnormal_Heart_Rhythm = hedimar, Fed_Forward_Abnormal_Heart_Rhythm = hedawar, Confirmed_diagnosis_Abnormal_Heart_Rhythm = hedacar, Disputed_diagnosis_Abnormal_Heart_Rhythm = hedanar,
    Still_Has_Abnormal_Heart_Rhythm = hedasar
  ) %>%
  mutate(Abnormal_Heart_Rhythm = case_when(
    (Fed_Forward_Abnormal_Heart_Rhythm == 6 & Confirmed_diagnosis_Abnormal_Heart_Rhythm == 1 & Still_Has_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 & Disputed_diagnosis_Abnormal_Heart_Rhythm == 3)) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Abnormal_Heart_Rhythm)
head(Abnormal_Heart_Rhythm_Data_5)

Abnormal_Heart_Rhythm_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedimar, hedawar, hedacar, hedanar, hedasar) %>%
  rename(
    Newly_Reported_Abnormal_Heart_Rhythm = hedimar, Fed_Forward_Abnormal_Heart_Rhythm = hedawar, Confirmed_diagnosis_Abnormal_Heart_Rhythm = hedacar, Disputed_diagnosis_Abnormal_Heart_Rhythm = hedanar,
    Still_Has_Abnormal_Heart_Rhythm = hedasar
  ) %>%
  mutate(Abnormal_Heart_Rhythm = case_when(
    (Fed_Forward_Abnormal_Heart_Rhythm == 6 & Confirmed_diagnosis_Abnormal_Heart_Rhythm == 1 & Still_Has_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 & Disputed_diagnosis_Abnormal_Heart_Rhythm == 3)) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Abnormal_Heart_Rhythm)
head(Abnormal_Heart_Rhythm_Data_6)

Abnormal_Heart_Rhythm_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedimar, hedawar, hedacar, hedanar, hedasar) %>%
  rename(
    Newly_Reported_Abnormal_Heart_Rhythm = hedimar, Fed_Forward_Abnormal_Heart_Rhythm = hedawar, Confirmed_diagnosis_Abnormal_Heart_Rhythm = hedacar, Disputed_diagnosis_Abnormal_Heart_Rhythm = hedanar,
    Still_Has_Abnormal_Heart_Rhythm = hedasar
  ) %>%
  mutate(Abnormal_Heart_Rhythm = case_when(
    (Fed_Forward_Abnormal_Heart_Rhythm == 6 & Confirmed_diagnosis_Abnormal_Heart_Rhythm == 1 & Still_Has_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 & Disputed_diagnosis_Abnormal_Heart_Rhythm == 3)) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Abnormal_Heart_Rhythm)
head(Abnormal_Heart_Rhythm_Data_7)

Abnormal_Heart_Rhythm_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedimar, hedawar, hedacar, hedanar, hedasar) %>%
  rename(
    Newly_Reported_Abnormal_Heart_Rhythm = hedimar, Fed_Forward_Abnormal_Heart_Rhythm = hedawar, Confirmed_diagnosis_Abnormal_Heart_Rhythm = hedacar, Disputed_diagnosis_Abnormal_Heart_Rhythm = hedanar,
    Still_Has_Abnormal_Heart_Rhythm = hedasar
  ) %>%
  mutate(Abnormal_Heart_Rhythm = case_when(
    (Fed_Forward_Abnormal_Heart_Rhythm == 6 & Confirmed_diagnosis_Abnormal_Heart_Rhythm == 1 & Still_Has_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 & Disputed_diagnosis_Abnormal_Heart_Rhythm == 3)) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Abnormal_Heart_Rhythm)
head(Abnormal_Heart_Rhythm_Data_8)

Abnormal_Heart_Rhythm_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedimar, hedawar, hedacar, hedanar, hedasar) %>%
  rename(
    Newly_Reported_Abnormal_Heart_Rhythm = hedimar, Fed_Forward_Abnormal_Heart_Rhythm = hedawar, Confirmed_diagnosis_Abnormal_Heart_Rhythm = hedacar, Disputed_diagnosis_Abnormal_Heart_Rhythm = hedanar,
    Still_Has_Abnormal_Heart_Rhythm = hedasar
  ) %>%
  mutate(Abnormal_Heart_Rhythm = case_when(
    (Fed_Forward_Abnormal_Heart_Rhythm == 6 & Confirmed_diagnosis_Abnormal_Heart_Rhythm == 1 & Still_Has_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 & Disputed_diagnosis_Abnormal_Heart_Rhythm == 3)) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 1) ~ 1,
    ((Fed_Forward_Abnormal_Heart_Rhythm != 6 | Confirmed_diagnosis_Abnormal_Heart_Rhythm != 1 | Still_Has_Abnormal_Heart_Rhythm != 1) & Newly_Reported_Abnormal_Heart_Rhythm == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Abnormal_Heart_Rhythm)
head(Abnormal_Heart_Rhythm_Data_9)


#### Extracting data on heart murmur (wave 4 to 9) ####
# hedimhm :heart murmur diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawhm :Diagnosed heart murmur fed forward (5:heart murmur,<0: missing)
# if hedawhm =5, then hedachm : whether confirms heart murmur (1:Yes, 2:No, other values (positive or negative: missing)
# if hedachm=2, then hedanhm: Reason disputed heart murmur diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedawhm =5, then hedashm :whether still has heart murmur (1:Yes, 2:No, other values (positive or negative: missing)
Heart_Murmur_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedimhm, hedawhm, hedachm, hedanhm, hedashm) %>%
  rename(
    Newly_Reported_Heart_Murmur = hedimhm, Fed_Forward_Heart_Murmur = hedawhm, Confirmed_diagnosis_Heart_Murmur = hedachm, Disputed_diagnosis_Heart_Murmur = hedanhm,
    Still_Has_Heart_Murmur = hedashm
  ) %>%
  mutate(Heart_Murmur = case_when(
    (Fed_Forward_Heart_Murmur == 5 & Confirmed_diagnosis_Heart_Murmur == 1 & Still_Has_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 & Disputed_diagnosis_Heart_Murmur == 3)) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Murmur)
head(Heart_Murmur_Data_4)

Heart_Murmur_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedimhm, hedawhm, hedachm, hedanhm, hedashm) %>%
  rename(
    Newly_Reported_Heart_Murmur = hedimhm, Fed_Forward_Heart_Murmur = hedawhm, Confirmed_diagnosis_Heart_Murmur = hedachm, Disputed_diagnosis_Heart_Murmur = hedanhm,
    Still_Has_Heart_Murmur = hedashm
  ) %>%
  mutate(Heart_Murmur = case_when(
    (Fed_Forward_Heart_Murmur == 5 & Confirmed_diagnosis_Heart_Murmur == 1 & Still_Has_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 & Disputed_diagnosis_Heart_Murmur == 3)) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Murmur)
head(Heart_Murmur_Data_5)

Heart_Murmur_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedimhm, hedawhm, hedachm, hedanhm, hedashm) %>%
  rename(
    Newly_Reported_Heart_Murmur = hedimhm, Fed_Forward_Heart_Murmur = hedawhm, Confirmed_diagnosis_Heart_Murmur = hedachm, Disputed_diagnosis_Heart_Murmur = hedanhm,
    Still_Has_Heart_Murmur = hedashm
  ) %>%
  mutate(Heart_Murmur = case_when(
    (Fed_Forward_Heart_Murmur == 5 & Confirmed_diagnosis_Heart_Murmur == 1 & Still_Has_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 & Disputed_diagnosis_Heart_Murmur == 3)) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Murmur)
head(Heart_Murmur_Data_6)

Heart_Murmur_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedimhm, hedawhm, hedachm, hedanhm, hedashm) %>%
  rename(
    Newly_Reported_Heart_Murmur = hedimhm, Fed_Forward_Heart_Murmur = hedawhm, Confirmed_diagnosis_Heart_Murmur = hedachm, Disputed_diagnosis_Heart_Murmur = hedanhm,
    Still_Has_Heart_Murmur = hedashm
  ) %>%
  mutate(Heart_Murmur = case_when(
    (Fed_Forward_Heart_Murmur == 5 & Confirmed_diagnosis_Heart_Murmur == 1 & Still_Has_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 & Disputed_diagnosis_Heart_Murmur == 3)) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Murmur)
head(Heart_Murmur_Data_7)

Heart_Murmur_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedimhm, hedawhm, hedachm, hedanhm, hedashm) %>%
  rename(
    Newly_Reported_Heart_Murmur = hedimhm, Fed_Forward_Heart_Murmur = hedawhm, Confirmed_diagnosis_Heart_Murmur = hedachm, Disputed_diagnosis_Heart_Murmur = hedanhm,
    Still_Has_Heart_Murmur = hedashm
  ) %>%
  mutate(Heart_Murmur = case_when(
    (Fed_Forward_Heart_Murmur == 5 & Confirmed_diagnosis_Heart_Murmur == 1 & Still_Has_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 & Disputed_diagnosis_Heart_Murmur == 3)) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Murmur)
head(Heart_Murmur_Data_8)

Heart_Murmur_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedimhm, hedawhm, hedachm, hedanhm, hedashm) %>%
  rename(
    Newly_Reported_Heart_Murmur = hedimhm, Fed_Forward_Heart_Murmur = hedawhm, Confirmed_diagnosis_Heart_Murmur = hedachm, Disputed_diagnosis_Heart_Murmur = hedanhm,
    Still_Has_Heart_Murmur = hedashm
  ) %>%
  mutate(Heart_Murmur = case_when(
    (Fed_Forward_Heart_Murmur == 5 & Confirmed_diagnosis_Heart_Murmur == 1 & Still_Has_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 & Disputed_diagnosis_Heart_Murmur == 3)) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 1) ~ 1,
    ((Fed_Forward_Heart_Murmur != 5 | Confirmed_diagnosis_Heart_Murmur != 1 | Still_Has_Heart_Murmur != 1) & Newly_Reported_Heart_Murmur == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Murmur)
head(Heart_Murmur_Data_9)

#### Extracting data on congestive heart failure (wave 4 to 9) ####
# hedimhf :congestive heart failure diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwos :Diagnosed congestive heart failure fed forward (4:congestive heart failure,<0: missing)
# if hedbwos =4, then hedbdos : whether confirms congestive heart failure (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdos=2, then hedbmos: Reason disputed congestive heart failure diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwos =4, then hedbsos :whether still has congestive heart failure (1:Yes, 2:No, other values (positive or negative: missing)
Heart_Failure_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedimhf, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Heart_Failure = hedimhf, Fed_Forward_Heart_Failure = hedbwos, Confirmed_diagnosis_Heart_Failure = hedbdos, Disputed_diagnosis_Heart_Failure = hedbmos,
    Still_Has_Heart_Failure = hedbsos
  ) %>%
  mutate(Heart_Failure = case_when(
    (Fed_Forward_Heart_Failure == 4 & Confirmed_diagnosis_Heart_Failure == 1 & Still_Has_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 & Disputed_diagnosis_Heart_Failure == 3)) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Failure)
head(Heart_Failure_Data_4)

Heart_Failure_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedimhf, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Heart_Failure = hedimhf, Fed_Forward_Heart_Failure = hedbwos, Confirmed_diagnosis_Heart_Failure = hedbdos, Disputed_diagnosis_Heart_Failure = hedbmos,
    Still_Has_Heart_Failure = hedbsos
  ) %>%
  mutate(Heart_Failure = case_when(
    (Fed_Forward_Heart_Failure == 4 & Confirmed_diagnosis_Heart_Failure == 1 & Still_Has_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 & Disputed_diagnosis_Heart_Failure == 3)) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Failure)
head(Heart_Failure_Data_5)

Heart_Failure_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedimhf, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Heart_Failure = hedimhf, Fed_Forward_Heart_Failure = hedbwos, Confirmed_diagnosis_Heart_Failure = hedbdos, Disputed_diagnosis_Heart_Failure = hedbmos,
    Still_Has_Heart_Failure = hedbsos
  ) %>%
  mutate(Heart_Failure = case_when(
    (Fed_Forward_Heart_Failure == 4 & Confirmed_diagnosis_Heart_Failure == 1 & Still_Has_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 & Disputed_diagnosis_Heart_Failure == 3)) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Failure)
head(Heart_Failure_Data_6)

Heart_Failure_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedimhf, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Heart_Failure = hedimhf, Fed_Forward_Heart_Failure = hedbwos, Confirmed_diagnosis_Heart_Failure = hedbdos, Disputed_diagnosis_Heart_Failure = hedbmos,
    Still_Has_Heart_Failure = hedbsos
  ) %>%
  mutate(Heart_Failure = case_when(
    (Fed_Forward_Heart_Failure == 4 & Confirmed_diagnosis_Heart_Failure == 1 & Still_Has_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 & Disputed_diagnosis_Heart_Failure == 3)) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Failure)
head(Heart_Failure_Data_7)


Heart_Failure_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedimhf, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Heart_Failure = hedimhf, Fed_Forward_Heart_Failure = hedbwos, Confirmed_diagnosis_Heart_Failure = hedbdos, Disputed_diagnosis_Heart_Failure = hedbmos,
    Still_Has_Heart_Failure = hedbsos
  ) %>%
  mutate(Heart_Failure = case_when(
    (Fed_Forward_Heart_Failure == 4 & Confirmed_diagnosis_Heart_Failure == 1 & Still_Has_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 & Disputed_diagnosis_Heart_Failure == 3)) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Failure)
head(Heart_Failure_Data_8)

Heart_Failure_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedimhf, hedbwos, hedbdos, hedbmos, hedbsos) %>%
  rename(
    Newly_Reported_Heart_Failure = hedimhf, Fed_Forward_Heart_Failure = hedbwos, Confirmed_diagnosis_Heart_Failure = hedbdos, Disputed_diagnosis_Heart_Failure = hedbmos,
    Still_Has_Heart_Failure = hedbsos
  ) %>%
  mutate(Heart_Failure = case_when(
    (Fed_Forward_Heart_Failure == 4 & Confirmed_diagnosis_Heart_Failure == 1 & Still_Has_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 & Disputed_diagnosis_Heart_Failure == 3)) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 1) ~ 1,
    ((Fed_Forward_Heart_Failure != 4 | Confirmed_diagnosis_Heart_Failure != 1 | Still_Has_Heart_Failure != 1) & Newly_Reported_Heart_Failure == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Failure)
head(Heart_Failure_Data_9)

#### Extracting data on heart attack (wave 4 to 9) ####
# hedimmi :heart attack diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawmi :Diagnosed heart attack fed forward (3:heart attack,<0: missing)
# then hedanmi: Reason disputed heart attack diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
Heart_Attack_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedimmi, hedawmi, hedanmi) %>%
  rename(Newly_Reported_Heart_Attack = hedimmi, Fed_Forward_Heart_Attack = hedawmi, Disputed_diagnosis_Heart_Attack = hedanmi) %>%
  mutate(Heart_Attack = case_when(
    (Fed_Forward_Heart_Attack == 3 & (Disputed_diagnosis_Heart_Attack == 1 |
                                        Disputed_diagnosis_Heart_Attack == 2)) ~ 0,
    (Fed_Forward_Heart_Attack == 3) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 1) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Attack)
tail(Heart_Attack_Data_4)

Heart_Attack_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedimmi, hedawmi, hedanmi) %>%
  rename(Newly_Reported_Heart_Attack = hedimmi, Fed_Forward_Heart_Attack = hedawmi, Disputed_diagnosis_Heart_Attack = hedanmi) %>%
  mutate(Heart_Attack = case_when(
    (Fed_Forward_Heart_Attack == 3 & (Disputed_diagnosis_Heart_Attack == 1 |
                                        Disputed_diagnosis_Heart_Attack == 2)) ~ 0,
    (Fed_Forward_Heart_Attack == 3) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 1) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Attack)
tail(Heart_Attack_Data_5)

Heart_Attack_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedimmi, hedawmi, hedanmi) %>%
  rename(Newly_Reported_Heart_Attack = hedimmi, Fed_Forward_Heart_Attack = hedawmi, Disputed_diagnosis_Heart_Attack = hedanmi) %>%
  mutate(Heart_Attack = case_when(
    (Fed_Forward_Heart_Attack == 3 & (Disputed_diagnosis_Heart_Attack == 1 |
                                        Disputed_diagnosis_Heart_Attack == 2)) ~ 0,
    (Fed_Forward_Heart_Attack == 3) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 1) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Attack)
tail(Heart_Attack_Data_6)

Heart_Attack_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedimmi, hedawmi, hedanmi) %>%
  rename(Newly_Reported_Heart_Attack = hedimmi, Fed_Forward_Heart_Attack = hedawmi, Disputed_diagnosis_Heart_Attack = hedanmi) %>%
  mutate(Heart_Attack = case_when(
    (Fed_Forward_Heart_Attack == 3 & (Disputed_diagnosis_Heart_Attack == 1 |
                                        Disputed_diagnosis_Heart_Attack == 2)) ~ 0,
    (Fed_Forward_Heart_Attack == 3) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 1) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Attack)
tail(Heart_Attack_Data_7)

Heart_Attack_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedimmi, hedawmi, hedanmi) %>%
  rename(Newly_Reported_Heart_Attack = hedimmi, Fed_Forward_Heart_Attack = hedawmi, Disputed_diagnosis_Heart_Attack = hedanmi) %>%
  mutate(Heart_Attack = case_when(
    (Fed_Forward_Heart_Attack == 3 & (Disputed_diagnosis_Heart_Attack == 1 |
                                        Disputed_diagnosis_Heart_Attack == 2)) ~ 0,
    (Fed_Forward_Heart_Attack == 3) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 1) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Attack)
tail(Heart_Attack_Data_8)

Heart_Attack_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedimmi, hedawmi, hedanmi) %>%
  rename(Newly_Reported_Heart_Attack = hedimmi, Fed_Forward_Heart_Attack = hedawmi, Disputed_diagnosis_Heart_Attack = hedanmi) %>%
  mutate(Heart_Attack = case_when(
    (Fed_Forward_Heart_Attack == 3 & (Disputed_diagnosis_Heart_Attack == 1 |
                                        Disputed_diagnosis_Heart_Attack == 2)) ~ 0,
    (Fed_Forward_Heart_Attack == 3) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 1) ~ 1,
    (Fed_Forward_Heart_Attack != 3 & Newly_Reported_Heart_Attack == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Heart_Attack)
tail(Heart_Attack_Data_9)

#### Extracting data on  angina (wave 4 to 9) ####
# hediman :angina diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawan :Diagnosed angina fed forward (2:angina,<0: missing)
# hedanan: Reason disputed angina diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
Angina_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hediman, hedawan, hedanan) %>%
  rename(Newly_Reported_Angina = hediman, Fed_Forward_Angina = hedawan, Disputed_diagnosis_Angina = hedanan) %>%
  mutate(Angina = case_when(
    (Fed_Forward_Angina == 2 & (Disputed_diagnosis_Angina == 1 |
                                  Disputed_diagnosis_Angina == 2)) ~ 0,
    (Fed_Forward_Angina == 2) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 1) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Angina)
head(Angina_Data_4)

Angina_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hediman, hedawan, hedanan) %>%
  rename(Newly_Reported_Angina = hediman, Fed_Forward_Angina = hedawan, Disputed_diagnosis_Angina = hedanan) %>%
  mutate(Angina = case_when(
    (Fed_Forward_Angina == 2 & (Disputed_diagnosis_Angina == 1 |
                                  Disputed_diagnosis_Angina == 2)) ~ 0,
    (Fed_Forward_Angina == 2) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 1) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Angina)
head(Angina_Data_5)

Angina_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hediman, hedawan, hedanan) %>%
  rename(Newly_Reported_Angina = hediman, Fed_Forward_Angina = hedawan, Disputed_diagnosis_Angina = hedanan) %>%
  mutate(Angina = case_when(
    (Fed_Forward_Angina == 2 & (Disputed_diagnosis_Angina == 1 |
                                  Disputed_diagnosis_Angina == 2)) ~ 0,
    (Fed_Forward_Angina == 2) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 1) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Angina)
head(Angina_Data_6)

Angina_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hediman, hedawan, hedanan) %>%
  rename(Newly_Reported_Angina = hediman, Fed_Forward_Angina = hedawan, Disputed_diagnosis_Angina = hedanan) %>%
  mutate(Angina = case_when(
    (Fed_Forward_Angina == 2 & (Disputed_diagnosis_Angina == 1 |
                                  Disputed_diagnosis_Angina == 2)) ~ 0,
    (Fed_Forward_Angina == 2) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 1) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Angina)
head(Angina_Data_7)

Angina_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hediman, hedawan, hedanan) %>%
  rename(Newly_Reported_Angina = hediman, Fed_Forward_Angina = hedawan, Disputed_diagnosis_Angina = hedanan) %>%
  mutate(Angina = case_when(
    (Fed_Forward_Angina == 2 & (Disputed_diagnosis_Angina == 1 |
                                  Disputed_diagnosis_Angina == 2)) ~ 0,
    (Fed_Forward_Angina == 2) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 1) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Angina)
head(Angina_Data_8)

Angina_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hediman, hedawan, hedanan) %>%
  rename(Newly_Reported_Angina = hediman, Fed_Forward_Angina = hedawan, Disputed_diagnosis_Angina = hedanan) %>%
  mutate(Angina = case_when(
    (Fed_Forward_Angina == 2 & (Disputed_diagnosis_Angina == 1 |
                                  Disputed_diagnosis_Angina == 2)) ~ 0,
    (Fed_Forward_Angina == 2) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 1) ~ 1,
    (Fed_Forward_Angina != 2 & Newly_Reported_Angina == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Angina)
head(Angina_Data_9)

#### Extracting data on parkinsons disease (wave 4 to 9) ####

# hedibpd :parkinsons disease diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwpd :Diagnosed parkinsons disease fed forward (6:parkinsons disease,<0: missing)
# if hedbwpd =6, then hedbdpd : whether confirms parkinsons disease (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdpd=2, then hedbmpd: Reason disputed parkinsons disease diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwpd =6, then hedbspd :whether still has parkinsons disease (1:Yes, 2:No, other values (positive or negative: missing)
Parkinsons_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedibpd, hedbwpd, hedbdpd, hedbmpd, hedbspd) %>%
  rename(
    Newly_Reported_Parkinsons = hedibpd, Fed_Forward_Parkinsons = hedbwpd, Confirmed_diagnosis_Parkinsons = hedbdpd, Disputed_diagnosis_Parkinsons = hedbmpd,
    Still_Has_Parkinsons = hedbspd
  ) %>%
  mutate(Parkinsons = case_when(
    (Fed_Forward_Parkinsons == 6 & Confirmed_diagnosis_Parkinsons == 1 & Still_Has_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 & Disputed_diagnosis_Parkinsons == 3)) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Parkinsons)
tail(Parkinsons_Data_4, 10)

Parkinsons_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedibpd, hedbwpd, hedbdpd, hedbmpd, hedbspd) %>%
  rename(
    Newly_Reported_Parkinsons = hedibpd, Fed_Forward_Parkinsons = hedbwpd, Confirmed_diagnosis_Parkinsons = hedbdpd, Disputed_diagnosis_Parkinsons = hedbmpd,
    Still_Has_Parkinsons = hedbspd
  ) %>%
  mutate(Parkinsons = case_when(
    (Fed_Forward_Parkinsons == 6 & Confirmed_diagnosis_Parkinsons == 1 & Still_Has_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 & Disputed_diagnosis_Parkinsons == 3)) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Parkinsons)
tail(Parkinsons_Data_5, 10)

Parkinsons_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedibpd, hedbwpd, hedbdpd, hedbmpd, hedbspd) %>%
  rename(
    Newly_Reported_Parkinsons = hedibpd, Fed_Forward_Parkinsons = hedbwpd, Confirmed_diagnosis_Parkinsons = hedbdpd, Disputed_diagnosis_Parkinsons = hedbmpd,
    Still_Has_Parkinsons = hedbspd
  ) %>%
  mutate(Parkinsons = case_when(
    (Fed_Forward_Parkinsons == 6 & Confirmed_diagnosis_Parkinsons == 1 & Still_Has_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 & Disputed_diagnosis_Parkinsons == 3)) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Parkinsons)
tail(Parkinsons_Data_6, 10)

Parkinsons_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedibpd, hedbwpd, hedbdpd, hedbmpd, hedbspd) %>%
  rename(
    Newly_Reported_Parkinsons = hedibpd, Fed_Forward_Parkinsons = hedbwpd, Confirmed_diagnosis_Parkinsons = hedbdpd, Disputed_diagnosis_Parkinsons = hedbmpd,
    Still_Has_Parkinsons = hedbspd
  ) %>%
  mutate(Parkinsons = case_when(
    (Fed_Forward_Parkinsons == 6 & Confirmed_diagnosis_Parkinsons == 1 & Still_Has_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 & Disputed_diagnosis_Parkinsons == 3)) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Parkinsons)
tail(Parkinsons_Data_7, 10)

Parkinsons_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedibpd, hedbwpd, hedbdpd, hedbmpd, hedbspd) %>%
  rename(
    Newly_Reported_Parkinsons = hedibpd, Fed_Forward_Parkinsons = hedbwpd, Confirmed_diagnosis_Parkinsons = hedbdpd, Disputed_diagnosis_Parkinsons = hedbmpd,
    Still_Has_Parkinsons = hedbspd
  ) %>%
  mutate(Parkinsons = case_when(
    (Fed_Forward_Parkinsons == 6 & Confirmed_diagnosis_Parkinsons == 1 & Still_Has_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 & Disputed_diagnosis_Parkinsons == 3)) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Parkinsons)
tail(Parkinsons_Data_8, 10)

Parkinsons_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedibpd, hedbwpd, hedbdpd, hedbmpd, hedbspd) %>%
  rename(
    Newly_Reported_Parkinsons = hedibpd, Fed_Forward_Parkinsons = hedbwpd, Confirmed_diagnosis_Parkinsons = hedbdpd, Disputed_diagnosis_Parkinsons = hedbmpd,
    Still_Has_Parkinsons = hedbspd
  ) %>%
  mutate(Parkinsons = case_when(
    (Fed_Forward_Parkinsons == 6 & Confirmed_diagnosis_Parkinsons == 1 & Still_Has_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 & Disputed_diagnosis_Parkinsons == 3)) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 1) ~ 1,
    ((Fed_Forward_Parkinsons != 6 | Confirmed_diagnosis_Parkinsons != 1 | Still_Has_Parkinsons != 1) & Newly_Reported_Parkinsons == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Parkinsons)
tail(Parkinsons_Data_9, 10)

#### Extracting data on cataracts (wave 4 to 9) ####
# heoptca :cataract diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# heopfca :Diagnosed cataract fed forward (4:cataract,<0: missing)
# if heopfca =4, then heopcca : whether confirms cataract (1:Yes, 2:No, other values (positive or negative: missing)
# if heopcca=2, then heopnca: Reason disputed cataract diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if heopfca =4, then heopsca :whether still has cataract (1:Yes, 2:No, other values (positive or negative: missing)
Cataract_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, heoptca, heopfca, heopcca, heopnca, heopsca) %>%
  rename(
    Newly_Reported_Cataract = heoptca, Fed_Forward_Cataract = heopfca, Confirmed_diagnosis_Cataract = heopcca, Disputed_diagnosis_Cataract = heopnca,
    Still_Has_Cataract = heopsca
  ) %>%
  mutate(Cataract = case_when(
    (Fed_Forward_Cataract == 4 & Confirmed_diagnosis_Cataract == 1 & Still_Has_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 & Disputed_diagnosis_Cataract == 3)) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cataract)
head(Cataract_Data_4, 10)

Cataract_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, heoptca, heopfca, heopcca, heopnca, heopsca) %>%
  rename(
    Newly_Reported_Cataract = heoptca, Fed_Forward_Cataract = heopfca, Confirmed_diagnosis_Cataract = heopcca, Disputed_diagnosis_Cataract = heopnca,
    Still_Has_Cataract = heopsca
  ) %>%
  mutate(Cataract = case_when(
    (Fed_Forward_Cataract == 4 & Confirmed_diagnosis_Cataract == 1 & Still_Has_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 & Disputed_diagnosis_Cataract == 3)) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cataract)
head(Cataract_Data_5, 10)

Cataract_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, heoptca, heopfca, heopcca, heopnca, heopsca) %>%
  rename(
    Newly_Reported_Cataract = heoptca, Fed_Forward_Cataract = heopfca, Confirmed_diagnosis_Cataract = heopcca, Disputed_diagnosis_Cataract = heopnca,
    Still_Has_Cataract = heopsca
  ) %>%
  mutate(Cataract = case_when(
    (Fed_Forward_Cataract == 4 & Confirmed_diagnosis_Cataract == 1 & Still_Has_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 & Disputed_diagnosis_Cataract == 3)) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cataract)
head(Cataract_Data_6, 10)

Cataract_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, heoptca, heopfca, heopcca, heopnca, heopsca) %>%
  rename(
    Newly_Reported_Cataract = heoptca, Fed_Forward_Cataract = heopfca, Confirmed_diagnosis_Cataract = heopcca, Disputed_diagnosis_Cataract = heopnca,
    Still_Has_Cataract = heopsca
  ) %>%
  mutate(Cataract = case_when(
    (Fed_Forward_Cataract == 4 & Confirmed_diagnosis_Cataract == 1 & Still_Has_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 & Disputed_diagnosis_Cataract == 3)) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cataract)
head(Cataract_Data_7, 10)

Cataract_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, heoptca, heopfca, heopcca, heopnca, heopsca) %>%
  rename(
    Newly_Reported_Cataract = heoptca, Fed_Forward_Cataract = heopfca, Confirmed_diagnosis_Cataract = heopcca, Disputed_diagnosis_Cataract = heopnca,
    Still_Has_Cataract = heopsca
  ) %>%
  mutate(Cataract = case_when(
    (Fed_Forward_Cataract == 4 & Confirmed_diagnosis_Cataract == 1 & Still_Has_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 & Disputed_diagnosis_Cataract == 3)) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cataract)
head(Cataract_Data_8, 10)

Cataract_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, heoptca, heopfca, heopcca, heopnca, heopsca) %>%
  rename(
    Newly_Reported_Cataract = heoptca, Fed_Forward_Cataract = heopfca, Confirmed_diagnosis_Cataract = heopcca, Disputed_diagnosis_Cataract = heopnca,
    Still_Has_Cataract = heopsca
  ) %>%
  mutate(Cataract = case_when(
    (Fed_Forward_Cataract == 4 & Confirmed_diagnosis_Cataract == 1 & Still_Has_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 & Disputed_diagnosis_Cataract == 3)) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 1) ~ 1,
    ((Fed_Forward_Cataract != 4 | Confirmed_diagnosis_Cataract != 1 | Still_Has_Cataract != 1) & Newly_Reported_Cataract == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Cataract)
head(Cataract_Data_9, 10)

#### Extracting data on asthma  (wave 4 to 9) ####
# hedibas :asthma diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwas :Diagnosed asthma fed forward (2:asthma,<0: missing)
# if hedbwas =2, then hedbdas : whether confirms asthma (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdas=2, then hedbmas: Reason disputed asthma diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwas =2, then hedbsas :whether still has asthma (1:Yes, 2:No, other values (positive or negative: missing)
Asthma_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedibas, hedbwas, hedbdas, hedbmas, hedbsas) %>%
  rename(
    Newly_Reported_Asthma = hedibas, Fed_Forward_Asthma = hedbwas, Confirmed_diagnosis_Asthma = hedbdas, Disputed_diagnosis_Asthma = hedbmas,
    Still_Has_Asthma = hedbsas
  ) %>%
  mutate(Asthma = case_when(
    (Fed_Forward_Asthma == 2 & Confirmed_diagnosis_Asthma == 1 & Still_Has_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 & Disputed_diagnosis_Asthma == 3)) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Asthma)
head(Asthma_Data_4)

Asthma_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedibas, hedbwas, hedbdas, hedbmas, hedbsas) %>%
  rename(
    Newly_Reported_Asthma = hedibas, Fed_Forward_Asthma = hedbwas, Confirmed_diagnosis_Asthma = hedbdas, Disputed_diagnosis_Asthma = hedbmas,
    Still_Has_Asthma = hedbsas
  ) %>%
  mutate(Asthma = case_when(
    (Fed_Forward_Asthma == 2 & Confirmed_diagnosis_Asthma == 1 & Still_Has_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 & Disputed_diagnosis_Asthma == 3)) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Asthma)
head(Asthma_Data_5)

Asthma_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedibas, hedbwas, hedbdas, hedbmas, hedbsas) %>%
  rename(
    Newly_Reported_Asthma = hedibas, Fed_Forward_Asthma = hedbwas, Confirmed_diagnosis_Asthma = hedbdas, Disputed_diagnosis_Asthma = hedbmas,
    Still_Has_Asthma = hedbsas
  ) %>%
  mutate(Asthma = case_when(
    (Fed_Forward_Asthma == 2 & Confirmed_diagnosis_Asthma == 1 & Still_Has_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 & Disputed_diagnosis_Asthma == 3)) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Asthma)
head(Asthma_Data_6)

Asthma_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedibas, hedbwas, hedbdas, hedbmas, hedbsas) %>%
  rename(
    Newly_Reported_Asthma = hedibas, Fed_Forward_Asthma = hedbwas, Confirmed_diagnosis_Asthma = hedbdas, Disputed_diagnosis_Asthma = hedbmas,
    Still_Has_Asthma = hedbsas
  ) %>%
  mutate(Asthma = case_when(
    (Fed_Forward_Asthma == 2 & Confirmed_diagnosis_Asthma == 1 & Still_Has_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 & Disputed_diagnosis_Asthma == 3)) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Asthma)
head(Asthma_Data_7)

Asthma_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedibas, hedbwas, hedbdas, hedbmas, hedbsas) %>%
  rename(
    Newly_Reported_Asthma = hedibas, Fed_Forward_Asthma = hedbwas, Confirmed_diagnosis_Asthma = hedbdas, Disputed_diagnosis_Asthma = hedbmas,
    Still_Has_Asthma = hedbsas
  ) %>%
  mutate(Asthma = case_when(
    (Fed_Forward_Asthma == 2 & Confirmed_diagnosis_Asthma == 1 & Still_Has_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 & Disputed_diagnosis_Asthma == 3)) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Asthma)
head(Asthma_Data_8)

Asthma_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedibas, hedbwas, hedbdas, hedbmas, hedbsas) %>%
  rename(
    Newly_Reported_Asthma = hedibas, Fed_Forward_Asthma = hedbwas, Confirmed_diagnosis_Asthma = hedbdas, Disputed_diagnosis_Asthma = hedbmas,
    Still_Has_Asthma = hedbsas
  ) %>%
  mutate(Asthma = case_when(
    (Fed_Forward_Asthma == 2 & Confirmed_diagnosis_Asthma == 1 & Still_Has_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 & Disputed_diagnosis_Asthma == 3)) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 1) ~ 1,
    ((Fed_Forward_Asthma != 2 | Confirmed_diagnosis_Asthma != 1 | Still_Has_Asthma != 1) & Newly_Reported_Asthma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Asthma)
head(Asthma_Data_9)

#### Extracting data on arthritis (wave 4 to 9) ####
# hedibar :arthritis diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwar :Diagnosed arthritis fed forward (3:arthritis,<0: missing)
# if hedbwar =3, then hedbdar : whether confirms arthritis (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdar=2, then hedbmar: Reason disputed arthritis diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwar =3, then hedbsar :whether still has arthritis (1:Yes, 2:No, other values (positive or negative: missing)
Arthritis_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedibar, hedbwar, hedbdar, hedbmar, hedbsar) %>%
  rename(
    Newly_Reported_Arthritis = hedibar, Fed_Forward_Arthritis = hedbwar, Confirmed_diagnosis_Arthritis = hedbdar, Disputed_diagnosis_Arthritis = hedbmar,
    Still_Has_Arthritis = hedbsar
  ) %>%
  mutate(Arthritis = case_when(
    (Fed_Forward_Arthritis == 3 & Confirmed_diagnosis_Arthritis == 1 & Still_Has_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 & Disputed_diagnosis_Arthritis == 3)) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Arthritis)
head(Arthritis_Data_4)

Arthritis_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedibar, hedbwar, hedbdar, hedbmar, hedbsar) %>%
  rename(
    Newly_Reported_Arthritis = hedibar, Fed_Forward_Arthritis = hedbwar, Confirmed_diagnosis_Arthritis = hedbdar, Disputed_diagnosis_Arthritis = hedbmar,
    Still_Has_Arthritis = hedbsar
  ) %>%
  mutate(Arthritis = case_when(
    (Fed_Forward_Arthritis == 3 & Confirmed_diagnosis_Arthritis == 1 & Still_Has_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 & Disputed_diagnosis_Arthritis == 3)) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Arthritis)
head(Arthritis_Data_5)

Arthritis_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedibar, hedbwar, hedbdar, hedbmar, hedbsar) %>%
  rename(
    Newly_Reported_Arthritis = hedibar, Fed_Forward_Arthritis = hedbwar, Confirmed_diagnosis_Arthritis = hedbdar, Disputed_diagnosis_Arthritis = hedbmar,
    Still_Has_Arthritis = hedbsar
  ) %>%
  mutate(Arthritis = case_when(
    (Fed_Forward_Arthritis == 3 & Confirmed_diagnosis_Arthritis == 1 & Still_Has_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 & Disputed_diagnosis_Arthritis == 3)) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Arthritis)
head(Arthritis_Data_6)

Arthritis_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedibar, hedbwar, hedbdar, hedbmar, hedbsar) %>%
  rename(
    Newly_Reported_Arthritis = hedibar, Fed_Forward_Arthritis = hedbwar, Confirmed_diagnosis_Arthritis = hedbdar, Disputed_diagnosis_Arthritis = hedbmar,
    Still_Has_Arthritis = hedbsar
  ) %>%
  mutate(Arthritis = case_when(
    (Fed_Forward_Arthritis == 3 & Confirmed_diagnosis_Arthritis == 1 & Still_Has_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 & Disputed_diagnosis_Arthritis == 3)) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Arthritis)
head(Arthritis_Data_7)

Arthritis_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedibar, hedbwar, hedbdar, hedbmar, hedbsar) %>%
  rename(
    Newly_Reported_Arthritis = hedibar, Fed_Forward_Arthritis = hedbwar, Confirmed_diagnosis_Arthritis = hedbdar, Disputed_diagnosis_Arthritis = hedbmar,
    Still_Has_Arthritis = hedbsar
  ) %>%
  mutate(Arthritis = case_when(
    (Fed_Forward_Arthritis == 3 & Confirmed_diagnosis_Arthritis == 1 & Still_Has_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 & Disputed_diagnosis_Arthritis == 3)) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Arthritis)
head(Arthritis_Data_8)

Arthritis_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedibar, hedbwar, hedbdar, hedbmar, hedbsar) %>%
  rename(
    Newly_Reported_Arthritis = hedibar, Fed_Forward_Arthritis = hedbwar, Confirmed_diagnosis_Arthritis = hedbdar, Disputed_diagnosis_Arthritis = hedbmar,
    Still_Has_Arthritis = hedbsar
  ) %>%
  mutate(Arthritis = case_when(
    (Fed_Forward_Arthritis == 3 & Confirmed_diagnosis_Arthritis == 1 & Still_Has_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 & Disputed_diagnosis_Arthritis == 3)) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 1) ~ 1,
    ((Fed_Forward_Arthritis != 3 | Confirmed_diagnosis_Arthritis != 1 | Still_Has_Arthritis != 1) & Newly_Reported_Arthritis == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Arthritis)
head(Arthritis_Data_9)

#### Extracting data on Alzheimer's (wave 4 to 9) ####
# hedibad :alzheimers diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwad :Diagnosed alzheimers fed forward (8:alzheimers,<0: missing)
# if hedbwad =8, then hedbdad : whether confirms alzheimers (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdad=2, then hedbmad: Reason disputed alzheimers diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
#

Alzheimers_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedibad, hedbwad, hedbdad, hedbmad) %>%
  rename(Newly_Reported_Alzheimers = hedibad, Fed_Forward_Alzheimers = hedbwad, Confirmed_diagnosis_Alzheimers = hedbdad, Disputed_diagnosis_Alzheimers = hedbmad) %>%
  mutate(Alzheimers = case_when(
    (Fed_Forward_Alzheimers == 8 & Confirmed_diagnosis_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 & Disputed_diagnosis_Alzheimers == 3)) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Alzheimers)
head(Alzheimers_Data_4)

Alzheimers_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedibad, hedbwad, hedbdad, hedbmad) %>%
  rename(Newly_Reported_Alzheimers = hedibad, Fed_Forward_Alzheimers = hedbwad, Confirmed_diagnosis_Alzheimers = hedbdad, Disputed_diagnosis_Alzheimers = hedbmad) %>%
  mutate(Alzheimers = case_when(
    (Fed_Forward_Alzheimers == 8 & Confirmed_diagnosis_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 & Disputed_diagnosis_Alzheimers == 3)) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Alzheimers)
head(Alzheimers_Data_5)

Alzheimers_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedibad, hedbwad, hedbdad, hedbmad) %>%
  rename(Newly_Reported_Alzheimers = hedibad, Fed_Forward_Alzheimers = hedbwad, Confirmed_diagnosis_Alzheimers = hedbdad, Disputed_diagnosis_Alzheimers = hedbmad) %>%
  mutate(Alzheimers = case_when(
    (Fed_Forward_Alzheimers == 8 & Confirmed_diagnosis_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 & Disputed_diagnosis_Alzheimers == 3)) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Alzheimers)
head(Alzheimers_Data_6)

Alzheimers_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedibad, hedbwad, hedbdad, hedbmad) %>%
  rename(Newly_Reported_Alzheimers = hedibad, Fed_Forward_Alzheimers = hedbwad, Confirmed_diagnosis_Alzheimers = hedbdad, Disputed_diagnosis_Alzheimers = hedbmad) %>%
  mutate(Alzheimers = case_when(
    (Fed_Forward_Alzheimers == 8 & Confirmed_diagnosis_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 & Disputed_diagnosis_Alzheimers == 3)) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Alzheimers)
head(Alzheimers_Data_7)

Alzheimers_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedibad, hedbwad, hedbdad, hedbmad) %>%
  rename(Newly_Reported_Alzheimers = hedibad, Fed_Forward_Alzheimers = hedbwad, Confirmed_diagnosis_Alzheimers = hedbdad, Disputed_diagnosis_Alzheimers = hedbmad) %>%
  mutate(Alzheimers = case_when(
    (Fed_Forward_Alzheimers == 8 & Confirmed_diagnosis_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 & Disputed_diagnosis_Alzheimers == 3)) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Alzheimers)
head(Alzheimers_Data_8)



Alzheimers_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedibad, hedbwad, hedbdad, hedbmad) %>%
  rename(Newly_Reported_Alzheimers = hedibad, Fed_Forward_Alzheimers = hedbwad, Confirmed_diagnosis_Alzheimers = hedbdad, Disputed_diagnosis_Alzheimers = hedbmad) %>%
  mutate(Alzheimers = case_when(
    (Fed_Forward_Alzheimers == 8 & Confirmed_diagnosis_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 & Disputed_diagnosis_Alzheimers == 3)) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 1) ~ 1,
    ((Fed_Forward_Alzheimers != 8 | Confirmed_diagnosis_Alzheimers != 1) & Newly_Reported_Alzheimers == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Alzheimers)
head(Alzheimers_Data_9)
#### Extracting data on Dementia (wave 4 to 9) ####
# hedibde :dementia diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwde :Diagnosed dementia fed forward (9:dementia,<0: missing)
# if hedbwde =9, then hedbdde : whether confirms dementia (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdar=2, then hedbmde: Reason disputed dementia diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwde =9, then hedbsde :whether still has dementia (1:Yes, 2:No, other values (positive or negative: missing)
Dementia_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hedibde, hedbwde, hedbdde, hedbmde, hedbsde) %>%
  rename(
    Newly_Reported_Dementia = hedibde, Fed_Forward_Dementia = hedbwde, Confirmed_diagnosis_Dementia = hedbdde, Disputed_diagnosis_Dementia = hedbmde,
    Still_Has_Dementia = hedbsde
  ) %>%
  mutate(Dementia = case_when(
    (Fed_Forward_Dementia == 9 & Confirmed_diagnosis_Dementia == 1 & Still_Has_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 & Disputed_diagnosis_Dementia == 3)) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Dementia)
head(Dementia_Data_4, 10)


Dementia_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, hedibde, hedbwde, hedbdde, hedbmde, hedbsde) %>%
  rename(
    Newly_Reported_Dementia = hedibde, Fed_Forward_Dementia = hedbwde, Confirmed_diagnosis_Dementia = hedbdde, Disputed_diagnosis_Dementia = hedbmde,
    Still_Has_Dementia = hedbsde
  ) %>%
  mutate(Dementia = case_when(
    (Fed_Forward_Dementia == 9 & Confirmed_diagnosis_Dementia == 1 & Still_Has_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 & Disputed_diagnosis_Dementia == 3)) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Dementia)
head(Dementia_Data_5, 10)

Dementia_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, hedibde, hedbwde, hedbdde, hedbmde, hedbsde) %>%
  rename(
    Newly_Reported_Dementia = hedibde, Fed_Forward_Dementia = hedbwde, Confirmed_diagnosis_Dementia = hedbdde, Disputed_diagnosis_Dementia = hedbmde,
    Still_Has_Dementia = hedbsde
  ) %>%
  mutate(Dementia = case_when(
    (Fed_Forward_Dementia == 9 & Confirmed_diagnosis_Dementia == 1 & Still_Has_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 & Disputed_diagnosis_Dementia == 3)) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Dementia)
head(Dementia_Data_6, 10)

Dementia_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, hedibde, hedbwde, hedbdde, hedbmde, hedbsde) %>%
  rename(
    Newly_Reported_Dementia = hedibde, Fed_Forward_Dementia = hedbwde, Confirmed_diagnosis_Dementia = hedbdde, Disputed_diagnosis_Dementia = hedbmde,
    Still_Has_Dementia = hedbsde
  ) %>%
  mutate(Dementia = case_when(
    (Fed_Forward_Dementia == 9 & Confirmed_diagnosis_Dementia == 1 & Still_Has_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 & Disputed_diagnosis_Dementia == 3)) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Dementia)
head(Dementia_Data_7, 10)

Dementia_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, hedibde, hedbwde, hedbdde, hedbmde, hedbsde) %>%
  rename(
    Newly_Reported_Dementia = hedibde, Fed_Forward_Dementia = hedbwde, Confirmed_diagnosis_Dementia = hedbdde, Disputed_diagnosis_Dementia = hedbmde,
    Still_Has_Dementia = hedbsde
  ) %>%
  mutate(Dementia = case_when(
    (Fed_Forward_Dementia == 9 & Confirmed_diagnosis_Dementia == 1 & Still_Has_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 & Disputed_diagnosis_Dementia == 3)) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Dementia)
head(Dementia_Data_8, 10)

Dementia_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedibde, hedbwde, hedbdde, hedbmde, hedbsde) %>%
  rename(
    Newly_Reported_Dementia = hedibde, Fed_Forward_Dementia = hedbwde, Confirmed_diagnosis_Dementia = hedbdde, Disputed_diagnosis_Dementia = hedbmde,
    Still_Has_Dementia = hedbsde
  ) %>%
  mutate(Dementia = case_when(
    (Fed_Forward_Dementia == 9 & Confirmed_diagnosis_Dementia == 1 & Still_Has_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 & Disputed_diagnosis_Dementia == 3)) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 1) ~ 1,
    ((Fed_Forward_Dementia != 9 | Confirmed_diagnosis_Dementia != 1 | Still_Has_Dementia != 1) & Newly_Reported_Dementia == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Dementia)
head(Dementia_Data_9, 10)
##### Glaucoma (wave 4 to 9) ######
# heoptgl :glaucoma diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# heopfgl :Diagnosed glaucoma fed forward (1:glaucoma,<0: missing)
# if heopfgl =1, then heopcgl : whether confirms glaucoma (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdar=2, then heopngl: Reason disputed glaucoma diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if heopfgl =1, then heopsgl :whether still has glaucoma (1:Yes, 2:No, other values (positive or negative: missing)
Glaucoma_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, heoptgl, heopfgl, heopcgl, heopngl, heopsgl) %>%
  rename(
    Newly_Reported_Glaucoma = heoptgl, Fed_Forward_Glaucoma = heopfgl, Confirmed_diagnosis_Glaucoma = heopcgl, Disputed_diagnosis_Glaucoma = heopngl,
    Still_Has_Glaucoma = heopsgl
  ) %>%
  mutate(Glaucoma = case_when(
    (Fed_Forward_Glaucoma == 1 & Confirmed_diagnosis_Glaucoma == 1 & Still_Has_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 & Disputed_diagnosis_Glaucoma == 3)) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Glaucoma)
head(Glaucoma_Data_4, 10)

Glaucoma_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, heoptgl, heopfgl, heopcgl, heopngl, heopsgl) %>%
  rename(
    Newly_Reported_Glaucoma = heoptgl, Fed_Forward_Glaucoma = heopfgl, Confirmed_diagnosis_Glaucoma = heopcgl, Disputed_diagnosis_Glaucoma = heopngl,
    Still_Has_Glaucoma = heopsgl
  ) %>%
  mutate(Glaucoma = case_when(
    (Fed_Forward_Glaucoma == 1 & Confirmed_diagnosis_Glaucoma == 1 & Still_Has_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 & Disputed_diagnosis_Glaucoma == 3)) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Glaucoma)
head(Glaucoma_Data_5, 10)

Glaucoma_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, heoptgl, heopfgl, heopcgl, heopngl, heopsgl) %>%
  rename(
    Newly_Reported_Glaucoma = heoptgl, Fed_Forward_Glaucoma = heopfgl, Confirmed_diagnosis_Glaucoma = heopcgl, Disputed_diagnosis_Glaucoma = heopngl,
    Still_Has_Glaucoma = heopsgl
  ) %>%
  mutate(Glaucoma = case_when(
    (Fed_Forward_Glaucoma == 1 & Confirmed_diagnosis_Glaucoma == 1 & Still_Has_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 & Disputed_diagnosis_Glaucoma == 3)) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Glaucoma)
head(Glaucoma_Data_6, 10)


Glaucoma_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, heoptgl, heopfgl, heopcgl, heopngl, heopsgl) %>%
  rename(
    Newly_Reported_Glaucoma = heoptgl, Fed_Forward_Glaucoma = heopfgl, Confirmed_diagnosis_Glaucoma = heopcgl, Disputed_diagnosis_Glaucoma = heopngl,
    Still_Has_Glaucoma = heopsgl
  ) %>%
  mutate(Glaucoma = case_when(
    (Fed_Forward_Glaucoma == 1 & Confirmed_diagnosis_Glaucoma == 1 & Still_Has_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 & Disputed_diagnosis_Glaucoma == 3)) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Glaucoma)
head(Glaucoma_Data_7, 10)

Glaucoma_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, heoptgl, heopfgl, heopcgl, heopngl, heopsgl) %>%
  rename(
    Newly_Reported_Glaucoma = heoptgl, Fed_Forward_Glaucoma = heopfgl, Confirmed_diagnosis_Glaucoma = heopcgl, Disputed_diagnosis_Glaucoma = heopngl,
    Still_Has_Glaucoma = heopsgl
  ) %>%
  mutate(Glaucoma = case_when(
    (Fed_Forward_Glaucoma == 1 & Confirmed_diagnosis_Glaucoma == 1 & Still_Has_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 & Disputed_diagnosis_Glaucoma == 3)) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Glaucoma)
head(Glaucoma_Data_8, 10)

Glaucoma_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, heoptgl, heopfgl, heopcgl, heopngl, heopsgl) %>%
  rename(
    Newly_Reported_Glaucoma = heoptgl, Fed_Forward_Glaucoma = heopfgl, Confirmed_diagnosis_Glaucoma = heopcgl, Disputed_diagnosis_Glaucoma = heopngl,
    Still_Has_Glaucoma = heopsgl
  ) %>%
  mutate(Glaucoma = case_when(
    (Fed_Forward_Glaucoma == 1 & Confirmed_diagnosis_Glaucoma == 1 & Still_Has_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 & Disputed_diagnosis_Glaucoma == 3)) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 1) ~ 1,
    ((Fed_Forward_Glaucoma != 1 | Confirmed_diagnosis_Glaucoma != 1 | Still_Has_Glaucoma != 1) & Newly_Reported_Glaucoma == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Glaucoma)
head(Glaucoma_Data_9, 10)
###   macular degeneration (wave 4 to 9)  ######
# heoptmd :macular degeneration diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# heopfmd :Diagnosed macular degeneration fed forward (3:macular degeneration,<0: missing)
# if heopfmd =3, then heopcmd : whether confirms macular degeneration (1:Yes, 2:No, other values (positive or negative: missing)
# if heopcmd=2, then heopnmd: Reason disputed macular degeneration diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if heopfmd =1, then heopsmd :whether still has macular degeneration (1:Yes, 2:No, other values (positive or negative: missing)
Macular_Degeneration_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, heoptmd, heopfmd, heopcmd, heopnmd, heopsmd) %>%
  rename(
    Newly_Reported_Macular_Degeneration = heoptmd, Fed_Forward_Macular_Degeneration = heopfmd, Confirmed_diagnosis_Macular_Degeneration = heopcmd, Disputed_diagnosis_Macular_Degeneration = heopnmd,
    Still_Has_Macular_Degeneration = heopsmd
  ) %>%
  mutate(Macular_Degeneration = case_when(
    (Fed_Forward_Macular_Degeneration == 3 & Confirmed_diagnosis_Macular_Degeneration == 1 & Still_Has_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 & Disputed_diagnosis_Macular_Degeneration == 3)) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Macular_Degeneration)
head(Macular_Degeneration_Data_4)

Macular_Degeneration_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, heoptmd, heopfmd, heopcmd, heopnmd, heopsmd) %>%
  rename(
    Newly_Reported_Macular_Degeneration = heoptmd, Fed_Forward_Macular_Degeneration = heopfmd, Confirmed_diagnosis_Macular_Degeneration = heopcmd, Disputed_diagnosis_Macular_Degeneration = heopnmd,
    Still_Has_Macular_Degeneration = heopsmd
  ) %>%
  mutate(Macular_Degeneration = case_when(
    (Fed_Forward_Macular_Degeneration == 3 & Confirmed_diagnosis_Macular_Degeneration == 1 & Still_Has_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 & Disputed_diagnosis_Macular_Degeneration == 3)) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Macular_Degeneration)
head(Macular_Degeneration_Data_5)

Macular_Degeneration_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, heoptmd, heopfmd, heopcmd, heopnmd, heopsmd) %>%
  rename(
    Newly_Reported_Macular_Degeneration = heoptmd, Fed_Forward_Macular_Degeneration = heopfmd, Confirmed_diagnosis_Macular_Degeneration = heopcmd, Disputed_diagnosis_Macular_Degeneration = heopnmd,
    Still_Has_Macular_Degeneration = heopsmd
  ) %>%
  mutate(Macular_Degeneration = case_when(
    (Fed_Forward_Macular_Degeneration == 3 & Confirmed_diagnosis_Macular_Degeneration == 1 & Still_Has_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 & Disputed_diagnosis_Macular_Degeneration == 3)) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Macular_Degeneration)
head(Macular_Degeneration_Data_6)

Macular_Degeneration_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, heoptmd, heopfmd, heopcmd, heopnmd, heopsmd) %>%
  rename(
    Newly_Reported_Macular_Degeneration = heoptmd, Fed_Forward_Macular_Degeneration = heopfmd, Confirmed_diagnosis_Macular_Degeneration = heopcmd, Disputed_diagnosis_Macular_Degeneration = heopnmd,
    Still_Has_Macular_Degeneration = heopsmd
  ) %>%
  mutate(Macular_Degeneration = case_when(
    (Fed_Forward_Macular_Degeneration == 3 & Confirmed_diagnosis_Macular_Degeneration == 1 & Still_Has_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 & Disputed_diagnosis_Macular_Degeneration == 3)) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Macular_Degeneration)
head(Macular_Degeneration_Data_7)

Macular_Degeneration_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, heoptmd, heopfmd, heopcmd, heopnmd, heopsmd) %>%
  rename(
    Newly_Reported_Macular_Degeneration = heoptmd, Fed_Forward_Macular_Degeneration = heopfmd, Confirmed_diagnosis_Macular_Degeneration = heopcmd, Disputed_diagnosis_Macular_Degeneration = heopnmd,
    Still_Has_Macular_Degeneration = heopsmd
  ) %>%
  mutate(Macular_Degeneration = case_when(
    (Fed_Forward_Macular_Degeneration == 3 & Confirmed_diagnosis_Macular_Degeneration == 1 & Still_Has_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 & Disputed_diagnosis_Macular_Degeneration == 3)) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Macular_Degeneration)
head(Macular_Degeneration_Data_8)

Macular_Degeneration_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, heoptmd, heopfmd, heopcmd, heopnmd, heopsmd) %>%
  rename(
    Newly_Reported_Macular_Degeneration = heoptmd, Fed_Forward_Macular_Degeneration = heopfmd, Confirmed_diagnosis_Macular_Degeneration = heopcmd, Disputed_diagnosis_Macular_Degeneration = heopnmd,
    Still_Has_Macular_Degeneration = heopsmd
  ) %>%
  mutate(Macular_Degeneration = case_when(
    (Fed_Forward_Macular_Degeneration == 3 & Confirmed_diagnosis_Macular_Degeneration == 1 & Still_Has_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 & Disputed_diagnosis_Macular_Degeneration == 3)) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 1) ~ 1,
    ((Fed_Forward_Macular_Degeneration != 3 | Confirmed_diagnosis_Macular_Degeneration != 1 | Still_Has_Macular_Degeneration != 1) & Newly_Reported_Macular_Degeneration == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Macular_Degeneration)
head(Macular_Degeneration_Data_9)
### diabetic eye disease (wave 4 to 9)  ######
# heoptdi :diabetic eye diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# heopfdi :Diagnosed diabetic eye fed forward (2:diabetic eye,<0: missing)
# if heopfdi =2, then heopcdi : whether confirms diabetic eye (1:Yes, 2:No, other values (positive or negative: missing)
# if heopcdi=2, then heopndi: Reason disputed diabetic eye diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if heopfdi =2, then heopsdi :whether still has diabetic eye (1:Yes, 2:No, other values (positive or negative: missing)
Diabetic_Eye_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, heoptdi, heopfdi, heopcdi, heopndi, heopsdi) %>%
  rename(
    Newly_Reported_Diabetic_Eye = heoptdi, Fed_Forward_Diabetic_Eye = heopfdi, Confirmed_diagnosis_Diabetic_Eye = heopcdi, Disputed_diagnosis_Diabetic_Eye = heopndi,
    Still_Has_Diabetic_Eye = heopsdi
  ) %>%
  mutate(Diabetic_Eye = case_when(
    (Fed_Forward_Diabetic_Eye == 2 & Confirmed_diagnosis_Diabetic_Eye == 1 & Still_Has_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 & Disputed_diagnosis_Diabetic_Eye == 3)) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetic_Eye)
head(Diabetic_Eye_Data_4, 10)

Diabetic_Eye_Data_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, heoptdi, heopfdi, heopcdi, heopndi, heopsdi) %>%
  rename(
    Newly_Reported_Diabetic_Eye = heoptdi, Fed_Forward_Diabetic_Eye = heopfdi, Confirmed_diagnosis_Diabetic_Eye = heopcdi, Disputed_diagnosis_Diabetic_Eye = heopndi,
    Still_Has_Diabetic_Eye = heopsdi
  ) %>%
  mutate(Diabetic_Eye = case_when(
    (Fed_Forward_Diabetic_Eye == 2 & Confirmed_diagnosis_Diabetic_Eye == 1 & Still_Has_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 & Disputed_diagnosis_Diabetic_Eye == 3)) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetic_Eye)
head(Diabetic_Eye_Data_5, 10)

Diabetic_Eye_Data_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, heoptdi, heopfdi, heopcdi, heopndi, heopsdi) %>%
  rename(
    Newly_Reported_Diabetic_Eye = heoptdi, Fed_Forward_Diabetic_Eye = heopfdi, Confirmed_diagnosis_Diabetic_Eye = heopcdi, Disputed_diagnosis_Diabetic_Eye = heopndi,
    Still_Has_Diabetic_Eye = heopsdi
  ) %>%
  mutate(Diabetic_Eye = case_when(
    (Fed_Forward_Diabetic_Eye == 2 & Confirmed_diagnosis_Diabetic_Eye == 1 & Still_Has_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 & Disputed_diagnosis_Diabetic_Eye == 3)) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetic_Eye)
head(Diabetic_Eye_Data_6, 10)

Diabetic_Eye_Data_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, heoptdi, heopfdi, heopcdi, heopndi, heopsdi) %>%
  rename(
    Newly_Reported_Diabetic_Eye = heoptdi, Fed_Forward_Diabetic_Eye = heopfdi, Confirmed_diagnosis_Diabetic_Eye = heopcdi, Disputed_diagnosis_Diabetic_Eye = heopndi,
    Still_Has_Diabetic_Eye = heopsdi
  ) %>%
  mutate(Diabetic_Eye = case_when(
    (Fed_Forward_Diabetic_Eye == 2 & Confirmed_diagnosis_Diabetic_Eye == 1 & Still_Has_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 & Disputed_diagnosis_Diabetic_Eye == 3)) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetic_Eye)
head(Diabetic_Eye_Data_7, 10)

Diabetic_Eye_Data_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, heoptdi, heopfdi, heopcdi, heopndi, heopsdi) %>%
  rename(
    Newly_Reported_Diabetic_Eye = heoptdi, Fed_Forward_Diabetic_Eye = heopfdi, Confirmed_diagnosis_Diabetic_Eye = heopcdi, Disputed_diagnosis_Diabetic_Eye = heopndi,
    Still_Has_Diabetic_Eye = heopsdi
  ) %>%
  mutate(Diabetic_Eye = case_when(
    (Fed_Forward_Diabetic_Eye == 2 & Confirmed_diagnosis_Diabetic_Eye == 1 & Still_Has_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 & Disputed_diagnosis_Diabetic_Eye == 3)) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetic_Eye)
head(Diabetic_Eye_Data_8, 10)

Diabetic_Eye_Data_9 <- Wave_9_datafile %>%
  dplyr::select(idauniq, heoptdi, heopfdi, heopcdi, heopndi, heopsdi) %>%
  rename(
    Newly_Reported_Diabetic_Eye = heoptdi, Fed_Forward_Diabetic_Eye = heopfdi, Confirmed_diagnosis_Diabetic_Eye = heopcdi, Disputed_diagnosis_Diabetic_Eye = heopndi,
    Still_Has_Diabetic_Eye = heopsdi
  ) %>%
  mutate(Diabetic_Eye = case_when(
    (Fed_Forward_Diabetic_Eye == 2 & Confirmed_diagnosis_Diabetic_Eye == 1 & Still_Has_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 & Disputed_diagnosis_Diabetic_Eye == 3)) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 1) ~ 1,
    ((Fed_Forward_Diabetic_Eye != 2 | Confirmed_diagnosis_Diabetic_Eye != 1 | Still_Has_Diabetic_Eye != 1) & Newly_Reported_Diabetic_Eye == 0) ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(idauniq, Diabetic_Eye)
head(Diabetic_Eye_Data_9, 10)


##### Psychiatric problems (wave 4 to 9) ####
Psychiatric_Problems <- Wave_4_datafile %>%
  dplyr::select(idauniq, hepsyha, hepsyan, hepsyde, hepsyem, hepsymo) %>%
  rename(Hallucination = hepsyha, Anxiety = hepsyan, Depression = hepsyde, Emotional_Problems = hepsyem, Mood_Swings = hepsymo)

Psychiatric_Problems_Data_4 <- replace(Psychiatric_Problems, Psychiatric_Problems < 0, -99)

head(Psychiatric_Problems_Data_4)

Psychiatric_Problems <- Wave_5_datafile %>%
  dplyr::select(idauniq, hepsyha, hepsyan, hepsyde, hepsyem, hepsymo) %>%
  rename(Hallucination = hepsyha, Anxiety = hepsyan, Depression = hepsyde, Emotional_Problems = hepsyem, Mood_Swings = hepsymo)

Psychiatric_Problems_Data_5 <- replace(Psychiatric_Problems, Psychiatric_Problems < 0, -99)

head(Psychiatric_Problems_Data_5)

Psychiatric_Problems <- Wave_6_datafile %>%
  dplyr::select(idauniq, hepsyha, hepsyan, hepsyde, hepsyem, hepsymo) %>%
  rename(Hallucination = hepsyha, Anxiety = hepsyan, Depression = hepsyde, Emotional_Problems = hepsyem, Mood_Swings = hepsymo)

Psychiatric_Problems_Data_6 <- replace(Psychiatric_Problems, Psychiatric_Problems < 0, -99)

head(Psychiatric_Problems_Data_6)

Psychiatric_Problems <- Wave_7_datafile %>%
  dplyr::select(idauniq, hepsyha, hepsyan, hepsyde, hepsyem, hepsymo) %>%
  rename(Hallucination = hepsyha, Anxiety = hepsyan, Depression = hepsyde, Emotional_Problems = hepsyem, Mood_Swings = hepsymo)

Psychiatric_Problems_Data_7 <- replace(Psychiatric_Problems, Psychiatric_Problems < 0, -99)

head(Psychiatric_Problems_Data_7)

Psychiatric_Problems <- Wave_8_datafile %>%
  dplyr::select(idauniq, hepsyha, hepsyan, hepsyde, hepsyem, hepsymo) %>%
  rename(Hallucination = hepsyha, Anxiety = hepsyan, Depression = hepsyde, Emotional_Problems = hepsyem, Mood_Swings = hepsymo)

Psychiatric_Problems_Data_8 <- replace(Psychiatric_Problems, Psychiatric_Problems < 0, -99)

head(Psychiatric_Problems_Data_8)

Psychiatric_Problems <- Wave_9_datafile %>%
  dplyr::select(idauniq, hepsyha, hepsyan, hepsyde, hepsyem, hepsymo) %>%
  rename(Hallucination = hepsyha, Anxiety = hepsyan, Depression = hepsyde, Emotional_Problems = hepsyem, Mood_Swings = hepsymo)

Psychiatric_Problems_Data_9 <- replace(Psychiatric_Problems, Psychiatric_Problems < 0, -99)

head(Psychiatric_Problems_Data_9)

#### Merging disease data  ####
Chronic_Conditions_Wave4 <- list(
  Blood_Pressure_Data_4, Diabetes_Data_4, Cancer_Data_4, Lung_Disease_Data_4,
  Stroke_Data_4, Osteoperosis_Data_4, Abnormal_Heart_Rhythm_Data_4,
  Heart_Murmur_Data_4, Heart_Failure_Data_4, Heart_Attack_Data_4, Angina_Data_4,
  Parkinsons_Data_4, Cataract_Data_4,
  Asthma_Data_4, Arthritis_Data_4, Alzheimers_Data_4, Dementia_Data_4,
  Glaucoma_Data_4, Macular_Degeneration_Data_4, Diabetic_Eye_Data_4,
  Psychiatric_Problems_Data_4
) %>%
  reduce(inner_join, by = "idauniq") %>% 
rename_with(.cols = -1,.fn = function(.x){paste0(.x,"_4")})
head(Chronic_Conditions_Wave4)

Chronic_Conditions_Wave5 <- list(
  Blood_Pressure_Data_5, Diabetes_Data_5, Cancer_Data_5, Lung_Disease_Data_5,
  Stroke_Data_5, Osteoperosis_Data_5, Abnormal_Heart_Rhythm_Data_5,
  Heart_Murmur_Data_5, Heart_Failure_Data_5, Heart_Attack_Data_5, Angina_Data_5,
  Parkinsons_Data_5, Cataract_Data_5,
  Asthma_Data_5, Arthritis_Data_5, Alzheimers_Data_5, Dementia_Data_5,
  Glaucoma_Data_5, Macular_Degeneration_Data_5, Diabetic_Eye_Data_5,
  Psychiatric_Problems_Data_5
) %>%
  reduce(inner_join, by = "idauniq") %>% 
  rename_with(.cols = -1,.fn = function(.x){paste0(.x,"_5")})
head(Chronic_Conditions_Wave5)

Chronic_Conditions_Wave6 <- list(
  Blood_Pressure_Data_6, Diabetes_Data_6, Cancer_Data_6, Lung_Disease_Data_6,
  Stroke_Data_6, Osteoperosis_Data_6, Abnormal_Heart_Rhythm_Data_6,
  Heart_Murmur_Data_6, Heart_Failure_Data_6, Heart_Attack_Data_6, Angina_Data_6,
  Parkinsons_Data_6, Cataract_Data_6,
  Asthma_Data_6, Arthritis_Data_6, Alzheimers_Data_6, Dementia_Data_6,
  Glaucoma_Data_6, Macular_Degeneration_Data_6, Diabetic_Eye_Data_6,
  Psychiatric_Problems_Data_6
) %>%
  reduce(inner_join, by = "idauniq") %>% 
  rename_with(.cols = -1,.fn = function(.x){paste0(.x,"_6")})
head(Chronic_Conditions_Wave6)

Chronic_Conditions_Wave7 <- list(
  Blood_Pressure_Data_7, Diabetes_Data_7, Cancer_Data_7, Lung_Disease_Data_7,
  Stroke_Data_7, Osteoperosis_Data_7, Abnormal_Heart_Rhythm_Data_7,
  Heart_Murmur_Data_7, Heart_Failure_Data_7, Heart_Attack_Data_7, Angina_Data_7,
  Parkinsons_Data_7, Cataract_Data_7,
  Asthma_Data_7, Arthritis_Data_7, Alzheimers_Data_7, Dementia_Data_7,
  Glaucoma_Data_7, Macular_Degeneration_Data_7, Diabetic_Eye_Data_7,
  Psychiatric_Problems_Data_7
) %>%
  reduce(inner_join, by = "idauniq") %>% 
  rename_with(.cols = -1,.fn = function(.x){paste0(.x,"_7")})
head(Chronic_Conditions_Wave7)

Chronic_Conditions_Wave8 <- list(
  Blood_Pressure_Data_8, Diabetes_Data_8, Cancer_Data_8, Lung_Disease_Data_8,
  Stroke_Data_8, Osteoperosis_Data_8, Abnormal_Heart_Rhythm_Data_8,
  Heart_Murmur_Data_8, Heart_Failure_Data_8, Heart_Attack_Data_8, Angina_Data_8,
  Parkinsons_Data_8, Cataract_Data_8,
  Asthma_Data_8, Arthritis_Data_8, Alzheimers_Data_8, Dementia_Data_8,
  Glaucoma_Data_8, Macular_Degeneration_Data_8, Diabetic_Eye_Data_8,
  Psychiatric_Problems_Data_8
) %>%
  reduce(inner_join, by = "idauniq") %>% 
  rename_with(.cols = -1,.fn = function(.x){paste0(.x,"_8")})
head(Chronic_Conditions_Wave8)

Chronic_Conditions_Wave9 <- list(
  Blood_Pressure_Data_9, Diabetes_Data_9, Cancer_Data_9, Lung_Disease_Data_9,
  Stroke_Data_9, Osteoperosis_Data_9, Abnormal_Heart_Rhythm_Data_9,
  Heart_Murmur_Data_9, Heart_Failure_Data_9, Heart_Attack_Data_9, Angina_Data_9,
  Parkinsons_Data_9, Cataract_Data_9,
  Asthma_Data_9, Arthritis_Data_9, Alzheimers_Data_9, Dementia_Data_9,
  Glaucoma_Data_9, Macular_Degeneration_Data_9, Diabetic_Eye_Data_9,
  Psychiatric_Problems_Data_9
) %>%
  reduce(inner_join, by = "idauniq")  %>% 
  rename_with(.cols = -1,.fn = function(.x){paste0(.x,"_9")})
head(Chronic_Conditions_Wave9)

Diseases <- list(Chronic_Conditions_Wave4, Chronic_Conditions_Wave5, 
                 Chronic_Conditions_Wave6, Chronic_Conditions_Wave7,
                 Chronic_Conditions_Wave8, Chronic_Conditions_Wave9, Weights) %>% 
  reduce(inner_join, by = "idauniq")
head(Diseases)

nrow(Diseases)


write.csv(Diseases, file = here("Data","Processed_Data","R_Data","Health_Outcomes","HMM_Clustering","Outcomes_4to9.csv"), row.names = FALSE)




