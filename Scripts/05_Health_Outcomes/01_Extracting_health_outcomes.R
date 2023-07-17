

Wave_9_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_9_elsa_data_eul_v1.tab"),
  header = TRUE, sep = "\t"
)
#### chose the merged versions for new reports of any cardiovascular condition, with a (merged) in the name in the data disctionary
#####
# hedimbp :high blood pressure diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawbp :Diagnosed high BP fed forward (1:high BP,<0: missing)
# if hedawbp =1, then hedacbp : whether confirms high BP (1:Yes, 2:No, other values (positive or negative: missing)
# if hedacbp=2, then hedanbp: Reason disputed high BP diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedawbp =1, then hedasbp :whether still has high BP (1:Yes, 2:No, other values (positive or negative: missing)
Blood_Pressure_Data <- Wave_9_datafile %>%
  dplyr::select(idauniq, hedimbp, hedawbp, hedanbp, hedacbp, hedasbp) %>%
  rename(Newly_Reported_BP = hedimbp, Fed_Forward_BP = hedawbp, Confirmed_diagnosis_BP = hedacbp, Disputed_diagnosis_BP = hedanbp,
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
head(Blood_Pressure_Data)
#### Extracting data on diabetes ####
# hedimdi :diabetes diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawdi :Diagnosed diabetes fed forward (7:Diabetes,<0: missing)
# if hedawdi =7, then hedacdi : whether confirms diabetes (1:Yes, 2:No, other values (positive or negative: missing)
# if hedacdi=2, then hedandi: Reason disputed diabetes diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# no question for 'still has diabetes'
Diabetes_Data <- Wave_9_datafile %>%
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

head(Diabetes_Data)

#### Extracting data on cancer ####
# hedibca :cancer diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwca :Diagnosed cancer fed forward (5:cancer,<0: missing)
# if hedbwca =5, then hedbdca : whether confirms cancer (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdca=2, then hedbmca: Reason disputed cancer diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwca =1, then hedbsca :whether still has cancer (1:Yes, 2:No, other values (positive or negative: missing)
Cancer_Data <- Wave_9_datafile %>%
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
tail(Cancer_Data, 20)

#### Extracting data on lung disease ####
# hediblu :Chronic lung disease diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwlu :Diagnosed Chronic lung disease fed forward (1:Chronic lung disease,<0: missing)
# if hedbwlu =1, then hedbdlu : whether confirms Chronic lung disease (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdlu=2, then hedbmlu: Reason disputed Chronic lung disease diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwlu =1, then hedblu :whether still has Chronic lung disease (1:Yes, 2:No, other values (positive or negative: missing)
Lung_Disease_Data <- Wave_9_datafile %>%
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

head(Lung_Disease_Data)

#### Extracting data on stroke ####
# hedimst : Stroke diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawst :Diagnosed  Stroke fed forward (8: Stroke,<0: missing)
# if hedawst =8, then hedacst : whether confirms  Stroke (1:Yes, 2:No, other values (positive or negative: missing)
# if hedacst=2, then hedanst: Reason disputed  Stroke diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# no question for 'still has stroke'
Stroke_Data <- Wave_9_datafile %>%
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

head(Stroke_Data, 60)

#### Extracting data on osteoporosis ####
# hedibos :osteoperosis diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwos :Diagnosed osteoperosis fed forward (4:osteoperosis,<0: missing)
# if hedbwos =4, then hedbdos : whether confirms osteoperosis (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdos=2, then hedbmos: Reason disputed osteoperosis diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwos =4, then hedbsos :whether still has osteoperosis (1:Yes, 2:No, other values (positive or negative: missing)
Osteoperosis_Data <- Wave_9_datafile %>%
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
head(Osteoperosis_Data, 10)

#### Extracting data on abnormal heart rhythms ####
# hedimar :abnormal heart rhythm diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawar :Diagnosed abnormal heart rhythm fed forward (6:abnormal heart rhythm,<0: missing)
# if hedawar =6, then hedacar : whether confirms abnormal heart rhythm (1:Yes, 2:No, other values (positive or negative: missing)
# if hedacar=2, then hedanar: Reason disputed abnormal heart rhythm diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedawar =6, then hedasar :whether still has abnormal heart rhythm (1:Yes, 2:No, other values (positive or negative: missing)
Abnormal_Heart_Rhythm_Data <- Wave_9_datafile %>%
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
tail(Abnormal_Heart_Rhythm_Data, 20)

#### Extracting data on heart murmur ####
# hedimhm :heart murmur diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawhm :Diagnosed heart murmur fed forward (5:heart murmur,<0: missing)
# if hedawhm =5, then hedachm : whether confirms heart murmur (1:Yes, 2:No, other values (positive or negative: missing)
# if hedachm=2, then hedanhm: Reason disputed heart murmur diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedawhm =5, then hedashm :whether still has heart murmur (1:Yes, 2:No, other values (positive or negative: missing)
Heart_Murmur_Data <- Wave_9_datafile %>%
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
tail(Heart_Murmur_Data, 40)

#### Extracting data on congestive heart failure ####
# hedimhf :congestive heart failure diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwos :Diagnosed congestive heart failure fed forward (4:congestive heart failure,<0: missing)
# if hedbwos =4, then hedbdos : whether confirms congestive heart failure (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdos=2, then hedbmos: Reason disputed congestive heart failure diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwos =4, then hedbsos :whether still has congestive heart failure (1:Yes, 2:No, other values (positive or negative: missing)
Heart_Failure_Data <- Wave_9_datafile %>%
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
head(Heart_Failure_Data)

#### Extracting data on heart attack ####
# hedimmi :heart attack diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawmi :Diagnosed heart attack fed forward (3:heart attack,<0: missing)
# then hedanmi: Reason disputed heart attack diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
Heart_Attack_Data <- Wave_9_datafile %>%
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
tail(Heart_Attack_Data)

#### Extracting data on  angina ####
# hediman :angina diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawan :Diagnosed angina fed forward (2:angina,<0: missing)
# hedanan: Reason disputed angina diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
Angina_Data <- Wave_9_datafile %>%
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
tail(Angina_Data, 40)

#### Extracting data on parkinsons disease ####

# hedibpd :parkinsons disease diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwpd :Diagnosed parkinsons disease fed forward (6:parkinsons disease,<0: missing)
# if hedbwpd =6, then hedbdpd : whether confirms parkinsons disease (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdpd=2, then hedbmpd: Reason disputed parkinsons disease diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwpd =6, then hedbspd :whether still has parkinsons disease (1:Yes, 2:No, other values (positive or negative: missing)
Parkinsons_Data <- Wave_9_datafile %>%
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
tail(Parkinsons_Data, 10)

#### Extracting data on cataracts ####
# heoptca :cataract diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# heopfca :Diagnosed cataract fed forward (4:cataract,<0: missing)
# if heopfca =4, then heopcca : whether confirms cataract (1:Yes, 2:No, other values (positive or negative: missing)
# if heopcca=2, then heopnca: Reason disputed cataract diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if heopfca =4, then heopsca :whether still has cataract (1:Yes, 2:No, other values (positive or negative: missing)
Cataract_Data <- Wave_9_datafile %>%
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
head(Cataract_Data, 10)

#### Extracting data on asthma ####
# hedibas :asthma diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwas :Diagnosed asthma fed forward (2:asthma,<0: missing)
# if hedbwas =2, then hedbdas : whether confirms asthma (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdas=2, then hedbmas: Reason disputed asthma diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwas =2, then hedbsas :whether still has asthma (1:Yes, 2:No, other values (positive or negative: missing)
Asthma_Data <- Wave_9_datafile %>%
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
head(Asthma_Data, 40)

#### Extracting data on arthritis ####
# hedibar :arthritis diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwar :Diagnosed arthritis fed forward (3:arthritis,<0: missing)
# if hedbwar =3, then hedbdar : whether confirms arthritis (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdar=2, then hedbmar: Reason disputed arthritis diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwar =3, then hedbsar :whether still has arthritis (1:Yes, 2:No, other values (positive or negative: missing)
Arthritis_Data <- Wave_9_datafile %>%
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
head(Arthritis_Data)

#### Extracting data on Alzheimer's ####
# hedibad :alzheimers diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwad :Diagnosed alzheimers fed forward (8:alzheimers,<0: missing)
# if hedbwad =8, then hedbdad : whether confirms alzheimers (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdad=2, then hedbmad: Reason disputed alzheimers diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
#
Alzheimers_Data <- Wave_9_datafile %>%
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
head(Alzheimers_Data)

#### Extracting data on Dementia ####
# hedibde :dementia diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwde :Diagnosed dementia fed forward (9:dementia,<0: missing)
# if hedbwde =9, then hedbdde : whether confirms dementia (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdar=2, then hedbmde: Reason disputed dementia diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwde =9, then hedbsde :whether still has dementia (1:Yes, 2:No, other values (positive or negative: missing)
Dementia_Data <- Wave_9_datafile %>%
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
head(Dementia_Data, 10)
##### Glaucoma ######
# heoptgl :glaucoma diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# heopfgl :Diagnosed glaucoma fed forward (1:glaucoma,<0: missing)
# if heopfgl =1, then heopcgl : whether confirms glaucoma (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdar=2, then heopngl: Reason disputed glaucoma diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if heopfgl =1, then heopsgl :whether still has glaucoma (1:Yes, 2:No, other values (positive or negative: missing)
Glaucoma_Data <- Wave_9_datafile %>%
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
head(Glaucoma_Data, 10)

###   macular degeneration  ######
# heoptmd :macular degeneration diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# heopfmd :Diagnosed macular degeneration fed forward (3:macular degeneration,<0: missing)
# if heopfmd =3, then heopcmd : whether confirms macular degeneration (1:Yes, 2:No, other values (positive or negative: missing)
# if heopcmd=2, then heopnmd: Reason disputed macular degeneration diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if heopfmd =1, then heopsmd :whether still has macular degeneration (1:Yes, 2:No, other values (positive or negative: missing)
Macular_Degeneration_Data <- Wave_9_datafile %>%
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
head(Macular_Degeneration_Data, 40)
### diabetic eye disease   ######
# heoptdi :diabetic eye diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# heopfdi :Diagnosed diabetic eye fed forward (2:diabetic eye,<0: missing)
# if heopfdi =2, then heopcdi : whether confirms diabetic eye (1:Yes, 2:No, other values (positive or negative: missing)
# if heopcdi=2, then heopndi: Reason disputed diabetic eye diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if heopfdi =2, then heopsdi :whether still has diabetic eye (1:Yes, 2:No, other values (positive or negative: missing)
Diabetic_Eye_Data <- Wave_9_datafile %>%
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
head(Diabetic_Eye_Data, 10)

##### Psychiatric problems ####

Psychiatric_Problems <- Wave_9_datafile %>%
  dplyr::select(idauniq, hepsyha, hepsyan, hepsyde, hepsyem, hepsymo) %>%
  rename(Hallucination = hepsyha, Anxiety = hepsyan, Depression = hepsyde, Emotional_Problems = hepsyem, Mood_Swings = hepsymo)

Psychiatric_Problems_Data <- replace(Psychiatric_Problems, Psychiatric_Problems < 0, -99)

head(Psychiatric_Problems_Data)

# find lung cancer data as well
Lung_Cancer_Data <- Wave_9_datafile %>%
  dplyr::select(idauniq, hecanaar) %>%
  mutate(Lung_Cancer = case_when(
    hecanaar == 1 ~ 1,
    hecanaar != 1 & hecanaar > 1 ~ 0,
    hecanaar != 1 & hecanaar < 0 ~ -99
  )) %>%
  dplyr::select(idauniq, Lung_Cancer)
head(Lung_Cancer_Data, 20)

### Merging disease data for Wave 9
Chronic_Conditions_Wave9 <- list(
  Blood_Pressure_Data, Diabetes_Data, Cancer_Data, Lung_Disease_Data,
  Stroke_Data, Osteoperosis_Data, Abnormal_Heart_Rhythm_Data,
  Heart_Murmur_Data, Heart_Failure_Data, Heart_Attack_Data, Angina_Data,
  Parkinsons_Data, Cataract_Data,
  Asthma_Data, Arthritis_Data, Alzheimers_Data, Dementia_Data,
  Glaucoma_Data, Macular_Degeneration_Data, Diabetic_Eye_Data,
  Psychiatric_Problems_Data, Lung_Cancer_Data
) %>%
  reduce(inner_join, by = "idauniq")
head(Chronic_Conditions_Wave9)

############# Calculating multimorbidity #################
## convert any missing data(i.e. coded as 99) to NA
Chronic_Conditions_Wave9[Chronic_Conditions_Wave9 == -99] <- NA
ncol(Chronic_Conditions_Wave9)
Chronic_Conditions_Wave9$Number_of_Conditions <- rowSums(Chronic_Conditions_Wave9[, -c(1, 27)], na.rm = TRUE)
head(Chronic_Conditions_Wave9, 400)
summary(Chronic_Conditions_Wave9)
Multimorbidity <- Chronic_Conditions_Wave9 %>%
  # if has 2 or more conditions - then has M_Present : coded as 1
  # else coded as 0 else -99 if missing
  mutate(M_Present = case_when(
    Number_of_Conditions >= 2 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::select(idauniq, M_Present)
head(Multimorbidity, 20)

############# Calculating complex multimorbidity #################
# Complex multimorbidity is a measure of three or more affected body systems
# Singer et. al, 2019
# The body systems are listed as new column names
Chronic_Conditions_Wave9[is.na(Chronic_Conditions_Wave9)] <- -99

Complex_Multimorbidity <- Chronic_Conditions_Wave9 %>%
  mutate(
    Eye_disorders = case_when(
      Glaucoma == 1 | Macular_Degeneration == 1 | Cataract == 1 ~ 1,
      Glaucoma == 0 & Macular_Degeneration == 0 & Cataract == 0 ~ 0,
      TRUE ~ -99
    ),
    Circulatory_disorders = case_when(
      Blood_Pressure == 1 | Angina == 1 | Heart_Attack == 1 |
        Heart_Failure == 1 | Heart_Murmur == 1 | Abnormal_Heart_Rhythm == 1 |
        Stroke == 1 ~ 1,
      Blood_Pressure == 0 & Angina == 0 & Heart_Attack == 0 &
        Heart_Failure == 0 & Heart_Murmur == 0 & Abnormal_Heart_Rhythm == 0 &
        Stroke == 0 ~ 0,
      TRUE ~ -99
    ),
    Endocrine_nutritional_metabolic = case_when(
      Diabetic_Eye == 1 | Diabetes == 1 ~ 1,
      Diabetic_Eye == 0 & Diabetes == 0 ~ 0,
      TRUE ~ -99
    ),
    Musculoskeletal_connective_system = case_when(
      Arthritis == 1 | Osteoperosis == 1 ~ 1,
      Arthritis == 0 & Osteoperosis == 0 ~ 0,
      TRUE ~ -99
    ),
    Respiratory = case_when(
      Lung_Disease == 1 | Asthma == 1 ~ 1,
      Lung_Disease == 0 & Asthma == 0 ~ 0,
      TRUE ~ -99
    ),
    Neoplasms = case_when(
      Cancer == 1 ~ 1,
      Cancer == 0 ~ 0,
      TRUE ~ -99
    ),
    Nervous_disorders = case_when(
      Parkinsons == 1 | Dementia == 1 | Alzheimers == 1 | Hallucination == 1 ~ 1,
      Parkinsons == 0 & Dementia == 0 & Alzheimers == 0 & Hallucination == 0 ~ 0,
      TRUE ~ -99
    ),
    Mental_behavioural = case_when(
      Anxiety == 1 | Depression == 1 | Emotional_Problems == 1 | Mood_Swings == 1 ~ 1,
      Anxiety == 0 & Depression == 0 & Emotional_Problems == 0 & Mood_Swings == 0 ~ 0,
      TRUE ~ -99
    )
  )

head(Complex_Multimorbidity)
Complex_Multimorbidity <- Complex_Multimorbidity %>%
  dplyr::select(c(1, 29:36))
Complex_Multimorbidity[Complex_Multimorbidity == -99] <- NA

Complex_Multimorbidity$Body_Systems_Affected <- rowSums(Complex_Multimorbidity[, -1], na.rm = TRUE)

# To calculate complex multimorbidity, a column CM_Present was created
# If three or more body systems are affected (i.e. equal to 1), CM_Present =1.

Complex_Multimorbidity <- Complex_Multimorbidity %>%
  mutate(CM_Present = case_when(
    Body_Systems_Affected >= 3 ~ 1,
    Body_Systems_Affected < 3 & Body_Systems_Affected > -1 ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(-(Body_Systems_Affected))
head(Complex_Multimorbidity)

Health_outcomes_W9 <- inner_join(Multimorbidity, Complex_Multimorbidity, by = "idauniq")
#### Adding lung cancer as an outcome to the modified dataset on health outcomes
Health_outcomes_withLungCancer <- inner_join(Health_outcomes_W9, Lung_Cancer_Data, by = "idauniq")
head(Health_outcomes_withLungCancer)
### rearranging the columns so that the body system disorders come first, followed by
# multimorbidity and complex multimorbidity and finally, lung cancer
Health_outcomes_withLungCancer <- Health_outcomes_withLungCancer[, c(1, 3:10, 2, 11, 12)]
Health_outcomes_withLungCancer[is.na(Health_outcomes_withLungCancer) == TRUE] <- -99
head(Health_outcomes_withLungCancer)

write.csv(Health_outcomes_withLungCancer, file = "Data/Processed_Data/R_Data/Health_Outcomes/Health_Outcomes_withBodySystems_Wave9.csv", row.names = FALSE)


######## Adding health outcomes from Wave 4 ######

#### chose the merged versions for new reports of any cardiovascular condition, with a (merged) in the name in the data disctionary
#####
# hedimbp :high blood pressure diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedawbp :Diagnosed high BP fed forward (1:high BP,<0: missing)
# if hedawbp =1, then hedacbp : whether confirms high BP (1:Yes, 2:No, other values (positive or negative: missing)
# if hedacbp=2, then hedanbp: Reason disputed high BP diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedawbp =1, then hedasbp :whether still has high BP (1:Yes, 2:No, other values (positive or negative: missing)

Wave_4_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", 
                                "wave_4_elsa_data_v3.tab"), header = TRUE, sep = "\t")

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
#### Extracting data on diabetes ####
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

#### Extracting data on cancer ####
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
tail(Cancer_Data_4)

#### Extracting data on lung disease ####
# hediblu :Chronic lung disease diagnosis newly reported (1: mentioned, 0:not mentioned, <0: missing)
# hedbwlu :Diagnosed Chronic lung disease fed forward (1:Chronic lung disease,<0: missing)
# if hedbwlu =1, then hedbdlu : whether confirms Chronic lung disease (1:Yes, 2:No, other values (positive or negative: missing)
# if hedbdlu=2, then hedbmlu: Reason disputed Chronic lung disease diagnosis fed forward (1: never had, 2: no longer has, 3: did not have previously but has now, 4:misdiganosed, <0:missing)
# if hedbwlu =1, then hedbslu :whether still has Chronic lung disease (1:Yes, 2:No, other values (positive or negative: missing)
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

#### Extracting data on stroke ####
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

#### Extracting data on osteoporosis ####
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

#### Extracting data on abnormal heart rhythms ####
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
tail(Abnormal_Heart_Rhythm_Data_4)

#### Extracting data on heart murmur ####
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
tail(Heart_Murmur_Data_4)

#### Extracting data on congestive heart failure ####
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

#### Extracting data on heart attack ####
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

#### Extracting data on  angina ####
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
tail(Angina_Data_4)

#### Extracting data on parkinsons disease ####

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
tail(Parkinsons_Data_4)

#### Extracting data on cataracts ####
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

#### Extracting data on asthma ####
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

#### Extracting data on arthritis ####
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

#### Extracting data on Alzheimer's ####
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

#### Extracting data on Dementia ####
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
head(Dementia_Data_4)
##### Glaucoma ######
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
head(Glaucoma_Data_4)

###   macular degeneration  ######
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
### diabetic eye disease   ######
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
head(Diabetic_Eye_Data_4)

##### Psychiatric problems ####

Psychiatric_Problems <- Wave_4_datafile %>%
  dplyr::select(idauniq, hepsyha, hepsyan, hepsyde, hepsyem, hepsymo) %>%
  rename(Hallucination = hepsyha, Anxiety = hepsyan, Depression = hepsyde, Emotional_Problems = hepsyem, Mood_Swings = hepsymo)

Psychiatric_Problems_Data_4 <- replace(Psychiatric_Problems, Psychiatric_Problems < 0, -99)

head(Psychiatric_Problems_Data_4)

# find lung cancer data as well
Lung_Cancer_Data_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, hecanaa) %>%
  mutate(Lung_Cancer = case_when(
    hecanaa == 1 ~ 1,
    hecanaa != 1 & hecanaa > 1 ~ 0,
    hecanaa != 1 & hecanaa < 0 ~ -99
  )) %>%
  dplyr::select(idauniq, Lung_Cancer)
head(Lung_Cancer_Data_4)

### Merging disease data for Wave 4
Chronic_Conditions_Wave4 <- list(
  Blood_Pressure_Data_4, Diabetes_Data_4, Cancer_Data_4, Lung_Disease_Data_4,
  Stroke_Data_4, Osteoperosis_Data_4, Abnormal_Heart_Rhythm_Data_4,
  Heart_Murmur_Data_4, Heart_Failure_Data_4, Heart_Attack_Data_4, Angina_Data_4,
  Parkinsons_Data_4, Cataract_Data_4,
  Asthma_Data_4, Arthritis_Data_4, Alzheimers_Data_4, Dementia_Data_4,
  Glaucoma_Data_4, Macular_Degeneration_Data_4, Diabetic_Eye_Data_4,
  Psychiatric_Problems_Data_4, Lung_Cancer_Data_4
) %>%
  reduce(inner_join, by = "idauniq")
head(Chronic_Conditions_Wave4)

############# Calculating multimorbidity #################
## convert any missing data(i.e. coded as 99) to NA
Chronic_Conditions_Wave4[Chronic_Conditions_Wave4 == -99] <- NA
ncol(Chronic_Conditions_Wave4)
Chronic_Conditions_Wave4$Number_of_Conditions <- rowSums(Chronic_Conditions_Wave4[, -c(1, 27)], na.rm = TRUE)
head(Chronic_Conditions_Wave4)
summary(Chronic_Conditions_Wave4)
Multimorbidity <- Chronic_Conditions_Wave4 %>%
  # if has 2 or more conditions - then has M_Present : coded as 1
  # else coded as 0 else -99 if missing
  mutate(M_Present = case_when(
    Number_of_Conditions >= 2 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::select(idauniq, M_Present)
head(Multimorbidity, 20)

############# Calculating complex multimorbidity #################
# Complex multimorbidity is a measure of three or more affected body systems
# Singer et. al, 2019
# The body systems are listed as new column names
Chronic_Conditions_Wave4[is.na(Chronic_Conditions_Wave4)] <- -99

Complex_Multimorbidity <- Chronic_Conditions_Wave4 %>%
  mutate(
    Eye_disorders = case_when(
      Glaucoma == 1 | Macular_Degeneration == 1 | Cataract == 1 ~ 1,
      Glaucoma == 0 & Macular_Degeneration == 0 & Cataract == 0 ~ 0,
      TRUE ~ -99
    ),
    Circulatory_disorders = case_when(
      Blood_Pressure == 1 | Angina == 1 | Heart_Attack == 1 |
        Heart_Failure == 1 | Heart_Murmur == 1 | Abnormal_Heart_Rhythm == 1 |
        Stroke == 1 ~ 1,
      Blood_Pressure == 0 & Angina == 0 & Heart_Attack == 0 &
        Heart_Failure == 0 & Heart_Murmur == 0 & Abnormal_Heart_Rhythm == 0 &
        Stroke == 0 ~ 0,
      TRUE ~ -99
    ),
    Endocrine_nutritional_metabolic = case_when(
      Diabetic_Eye == 1 | Diabetes == 1 ~ 1,
      Diabetic_Eye == 0 & Diabetes == 0 ~ 0,
      TRUE ~ -99
    ),
    Musculoskeletal_connective_system = case_when(
      Arthritis == 1 | Osteoperosis == 1 ~ 1,
      Arthritis == 0 & Osteoperosis == 0 ~ 0,
      TRUE ~ -99
    ),
    Respiratory = case_when(
      Lung_Disease == 1 | Asthma == 1 ~ 1,
      Lung_Disease == 0 & Asthma == 0 ~ 0,
      TRUE ~ -99
    ),
    Neoplasms = case_when(
      Cancer == 1 ~ 1,
      Cancer == 0 ~ 0,
      TRUE ~ -99
    ),
    Nervous_disorders = case_when(
      Parkinsons == 1 | Dementia == 1 | Alzheimers == 1 | Hallucination == 1 ~ 1,
      Parkinsons == 0 & Dementia == 0 & Alzheimers == 0 & Hallucination == 0 ~ 0,
      TRUE ~ -99
    ),
    Mental_behavioural = case_when(
      Anxiety == 1 | Depression == 1 | Emotional_Problems == 1 | Mood_Swings == 1 ~ 1,
      Anxiety == 0 & Depression == 0 & Emotional_Problems == 0 & Mood_Swings == 0 ~ 0,
      TRUE ~ -99
    )
  )

head(Complex_Multimorbidity)
Complex_Multimorbidity <- Complex_Multimorbidity %>%
  dplyr::select(c(1, 29:36))
Complex_Multimorbidity[Complex_Multimorbidity == -99] <- NA

Complex_Multimorbidity$Body_Systems_Affected <- rowSums(Complex_Multimorbidity[, -1], na.rm = TRUE)

# To calculate complex multimorbidity, a column CM_Present was created
# If three or more body systems are affected (i.e. equal to 1), CM_Present =1.

Complex_Multimorbidity <- Complex_Multimorbidity %>%
  mutate(CM_Present = case_when(
    Body_Systems_Affected >= 3 ~ 1,
    Body_Systems_Affected < 3 & Body_Systems_Affected > -1 ~ 0,
    TRUE ~ -99
  )) %>%
  dplyr::select(-(Body_Systems_Affected))
head(Complex_Multimorbidity)

Health_outcomes_W4 <- inner_join(Multimorbidity, Complex_Multimorbidity, by = "idauniq")
#### Adding lung cancer as an outcome to the modified dataset on health outcomes
Health_outcomes_withLungCancer_4 <- inner_join(Health_outcomes_W4, Lung_Cancer_Data_4, by = "idauniq")
head(Health_outcomes_withLungCancer_4)
### rearranging the columns so that the body system disorders come first, followed by
# multimorbidity and complex multimorbidity and finally, lung cancer
Health_outcomes_withLungCancer_4 <- Health_outcomes_withLungCancer_4[, c(1, 3:10, 2, 11, 12)]
Health_outcomes_withLungCancer_4[is.na(Health_outcomes_withLungCancer_4) == TRUE] <- -99

colnames(Health_outcomes_withLungCancer_4)[2:12] <- paste(colnames(Health_outcomes_withLungCancer_4)[2:12], "W4", sep = "_")
head(Health_outcomes_withLungCancer_4)


write.csv(Health_outcomes_withLungCancer_4, file = "Data/Processed_Data/R_Data/Health_Outcomes/Health_Outcomes_withBodySystems_Wave4.csv", row.names = FALSE)



#### Combining with the data on health behaviours and demographics


Demographics_Health_Behaviours <- read.csv("Data/Processed_Data/R_Data/All_Health_Behaviours/Health_Behaviours_4to8_With_Demographics.csv",
                                           header = TRUE
)
head(Demographics_Health_Behaviours)
Outcomes_Demographics_Behaviours <- list(Demographics_Health_Behaviours, Health_outcomes_withLungCancer, Health_outcomes_withLungCancer_4) %>% 
  reduce(inner_join, by = "idauniq")


Outcomes_Demographics_Behaviours <- Outcomes_Demographics_Behaviours[, c(1,11,10,2:9,12:31,32:54)]
head(Outcomes_Demographics_Behaviours)


Outcomes_Demographics_Behaviours[Outcomes_Demographics_Behaviours < 0] <- -99
Outcomes_Demographics_Behaviours[is.na(Outcomes_Demographics_Behaviours)] <- -99

write.csv(Outcomes_Demographics_Behaviours, file = "Data/Processed_Data/R_Data/Health_Outcomes/Outcomes_Demographics_Behaviours_4to9.csv", row.names = FALSE)
