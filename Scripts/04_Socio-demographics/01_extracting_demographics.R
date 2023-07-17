
############## LOAD DATA FILES FROM ELSA WAVES 4-9 ############################

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


############## EXTRACT DATA ON SEX & AGE FROM ELSA WAVES 4-8 ############################

Wave_4 <- Wave_4_datafile %>%
  dplyr::select(idauniq, indager, dhsex) %>%
  rename(Sex = dhsex, W4_Age = indager)
head(Wave_4)

Wave_5 <- Wave_5_datafile %>%
  dplyr::select(idauniq, indager, dhsex) %>%
  rename(Sex = dhsex, W5_Age = indager)
head(Wave_5)

Wave_6 <- Wave_6_datafile %>%
  dplyr::select(idauniq, indager, DhSex) %>%
  rename(Sex = DhSex, W6_Age = indager)
head(Wave_6)

Wave_7 <- Wave_7_datafile %>%
  dplyr::select(idauniq, indager, DhSex) %>%
  rename(Sex = DhSex, W7_Age = indager)
head(Wave_7)

Wave_8 <- Wave_8_datafile %>%
  dplyr::select(idauniq, indager, indsex) %>%
  rename(Sex = indsex, W8_Age = indager)
head(Wave_8)

Age_Sex_4to8 <- list(Wave_4, Wave_5, Wave_6, Wave_7, Wave_8) %>%
  reduce(full_join, by = c("idauniq", "Sex")) 
head(Age_Sex_4to8, 20)
Age_Sex_4to8[is.na(Age_Sex_4to8)] <- -99


#### EXTRACT DATA ON SES, PATERNAL OCCUPATION, SELF OCCUPATION, EDUCATION, WEALTH FROM ELSA WAVE 4####

# Extracting data from relevant files
# Used files from Wave 4 and ifs datafiles 


Wave_4_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_4_elsa_data_v3.tab"),
                            header = TRUE, sep = "\t")
head(Wave_4_datafile)
Wave_4_finance_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_4_financial_derived_variables.tab"),
                                    header = TRUE, sep = "\t")

Wave_4_ifs_datafile <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_4_ifs_derived_variables.tab"),
                                header = TRUE, sep = "\t")

Wave_4_ifs_datafile <- Wave_4_ifs_datafile %>%
  dplyr::select(edqual, idauniq) # extracting pre-categorised data on education

Combined_Wave4 <- list(Wave_4_datafile, Wave_4_finance_datafile, Wave_4_ifs_datafile) %>%
  reduce(inner_join, by = c("idauniq"))


Wave_4_SES <- Combined_Wave4 %>%
  dplyr::select(idauniq, difjobd, w4nssec3, nettotw_bu_s, edqual)
head(Wave_4_SES)


# difjobd - PATERNAL OCCUPATION (main carer YOU LIVED WITH IN YOUR CHILDHOOD)

# Converting paternal occupation to three catgories
# Value = 1.0	Label = Armed Forces
# Value = 2.0	Label = Manager or senior official in someone else s business
# Value = 3.0	Label = Running his own business
# Value = 4.0	Label = Professional or technical
# Value = 5.0	Label = Administrative, clerical or secretarial
# Value = 6.0	Label = Skilled trade
# Value = 7.0	Label = Caring, leisure, travel or personal services
# Value = 8.0	Label = Sales or customer service
# Value = 9.0	Label = Plant, process or machine drivers or operators
# Value = 10.0	Label = Other jobs
# Value = 11.0	Label = Something else
# Value = 12.0	Label = Casual jobs
# Value = 13.0	Label = Retired
# Value = 14.0	Label = Unemployed
# Value = 15.0	Label = Sick / disabled
# Value = -2.0	Label = Capi/Interview Error
# Value = -9.0	Label = Refused
# Value = -8.0	Label = Don't know
# Value = -3.0	Label = Incomplete/No information
# Value = -1.0	Label = Not applicable


# high (managerial, professional and administrative occupations or business owners)
# intermediate (trade and services related occupations)
# low (manual and casual occupations and other occupations)
## /unemployed/sick-disabled/retired as missing since this occupation would be hard to categorise as high, intermediate or low

table(Wave_4_SES$difjobd)

for (i in 1:nrow(Wave_4_SES))
{
  Wave_4_SES[i, 2] <- ifelse(Wave_4_SES[i, 2] >= 1 && Wave_4_SES[i, 2] <= 5, 3, # High
                             ifelse(Wave_4_SES[i, 2] >= 6 && Wave_4_SES[i, 2] <= 8, 2, # Intermediate
                                    ifelse(Wave_4_SES[i, 2] >= 9 && Wave_4_SES[i, 2] <= 12, 1, # Low
                                           -1
                                    )
                             )
  )
}

table(Wave_4_SES$difjobd)


# We let low level occupations be the baseline and create dummy variables for paternal-occupation
Wave_4_SES <- Wave_4_SES %>%
  mutate(Paternal_Occ_Intermediate = ifelse(Wave_4_SES[, 2] == -1, -99, ifelse(Wave_4_SES[, 2] == 2, 1, 0))) %>%
  mutate(Paternal_Occ_High = ifelse(Wave_4_SES[, 2] == -1, -99, ifelse(Wave_4_SES[, 2] == 3, 1, 0)))

head(Wave_4_SES, 15)

### Self-occupation

# Self occupation divided into three categories
# w4nssec3
# Value label information for w4nssec3
# 	Value = 3.0	Label = Routine and manual occupations
# 	Value = 1.0	Label = Managerial and professional occupations
# 	Value = 2.0	Label = Intermediate occupations
# 	Value = 99.0	Label = Other
# 	Value = -3.0	Label = Incomplete/No job information
# 	Value = -1.0	Label = Not applicable

#### other (99) = n=12 were coded as missing since it was hard to categorise into either of the three categories
##### other includes people who "Never worked" "Full-time students"
##### "Occupations not stated or inadequately described" "Not classifiable for other reasons"
head(Wave_4_SES)

for (i in 1:nrow(Wave_4_SES))
{
  Wave_4_SES[i, 3] <- ifelse(Wave_4_SES[i, 3] == 1, 3, # High
                             ifelse(Wave_4_SES[i, 3] == 2, 2, # Intermediate
                                    ifelse(Wave_4_SES[i, 3] == 3, 1, # Low
                                           -1
                                    )
                             )
  )
}

table(Wave_4_SES[3])
head(Wave_4_SES)


# We let low level occupations be the baseline and create dummy variables for self-occupation
Wave_4_SES <- Wave_4_SES %>%
  mutate(Self_Occ_Intermediate = ifelse(Wave_4_SES[, 3] == -1, -99, ifelse(Wave_4_SES[, 3] == 2, 1, 0))) %>%
  mutate(Self_Occ_High = ifelse(Wave_4_SES[, 3] == -1, -99, ifelse(Wave_4_SES[, 3] == 3, 1, 0)))

head(Wave_4_SES)

###  3. EDUCATION    ###
# Value = 1.0	Label = nvq4/nvq5/degree or equiv
# Value = 2.0	Label = higher ed below degree
# Value = 3.0	Label = nvq3/gce a level equiv
# Value = 4.0	Label = nvq2/gce o level equiv
# Value = 5.0	Label = nvq1/cse other grade equiv
# Value = 6.0	Label = foreign/other
# Value = 7.0	Label = no qualification
# Value = -1.0	Label = not applicable
# Value = -9.0	Label = refusal
# Value = -8.0	Label = don't know
# Value = -2.0	Label = not asked


# Education was grouped into ‘degree/higher’ 
# (National Vocational Qualification NVQ4/NVQ5/degree or equivalent),
# ‘intermediate’ (higher education below degree, NVQ3/GCE A-level equivalent,
#                 NVQ2/GCE O-level equivalent, NVQ1/CSE other grade equivalent 
#                 or foreign/other),
# ‘no qualifications’. 
for (i in 1:nrow(Wave_4_SES))
{
  Wave_4_SES[i, 5] <- ifelse(Wave_4_SES[i, 5] == 1, 3, # Degree/higher
                             ifelse(Wave_4_SES[i, 5] >=2 && Wave_4_SES[i, 5] <=6, 2, # Intermediate
                                    ifelse(Wave_4_SES[i, 5] == 7, 1, # Low
                                           -1
                                    )
                             )
  )
}



Wave_4_SES <- Wave_4_SES %>%
  mutate(Education_Intermediate = ifelse(Wave_4_SES[, 5] <0, -99, ifelse(Wave_4_SES[, 5] == 2, 1, 0))) %>%
  mutate(Education_Degree = ifelse(Wave_4_SES[, 5] <0, -99, ifelse(Wave_4_SES[, 5] == 3, 1, 0)))

head(Wave_4_SES)

# Wealth

boxplot(Wave_4_SES$nettotw_bu_s)


Wave_4_SES[, 4][Wave_4_SES[, 4] < 0] <- NA

na.omit(Wave_4_SES$nettotw_bu_s)

Wave_4_SES <- Wave_4_SES %>%
  mutate(Wealth_tertiles = ntile(nettotw_bu_s, 3))
head(Wave_4_SES)

Wave_4_SES[, 12][is.na(Wave_4_SES[, 12])] <- -1


Wave_4_SES <- Wave_4_SES %>%
  mutate(Wealth_Second_Tertile = ifelse(Wave_4_SES[, 12] == -1, -99, ifelse(Wave_4_SES[, 12] == 2, 1, 0))) %>%
  mutate(Wealth_Third_Tertile = ifelse(Wave_4_SES[, 12] == -1, -99, ifelse(Wave_4_SES[, 12] == 3, 1, 0))) %>%
  dplyr::select(-c(2, 3, 4, 5, 12))

head(Wave_4_SES)


#### Combining all demographic information ######

Demographics_Wave4 <- list(
  Wave_4_SES, Age_Sex_4to8[,1:3]
) %>% reduce(full_join, by = "idauniq")

Demographics_Wave4 <- dplyr::rename(Demographics_Wave4,Age=W4_Age)
##### Only retaining  information on the ID's that participated from Wave 4 to 9 #####

Merged_4to9_IDs <- read.csv(here("Data","Processed_Data","R_Data","Included_IDs","Merged_4to9_IDs"))

Demographics_Wave4 <- list(
  Demographics_Wave4, Merged_4to9_IDs
) %>% reduce(inner_join, by = "idauniq")
Demographics_Wave4 <- Demographics_Wave4[!duplicated(Demographics_Wave4$idauniq), ]
head(Demographics_Wave4)

write.csv(Demographics_Wave4, file = "Data/Processed_Data/R_Data/Demographics/01_Extracted_Demographics_Wave4.csv", row.names = FALSE)


##### Combining demographic data with four health behaviours #####

Health_Behaviours_4to8 <- read.csv("Data/Processed_Data/R_Data/All_Health_Behaviours/01_All_Health_Behaviours_4to8.csv")
Demographics_Health_Behaviours <- list(
  Demographics_Wave4, Health_Behaviours_4to8
) %>% reduce(inner_join, by = "idauniq")


write.csv(Demographics_Health_Behaviours,"Data/Processed_Data/R_Data/All_Health_Behaviours/Health_Behaviours_4to8_With_Demographics.csv",row.names = FALSE)
