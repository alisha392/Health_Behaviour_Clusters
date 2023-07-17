

#########  Categorising drinking data as per quantity of intake over last week #########
# Data to be categorised into the following categories :
# 4 - Drank in last week: > 35 units if female and > 50 units if male (high risk drinker)
# 3 - Drank in last week: >14-35 units if female and >14-50 units if male (increasing risk drinker)
# 2 - Drank in last week: <=14 units for female and male (low risk drinker)
# 1 - Did not drink in last week (abstainer)
#-99 - Missing

# Alcohol by volume assumptions
# 4% for beer, 12.5% for wine and 37.5% for spirits
# single measure of spirit - 25ml
# single wine glass - 175ml
# one pint of beer - 568 ml

Wave_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Drinking", "01_Extracted_Drinking_4to8.csv"))
head(Wave_4to8)  # Contains extracted information on drinking behaviour between Waves 4 to 9

Sex <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_4_elsa_data_v3.tab"),
                            header = TRUE, sep = "\t") %>% 
  dplyr::select(idauniq,dhsex) %>% #extracting sex of participants
  rename(Sex = dhsex)

Wave_4to8_WithSex <- merge(Wave_4to8, Sex)
head(Wave_4to8_WithSex)

# removing duplicates in the merged dataframe
length(unique(Wave_4to8_WithSex$idauniq))
Wave_4to8_WithSex <- Wave_4to8_WithSex[!duplicated(Wave_4to8_WithSex$idauniq), ]

# Wave 4 categorisation as per aforementioned criterion
Wave_4to8 <- Wave_4to8_WithSex %>%
  mutate(
    alcohol_units_4 = 0, W4_Drink = 0, alcohol_units_5 = 0, W5_Drink = 0,
    alcohol_units_6 = 0, W6_Drink = 0, alcohol_units_7 = 0, W7_Drink = 0,
    alcohol_units_8 = 0, W8_Drink = 0, alcohol_units_9 = 0, W9_Drink = 0
  )

for (i in 1:5429)
{
  # if respondent has had a drink in the past week...
  if (Wave_4to8$Drink_Last_Week_4[i] == 1) {
    alcohol <- (((Wave_4to8$Spirits_4[i] * 25 * .375) / 10) + ((Wave_4to8$Wine_4[i] * 175 * .125) / 10) +
      ((Wave_4to8$Beer_4[i] * 568 * .04) / 10))
    Wave_4to8$alcohol_units_4[i] <- alcohol
    # if respondent in male
    if (Wave_4to8$Sex[i] == 1) {
      if (Wave_4to8$alcohol_units_4[i] > 50) {
        # high risk drinker
        Wave_4to8$W4_Drink[i] <- 4
      } # increasing risk drinker
      else if (Wave_4to8$alcohol_units_4[i] <= 50 & Wave_4to8$alcohol_units_4[i] > 14) {
        Wave_4to8$W4_Drink[i] <- 3
      } # missing data on quantity
      else if (Wave_4to8$alcohol_units_4[i] < 0) {
        Wave_4to8$W4_Drink[i] <- -99
      } # low risk drinker
      else {
        Wave_4to8$W4_Drink[i] <- 2
      }
    }
    # if respondent is female
    else if (Wave_4to8$Sex[i] == 2) {
      if (Wave_4to8$alcohol_units_4[i] > 35) {
        # high risk drinker
        Wave_4to8$W4_Drink[i] <- 4
      } # increasing risk drinker
      else if (Wave_4to8$alcohol_units_4[i] <= 35 & Wave_4to8$alcohol_units_4[i] > 14) {
        Wave_4to8$W4_Drink[i] <- 3
      } # missing data on quantity
      else if (Wave_4to8$alcohol_units_4[i] < 0) {
        Wave_4to8$W4_Drink[i] <- -99
      } # low risk drinker
      else {
        Wave_4to8$W4_Drink[i] <- 2
      }
    }
  }
  # if respondent has not had a drink in the last week...

  if (Wave_4to8$Drink_Last_Week_4[i] == 2) {
    Wave_4to8$W4_Drink[i] <- 1
  } else if (Wave_4to8$Drink_Last_Week_4[i] < 0) {
    Wave_4to8$W4_Drink[i] <- -99
  }
}
head(Wave_4to8$W4_Drink)

# WAVE 5
for (i in 1:5429)
{
  # if respondent has had a drink in the past week...
  if (Wave_4to8$Drink_Last_Week_5[i] == 1) {
    alcohol <- (((Wave_4to8$Spirits_5[i] * 25 * .375) / 10) + ((Wave_4to8$Wine_5[i] * 175 * .125) / 10) +
      ((Wave_4to8$Beer_5[i] * 568 * .04) / 10))
    Wave_4to8$alcohol_units_5[i] <- alcohol
    # if respondent in male
    if (Wave_4to8$Sex[i] == 1) {
      if (Wave_4to8$alcohol_units_5[i] > 50) {
        # high risk drinker
        Wave_4to8$W5_Drink[i] <- 4
      } # increasing risk drinker
      else if (Wave_4to8$alcohol_units_5[i] <= 50 & Wave_4to8$alcohol_units_5[i] > 14) {
        Wave_4to8$W5_Drink[i] <- 3
      } # missing data on quantity
      else if (Wave_4to8$alcohol_units_5[i] < 0) {
        Wave_4to8$W5_Drink[i] <- -99
      } # low risk drinker
      else {
        Wave_4to8$W5_Drink[i] <- 2
      }
    }
    # if respondent is female
    else if (Wave_4to8$Sex[i] == 2) {
      if (Wave_4to8$alcohol_units_5[i] > 35) {
        # high risk drinker
        Wave_4to8$W5_Drink[i] <- 4
      } # increasing risk drinker
      else if (Wave_4to8$alcohol_units_5[i] <= 35 & Wave_4to8$alcohol_units_5[i] > 14) {
        Wave_4to8$W5_Drink[i] <- 3
      } # missing data on quantity
      else if (Wave_4to8$alcohol_units_5[i] < 0) {
        Wave_4to8$W5_Drink[i] <- -99
      } # low risk drinker
      else {
        Wave_4to8$W5_Drink[i] <- 2
      }
    }
  }
  # if respondent has not had a drink in the last week...

  if (Wave_4to8$Drink_Last_Week_5[i] == 2) {
    Wave_4to8$W5_Drink[i] <- 1
  } else if (Wave_4to8$Drink_Last_Week_5[i] < 0) {
    Wave_4to8$W5_Drink[i] <- -99
  }
}
head(Wave_4to8$W5_Drink)


# WAVE 6
for (i in 1:5429)
{
  # if respondent has had a drink in the past week...
  if (Wave_4to8$Drink_Last_Week_6[i] == 1) {
    alcohol <- (((Wave_4to8$Spirits_6[i] * 25 * .375) / 10) + ((Wave_4to8$Wine_6[i] * 175 * .125) / 10) +
      ((Wave_4to8$Beer_6[i] * 568 * .04) / 10))
    Wave_4to8$alcohol_units_6[i] <- alcohol
    # if respondent in male
    if (Wave_4to8$Sex[i] == 1) {
      if (Wave_4to8$alcohol_units_6[i] > 50) {
        # high risk drinker
        Wave_4to8$W6_Drink[i] <- 4
      } # increasing risk drinker
      else if (Wave_4to8$alcohol_units_6[i] <= 50 & Wave_4to8$alcohol_units_6[i] > 14) {
        Wave_4to8$W6_Drink[i] <- 3
      } # missing data on quantity
      else if (Wave_4to8$alcohol_units_6[i] < 0) {
        Wave_4to8$W6_Drink[i] <- -99
      } # low risk drinker
      else {
        Wave_4to8$W6_Drink[i] <- 2
      }
    }
    # if respondent is female
    else if (Wave_4to8$Sex[i] == 2) {
      if (Wave_4to8$alcohol_units_6[i] > 35) {
        # high risk drinker
        Wave_4to8$W6_Drink[i] <- 4
      } # increasing risk drinker
      else if (Wave_4to8$alcohol_units_6[i] <= 35 & Wave_4to8$alcohol_units_6[i] > 14) {
        Wave_4to8$W6_Drink[i] <- 3
      } # missing data on quantity
      else if (Wave_4to8$alcohol_units_6[i] < 0) {
        Wave_4to8$W6_Drink[i] <- -99
      } # low risk drinker
      else {
        Wave_4to8$W6_Drink[i] <- 2
      }
    }
  }
  # if respondent has not had a drink in the last week...

  if (Wave_4to8$Drink_Last_Week_6[i] == 2) {
    Wave_4to8$W6_Drink[i] <- 1
  } else if (Wave_4to8$Drink_Last_Week_6[i] < 0) {
    Wave_4to8$W6_Drink[i] <- -99
  }
}
head(Wave_4to8$W6_Drink)


# WAVE 7
for (i in 1:5429)
{
  # if respondent has had a drink in the past week...
  if (Wave_4to8$Drink_Last_Week_7[i] == 1) {
    alcohol <- (((Wave_4to8$Spirits_7[i] * 25 * .375) / 10) + ((Wave_4to8$Wine_7[i] * 175 * .125) / 10) +
      ((Wave_4to8$Beer_7[i] * 568 * .04) / 10))
    Wave_4to8$alcohol_units_7[i] <- alcohol
    # if respondent in male
    if (Wave_4to8$Sex[i] == 1) {
      if (Wave_4to8$alcohol_units_7[i] > 50) {
        # high risk drinker
        Wave_4to8$W7_Drink[i] <- 4
      } # increasing risk drinker
      else if (Wave_4to8$alcohol_units_7[i] <= 50 & Wave_4to8$alcohol_units_7[i] > 14) {
        Wave_4to8$W7_Drink[i] <- 3
      } # missing data on quantity
      else if (Wave_4to8$alcohol_units_7[i] < 0) {
        Wave_4to8$W7_Drink[i] <- -99
      } # low risk drinker
      else {
        Wave_4to8$W7_Drink[i] <- 2
      }
    }
    # if respondent is female
    else if (Wave_4to8$Sex[i] == 2) {
      if (Wave_4to8$alcohol_units_7[i] > 35) {
        # high risk drinker
        Wave_4to8$W7_Drink[i] <- 4
      } # increasing risk drinker
      else if (Wave_4to8$alcohol_units_7[i] <= 35 & Wave_4to8$alcohol_units_7[i] > 14) {
        Wave_4to8$W7_Drink[i] <- 3
      } # missing data on quantity
      else if (Wave_4to8$alcohol_units_7[i] < 0) {
        Wave_4to8$W7_Drink[i] <- -99
      } # low risk drinker
      else {
        Wave_4to8$W7_Drink[i] <- 2
      }
    }
  }
  # if respondent has not had a drink in the last week...

  if (Wave_4to8$Drink_Last_Week_7[i] == 2) {
    Wave_4to8$W7_Drink[i] <- 1
  } else if (Wave_4to8$Drink_Last_Week_7[i] < 0) {
    Wave_4to8$W7_Drink[i] <- -99
  }
}
head(Wave_4to8$W7_Drink)

# WAVE 8
for (i in 1:5429)
{
  # if respondent has had a drink in the past week...
  if (Wave_4to8$Drink_Last_Week_8[i] == 1) {
    alcohol <- (((Wave_4to8$Spirits_8[i] * 25 * .375) / 10) + ((Wave_4to8$Wine_8[i] * 175 * .125) / 10) +
      ((Wave_4to8$Beer_8[i] * 568 * .04) / 10))
    Wave_4to8$alcohol_units_8[i] <- alcohol
    # if respondent in male
    if (Wave_4to8$Sex[i] == 1) {
      if (Wave_4to8$alcohol_units_8[i] > 50) {
        # high risk drinker
        Wave_4to8$W8_Drink[i] <- 4
      } # increasing risk drinker
      else if (Wave_4to8$alcohol_units_8[i] <= 50 & Wave_4to8$alcohol_units_8[i] > 14) {
        Wave_4to8$W8_Drink[i] <- 3
      } # missing data on quantity
      else if (Wave_4to8$alcohol_units_8[i] < 0) {
        Wave_4to8$W8_Drink[i] <- -99
      } # low risk drinker
      else {
        Wave_4to8$W8_Drink[i] <- 2
      }
    }
    # if respondent is female
    else if (Wave_4to8$Sex[i] == 2) {
      if (Wave_4to8$alcohol_units_8[i] > 35) {
        # high risk drinker
        Wave_4to8$W8_Drink[i] <- 4
      } # increasing risk drinker
      else if (Wave_4to8$alcohol_units_8[i] <= 35 & Wave_4to8$alcohol_units_8[i] > 14) {
        Wave_4to8$W8_Drink[i] <- 3
      } # missing data on quantity
      else if (Wave_4to8$alcohol_units_8[i] < 0) {
        Wave_4to8$W8_Drink[i] <- -99
      } # low risk drinker
      else {
        Wave_4to8$W8_Drink[i] <- 2
      }
    }
  }
  # if respondent has not had a drink in the last week...

  if (Wave_4to8$Drink_Last_Week_8[i] == 2) {
    Wave_4to8$W8_Drink[i] <- 1
  } else if (Wave_4to8$Drink_Last_Week_8[i] < 0) {
    Wave_4to8$W8_Drink[i] <- -99
  }
}
head(Wave_4to8$W8_Drink)

head(Wave_4to8)
Categorised_Alcohol_4to8 <- dplyr::select(Wave_4to8, c(1, 24, 26, 28, 30, 32, Sex))
head(Categorised_Alcohol_4to8)

Categorised_Alcohol_4to8_without_Sex <- dplyr::select(Wave_4to8, c(1, 24, 26, 28, 30, 32))
head(Categorised_Alcohol_4to8_without_Sex)


write.csv(Categorised_Alcohol_4to8, file = "Data/Processed_Data/R_Data/Drinking/02_Categorised_Drinking_4to8_Sex.csv", row.names = FALSE)

write.csv(Categorised_Alcohol_4to8_without_Sex, file = "Data/Processed_Data/R_Data/Drinking/02_Categorised_Drinking_4to8.csv", row.names = FALSE)

