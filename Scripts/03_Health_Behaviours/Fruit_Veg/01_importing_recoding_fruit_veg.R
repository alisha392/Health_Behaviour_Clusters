
############## 1.1. LOAD DATA FILES FROM ELSA WAVES 4-9 ########################

Wave4_datafile <- read.csv(here(
  "Data", "Raw_Data", "Original_ELSA_Data",
  "tab", "wave_4_elsa_data_v3.tab"
),
header = TRUE, sep = "\t"
)

Wave5_datafile <- read.csv(here(
  "Data", "Raw_Data", "Original_ELSA_Data",
  "tab", "wave_5_elsa_data_v4.tab"
),
header = TRUE, sep = "\t"
)

Wave6_datafile <- read.csv(here(
  "Data", "Raw_Data", "Original_ELSA_Data",
  "tab", "wave_6_elsa_data_v2.tab"
),
header = TRUE, sep = "\t"
)

Wave7_datafile <- read.csv(here(
  "Data", "Raw_Data", "Original_ELSA_Data",
  "tab", "wave_7_elsa_data.tab"
),
header = TRUE, sep = "\t"
)

Wave8_datafile <- read.csv(here(
  "Data", "Raw_Data", "Original_ELSA_Data",
  "tab", "wave_8_elsa_data_eul_v2.tab"
),
header = TRUE, sep = "\t"
)


############## 1.2.	EXTRACT FRUIT & VEG INTAKE DATA FROM WAVES 4-9 #############
# idauniq is the crosswave ID for participants

Wave4_Fruit_Veg <- dplyr::select(Wave4_datafile, c(
  idauniq, scfruia, scfruib, scfruic, scfruid,
  scfruie, scfruif, scfruig, scfruih, scfruii,
  scvega, scvegb, scvegc, scvegd
))
head(Wave4_Fruit_Veg)
Wave5_Fruit_Veg <- dplyr::select(Wave5_datafile, c(idauniq, scfru, scveg))
names(Wave5_Fruit_Veg) <- c("idauniq", "W5_Fruit", "W5_Veg")
head(Wave5_Fruit_Veg)
Wave6_Fruit_Veg <- dplyr::select(Wave6_datafile, c(idauniq, scfru, scveg))
names(Wave6_Fruit_Veg) <- c("idauniq", "W6_Fruit", "W6_Veg")
head(Wave6_Fruit_Veg)
Wave7_Fruit_Veg <- dplyr::select(Wave7_datafile, c(idauniq, scfru, scveg))
names(Wave7_Fruit_Veg) <- c("idauniq", "W7_Fruit", "W7_Veg")
head(Wave7_Fruit_Veg)
Wave8_Fruit_Veg <- dplyr::select(Wave8_datafile, c(idauniq, scfru, scveg))
names(Wave8_Fruit_Veg) <- c("idauniq", "W8_Fruit", "W8_Veg")
head(Wave8_Fruit_Veg)

##################### 1.3. RE-CODING WAVE 3 AND 4 TO MATCH WAVES 5-8 ##########
# one small fruit or a handful of very small fruit was coded as being half a portion
# one medium fruit, half a large fruit, and one slice of a very large fruit
# were all coded as being one portion each
# One bowlful of salad was coded as being one portion
# Every three tablespoons of either fruits or vegetables were coded as being one portion.

Wave4_Fruit_Veg$scfruia <- with(Wave4_Fruit_Veg, ifelse(scfruia > 0, scfruia / 2,
  ifelse(scfruia %in% c(-1, -9), scfruia, 0)
))
head(Wave4_Fruit_Veg)
Wave4_Fruit_Veg$scfruib <- with(Wave4_Fruit_Veg, ifelse(scfruib > 0, scfruib / 2,
  ifelse(scfruib %in% c(-1, -9), scfruib, 0)
))
head(Wave4_Fruit_Veg)
Wave4_Fruit_Veg$scfruif <- with(Wave4_Fruit_Veg, ifelse(scfruif > 0, scfruif / 3,
  ifelse(scfruif %in% c(-1, -9), scfruif, 0)
))
head(Wave4_Fruit_Veg)
Wave4_Fruit_Veg$scfruih <- with(Wave4_Fruit_Veg, ifelse(scfruih > 0, scfruih / 3,
  ifelse(scfruih %in% c(-1, -9), scfruih, 0)
))
head(Wave4_Fruit_Veg)
Wave4_Fruit_Veg$scvegb <- with(Wave4_Fruit_Veg, ifelse(scvegb > 0, scvegb / 3,
  ifelse(scvegb %in% c(-1, -9), scvegb, 0)
))
head(Wave4_Fruit_Veg)
Wave4_Fruit_Veg$scvegc <- with(Wave4_Fruit_Veg, ifelse(scvegc > 0, scvegc / 3,
  ifelse(scvegc %in% c(-1, -9), scvegc, 0)
))
head(Wave4_Fruit_Veg)
Wave4_Fruit_Veg$scvegd <- with(Wave4_Fruit_Veg, ifelse(scvegd > 0, scvegd / 3,
  ifelse(scvegd %in% c(-1, -9), scvegd, 0)
))

head(Wave4_Fruit_Veg)
Wave4_Fruit_Veg[, -1] <- round(Wave4_Fruit_Veg[, -1], 2)
head(Wave4_Fruit_Veg)

###### Re-configuring fruit and veg data of Waves 4 to match Waves 5-9#####
Fruit_Wave4 <- Wave4_Fruit_Veg %>%
  dplyr::select(
    scfruia, scfruib, scfruic, scfruid,
    scfruie, scfruif, scfruig, scfruih, scfruii
  )

sumpos <- function(x) sum(x[x > 0])
New_Fruit_Wave4 <- Fruit_Wave4 %>%
  mutate(W4_Fruit = apply(Fruit_Wave4, 1, sumpos))
head(New_Fruit_Wave4)

n_Fruit_Wave4 <- nrow(New_Fruit_Wave4)
for (i in 1:n_Fruit_Wave4)
{
  if (New_Fruit_Wave4$scfruia[i] < 0 & New_Fruit_Wave4$scfruib[i] < 0 &
    New_Fruit_Wave4$scfruic[i] < 0 &
    New_Fruit_Wave4$scfruid[i] < 0 & New_Fruit_Wave4$scfruie[i] < 0 &
    New_Fruit_Wave4$scfruif[i] < 0 & New_Fruit_Wave4$scfruig[i] < 0 &
    New_Fruit_Wave4$scfruih[i] < 0 & New_Fruit_Wave4$scfruii[i] < 0) {
    New_Fruit_Wave4$W4_Fruit[i] <- New_Fruit_Wave4$scfruia[i]
  }
}
New_Fruit_Wave4 <- dplyr::select(New_Fruit_Wave4, W4_Fruit)
head(New_Fruit_Wave4)

Veg_Wave4 <- Wave4_Fruit_Veg %>%
  dplyr::select(scvega, scvegb, scvegc, scvegd)

sumpos <- function(x) sum(x[x > 0])
New_Veg_Wave4 <- Veg_Wave4 %>%
  mutate(W4_Veg = apply(Veg_Wave4, 1, sumpos))
head(New_Veg_Wave4)


n_Veg_Wave4 <- nrow(New_Veg_Wave4)

for (i in 1:n_Veg_Wave4)
{
  if (New_Veg_Wave4$scvega[i] < 0 & New_Veg_Wave4$scvegb[i] < 0 &
    New_Veg_Wave4$scvegc[i] < 0 &
    New_Veg_Wave4$scvegd[i] < 0) {
    New_Veg_Wave4$W4_Veg[i] <- New_Veg_Wave4$scvega[i]
  }
}
New_Veg_Wave4 <- dplyr::select(New_Veg_Wave4, W4_Veg)
head(New_Veg_Wave4)

Recoded_Fruit_Veg_Wave4 <- New_Fruit_Wave4 %>%
  mutate(W4_Veg = New_Veg_Wave4$W4_Veg, idauniq = Wave4_Fruit_Veg$idauniq)
head(Recoded_Fruit_Veg_Wave4)

# Changing NAs in Wave 9 to -1 to standardise with missing data Waves 4-8

################## 1.4. COMBINING FRUIT AND VEG DATA FROM WAVES 4-8#############

#### this means only those cases who have responded across all waves (4-8) will be included
Fruit_Veg_4to8 <- list(
  Recoded_Fruit_Veg_Wave4, Wave5_Fruit_Veg,
  Wave6_Fruit_Veg, Wave7_Fruit_Veg, Wave8_Fruit_Veg
) %>% reduce(inner_join, by = "idauniq")
head(Fruit_Veg_4to8)
Fruit_Veg_4to8[is.na(Fruit_Veg_4to8)] <- -99

#### re-ordering fruit_veg dataframes

col_order <- c(
  "idauniq", "W4_Fruit", "W4_Veg", "W5_Fruit", "W5_Veg",
  "W6_Fruit", "W6_Veg", "W7_Fruit", "W7_Veg", "W8_Fruit", "W8_Veg")

Fruit_Veg_4to8 <- Fruit_Veg_4to8[, col_order]
head(Fruit_Veg_4to8)

##### Only retaining  information on the ID's that participated from Wave 4 to 9 #####

Merged_4to9_IDs <- read.csv(here("Data","Processed_Data","R_Data","Included_IDs","Merged_4to9_IDs"))

Fruit_Veg_4to8 <- list(
  Fruit_Veg_4to8, Merged_4to9_IDs
) %>% reduce(inner_join, by = "idauniq")

write.csv(Fruit_Veg_4to8, file = "Data/Processed_Data/R_Data/Fruit_Veg_Intake/01_Recoded_FruitVeg_4to8.csv", row.names = FALSE)
