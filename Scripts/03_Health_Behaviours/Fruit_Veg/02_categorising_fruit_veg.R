
####### COMBINING FRUIT AND VEG INTAKE DATA FOR EACH WAVE ######################
# summing fruit data and vegtable data for each wave

Uncategorised_FV <- read.csv(here(
  "Data", "Processed_Data", "R_Data", "Fruit_Veg_Intake",
  "01_Recoded_FruitVeg_4to8.csv"
))
head(Uncategorised_FV)


# Summing up fruit and vegetable intake for Wave 4 while retaining values for missing data
FV_Wave4 <- Uncategorised_FV %>%
  dplyr::select(W4_Fruit, W4_Veg)
head(FV_Wave4)

sumpos <- function(x) sum(x[x > 0])
Tot_FV_Wave4 <- FV_Wave4 %>%
  mutate(W4_FV = apply(FV_Wave4, 1, sumpos))

n <- nrow(FV_Wave4)
for (i in 1:n)
{
  if (FV_Wave4$W4_Fruit[i] < 0 & FV_Wave4$W4_Veg[i] < 0) {
    Tot_FV_Wave4$W4_FV[i] <- FV_Wave4$W4_Fruit[i]
  }
}

Tot_FV_Wave4 <- dplyr::select(Tot_FV_Wave4, W4_FV)
head(Tot_FV_Wave4)

# Summing up fruit and vegetable intake for Wave 5 while retaining values for missing data
FV_Wave5 <- Uncategorised_FV %>%
  dplyr::select(W5_Fruit, W5_Veg)
head(FV_Wave5)

sumpos <- function(x) sum(x[x > 0])
Tot_FV_Wave5 <- FV_Wave5 %>%
  mutate(W5_FV = apply(FV_Wave5, 1, sumpos))

n <- nrow(FV_Wave5)
for (i in 1:n)
{
  if (FV_Wave5$W5_Fruit[i] < 0 & FV_Wave5$W5_Veg[i] < 0) {
    Tot_FV_Wave5$W5_FV[i] <- FV_Wave5$W5_Fruit[i]
  }
}

Tot_FV_Wave5 <- dplyr::select(Tot_FV_Wave5, W5_FV)
head(Tot_FV_Wave5)

# Summing up fruit and vegetable intake for Wave 6 while retaining values for missing data
FV_Wave6 <- Uncategorised_FV %>%
  dplyr::select(W6_Fruit, W6_Veg)
head(FV_Wave6)

sumpos <- function(x) sum(x[x > 0])
Tot_FV_Wave6 <- FV_Wave6 %>%
  mutate(W6_FV = apply(FV_Wave6, 1, sumpos))

n <- nrow(FV_Wave6)
for (i in 1:n)
{
  if (FV_Wave6$W6_Fruit[i] < 0 & FV_Wave6$W6_Veg[i] < 0) {
    Tot_FV_Wave6$W6_FV[i] <- FV_Wave6$W6_Fruit[i]
  }
}

Tot_FV_Wave6 <- dplyr::select(Tot_FV_Wave6, W6_FV)
head(Tot_FV_Wave6)

# Summing up fruit and vegetable intake for Wave 7 while retaining values for missing data
FV_Wave7 <- Uncategorised_FV %>%
  dplyr::select(W7_Fruit, W7_Veg)
head(FV_Wave7)

sumpos <- function(x) sum(x[x > 0])
Tot_FV_Wave7 <- FV_Wave7 %>%
  mutate(W7_FV = apply(FV_Wave7, 1, sumpos))

n <- nrow(FV_Wave7)
for (i in 1:n)
{
  if (FV_Wave7$W7_Fruit[i] < 0 & FV_Wave7$W7_Veg[i] < 0) {
    Tot_FV_Wave7$W7_FV[i] <- FV_Wave7$W7_Fruit[i]
  }
}

Tot_FV_Wave7 <- dplyr::select(Tot_FV_Wave7, W7_FV)
head(Tot_FV_Wave7)

# Summing up fruit and vegetable intake for Wave 8 while retaining values for missing data
FV_Wave8 <- Uncategorised_FV %>%
  dplyr::select(W8_Fruit, W8_Veg)
head(FV_Wave8)

sumpos <- function(x) sum(x[x > 0])
Tot_FV_Wave8 <- FV_Wave8 %>%
  mutate(W8_FV = apply(FV_Wave8, 1, sumpos))

n <- nrow(FV_Wave8)
for (i in 1:n)
{
  if (FV_Wave8$W8_Fruit[i] < 0 & FV_Wave8$W8_Veg[i] < 0) {
    Tot_FV_Wave8$W8_FV[i] <- FV_Wave8$W8_Fruit[i]
  }
}

Tot_FV_Wave8 <- dplyr::select(Tot_FV_Wave8, W8_FV)
head(Tot_FV_Wave8)


###### COMBINING  FRUIT AND VEG DATA FOR WAVES 4-8  IN ONE DATAFRAME #######
FV_4to8 <- list(
  Uncategorised_FV$idauniq, Tot_FV_Wave4, Tot_FV_Wave5,
  Tot_FV_Wave6, Tot_FV_Wave7, Tot_FV_Wave8
) %>% reduce(cbind)
FV_4to8 <- FV_4to8 %>%
  rename(idauniq = out)
head(FV_4to8)

############  CATEGORISING  FRUIT AND VEG DATA FOR WAVES 4-9 #################
# The total portion of fruit and vegetable consumption was calculated as a sum
# of fruit and vegetable consumption and was divided into five groups
# (≥0–5, <0 portions per day)
# All missing data coded as -99

FV_4to8 <- FV_4to8 %>%
  mutate(W4 = case_when(
    FV_4to8$W4_FV >= 5 ~ 1,
    FV_4to8$W4_FV >= 0 & FV_4to8$W4_FV < 5 ~ 0,
    FV_4to8$W4_FV < 0 ~ -99
  ))
head(FV_4to8)

FV_4to8 <- FV_4to8 %>%
  mutate(W5 = case_when(
    FV_4to8$W5_FV >= 5 ~ 1,
    FV_4to8$W5_FV >= 0 & FV_4to8$W5_FV < 5 ~ 0,
    FV_4to8$W5_FV < 0 ~ -99
  ))
head(FV_4to8)


FV_4to8 <- FV_4to8 %>%
  mutate(W6 = case_when(
    FV_4to8$W6_FV >= 5 ~ 1,
    FV_4to8$W6_FV >= 0 & FV_4to8$W6_FV < 5 ~ 0,
    FV_4to8$W6_FV < 0 ~ -99
  ))
head(FV_4to8)


FV_4to8 <- FV_4to8 %>%
  mutate(W7 = case_when(
    FV_4to8$W7_FV >= 5 ~ 1,
    FV_4to8$W7_FV >= 0 & FV_4to8$W7_FV < 5 ~ 0,
    FV_4to8$W7_FV < 0 ~ -99
  ))
head(FV_4to8)



FV_4to8 <- FV_4to8 %>%
  mutate(W8 = case_when(
    FV_4to8$W8_FV >= 5 ~ 1,
    FV_4to8$W8_FV >= 0 & FV_4to8$W8_FV < 5 ~ 0,
    FV_4to8$W8_FV < 0 ~ -99
  ))
head(FV_4to8)

Categorised_FV_4to8 <- dplyr::select(FV_4to8, c(1, 7:11))
Categorised_FV_4to8 <- Categorised_FV_4to8 %>%
  rename(W4_FV = W4, W5_FV = W5, W6_FV = W6, W7_FV = W7, W8_FV = W8)
head(Categorised_FV_4to8)

# Note :Weighted here means that weights have been added to the dataframe but does
# not mean the weights have been applied to the data
write.csv(Categorised_FV_4to8, file = "Data/Processed_Data/R_Data/Fruit_Veg_Intake/02_Categorised_FruitVeg_4to8.csv", row.names = FALSE)
