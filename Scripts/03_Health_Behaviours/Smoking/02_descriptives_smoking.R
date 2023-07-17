############## Load categorised smoking data from Waves 4-9 ###############
Smoking_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Smoking", "01_Categorised_Smoking_4to8.csv"),
  header = TRUE
)
head(Smoking_4to8)


#################### VISUALISING SMOKING DATA ##################
for (i in 2:6)
{
  Smoking_4to8[, i] <- ifelse(Smoking_4to8[, i] == 1, "Smoker",
    ifelse(Smoking_4to8[, i] == 0, "Non-smoker", "Missing")
  )
}
######### Visualizing Smoking data for each wave ###########
Smoking_Hist <- function(Wave, W, title_Wave) {
  Wave_Data <- Wave %>%
    count({{ W }}) %>%
    mutate(
      per = n / sum(n),
      per_label = paste0(round(per * 100, digits = 2), sep = "%")
    )
  Wave_Data
  Hist_Plot <- ggplot(Wave_Data, aes(x = {{ W }}, y = n)) +
    geom_bar(stat = "identity", fill = "yellow", color = "black") +
    geom_text(aes(label = per_label), vjust = -0.25, size = 4) +
    labs(x = "Smoking", y = "Frequency", title = paste0("", title_Wave)) +
    scale_x_discrete(limits = c("Non-smoker", "Smoker", "Missing")) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black")
    )
  Hist_Plot
}
Smoking_Hist(Smoking_4to8, W4_Smoke, "Wave 4")
Smoking_Hist(Smoking_4to8, W5_Smoke, "Wave 5")
Smoking_Hist(Smoking_4to8, W6_Smoke, "Wave 6")
Smoking_Hist(Smoking_4to8, W7_Smoke, "Wave 7")
Smoking_Hist(Smoking_4to8, W8_Smoke, "Wave 8")

########## Visualizing smoking data for each wave - By sex #################
Sex <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_4_elsa_data_v3.tab"),
                header = TRUE, sep = "\t") %>% 
  dplyr::select(idauniq,dhsex) %>% #extracting sex of participants
  rename(Sex = dhsex)

# To remove duplicates when conducting a left join
Smoking_4to8_WithSex <- inner_join(Sex, Smoking_4to8, by = "idauniq")
Smoking_4to8_WithSex <- Smoking_4to8_WithSex[!duplicated(Smoking_4to8_WithSex$idauniq), ]

# Changing values in column Sex, 1-> Male and 2-> Female
Smoking_4to8_WithSex["Sex"] <- replace(Smoking_4to8_WithSex["Sex"], Smoking_4to8_WithSex["Sex"] == 1, "Male")
Smoking_4to8_WithSex["Sex"] <- replace(Smoking_4to8_WithSex["Sex"], Smoking_4to8_WithSex["Sex"] == 2, "Female")
Smoking_4to8_WithSex["Sex"] <- replace(Smoking_4to8_WithSex["Sex"], Smoking_4to8_WithSex["Sex"] == -1, "Missing")

Smoking_Hist_BySeX <- function(Wave, W, S, wave_title) {
  Wave_Data <- Wave %>%
    count({{ W }}, {{ S }}) %>%
    mutate(
      per = n / sum(n),
      per_label = paste0(round(per * 100, digits = 4), sep = "%")
    )
  Wave_Data
  Hist_Plot <- ggplot(Wave_Data, aes(x = {{ W }}, y = n)) +
    scale_x_discrete(labels = c("-99" = "Missing", "1" = "Smoker")) +
    geom_col(aes(fill = {{ S }}), position = "dodge") +
    geom_text(
      size = 2,
      aes(label = per_label),
      vjust = -0.2,
    ) +
    labs(x = "Smoking", y = "Frequency", title = paste0(" ", {{ wave_title }})) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none",
      panel.background = element_blank(), axis.line = element_line(colour = "black")
    ) +
    facet_wrap(vars({{ S }})) +
    scale_fill_brewer(palette = "Set1")
  Hist_Plot
}
Smoking_Hist_BySeX(Wave = Smoking_4to8_WithSex, W = W4_Smoke, Sex, "Wave 4")
Smoking_Hist_BySeX(Wave = Smoking_4to8_WithSex, W = W5_Smoke, Sex, "Wave 5")
Smoking_Hist_BySeX(Wave = Smoking_4to8_WithSex, W = W6_Smoke, Sex, "Wave 6")
Smoking_Hist_BySeX(Wave = Smoking_4to8_WithSex, W = W7_Smoke, Sex, "Wave 7")
Smoking_Hist_BySeX(Wave = Smoking_4to8_WithSex, W = W8_Smoke, Sex, "Wave 8")
