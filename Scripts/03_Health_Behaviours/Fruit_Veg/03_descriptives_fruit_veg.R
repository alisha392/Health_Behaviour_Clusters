
#### Importing recoded and categorised Fruit Veg File ##########################
FV_4to8 <- read.csv(here(
  "Data", "Processed_Data", "R_Data", "Fruit_Veg_Intake",
  "02_Categorised_FruitVeg_4to8.csv"
))
head(FV_4to8)


for (i in 2:6)
{
  FV_4to8[, i] <- ifelse(FV_4to8[, i] == 1, "5 or more/day",
    ifelse(FV_4to8[, i] == 0, "<5/day", "Missing")
  )
}

######### Visualizing Fruit Veg data for each wave ###########

FV_Hist <- function(Wave, W, title_Wave) {
  Wave_Data <- Wave %>%
    count({{ W }}) %>%
    mutate(
      per = n / sum(n),
      per_label = paste0(round(per * 100, digits = 2), sep = "%")
    )
  Wave_Data
  Hist_plot <- ggplot(Wave_Data, aes(x = {{ W }}, y = n)) +
    geom_bar(stat = "identity", fill = "yellow", color = "black") +
    geom_text(aes(label = per_label), vjust = -0.25, size = 4) +
    labs(x = "FV", y = "Frequency", title = paste0(" ", title_Wave)) +
    scale_x_discrete(limits = c("<5/day", "5 or more/day", "Missing")) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black")
    )
  Hist_plot
}
FV_Hist(FV_4to8, W4_FV, "Wave 4")
FV_Hist(FV_4to8, W5_FV, "Wave 5")
FV_Hist(FV_4to8, W6_FV, "Wave 6")
FV_Hist(FV_4to8, W7_FV, "Wave 7")
FV_Hist(FV_4to8, W8_FV, "Wave 8")


########## Visualizing fruit and veg data for each wave - By sex #################
Sex <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_4_elsa_data_v3.tab"),
                header = TRUE, sep = "\t") %>% 
  dplyr::select(idauniq,dhsex) %>% #extracting sex of participants
  rename(Sex = dhsex)

# To remove duplicates when conducting a left join
FV_4to8_WithSex <- left_join(Sex, FV_4to8, by = "idauniq")
FV_4to8_WithSex <- FV_4to8_WithSex[!duplicated(FV_4to8_WithSex$idauniq), ]


# Changing values in column Sex, 1-> Male and 2-> Female
FV_4to8_WithSex["Sex"] <- replace(FV_4to8_WithSex["Sex"], FV_4to8_WithSex["Sex"] == 1, "Male")
FV_4to8_WithSex["Sex"] <- replace(FV_4to8_WithSex["Sex"], FV_4to8_WithSex["Sex"] == 2, "Female")
FV_4to8_WithSex["Sex"] <- replace(FV_4to8_WithSex["Sex"], FV_4to8_WithSex["Sex"] == -1, "Missing")

FV_Hist_BySeX <- function(Wave, W, S, wave_title) {
  Wave_Data <- Wave %>%
    count({{ W }}, {{ S }}) %>%
    mutate(
      per = n / sum(n),
      per_label = paste0(round(per * 100, digits = 4), sep = "%")
    )
  Wave_Data
  Hist_plot <- ggplot(Wave_Data, aes(x = {{ W }}, y = n)) +
    scale_x_discrete(limits = c("<5/day", "5 or more/day", "Missing")) +
    geom_col(aes(fill = {{ S }}), position = "dodge") +
    geom_text(
      size = 2,
      aes(label = per_label),
      vjust = -0.2,
    ) +
    labs(x = "FV", y = "Frequency", title = paste0(" ", {{ wave_title }})) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none",
      panel.background = element_blank(), axis.line = element_line(colour = "black")
    ) +
    facet_wrap(vars({{ S }})) +
    scale_fill_brewer(palette = "Set1")
  Hist_plot
}
FV_Hist_BySeX(Wave = FV_4to8_WithSex, W = W4_FV, Sex, "Wave 4")
FV_Hist_BySeX(Wave = FV_4to8_WithSex, W = W5_FV, Sex, "Wave 5")
FV_Hist_BySeX(Wave = FV_4to8_WithSex, W = W6_FV, Sex, "Wave 6")
FV_Hist_BySeX(Wave = FV_4to8_WithSex, W = W7_FV, Sex, "Wave 7")
FV_Hist_BySeX(Wave = FV_4to8_WithSex, W = W8_FV, Sex, "Wave 8")
