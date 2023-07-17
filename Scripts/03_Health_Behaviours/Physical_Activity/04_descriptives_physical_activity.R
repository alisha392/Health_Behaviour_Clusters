
############# Descriptive PA Data for Waves 4 to 8 ###################

###### Relabeling PA data to sedentary, low, moderate and high #########
PA_4to8 <- read.csv(here("Data","Processed_Data","R_Data","Physical_Activity","04_Categorised_PA_4to8.csv"), header = TRUE)

for (i in 2:6)
{
  PA_4to8[, i] <- ifelse(PA_4to8[, i] == 1, "Sedentary",
    ifelse(PA_4to8[, i] == 2, "Low",
      ifelse(PA_4to8[, i] == 3, "Moderate",
        ifelse(PA_4to8[, i] == 4, "High", "Missing")
      )
    )
  )
}
head(PA_4to8)

####### Visualizing PA data for each wave #####################################


Physical_Hist <- function(Wave, W, title_Wave) {
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
    labs(x = "Physical Activity", y = "Frequency", title = paste0("Histogram for ", title_Wave)) +
    scale_x_discrete(limits = c("Sedentary", "Low", "Moderate", "High", "Missing")) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black")
    )
  Hist_plot
}

Physical_Hist(PA_4to8, W4_PA, "Wave 4")
Physical_Hist(PA_4to8, W5_PA, "Wave 5")
Physical_Hist(PA_4to8, W6_PA, "Wave 6")
Physical_Hist(PA_4to8, W7_PA, "Wave 7")
Physical_Hist(PA_4to8, W8_PA, "Wave 8")

########## Visualizing fruit and veg data for each wave - By sex #################
Sex <- read.csv(here("Data", "Raw_Data", "Original_ELSA_Data", "tab", "wave_4_elsa_data_v3.tab"),
                header = TRUE, sep = "\t") %>% 
  dplyr::select(idauniq,dhsex) %>% #extracting sex of participants
  rename(Sex = dhsex)

# To remove duplicates when conducting a left join
PA_4to8_WithSex <- inner_join(Sex, PA_4to8, by = "idauniq")
PA_4to8_WithSex <- PA_4to8_WithSex[!duplicated(PA_4to8_WithSex$idauniq), ]

PA_4to8_WithSex$Sex <- replace(PA_4to8_WithSex$Sex, PA_4to8_WithSex$Sex == "1", "Male")
PA_4to8_WithSex$Sex <- replace(PA_4to8_WithSex$Sex, PA_4to8_WithSex$Sex == "2", "Female")
PA_4to8_WithSex$Sex <- replace(PA_4to8_WithSex$Sex, PA_4to8_WithSex$Sex == "-1", "Missing")

head(PA_4to8_WithSex)
#### sex-wise grouped bar chart for PA data

PA_Hist_BySeX <- function(Wave, W, S, wave_title) {
  Wave_Data <- Wave %>%
    count({{ W }}, {{ S }}) %>%
    mutate(
      per = n / sum(n),
      per_label = paste0(round(per * 100, digits = 4), sep = "%")
    )
  Wave_Data
  Hist_Plot <- ggplot(Wave_Data, aes(x = {{ W }}, y = n)) +
    scale_x_discrete(limits = c("Sedentary", "Low", "Moderate", "High", "Missing")) +
    geom_col(aes(fill = {{ S }}), position = "dodge") +
    geom_text(
      size = 2,
      aes(label = per_label),
      vjust = -0.2,
    ) +
    labs(x = "Physical Activity", y = "Frequency", title = paste0("Histogram for ", {{ wave_title }})) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none",
      panel.background = element_blank(), axis.line = element_line(colour = "black")
    ) +
    facet_wrap(vars({{ S }})) +
    scale_fill_brewer(palette = "Set1")
  Hist_Plot
}
PA_Hist_BySeX(Wave = PA_4to8_WithSex, W = W4_PA, Sex, "Wave 4")
PA_Hist_BySeX(Wave = PA_4to8_WithSex, W = W5_PA, Sex, "Wave 5")
PA_Hist_BySeX(Wave = PA_4to8_WithSex, W = W6_PA, Sex, "Wave 6")
PA_Hist_BySeX(Wave = PA_4to8_WithSex, W = W7_PA, Sex, "Wave 7")
PA_Hist_BySeX(Wave = PA_4to8_WithSex, W = W8_PA, Sex, "Wave 8")

