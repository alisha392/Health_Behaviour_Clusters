
############# 3.1. DESCRIPTIVES - DRINKING QUANTITY DATA ##########

Drinking_4to8 <- read.csv(here("Data", "Processed_Data", "R_Data", "Drinking", "02_Categorised_Drinking_4to8_Sex.csv"),
  header = TRUE
)
head(Drinking_4to8)

for (i in 2:6)
{
  Drinking_4to8[, i] <- ifelse(Drinking_4to8[, i] == -99, "Missing",
    ifelse(Drinking_4to8[, i] == 1, "Abstainer",
      ifelse(Drinking_4to8[, i] == 2, "Low risk drinker",
        ifelse(Drinking_4to8[, i] == 3, "Increasing risk drinker",
          ifelse(Drinking_4to8[, i] == 4, "High risk drinker", "")
        )
      )
    )
  )
}
head(Drinking_4to8)


Drinking_Hist <- function(Wave, W, title_Wave) {
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
    labs(x = "Drinking", y = "Frequency", title = paste0("Histogram for ", title_Wave)) +
    scale_x_discrete(limits = c("Missing", "Abstainer", "Low risk drinker", "Increasing risk drinker", "High risk drinker")) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black")
    )
  Hist_Plot
}
Drinking_Hist(Drinking_4to8, W4_Drink, "Wave 4")
Drinking_Hist(Drinking_4to8, W5_Drink, "Wave 5")
Drinking_Hist(Drinking_4to8, W6_Drink, "Wave 6")
Drinking_Hist(Drinking_4to8, W7_Drink, "Wave 7")
Drinking_Hist(Drinking_4to8, W8_Drink, "Wave 8")


########## 3.2. DESCRIPTIVES - DRINKING QUANTITY DATA - By sex #################


Drinking_4to8$Sex <- replace(Drinking_4to8$Sex, Drinking_4to8$Sex == "1", "Male")
Drinking_4to8$Sex <- replace(Drinking_4to8$Sex, Drinking_4to8$Sex == "2", "Female")
head(Drinking_4to8)

# swap columns sex and W4_Age

Drinking_Hist_BySex <- function(Wave, W, S, wave_title) {
  Wave_Data <- Wave %>%
    count({{ W }}, {{ S }}) %>%
    mutate(
      per = n / sum(n),
      per_label = paste0(round(per * 100, digits = 4), sep = "%")
    )
  Wave_Data
  Hist_Plot <- ggplot(Wave_Data, aes(x = {{ W }}, y = n)) +
    scale_x_discrete(limits = c("Missing", "Abstainer", "Low risk drinker", "Increasing risk drinker", "High risk drinker")) +
    geom_col(aes(fill = {{ S }}), position = "dodge") +
    geom_text(
      size = 2,
      aes(label = per_label),
      vjust = -0.2,
    ) +
    labs(x = "Drinking", y = "Frequency", title = paste0("Histogram for ", {{ wave_title }})) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none",
      panel.background = element_blank(), axis.line = element_line(colour = "black")
    ) +
    facet_wrap(vars({{ S }})) +
    scale_fill_brewer(palette = "Set1")
  Hist_Plot
}
Drinking_Hist_BySex(Wave = Drinking_4to8, W = W4_Drink, Sex, "Wave 4")
Drinking_Hist_BySex(Wave = Drinking_4to8, W = W5_Drink, Sex, "Wave 5")
Drinking_Hist_BySex(Wave = Drinking_4to8, W = W6_Drink, Sex, "Wave 6")
Drinking_Hist_BySex(Wave = Drinking_4to8, W = W7_Drink, Sex, "Wave 7")
Drinking_Hist_BySex(Wave = Drinking_4to8, W = W8_Drink, Sex, "Wave 8")


