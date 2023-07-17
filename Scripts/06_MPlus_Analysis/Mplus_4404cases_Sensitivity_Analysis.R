#Check line 60 before starting
# As Mplus requires that files not have a header row and that the variable names

main_data <- read_csv(here("Data", "Processed_Data", "R_Data", "Health_Outcomes", "Outcomes_Demographics_Behaviours_4to9.csv"))

## Dummy coding Sex with Male as baseline, so Female =1
main_data <- main_data %>% 
  dplyr::mutate(Female = ifelse(main_data$Sex<0, -99, ifelse(main_data$Sex == 2, 1, 0))) %>% 
  dplyr::select(-2) 

main_data <- main_data[, c(1,11:31,54,2:10,43:53,32:42)]
head(main_data)

main_data[main_data == -99] <- NA
Complete_case_data <- main_data[complete.cases(main_data[23:32]), ]
Complete_case_data[is.na(Complete_case_data)]<- -99



main_data_new_names <- Complete_case_data %>%
  rename(
    Pat_Int = Paternal_Occ_Intermediate,
    Pat_Hig = Paternal_Occ_High,
    Self_Int = Self_Occ_Intermediate,
    Self_Hig = Self_Occ_High,
    Edu_Upp = Education_Intermediate,
    Edu_Ter = Education_Degree,
    Wealth_2 = Wealth_Second_Tertile,
    Wealth_3 = Wealth_Third_Tertile,
    Multi = M_Present,
    CMulti = CM_Present,
    Eye = Eye_disorders,
    Circul = Circulatory_disorders,
    Endocr = Endocrine_nutritional_metabolic,
    Muscul = Musculoskeletal_connective_system,
    Neopla = Neoplasms,
    Mental = Mental_behavioural,
    Nervou = Nervous_disorders,
    Respir = Respiratory,
    Luncan = Lung_Cancer,
    Multi_4 = M_Present_W4,
    CMulti_4 = CM_Present_W4,
    Eye_4 = Eye_disorders_W4,
    Circul_4 = Circulatory_disorders_W4,
    Endocr_4 = Endocrine_nutritional_metabolic_W4,
    Muscul_4 = Musculoskeletal_connective_system_W4,
    Neopla_4 = Neoplasms_W4,
    Mental_4 = Mental_behavioural_W4,
    Nervou_4 = Nervous_disorders_W4,
    Respir_4 = Respiratory_W4,
    Luncan_4 = Lung_Cancer_W4
  )

##### Sensitivity analysis #####
#For reference only : the code for creating the 50% subsamples was
#sample_size = floor(0.5*nrow(main_data_new_names))
#picked = sample(seq_len(nrow(main_data_new_names)),size = sample_size)
#Sample_A =main_data_new_names[picked,]
#Sample_B =main_data_new_names[-picked,]
#write.csv(Sample_A,here("Data","Processed_Data","MPlus_data","MPlus_4404cases","Sensitivity_analysis","Sample_A.csv"))
#write.csv(Sample_B,here("Data","Processed_Data","MPlus_data","MPlus_4404cases","Sensitivity_analysis","Sample_B.csv"))

#
Sample_A <- read_csv(here("Data","Processed_Data","MPlus_data","MPlus_4404cases","Sensitivity_analysis","Sample_A.csv"))

Sample_A <- Sample_A %>% 
  dplyr::select(-1)

Sample_B <- read_csv(here("Data","Processed_Data","MPlus_data","MPlus_4404cases","Sensitivity_analysis","Sample_B.csv"))

Sample_B <- Sample_B %>% 
  dplyr::select(-1)


#### SAMPLE A : Class enumeration #####
#setting to relative path as MPlus only accepts 90 characters in each line 
setwd(here("Data", "Processed_Data", "MPlus_data","MPlus_4404cases","Sensitivity_analysis", "Class_enumeration_SampleA"))
getwd()
lca_k1_6 <- lapply(1:9, function(k) {
  lca_enum <- mplusObject(
    TITLE = glue("C{k}_LCA_enumerate"),
    VARIABLE =
      glue(
        "CATEGORICAL = W4_Smoke,W5_Smoke,W6_Smoke,W7_Smoke,W8_Smoke,
W4_Drink,W5_Drink,W6_Drink,W7_Drink,W8_Drink,
W4_PA,W5_PA,W6_PA,W7_PA,W8_PA,W4_FV,W5_FV,W6_FV,
W7_FV,W8_FV;
USEVARIABLES = W4_Smoke,W5_Smoke,W6_Smoke,W7_Smoke,W8_Smoke,
W4_Drink,W5_Drink,W6_Drink,W7_Drink,W8_Drink,
W4_PA,W5_PA,W6_PA,W7_PA,W8_PA,W4_FV,W5_FV,W6_FV,
W7_FV,W8_FV,Weights;
        WEIGHT        = Weights;
MISSING ARE ALL (-99);
        classes = c({k});"
      ),
    ANALYSIS =
      "estimator = mlr;
    type = mixture;
    starts = 500 100;
    STITERATIONS=20;
LRTSTARTS = 50 10 400 20;",
    MODEL = "",
    OUTPUT = "TECH10 TECH11 TECH14",
    PLOT =
      "type = plot3;
    series is W4_Smoke(2) W5_Smoke(3) W6_Smoke(4) W7_Smoke(5)
    W8_Smoke(6) W4_Drink(7) W5_Drink(8)
    W6_Drink(9) W7_Drink(10)
    W8_Drink(11) W4_PA(12) W5_PA(13)
    W6_PA(14) W7_PA(15) W8_PA(16)
    W4_FV(17) W5_FV(18) W6_FV(19)
    W7_FV(20) W8_FV(21);",
    usevariables = colnames(Sample_A),
    rdata = Sample_A
  )
  
  lca_enum_fit <- mplusModeler(lca_enum,
                               dataout = glue("./c_lca_enumerate.dat"),
                               modelout = glue("./c{k}_lca_enumerate.inp"),
                               check = TRUE, run = TRUE, hashfilename = FALSE
  )
  
  
}


)


##### Step 1 for SAMPLE A ####

output_enum <- readModels("./")

enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols = c(
                                    "Title",
                                    "LL", "AIC",
                                    "BIC",
                                    "aBIC", "Entropy", "T11_VLMR_PValue", "T11_BLRT_PValue"
                                  ),
                                  sortBy = "Title"
)


Indices <- enum_summary %>%
  kable(booktabs = T, linesep = "") %>%
  kable_styling(c("striped"),
                full_width = F,
                position = "left"
  )

save_kable(Indices,here("Figures","4404_cases","Fit_Indices_Sample_A.png"))

#### Plotting AIC, BIC, aBIC of models with different number of classes
results1 <- enum_summary[,c(1,3:5)]
classes <- as.character(c(1:9))
results1[,1]<-classes
#convert to long format
results2<-tidyr::gather(results1,"criterias","values", -Title)
results2$criterias<-as.factor(results2$criterias)

Screeplot <- qplot(Title, values, data = results2, color = factor(criterias))+
  geom_point()+
  geom_line(aes(group = criterias)) + 
  xlab("Latent classes") + 
  ylab("Values") +
  scale_color_discrete("Information criteria")+
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),
        legend.title = element_text(size = 14, face="bold"),
        legend.text = element_text(size = 12))+
  ggtitle("Scree Plot of Sample A (2379 cases)") 
ggsave(here("Figures","4404_cases","Scree_Plot_Sample_A.png"),Screeplot,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)

#### Plotting the log likelihood of models with different number of classes

results1 <- enum_summary[,c(1:2)]
classes <- as.character(c(1:9))
results1[,1]<-classes
#convert to long format
results2<-tidyr::gather(results1,"criterias","values", -Title)
results2$criterias<-as.factor(results2$criterias)

Loglikelihood <- qplot(Title, values, data = results2, color = factor(criterias))+
  geom_point()+
  geom_line(aes(group = criterias)) + 
  xlab("Latent classes") + 
  ylab("Values") +
  scale_color_discrete("Loglikelihood") +
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),
        legend.title = element_text(size = 14, face="bold"),
        legend.text = element_text(size = 12))+
  ggtitle("Loglikelihood Plot of Sample A (2379 cases)") 
ggsave(here("Figures","4404_cases","Loglikelihood_Plot_Sample_A.png"),Loglikelihood,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)

###### Visualising a 5 class model : SAMPLE A ######

LCA_output <- readModels("c5_lca_enumerate.out", recursive = TRUE, what = "all")

enum_summary <- LatexSummaryTable(LCA_output, #
                                  keepCols = c(
                                    "Title", "AIC", "BIC", "aBIC", "Entropy", #
                                    "T11_VLMR_PValue"
                                  ), #
                                  sortBy = "Title"
) #

gt(enum_summary) %>% #
  tab_header( #
    title = "Fit Indices"
  ) %>% #
  tab_options( #
    table.width = pct(80)
  ) %>% #
  tab_footnote( #
    footnote = "English Longitudinal Study of Ageing : SNAP behaviours", #
    location = cells_title()
  )

model_results <- LCA_output$parameters$probability.scale #

###

model_results <- model_results %>%
  mutate(Behaviour = c(ifelse(endsWith(param, "SMOKE") == TRUE, "SMOKE",
                              ifelse(endsWith(param, "DRINK") == TRUE, "DRINK",
                                     ifelse(endsWith(param, "FV") == TRUE, "FV",
                                            ifelse(endsWith(param, "PA") == TRUE, "PA",
                                                   NA
                                            )
                                     )
                              )
  )))
head(model_results)


ColourPalleteMulti <- function(df, group, subgroup) {
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep = "~")), df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 100)(nrow(categories))) # Set the top of the colour pallete
  category.end <- (scales::hue_pal(l = 40)(nrow(categories))) # set the bottom
  
  # Build Colour pallette
  colours <- unlist(lapply(
    1:nrow(categories),
    function(i) {
      colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i, 2])
    }
  ))
  return(colours)
}


class_count <- LCA_output$class_counts$modelEstimated %>%
  mutate(percent = percent(proportion, accuracy = .01)) %>%
  dplyr::select(-proportion)

labels <- c(
  "1" = paste0("Class 2 (", class_count[1, 3], ")"),
  "2" = paste0("Class 5 (", class_count[2, 3], ")"),
  "3" = paste0("Class 3 (", class_count[3, 3], ")"),
  "4" = paste0("Class 4 (", class_count[4, 3], ")"),
  "5" = paste0("Class 1 (", class_count[5, 3], ")"),
  "6" = paste0("Class 6 (", class_count[6, 3], ")"),
  "7" = paste0("Class 7 (", class_count[7, 3], ")")
)




colours <- ColourPalleteMulti(model_results, "Behaviour", "category")

model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('5','1','3','4','2','6','7'))

##### changing the category column to produce a clear graph
Recoded_model_results <- model_results %>%
  mutate(new_category = ifelse(Behaviour == "SMOKE" & category == 1, 1,
                               ifelse(Behaviour == "SMOKE" & category == 2, 2,
                                      ifelse(Behaviour == "DRINK" & category == 1, 3,
                                             ifelse(Behaviour == "DRINK" & category == 2, 4,
                                                    ifelse(Behaviour == "DRINK" & category == 3, 5,
                                                           ifelse(Behaviour == "DRINK" & category == 4, 6,
                                                                  ifelse(Behaviour == "PA" & category == 4, 7,
                                                                         ifelse(Behaviour == "PA" & category == 3, 8,
                                                                                ifelse(Behaviour == "PA" & category == 2, 9,
                                                                                       ifelse(Behaviour == "PA" & category == 1, 10,
                                                                                              ifelse(Behaviour == "FV" & category == 2, 11,
                                                                                                     ifelse(Behaviour == "FV" & category == 1, 12, 100)
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
  ))
Recoded_model_results$new_category <- as.factor(Recoded_model_results$new_category)
Recoded_model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('5','1','3','4','2','6','7'))


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

bar1 <- Recoded_model_results %>%
  ggplot(aes(fill = new_category)) +
  geom_bar(aes(x = param, y = est), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model (Sample A)",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_rep_wrap(~LatentClass_factor, labeller = as_labeller(labels), scales = "fixed", repeat.tick.labels = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text.x = element_text(size = 10))
bar1

g1 <- ggplotGrob(bar1)
gtable_show_layout(g1)
g1_am0 <- g1[c(1:10), -c(8:17)]
grid.newpage()
grid.draw(g1_am0)


g2_am0 <- g1[c(1:10), c(6:11)]
grid.newpage()
grid.draw(g2_am0)

g3_am0 <- g1[c(1:10), c(11:16)]
grid.newpage()
grid.draw(g3_am0)

g4_am0 <- g1[c(10:18), -c(8:17)]
grid.newpage()
grid.draw(g4_am0)

g5_am0 <- g1[c(10:18), c(6:11)]
grid.newpage()
grid.draw(g5_am0)

g6_am0 <- g1[c(10:18), c(11:16)]
grid.newpage()
grid.draw(g6_am0)


data_split_1 <- model_results[model_results$Behaviour %in% "SMOKE", ]
head(data_split_1, 100)

data_split_2 <- model_results[model_results$Behaviour %in% "DRINK", ]
head(data_split_2)

data_split_3 <- model_results[model_results$Behaviour %in% "PA", ]
head(data_split_3)

data_split_4 <- model_results[model_results$Behaviour %in% "FV", ]
head(data_split_4)

gg_split_1 <- data_split_1 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("yellow", "yellow3"), name = "Smoking", labels = c("Non-smoker", "Smoker")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_1

gg_legend_1 <- get_legend(gg_split_1)


gg_split_2 <- data_split_2 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("darkseagreen1", "palegreen3", "mediumseagreen", "seagreen"), name = "Alcohol consumption", labels = c("Abstainer", "Moderate", "Hazardous", "Harmful")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_2

gg_legend_2 <- get_legend(gg_split_2)


gg_split_3 <- data_split_3 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("azure1", "paleturquoise2", "turquoise3", "darkcyan"), name = "Physical activity", labels = c("High", "Moderate", "Low", "Sedentary")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_3

gg_legend_3 <- get_legend(gg_split_3)

gg_split_4 <- data_split_4 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("lightsteelblue2","steelblue" ), name = "Fruit & vegetable intake", labels = c(">=5/day", "<5/day")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_4


gg_legend_4 <- get_legend(gg_split_4)


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

head(Recoded_model_results)
gg_no_legend <- Recoded_model_results %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = new_category), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      "4", "5", "6\n\nSmoking", "7", "8",
      "4", "5", "6\n\nAlcohol\nconsumption","7", "8",
      "4", "5", "6\n\nPhysical \nactivity", "7", "8",
      "4", "5", "6\n\nFruit &\nVegetables","7","8"
    )
  ) +
  labs(    title = "Class-membership probabilities for LCA model (Sample A)",
    x = "Health behaviours across waves", 
    y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12,face = "bold",margin = margin(0.3,0,0.3,0, "cm")),
        legend.position = "none",
        axis.title.x = element_text(vjust = -0.75)
  ) 
gg_no_legend

legends <- plot_grid(gg_legend_1, gg_legend_2, gg_legend_3, gg_legend_4, ncol = 1, align = "v", rel_widths = c(1 / 4, 1 / 4, 1 / 4, 1 / 4))
final_plot <- grid.arrange(gg_no_legend, legends, ncol = 2, widths = c(5, 1))
final_plot
ggsave(here("Figures","4404_cases","LCA_5class_SampleA.png"),final_plot,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)




###### Visualising a 6 class model : SAMPLE A ######

LCA_output <- readModels("c6_lca_enumerate.out", recursive = TRUE, what = "all")

enum_summary <- LatexSummaryTable(LCA_output, #
                                  keepCols = c(
                                    "Title", "AIC", "BIC", "aBIC", "Entropy", #
                                    "T11_VLMR_PValue"
                                  ), #
                                  sortBy = "Title"
) #

gt(enum_summary) %>% #
  tab_header( #
    title = "Fit Indices"
  ) %>% #
  tab_options( #
    table.width = pct(80)
  ) %>% #
  tab_footnote( #
    footnote = "English Longitudinal Study of Ageing : SNAP behaviours", #
    location = cells_title()
  )

model_results <- LCA_output$parameters$probability.scale #

###

model_results <- model_results %>%
  mutate(Behaviour = c(ifelse(endsWith(param, "SMOKE") == TRUE, "SMOKE",
                              ifelse(endsWith(param, "DRINK") == TRUE, "DRINK",
                                     ifelse(endsWith(param, "FV") == TRUE, "FV",
                                            ifelse(endsWith(param, "PA") == TRUE, "PA",
                                                   NA
                                            )
                                     )
                              )
  )))
head(model_results)


ColourPalleteMulti <- function(df, group, subgroup) {
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep = "~")), df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 100)(nrow(categories))) # Set the top of the colour pallete
  category.end <- (scales::hue_pal(l = 40)(nrow(categories))) # set the bottom
  
  # Build Colour pallette
  colours <- unlist(lapply(
    1:nrow(categories),
    function(i) {
      colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i, 2])
    }
  ))
  return(colours)
}


class_count <- LCA_output$class_counts$modelEstimated %>%
  mutate(percent = percent(proportion, accuracy = .01)) %>%
  dplyr::select(-proportion)

labels <- c(
  "1" = paste0("Class 4 (", class_count[1, 3], ")"),
  "2" = paste0("Class 3 (", class_count[2, 3], ")"),
  "3" = paste0("Class 1 (", class_count[3, 3], ")"),
  "4" = paste0("Class 5 (", class_count[4, 3], ")"),
  "5" = paste0("Class 6 (", class_count[5, 3], ")"),
  "6" = paste0("Class 2 (", class_count[6, 3], ")"),
  "7" = paste0("Class 7 (", class_count[7, 3], ")")
)




colours <- ColourPalleteMulti(model_results, "Behaviour", "category")

model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('3','6','2','1','4','5','7'))

##### changing the category column to produce a clear graph
Recoded_model_results <- model_results %>%
  mutate(new_category = ifelse(Behaviour == "SMOKE" & category == 1, 1,
                               ifelse(Behaviour == "SMOKE" & category == 2, 2,
                                      ifelse(Behaviour == "DRINK" & category == 1, 3,
                                             ifelse(Behaviour == "DRINK" & category == 2, 4,
                                                    ifelse(Behaviour == "DRINK" & category == 3, 5,
                                                           ifelse(Behaviour == "DRINK" & category == 4, 6,
                                                                  ifelse(Behaviour == "PA" & category == 4, 7,
                                                                         ifelse(Behaviour == "PA" & category == 3, 8,
                                                                                ifelse(Behaviour == "PA" & category == 2, 9,
                                                                                       ifelse(Behaviour == "PA" & category == 1, 10,
                                                                                              ifelse(Behaviour == "FV" & category == 2, 11,
                                                                                                     ifelse(Behaviour == "FV" & category == 1, 12, 100)
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
  ))
Recoded_model_results$new_category <- as.factor(Recoded_model_results$new_category)
Recoded_model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('3','6','2','1','4','5','7'))


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

bar1 <- Recoded_model_results %>%
  ggplot(aes(fill = new_category)) +
  geom_bar(aes(x = param, y = est), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model (Sample A)",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_rep_wrap(~LatentClass_factor, labeller = as_labeller(labels), scales = "fixed", repeat.tick.labels = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text.x = element_text(size = 10))
bar1

g1 <- ggplotGrob(bar1)
gtable_show_layout(g1)
g1_am0 <- g1[c(1:10), -c(8:17)]
grid.newpage()
grid.draw(g1_am0)


g2_am0 <- g1[c(1:10), c(6:11)]
grid.newpage()
grid.draw(g2_am0)

g3_am0 <- g1[c(1:10), c(11:16)]
grid.newpage()
grid.draw(g3_am0)

g4_am0 <- g1[c(10:18), -c(8:17)]
grid.newpage()
grid.draw(g4_am0)

g5_am0 <- g1[c(10:18), c(6:11)]
grid.newpage()
grid.draw(g5_am0)

g6_am0 <- g1[c(10:18), c(11:16)]
grid.newpage()
grid.draw(g6_am0)


data_split_1 <- model_results[model_results$Behaviour %in% "SMOKE", ]
head(data_split_1, 100)

data_split_2 <- model_results[model_results$Behaviour %in% "DRINK", ]
head(data_split_2)

data_split_3 <- model_results[model_results$Behaviour %in% "PA", ]
head(data_split_3)

data_split_4 <- model_results[model_results$Behaviour %in% "FV", ]
head(data_split_4)

gg_split_1 <- data_split_1 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("yellow", "yellow3"), name = "Smoking", labels = c("Non-smoker", "Smoker")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_1

gg_legend_1 <- get_legend(gg_split_1)


gg_split_2 <- data_split_2 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("darkseagreen1", "palegreen3", "mediumseagreen", "seagreen"), name = "Alcohol consumption", labels = c("Abstainer", "Moderate", "Hazardous", "Harmful")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_2

gg_legend_2 <- get_legend(gg_split_2)


gg_split_3 <- data_split_3 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("azure1", "paleturquoise2", "turquoise3", "darkcyan"), name = "Physical activity", labels = c("High", "Moderate", "Low", "Sedentary")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_3

gg_legend_3 <- get_legend(gg_split_3)

gg_split_4 <- data_split_4 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("lightsteelblue2","steelblue" ), name = "Fruit & vegetable intake", labels = c(">=5/day", "<5/day")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_4


gg_legend_4 <- get_legend(gg_split_4)


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

head(Recoded_model_results)
gg_no_legend <- Recoded_model_results %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = new_category), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      "4", "5", "6\n\nSmoking", "7", "8",
      "4", "5", "6\n\nAlcohol\nconsumption","7", "8",
      "4", "5", "6\n\nPhysical \nactivity", "7", "8",
      "4", "5", "6\n\nFruit &\nVegetables","7","8"
    )
  ) +
  labs(    title = "Class-membership probabilities for LCA model (Sample A)",
    x = "Health behaviours across waves", 
    y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12,face = "bold",margin = margin(0.3,0,0.3,0, "cm")),
        legend.position = "none",
        axis.title.x = element_text(vjust = -0.75)
  ) 
gg_no_legend

legends <- plot_grid(gg_legend_1, gg_legend_2, gg_legend_3, gg_legend_4, ncol = 1, align = "v", rel_widths = c(1 / 4, 1 / 4, 1 / 4, 1 / 4))
final_plot <- grid.arrange(gg_no_legend, legends, ncol = 2, widths = c(5, 1))
final_plot

ggsave(here("Figures","4404_cases","LCA_6class_SampleA.pdf"),final_plot,width=30, height=22, dpi=300,units=c("cm"),limitsize = FALSE)




###### Visualising a 7 class model : SAMPLE A ######

LCA_output <- readModels("c7_lca_enumerate.out", recursive = TRUE, what = "all")

enum_summary <- LatexSummaryTable(LCA_output, #
                                  keepCols = c(
                                    "Title", "AIC", "BIC", "aBIC", "Entropy", #
                                    "T11_VLMR_PValue"
                                  ), #
                                  sortBy = "Title"
) #

gt(enum_summary) %>% #
  tab_header( #
    title = "Fit Indices"
  ) %>% #
  tab_options( #
    table.width = pct(80)
  ) %>% #
  tab_footnote( #
    footnote = "English Longitudinal Study of Ageing : SNAP behaviours", #
    location = cells_title()
  )

model_results <- LCA_output$parameters$probability.scale #

###

model_results <- model_results %>%
  mutate(Behaviour = c(ifelse(endsWith(param, "SMOKE") == TRUE, "SMOKE",
                              ifelse(endsWith(param, "DRINK") == TRUE, "DRINK",
                                     ifelse(endsWith(param, "FV") == TRUE, "FV",
                                            ifelse(endsWith(param, "PA") == TRUE, "PA",
                                                   NA
                                            )
                                     )
                              )
  )))
head(model_results)


ColourPalleteMulti <- function(df, group, subgroup) {
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep = "~")), df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 100)(nrow(categories))) # Set the top of the colour pallete
  category.end <- (scales::hue_pal(l = 40)(nrow(categories))) # set the bottom
  
  # Build Colour pallette
  colours <- unlist(lapply(
    1:nrow(categories),
    function(i) {
      colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i, 2])
    }
  ))
  return(colours)
}


class_count <- LCA_output$class_counts$modelEstimated %>%
  mutate(percent = percent(proportion, accuracy = .01)) %>%
  dplyr::select(-proportion)

labels <- c(
  "1" = paste0("Class 3 (", class_count[1, 3], ")"),
  "2" = paste0("Class 6 (", class_count[2, 3], ")"),
  "3" = paste0("Class 7 (", class_count[3, 3], ")"),
  "4" = paste0("Class 1 (", class_count[4, 3], ")"),
  "5" = paste0("Class 2 (", class_count[5, 3], ")"),
  "6" = paste0("Class 5 (", class_count[6, 3], ")"),
  "7" = paste0("Class 4 (", class_count[7, 3], ")")
)



colours <- ColourPalleteMulti(model_results, "Behaviour", "category")

model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('4','5','1','7','6','2','3'))

##### changing the category column to produce a clear graph
Recoded_model_results <- model_results %>%
  mutate(new_category = ifelse(Behaviour == "SMOKE" & category == 1, 1,
                               ifelse(Behaviour == "SMOKE" & category == 2, 2,
                                      ifelse(Behaviour == "DRINK" & category == 1, 3,
                                             ifelse(Behaviour == "DRINK" & category == 2, 4,
                                                    ifelse(Behaviour == "DRINK" & category == 3, 5,
                                                           ifelse(Behaviour == "DRINK" & category == 4, 6,
                                                                  ifelse(Behaviour == "PA" & category == 4, 7,
                                                                         ifelse(Behaviour == "PA" & category == 3, 8,
                                                                                ifelse(Behaviour == "PA" & category == 2, 9,
                                                                                       ifelse(Behaviour == "PA" & category == 1, 10,
                                                                                              ifelse(Behaviour == "FV" & category == 2, 11,
                                                                                                     ifelse(Behaviour == "FV" & category == 1, 12, 100)
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
  ))
Recoded_model_results$new_category <- as.factor(Recoded_model_results$new_category)
Recoded_model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('4','5','1','7','6','2','3'))


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

bar1 <- Recoded_model_results %>%
  ggplot(aes(fill = new_category)) +
  geom_bar(aes(x = param, y = est), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model (Sample A)",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_rep_wrap(~LatentClass_factor, labeller = as_labeller(labels), scales = "fixed", repeat.tick.labels = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text.x = element_text(size = 10))
bar1

g1 <- ggplotGrob(bar1)
gtable_show_layout(g1)
g1_am0 <- g1[c(1:10), -c(8:17)]
grid.newpage()
grid.draw(g1_am0)


g2_am0 <- g1[c(1:10), c(6:11)]
grid.newpage()
grid.draw(g2_am0)

g3_am0 <- g1[c(1:10), c(11:16)]
grid.newpage()
grid.draw(g3_am0)

g4_am0 <- g1[c(10:18), -c(8:17)]
grid.newpage()
grid.draw(g4_am0)

g5_am0 <- g1[c(10:18), c(6:11)]
grid.newpage()
grid.draw(g5_am0)

g6_am0 <- g1[c(10:18), c(11:16)]
grid.newpage()
grid.draw(g6_am0)


data_split_1 <- model_results[model_results$Behaviour %in% "SMOKE", ]
head(data_split_1, 100)

data_split_2 <- model_results[model_results$Behaviour %in% "DRINK", ]
head(data_split_2)

data_split_3 <- model_results[model_results$Behaviour %in% "PA", ]
head(data_split_3)

data_split_4 <- model_results[model_results$Behaviour %in% "FV", ]
head(data_split_4)

gg_split_1 <- data_split_1 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("yellow", "yellow3"), name = "Smoking", labels = c("Non-smoker", "Smoker")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model (Sample A)",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_1

gg_legend_1 <- get_legend(gg_split_1)


gg_split_2 <- data_split_2 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("darkseagreen1", "palegreen3", "mediumseagreen", "seagreen"), name = "Alcohol consumption", labels = c("Abstainer", "Moderate", "Hazardous", "Harmful")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_2

gg_legend_2 <- get_legend(gg_split_2)


gg_split_3 <- data_split_3 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("azure1", "paleturquoise2", "turquoise3", "darkcyan"), name = "Physical activity", labels = c("High", "Moderate", "Low", "Sedentary")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_3

gg_legend_3 <- get_legend(gg_split_3)

gg_split_4 <- data_split_4 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("lightsteelblue2","steelblue" ), name = "Fruit & vegetable intake", labels = c(">=5/day", "<5/day")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_4


gg_legend_4 <- get_legend(gg_split_4)


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

head(Recoded_model_results)
gg_no_legend <- Recoded_model_results %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = new_category), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      "4", "5", "6\n\nSmoking", "7", "8",
      "4", "5", "6\n\nAlcohol\nconsumption","7", "8",
      "4", "5", "6\n\nPhysical \nactivity", "7", "8",
      "4", "5", "6\n\nFruit &\nVegetables","7","8"
    )
  ) +
  labs(   title = "Class-membership probabilities for LCA model (Sample A)",

    x = "Health behaviours across waves", 
    y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12,face = "bold",margin = margin(0.3,0,0.3,0, "cm")),
        legend.position = "none",
        axis.title.x = element_text(vjust = -0.75)
  ) 
gg_no_legend

legends <- plot_grid(gg_legend_1, gg_legend_2, gg_legend_3, gg_legend_4, ncol = 1, align = "v", rel_widths = c(1 / 4, 1 / 4, 1 / 4, 1 / 4))
final_plot <- grid.arrange(gg_no_legend, legends, ncol = 2, widths = c(5, 1))
final_plot

ggsave(here("Figures","4404_cases","LCA_7class_SampleA.png"),final_plot,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)



#### SAMPLE B : Class enumeration #####
#setting to relative path as MPlus only accepts 90 characters in each line 
setwd(here("Data", "Processed_Data", "MPlus_data","MPlus_4404cases","Sensitivity_analysis", "Class_enumeration_SampleB"))
getwd()
lca_k1_6 <- lapply(1:9, function(k) {
  lca_enum <- mplusObject(
    TITLE = glue("C{k}_LCA_enumerate"),
    VARIABLE =
      glue(
        "CATEGORICAL = W4_Smoke,W5_Smoke,W6_Smoke,W7_Smoke,W8_Smoke,
W4_Drink,W5_Drink,W6_Drink,W7_Drink,W8_Drink,
W4_PA,W5_PA,W6_PA,W7_PA,W8_PA,W4_FV,W5_FV,W6_FV,
W7_FV,W8_FV;
USEVARIABLES = W4_Smoke,W5_Smoke,W6_Smoke,W7_Smoke,W8_Smoke,
W4_Drink,W5_Drink,W6_Drink,W7_Drink,W8_Drink,
W4_PA,W5_PA,W6_PA,W7_PA,W8_PA,W4_FV,W5_FV,W6_FV,
W7_FV,W8_FV,Weights;
        WEIGHT        = Weights;
MISSING ARE ALL (-99);
        classes = c({k});"
      ),
    ANALYSIS =
      "estimator = mlr;
    type = mixture;
    starts = 500 100;
    STITERATIONS=20;
LRTSTARTS = 50 10 400 20;",
    MODEL = "",
    OUTPUT = "TECH10 TECH11 TECH14",
    PLOT =
      "type = plot3;
    series is W4_Smoke(2) W5_Smoke(3) W6_Smoke(4) W7_Smoke(5)
    W8_Smoke(6) W4_Drink(7) W5_Drink(8)
    W6_Drink(9) W7_Drink(10)
    W8_Drink(11) W4_PA(12) W5_PA(13)
    W6_PA(14) W7_PA(15) W8_PA(16)
    W4_FV(17) W5_FV(18) W6_FV(19)
    W7_FV(20) W8_FV(21);",
    usevariables = colnames(Sample_B),
    rdata = Sample_B
  )
  
  lca_enum_fit <- mplusModeler(lca_enum,
                               dataout = glue("./c_lca_enumerate.dat"),
                               modelout = glue("./c{k}_lca_enumerate.inp"),
                               check = TRUE, run = TRUE, hashfilename = FALSE
  )
  
  
}


)


##### Step 1 for SAMPLE B ####

output_enum <- readModels("./")

enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols = c(
                                    "Title",
                                    "LL", "AIC",
                                    "BIC",
                                    "aBIC", "Entropy", "T11_VLMR_PValue", "T11_BLRT_PValue"
                                  ),
                                  sortBy = "Title"
)


Indices <- enum_summary %>%
  kable(booktabs = T, linesep = "") %>%
  kable_styling(c("striped"),
                full_width = F,
                position = "left"
  )

save_kable(Indices,here("Figures","4404_cases","Fit_Indices_Sample_B.png"))

#### Plotting AIC, BIC, aBIC of models with different number of classes
results1 <- enum_summary[,c(1,3:5)]
classes <- as.character(c(1:9))
results1[,1]<-classes
#convert to long format
results2<-tidyr::gather(results1,"criterias","values", -Title)
results2$criterias<-as.factor(results2$criterias)

Screeplot <- qplot(Title, values, data = results2, color = factor(criterias))+
  geom_point()+
  geom_line(aes(group = criterias)) + 
  xlab("Latent classes") + 
  ylab("Values") +
  scale_color_discrete("Information criteria")+
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),
        legend.title = element_text(size = 14, face="bold"),
        legend.text = element_text(size = 12))+
  ggtitle("Scree Plot of Sample B (2380 cases)") 
ggsave(here("Figures","4404_cases","Scree_Plot_Sample_B.png"),Screeplot,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)

#### Plotting the log likelihood of models with different number of classes

results1 <- enum_summary[,c(1:2)]
classes <- as.character(c(1:9))
results1[,1]<-classes
#convert to long format
results2<-tidyr::gather(results1,"criterias","values", -Title)
results2$criterias<-as.factor(results2$criterias)

Loglikelihood <- qplot(Title, values, data = results2, color = factor(criterias))+
  geom_point()+
  geom_line(aes(group = criterias)) + 
  xlab("Latent classes") + 
  ylab("Values") +
  scale_color_discrete("Loglikelihood") +
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),
        legend.title = element_text(size = 14, face="bold"),
        legend.text = element_text(size = 12))+
  ggtitle("Loglikelihood Plot of Sample B (2380 cases)") 
ggsave(here("Figures","4404_cases","Loglikelihood_Plot_Sample_B.png"),Loglikelihood,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)



###### Visualising a 5 class model : SAMPLE B ######

LCA_output <- readModels("c5_lca_enumerate.out", recursive = TRUE, what = "all")

enum_summary <- LatexSummaryTable(LCA_output, #
                                  keepCols = c(
                                    "Title", "AIC", "BIC", "aBIC", "Entropy", #
                                    "T11_VLMR_PValue"
                                  ), #
                                  sortBy = "Title"
) #

gt(enum_summary) %>% #
  tab_header( #
    title = "Fit Indices"
  ) %>% #
  tab_options( #
    table.width = pct(80)
  ) %>% #
  tab_footnote( #
    footnote = "English Longitudinal Study of Ageing : SNAP behaviours", #
    location = cells_title()
  )

model_results <- LCA_output$parameters$probability.scale #

###

model_results <- model_results %>%
  mutate(Behaviour = c(ifelse(endsWith(param, "SMOKE") == TRUE, "SMOKE",
                              ifelse(endsWith(param, "DRINK") == TRUE, "DRINK",
                                     ifelse(endsWith(param, "FV") == TRUE, "FV",
                                            ifelse(endsWith(param, "PA") == TRUE, "PA",
                                                   NA
                                            )
                                     )
                              )
  )))
head(model_results)


ColourPalleteMulti <- function(df, group, subgroup) {
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep = "~")), df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 100)(nrow(categories))) # Set the top of the colour pallete
  category.end <- (scales::hue_pal(l = 40)(nrow(categories))) # set the bottom
  
  # Build Colour pallette
  colours <- unlist(lapply(
    1:nrow(categories),
    function(i) {
      colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i, 2])
    }
  ))
  return(colours)
}


class_count <- LCA_output$class_counts$modelEstimated %>%
  mutate(percent = percent(proportion, accuracy = .01)) %>%
  dplyr::select(-proportion)

labels <- c(
  "1" = paste0("Class 1 (", class_count[1, 3], ")"),
  "2" = paste0("Class 2 (", class_count[2, 3], ")"),
  "3" = paste0("Class 4 (", class_count[3, 3], ")"),
  "4" = paste0("Class 3 (", class_count[4, 3], ")"),
  "5" = paste0("Class 5 (", class_count[5, 3], ")"),
  "6" = paste0("Class 6 (", class_count[6, 3], ")"),
  "7" = paste0("Class 7 (", class_count[7, 3], ")")
)



colours <- ColourPalleteMulti(model_results, "Behaviour", "category")

model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('1','2','4','3','5','6','7'))

##### changing the category column to produce a clear graph
Recoded_model_results <- model_results %>%
  mutate(new_category = ifelse(Behaviour == "SMOKE" & category == 1, 1,
                               ifelse(Behaviour == "SMOKE" & category == 2, 2,
                                      ifelse(Behaviour == "DRINK" & category == 1, 3,
                                             ifelse(Behaviour == "DRINK" & category == 2, 4,
                                                    ifelse(Behaviour == "DRINK" & category == 3, 5,
                                                           ifelse(Behaviour == "DRINK" & category == 4, 6,
                                                                  ifelse(Behaviour == "PA" & category == 4, 7,
                                                                         ifelse(Behaviour == "PA" & category == 3, 8,
                                                                                ifelse(Behaviour == "PA" & category == 2, 9,
                                                                                       ifelse(Behaviour == "PA" & category == 1, 10,
                                                                                              ifelse(Behaviour == "FV" & category == 2, 11,
                                                                                                     ifelse(Behaviour == "FV" & category == 1, 12, 100)
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
  ))
Recoded_model_results$new_category <- as.factor(Recoded_model_results$new_category)
Recoded_model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('1','2','4','3','5','6','7'))


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

bar1 <- Recoded_model_results %>%
  ggplot(aes(fill = new_category)) +
  geom_bar(aes(x = param, y = est), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_rep_wrap(~LatentClass_factor, labeller = as_labeller(labels), scales = "fixed", repeat.tick.labels = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text.x = element_text(size = 10))
bar1

g1 <- ggplotGrob(bar1)
gtable_show_layout(g1)
g1_am0 <- g1[c(1:10), -c(8:17)]
grid.newpage()
grid.draw(g1_am0)


g2_am0 <- g1[c(1:10), c(6:11)]
grid.newpage()
grid.draw(g2_am0)

g3_am0 <- g1[c(1:10), c(11:16)]
grid.newpage()
grid.draw(g3_am0)

g4_am0 <- g1[c(10:18), -c(8:17)]
grid.newpage()
grid.draw(g4_am0)

g5_am0 <- g1[c(10:18), c(6:11)]
grid.newpage()
grid.draw(g5_am0)

g6_am0 <- g1[c(10:18), c(11:16)]
grid.newpage()
grid.draw(g6_am0)


data_split_1 <- model_results[model_results$Behaviour %in% "SMOKE", ]
head(data_split_1, 100)

data_split_2 <- model_results[model_results$Behaviour %in% "DRINK", ]
head(data_split_2)

data_split_3 <- model_results[model_results$Behaviour %in% "PA", ]
head(data_split_3)

data_split_4 <- model_results[model_results$Behaviour %in% "FV", ]
head(data_split_4)

gg_split_1 <- data_split_1 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("yellow", "yellow3"), name = "Smoking", labels = c("Non-smoker", "Smoker")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_1

gg_legend_1 <- get_legend(gg_split_1)


gg_split_2 <- data_split_2 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("darkseagreen1", "palegreen3", "mediumseagreen", "seagreen"), name = "Alcohol consumption", labels = c("Abstainer", "Moderate", "Hazardous", "Harmful")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_2

gg_legend_2 <- get_legend(gg_split_2)


gg_split_3 <- data_split_3 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("azure1", "paleturquoise2", "turquoise3", "darkcyan"), name = "Physical activity", labels = c("High", "Moderate", "Low", "Sedentary")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_3

gg_legend_3 <- get_legend(gg_split_3)

gg_split_4 <- data_split_4 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("lightsteelblue2","steelblue" ), name = "Fruit & vegetable intake", labels = c(">=5/day", "<5/day")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_4


gg_legend_4 <- get_legend(gg_split_4)


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

head(Recoded_model_results)
gg_no_legend <- Recoded_model_results %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = new_category), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      "4", "5", "6\n\nSmoking", "7", "8",
      "4", "5", "6\n\nAlcohol\nconsumption","7", "8",
      "4", "5", "6\n\nPhysical \nactivity", "7", "8",
      "4", "5", "6\n\nFruit &\nVegetables","7","8"
    )
  ) +
  labs(    title = "Class-membership probabilities for LCA model (Sample B)",
    x = "Health behaviours across waves", 
    y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12,face = "bold",margin = margin(0.3,0,0.3,0, "cm")),
        legend.position = "none",
        axis.title.x = element_text(vjust = -0.75)
  ) 
gg_no_legend

legends <- plot_grid(gg_legend_1, gg_legend_2, gg_legend_3, gg_legend_4, ncol = 1, align = "v", rel_widths = c(1 / 4, 1 / 4, 1 / 4, 1 / 4))
final_plot <- grid.arrange(gg_no_legend, legends, ncol = 2, widths = c(5, 1))
final_plot
ggsave(here("Figures","4404_cases","LCA_5class_SampleB.png"),final_plot,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)



###### Visualising a 6 class model : SAMPLE B ######

LCA_output <- readModels("c6_lca_enumerate.out", recursive = TRUE, what = "all")

enum_summary <- LatexSummaryTable(LCA_output, #
                                  keepCols = c(
                                    "Title", "AIC", "BIC", "aBIC", "Entropy", #
                                    "T11_VLMR_PValue"
                                  ), #
                                  sortBy = "Title"
) #

gt(enum_summary) %>% #
  tab_header( #
    title = "Fit Indices"
  ) %>% #
  tab_options( #
    table.width = pct(80)
  ) %>% #
  tab_footnote( #
    footnote = "English Longitudinal Study of Ageing : SNAP behaviours", #
    location = cells_title()
  )

model_results <- LCA_output$parameters$probability.scale #

###

model_results <- model_results %>%
  mutate(Behaviour = c(ifelse(endsWith(param, "SMOKE") == TRUE, "SMOKE",
                              ifelse(endsWith(param, "DRINK") == TRUE, "DRINK",
                                     ifelse(endsWith(param, "FV") == TRUE, "FV",
                                            ifelse(endsWith(param, "PA") == TRUE, "PA",
                                                   NA
                                            )
                                     )
                              )
  )))
head(model_results)


ColourPalleteMulti <- function(df, group, subgroup) {
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep = "~")), df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 100)(nrow(categories))) # Set the top of the colour pallete
  category.end <- (scales::hue_pal(l = 40)(nrow(categories))) # set the bottom
  
  # Build Colour pallette
  colours <- unlist(lapply(
    1:nrow(categories),
    function(i) {
      colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i, 2])
    }
  ))
  return(colours)
}


class_count <- LCA_output$class_counts$modelEstimated %>%
  mutate(percent = percent(proportion, accuracy = .01)) %>%
  dplyr::select(-proportion)

labels <- c(
  "1" = paste0("Class 3 (", class_count[1, 3], ")"),
  "2" = paste0("Class 5 (", class_count[2, 3], ")"),
  "3" = paste0("Class 1 (", class_count[3, 3], ")"),
  "4" = paste0("Class 2 (", class_count[4, 3], ")"),
  "5" = paste0("Class 4 (", class_count[5, 3], ")"),
  "6" = paste0("Class 6 (", class_count[6, 3], ")"),
  "7" = paste0("Class 7 (", class_count[7, 3], ")")
)





colours <- ColourPalleteMulti(model_results, "Behaviour", "category")

model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('3','4','1','5','2','6','7'))

##### changing the category column to produce a clear graph
Recoded_model_results <- model_results %>%
  mutate(new_category = ifelse(Behaviour == "SMOKE" & category == 1, 1,
                               ifelse(Behaviour == "SMOKE" & category == 2, 2,
                                      ifelse(Behaviour == "DRINK" & category == 1, 3,
                                             ifelse(Behaviour == "DRINK" & category == 2, 4,
                                                    ifelse(Behaviour == "DRINK" & category == 3, 5,
                                                           ifelse(Behaviour == "DRINK" & category == 4, 6,
                                                                  ifelse(Behaviour == "PA" & category == 4, 7,
                                                                         ifelse(Behaviour == "PA" & category == 3, 8,
                                                                                ifelse(Behaviour == "PA" & category == 2, 9,
                                                                                       ifelse(Behaviour == "PA" & category == 1, 10,
                                                                                              ifelse(Behaviour == "FV" & category == 2, 11,
                                                                                                     ifelse(Behaviour == "FV" & category == 1, 12, 100)
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
  ))
Recoded_model_results$new_category <- as.factor(Recoded_model_results$new_category)
Recoded_model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('3','4','1','5','2','6','7'))


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

bar1 <- Recoded_model_results %>%
  ggplot(aes(fill = new_category)) +
  geom_bar(aes(x = param, y = est), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_rep_wrap(~LatentClass_factor, labeller = as_labeller(labels), scales = "fixed", repeat.tick.labels = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text.x = element_text(size = 10))
bar1

g1 <- ggplotGrob(bar1)
gtable_show_layout(g1)
g1_am0 <- g1[c(1:10), -c(8:17)]
grid.newpage()
grid.draw(g1_am0)


g2_am0 <- g1[c(1:10), c(6:11)]
grid.newpage()
grid.draw(g2_am0)

g3_am0 <- g1[c(1:10), c(11:16)]
grid.newpage()
grid.draw(g3_am0)

g4_am0 <- g1[c(10:18), -c(8:17)]
grid.newpage()
grid.draw(g4_am0)

g5_am0 <- g1[c(10:18), c(6:11)]
grid.newpage()
grid.draw(g5_am0)

g6_am0 <- g1[c(10:18), c(11:16)]
grid.newpage()
grid.draw(g6_am0)


data_split_1 <- model_results[model_results$Behaviour %in% "SMOKE", ]
head(data_split_1, 100)

data_split_2 <- model_results[model_results$Behaviour %in% "DRINK", ]
head(data_split_2)

data_split_3 <- model_results[model_results$Behaviour %in% "PA", ]
head(data_split_3)

data_split_4 <- model_results[model_results$Behaviour %in% "FV", ]
head(data_split_4)

gg_split_1 <- data_split_1 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("yellow", "yellow3"), name = "Smoking", labels = c("Non-smoker", "Smoker")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_1

gg_legend_1 <- get_legend(gg_split_1)


gg_split_2 <- data_split_2 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("darkseagreen1", "palegreen3", "mediumseagreen", "seagreen"), name = "Alcohol consumption", labels = c("Abstainer", "Moderate", "Hazardous", "Harmful")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_2

gg_legend_2 <- get_legend(gg_split_2)


gg_split_3 <- data_split_3 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("azure1", "paleturquoise2", "turquoise3", "darkcyan"), name = "Physical activity", labels = c("High", "Moderate", "Low", "Sedentary")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_3

gg_legend_3 <- get_legend(gg_split_3)

gg_split_4 <- data_split_4 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("lightsteelblue2","steelblue" ), name = "Fruit & vegetable intake", labels = c(">=5/day", "<5/day")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_4


gg_legend_4 <- get_legend(gg_split_4)


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

head(Recoded_model_results)
gg_no_legend <- Recoded_model_results %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = new_category), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      "4", "5", "6\n\nSmoking", "7", "8",
      "4", "5", "6\n\nAlcohol\nconsumption","7", "8",
      "4", "5", "6\n\nPhysical \nactivity", "7", "8",
      "4", "5", "6\n\nFruit &\nVegetables","7","8"
    )
  ) +
  labs(    title = "Class-membership probabilities for LCA model (Sample B)",
    x = "Health behaviours across waves", 
    y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12,face = "bold",margin = margin(0.3,0,0.3,0, "cm")),
        legend.position = "none",
        axis.title.x = element_text(vjust = -0.75)
  ) 
gg_no_legend

legends <- plot_grid(gg_legend_1, gg_legend_2, gg_legend_3, gg_legend_4, ncol = 1, align = "v", rel_widths = c(1 / 4, 1 / 4, 1 / 4, 1 / 4))
final_plot <- grid.arrange(gg_no_legend, legends, ncol = 2, widths = c(5, 1))
final_plot

ggsave(here("Figures","4404_cases","LCA_6class_SampleB.png"),final_plot,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)



###### Visualising a 7 class model : SAMPLE B ######

LCA_output <- readModels("c7_lca_enumerate.out", recursive = TRUE, what = "all")

enum_summary <- LatexSummaryTable(LCA_output, #
                                  keepCols = c(
                                    "Title", "AIC", "BIC", "aBIC", "Entropy", #
                                    "T11_VLMR_PValue"
                                  ), #
                                  sortBy = "Title"
) #

gt(enum_summary) %>% #
  tab_header( #
    title = "Fit Indices"
  ) %>% #
  tab_options( #
    table.width = pct(80)
  ) %>% #
  tab_footnote( #
    footnote = "English Longitudinal Study of Ageing : SNAP behaviours", #
    location = cells_title()
  )

model_results <- LCA_output$parameters$probability.scale #

###

model_results <- model_results %>%
  mutate(Behaviour = c(ifelse(endsWith(param, "SMOKE") == TRUE, "SMOKE",
                              ifelse(endsWith(param, "DRINK") == TRUE, "DRINK",
                                     ifelse(endsWith(param, "FV") == TRUE, "FV",
                                            ifelse(endsWith(param, "PA") == TRUE, "PA",
                                                   NA
                                            )
                                     )
                              )
  )))
head(model_results)


ColourPalleteMulti <- function(df, group, subgroup) {
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep = "~")), df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 100)(nrow(categories))) # Set the top of the colour pallete
  category.end <- (scales::hue_pal(l = 40)(nrow(categories))) # set the bottom
  
  # Build Colour pallette
  colours <- unlist(lapply(
    1:nrow(categories),
    function(i) {
      colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i, 2])
    }
  ))
  return(colours)
}


class_count <- LCA_output$class_counts$modelEstimated %>%
  mutate(percent = percent(proportion, accuracy = .01)) %>%
  dplyr::select(-proportion)

labels <- c(
  "1" = paste0("Class 6 (", class_count[1, 3], ")"),
  "2" = paste0("Class 7 (", class_count[2, 3], ")"),
  "3" = paste0("Class 1 (", class_count[3, 3], ")"),
  "4" = paste0("Class 4 (", class_count[4, 3], ")"),
  "5" = paste0("Class 5 (", class_count[5, 3], ")"),
  "6" = paste0("Class 3 (", class_count[6, 3], ")"),
  "7" = paste0("Class 2 (", class_count[7, 3], ")")
)




colours <- ColourPalleteMulti(model_results, "Behaviour", "category")

model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('3','7','6','4','5','1','2'))

##### changing the category column to produce a clear graph
Recoded_model_results <- model_results %>%
  mutate(new_category = ifelse(Behaviour == "SMOKE" & category == 1, 1,
                               ifelse(Behaviour == "SMOKE" & category == 2, 2,
                                      ifelse(Behaviour == "DRINK" & category == 1, 3,
                                             ifelse(Behaviour == "DRINK" & category == 2, 4,
                                                    ifelse(Behaviour == "DRINK" & category == 3, 5,
                                                           ifelse(Behaviour == "DRINK" & category == 4, 6,
                                                                  ifelse(Behaviour == "PA" & category == 4, 7,
                                                                         ifelse(Behaviour == "PA" & category == 3, 8,
                                                                                ifelse(Behaviour == "PA" & category == 2, 9,
                                                                                       ifelse(Behaviour == "PA" & category == 1, 10,
                                                                                              ifelse(Behaviour == "FV" & category == 2, 11,
                                                                                                     ifelse(Behaviour == "FV" & category == 1, 12, 100)
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
  ))
Recoded_model_results$new_category <- as.factor(Recoded_model_results$new_category)
Recoded_model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('3','7','6','4','5','1','2'))


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

bar1 <- Recoded_model_results %>%
  ggplot(aes(fill = new_category)) +
  geom_bar(aes(x = param, y = est), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_rep_wrap(~LatentClass_factor, labeller = as_labeller(labels), scales = "fixed", repeat.tick.labels = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text.x = element_text(size = 10))
bar1

g1 <- ggplotGrob(bar1)
gtable_show_layout(g1)
g1_am0 <- g1[c(1:10), -c(8:17)]
grid.newpage()
grid.draw(g1_am0)


g2_am0 <- g1[c(1:10), c(6:11)]
grid.newpage()
grid.draw(g2_am0)

g3_am0 <- g1[c(1:10), c(11:16)]
grid.newpage()
grid.draw(g3_am0)

g4_am0 <- g1[c(10:18), -c(8:17)]
grid.newpage()
grid.draw(g4_am0)

g5_am0 <- g1[c(10:18), c(6:11)]
grid.newpage()
grid.draw(g5_am0)

g6_am0 <- g1[c(10:18), c(11:16)]
grid.newpage()
grid.draw(g6_am0)


data_split_1 <- model_results[model_results$Behaviour %in% "SMOKE", ]
head(data_split_1, 100)

data_split_2 <- model_results[model_results$Behaviour %in% "DRINK", ]
head(data_split_2)

data_split_3 <- model_results[model_results$Behaviour %in% "PA", ]
head(data_split_3)

data_split_4 <- model_results[model_results$Behaviour %in% "FV", ]
head(data_split_4)

gg_split_1 <- data_split_1 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("yellow", "yellow3"), name = "Smoking", labels = c("Non-smoker", "Smoker")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_1

gg_legend_1 <- get_legend(gg_split_1)


gg_split_2 <- data_split_2 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("darkseagreen1", "palegreen3", "mediumseagreen", "seagreen"), name = "Alcohol consumption", labels = c("Abstainer", "Moderate", "Hazardous", "Harmful")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_2

gg_legend_2 <- get_legend(gg_split_2)


gg_split_3 <- data_split_3 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("azure1", "paleturquoise2", "turquoise3", "darkcyan"), name = "Physical activity", labels = c("High", "Moderate", "Low", "Sedentary")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_3

gg_legend_3 <- get_legend(gg_split_3)

gg_split_4 <- data_split_4 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("lightsteelblue2","steelblue" ), name = "Fruit & vegetable intake", labels = c(">=5/day", "<5/day")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_4


gg_legend_4 <- get_legend(gg_split_4)


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

head(Recoded_model_results)
gg_no_legend <- Recoded_model_results %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = new_category), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      "4", "5", "6\n\nSmoking", "7", "8",
      "4", "5", "6\n\nAlcohol\nconsumption","7", "8",
      "4", "5", "6\n\nPhysical \nactivity", "7", "8",
      "4", "5", "6\n\nFruit &\nVegetables","7","8"
    )
  ) +
  labs(    title = "Class-membership probabilities for LCA model (Sample B)",
    x = "Health behaviours across waves", 
    y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12,face = "bold",margin = margin(0.3,0,0.3,0, "cm")),
        legend.position = "none",
        axis.title.x = element_text(vjust = -0.75)
  ) 
gg_no_legend

legends <- plot_grid(gg_legend_1, gg_legend_2, gg_legend_3, gg_legend_4, ncol = 1, align = "v", rel_widths = c(1 / 4, 1 / 4, 1 / 4, 1 / 4))
final_plot <- grid.arrange(gg_no_legend, legends, ncol = 2, widths = c(5, 1))
final_plot

ggsave(here("Figures","4404_cases","LCA_7class_SampleB.png"),final_plot,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)




#### Complete sample : Class enumeration #####
#setting to relative path as MPlus only accepts 90 characters in each line 
setwd(here("Data", "Processed_Data", "MPlus_data","MPlus_4404cases","Complete_sample"))
getwd()

##### Step 1 for Complete Sample ####

output_enum <- readModels("./")

enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols = c(
                                    "Title",
                                    "LL", "AIC",
                                    "BIC",
                                    "aBIC", "Entropy", "T11_VLMR_PValue", "T11_BLRT_PValue"
                                  ),
                                  sortBy = "Title"
)
save_kable(enum_summary,here("Figures","4404_cases","Fit_Indices_Complete_Sample.png"))
Indices <- enum_summary %>%
  kable(booktabs = T, linesep = "") %>%
  kable_styling(c("striped"),
                full_width = F,
                position = "left"
  )
Indices <- save_kable(Indices,here("Figures","4404_cases","Fit_Indices_Complete_Sample.png"))


#### Plotting AIC, BIC, aBIC of models with different number of classes
results1 <- enum_summary[,c(1,3:5)]
classes <- as.character(c(1:9))
results1[,1]<-classes
#convert to long format
results2<-tidyr::gather(results1,"criterias","values", -Title)
results2$criterias<-as.factor(results2$criterias)

Screeplot <- qplot(Title, values, data = results2, color = factor(criterias))+
  geom_point()+
  geom_line(aes(group = criterias)) + 
  xlab("Latent classes") + 
  ylab("Values") +
  scale_color_discrete("Information criteria")+
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),
        legend.title = element_text(size = 14, face="bold"),
        legend.text = element_text(size = 12))+
  ggtitle("Scree Plot of complete sample (4759 cases)") 
ggsave(here("Figures","4404_cases","Scree_Plot_Complete_Sample.png"),Screeplot,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)
#### Plotting the log likelihood of models with different number of classes

results1 <- enum_summary[,c(1:2)]
classes <- as.character(c(1:9))
results1[,1]<-classes
#convert to long format
results2<-tidyr::gather(results1,"criterias","values", -Title)
results2$criterias<-as.factor(results2$criterias)

Loglikelihood <- qplot(Title, values, data = results2, color = factor(criterias))+
  geom_point()+
  geom_line(aes(group = criterias)) + 
  xlab("Latent classes") + 
  ylab("Values") +
  scale_color_discrete("Loglikelihood") +
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),
        legend.title = element_text(size = 14, face="bold"),
        legend.text = element_text(size = 12))+
  ggtitle("Loglikelihood Plot of complete sample (4759 cases)") 
ggsave(here("Figures","4404_cases","Loglikelihood_Plot_Complete_Sample.png"),Loglikelihood,width=30, height=22, dpi=300, units=c("cm"),limitsize = FALSE)


###### Visualising a 5 class model : COMPLETE SAMPLE ######

LCA_output <- readModels("c5_lca_enumerate.out", recursive = TRUE, what = "all")

enum_summary <- LatexSummaryTable(LCA_output, #
                                  keepCols = c(
                                    "Title", "AIC", "BIC", "aBIC", "Entropy", #
                                    "T11_VLMR_PValue"
                                  ), #
                                  sortBy = "Title"
) #

gt(enum_summary) %>% #
  tab_header( #
    title = "Fit Indices"
  ) %>% #
  tab_options( #
    table.width = pct(80)
  ) %>% #
  tab_footnote( #
    footnote = "English Longitudinal Study of Ageing : SNAP behaviours", #
    location = cells_title()
  )

model_results <- LCA_output$parameters$probability.scale #

###

model_results <- model_results %>%
  mutate(Behaviour = c(ifelse(endsWith(param, "SMOKE") == TRUE, "SMOKE",
                              ifelse(endsWith(param, "DRINK") == TRUE, "DRINK",
                                     ifelse(endsWith(param, "FV") == TRUE, "FV",
                                            ifelse(endsWith(param, "PA") == TRUE, "PA",
                                                   NA
                                            )
                                     )
                              )
  )))
head(model_results)


ColourPalleteMulti <- function(df, group, subgroup) {
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep = "~")), df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 100)(nrow(categories))) # Set the top of the colour pallete
  category.end <- (scales::hue_pal(l = 40)(nrow(categories))) # set the bottom
  
  # Build Colour pallette
  colours <- unlist(lapply(
    1:nrow(categories),
    function(i) {
      colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i, 2])
    }
  ))
  return(colours)
}


class_count <- LCA_output$class_counts$modelEstimated %>%
  mutate(percent = percent(proportion, accuracy = .01)) %>%
  dplyr::select(-proportion)

labels <- c(
  "1" = paste0("Class 1 (", class_count[1, 3], ")"),
  "2" = paste0("Class 4 (", class_count[2, 3], ")"),
  "3" = paste0("Class 2 (", class_count[3, 3], ")"),
  "4" = paste0("Class 5 (", class_count[4, 3], ")"),
  "5" = paste0("Class 3 (", class_count[5, 3], ")"),
  "6" = paste0("Class 6 (", class_count[6, 3], ")"),
  "7" = paste0("Class 7 (", class_count[7, 3], ")")
)



colours <- ColourPalleteMulti(model_results, "Behaviour", "category")

model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('1','3','5','2','4','6','7'))

##### changing the category column to produce a clear graph
Recoded_model_results <- model_results %>%
  mutate(new_category = ifelse(Behaviour == "SMOKE" & category == 1, 1,
                               ifelse(Behaviour == "SMOKE" & category == 2, 2,
                                      ifelse(Behaviour == "DRINK" & category == 1, 3,
                                             ifelse(Behaviour == "DRINK" & category == 2, 4,
                                                    ifelse(Behaviour == "DRINK" & category == 3, 5,
                                                           ifelse(Behaviour == "DRINK" & category == 4, 6,
                                                                  ifelse(Behaviour == "PA" & category == 4, 7,
                                                                         ifelse(Behaviour == "PA" & category == 3, 8,
                                                                                ifelse(Behaviour == "PA" & category == 2, 9,
                                                                                       ifelse(Behaviour == "PA" & category == 1, 10,
                                                                                              ifelse(Behaviour == "FV" & category == 2, 11,
                                                                                                     ifelse(Behaviour == "FV" & category == 1, 12, 100)
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
  ))
Recoded_model_results$new_category <- as.factor(Recoded_model_results$new_category)
Recoded_model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('1','3','5','2','4','6','7'))


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

bar1 <- Recoded_model_results %>%
  ggplot(aes(fill = new_category)) +
  geom_bar(aes(x = param, y = est), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_rep_wrap(~LatentClass_factor, labeller = as_labeller(labels), scales = "fixed", repeat.tick.labels = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text.x = element_text(size = 10))
bar1

g1 <- ggplotGrob(bar1)
gtable_show_layout(g1)
g1_am0 <- g1[c(1:10), -c(8:17)]
grid.newpage()
grid.draw(g1_am0)


g2_am0 <- g1[c(1:10), c(6:11)]
grid.newpage()
grid.draw(g2_am0)

g3_am0 <- g1[c(1:10), c(11:16)]
grid.newpage()
grid.draw(g3_am0)

g4_am0 <- g1[c(10:18), -c(8:17)]
grid.newpage()
grid.draw(g4_am0)

g5_am0 <- g1[c(10:18), c(6:11)]
grid.newpage()
grid.draw(g5_am0)

g6_am0 <- g1[c(10:18), c(11:16)]
grid.newpage()
grid.draw(g6_am0)


data_split_1 <- model_results[model_results$Behaviour %in% "SMOKE", ]
head(data_split_1, 100)

data_split_2 <- model_results[model_results$Behaviour %in% "DRINK", ]
head(data_split_2)

data_split_3 <- model_results[model_results$Behaviour %in% "PA", ]
head(data_split_3)

data_split_4 <- model_results[model_results$Behaviour %in% "FV", ]
head(data_split_4)

gg_split_1 <- data_split_1 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("yellow", "yellow3"), name = "Smoking", labels = c("Non-smoker", "Smoker")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_1

gg_legend_1 <- get_legend(gg_split_1)


gg_split_2 <- data_split_2 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("darkseagreen1", "palegreen3", "mediumseagreen", "seagreen"), name = "Alcohol consumption", labels = c("Abstainer", "Moderate", "Hazardous", "Harmful")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_2

gg_legend_2 <- get_legend(gg_split_2)


gg_split_3 <- data_split_3 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("azure1", "paleturquoise2", "turquoise3", "darkcyan"), name = "Physical activity", labels = c("High", "Moderate", "Low", "Sedentary")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_3

gg_legend_3 <- get_legend(gg_split_3)

gg_split_4 <- data_split_4 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("lightsteelblue2","steelblue" ), name = "Fruit & vegetable intake", labels = c(">=5/day", "<5/day")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_4


gg_legend_4 <- get_legend(gg_split_4)


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

head(Recoded_model_results)
gg_no_legend <- Recoded_model_results %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = new_category), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      "4", "5", "6\n\nSmoking", "7", "8",
      "4", "5", "6\n\nAlcohol\nconsumption","7", "8",
      "4", "5", "6\n\nPhysical \nactivity", "7", "8",
      "4", "5", "6\n\nFruit &\nVegetables","7","8"
    )
  ) +
  labs(    title = "Class-membership probabilities for LCA model (Complete Sample)",
           
    x = "Health behaviours across waves", 
    y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12,face = "bold",margin = margin(0.3,0,0.3,0, "cm")),
        legend.position = "none",
        axis.title.x = element_text(vjust = -0.75)
  ) 
gg_no_legend

legends <- plot_grid(gg_legend_1, gg_legend_2, gg_legend_3, gg_legend_4, ncol = 1, align = "v", rel_widths = c(1 / 4, 1 / 4, 1 / 4, 1 / 4))
final_plot <- grid.arrange(gg_no_legend, legends, ncol = 2, widths = c(5, 1))
final_plot

ggsave(here("Figures","4404_cases","LCA_5class_Complete_Sample.png"),final_plot,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)


###### Visualising a 6 class model : COMPLETE SAMPLE ######

LCA_output <- readModels("c6_lca_enumerate.out", recursive = TRUE, what = "all")

enum_summary <- LatexSummaryTable(LCA_output, #
                                  keepCols = c(
                                    "Title", "AIC", "BIC", "aBIC", "Entropy", #
                                    "T11_VLMR_PValue"
                                  ), #
                                  sortBy = "Title"
) #

gt(enum_summary) %>% #
  tab_header( #
    title = "Fit Indices"
  ) %>% #
  tab_options( #
    table.width = pct(80)
  ) %>% #
  tab_footnote( #
    footnote = "English Longitudinal Study of Ageing : SNAP behaviours", #
    location = cells_title()
  )

model_results <- LCA_output$parameters$probability.scale #
head(model_results)
###

model_results <- model_results %>%
  mutate(Behaviour = c(ifelse(endsWith(param, "SMOKE") == TRUE, "SMOKE",
                              ifelse(endsWith(param, "DRINK") == TRUE, "DRINK",
                                     ifelse(endsWith(param, "FV") == TRUE, "FV",
                                            ifelse(endsWith(param, "PA") == TRUE, "PA",
                                                   NA
                                            )
                                     )
                              )
  )))
head(model_results)


ColourPalleteMulti <- function(df, group, subgroup) {
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep = "~")), df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 100)(nrow(categories))) # Set the top of the colour pallete
  category.end <- (scales::hue_pal(l = 40)(nrow(categories))) # set the bottom
  
  # Build Colour pallette
  colours <- unlist(lapply(
    1:nrow(categories),
    function(i) {
      colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i, 2])
    }
  ))
  return(colours)
}


class_count <- LCA_output$class_counts$modelEstimated %>%
  mutate(percent = percent(proportion, accuracy = .1)) %>%
  dplyr::select(-proportion)
font_add_google("Roboto", "roboto")

labels <- c(
  "1" = paste0("Class 3 (", class_count[1, 3], ")"),
  "2" = paste0("Class 5 (", class_count[2, 3], ")"),
  "3" = paste0("Class 1 (", class_count[3, 3], ")"),
  "4" = paste0("Class 6 (", class_count[4, 3], ")"),
  "5" = paste0("Class 2 (", class_count[5, 3], ")"),
  "6" = paste0("Class 4 (", class_count[6, 3], ")"),
  "7" = paste0("Class 7 (", class_count[7, 3], ")")
)




colours <- ColourPalleteMulti(model_results, "Behaviour", "category")

model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('3','5','1','6','2','4','7'))

##### changing the category column to produce a clear graph
Recoded_model_results <- model_results %>%
  mutate(new_category = ifelse(Behaviour == "SMOKE" & category == 1, 1,
                               ifelse(Behaviour == "SMOKE" & category == 2, 2,
                                      ifelse(Behaviour == "DRINK" & category == 1, 3,
                                             ifelse(Behaviour == "DRINK" & category == 2, 4,
                                                    ifelse(Behaviour == "DRINK" & category == 3, 5,
                                                           ifelse(Behaviour == "DRINK" & category == 4, 6,
                                                                  ifelse(Behaviour == "PA" & category == 4, 7,
                                                                         ifelse(Behaviour == "PA" & category == 3, 8,
                                                                                ifelse(Behaviour == "PA" & category == 2, 9,
                                                                                       ifelse(Behaviour == "PA" & category == 1, 10,
                                                                                              ifelse(Behaviour == "FV" & category == 2, 11,
                                                                                                     ifelse(Behaviour == "FV" & category == 1, 12, 100)
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
  ))
Recoded_model_results$new_category <- as.factor(Recoded_model_results$new_category)
Recoded_model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('3','5','1','6','2','4','7'))


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

bar1 <- Recoded_model_results %>%
  ggplot(aes(fill = new_category)) +
  geom_bar(aes(x = param, y = est), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_rep_wrap(~LatentClass_factor, labeller = as_labeller(labels), scales = "fixed", repeat.tick.labels = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text.x = element_text(size = 10))
bar1

g1 <- ggplotGrob(bar1)
gtable_show_layout(g1)
g1_am0 <- g1[c(1:10), -c(8:17)]
grid.newpage()
grid.draw(g1_am0)


g2_am0 <- g1[c(1:10), c(6:11)]
grid.newpage()
grid.draw(g2_am0)

g3_am0 <- g1[c(1:10), c(11:16)]
grid.newpage()
grid.draw(g3_am0)

g4_am0 <- g1[c(10:18), -c(8:17)]
grid.newpage()
grid.draw(g4_am0)

g5_am0 <- g1[c(10:18), c(6:11)]
grid.newpage()
grid.draw(g5_am0)

g6_am0 <- g1[c(10:18), c(11:16)]
grid.newpage()
grid.draw(g6_am0)


data_split_1 <- model_results[model_results$Behaviour %in% "SMOKE", ]
head(data_split_1, 100)

data_split_2 <- model_results[model_results$Behaviour %in% "DRINK", ]
head(data_split_2)

data_split_3 <- model_results[model_results$Behaviour %in% "PA", ]
head(data_split_3)

data_split_4 <- model_results[model_results$Behaviour %in% "FV", ]
head(data_split_4)

gg_split_1 <- data_split_1 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("yellow", "yellow3"), name = "Smoking", labels = c("Non-smoker", "Smoker")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_1

gg_legend_1 <- get_legend(gg_split_1)


gg_split_2 <- data_split_2 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("darkseagreen1", "palegreen3", "mediumseagreen", "seagreen"), name = "Alcohol consumption", labels = c("Abstainer", "Moderate", "Hazardous", "Harmful")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_2

gg_legend_2 <- get_legend(gg_split_2)


gg_split_3 <- data_split_3 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("azure1", "paleturquoise2", "turquoise3", "darkcyan"), name = "Physical activity", labels = c("High", "Moderate", "Low", "Sedentary")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_3

gg_legend_3 <- get_legend(gg_split_3)

gg_split_4 <- data_split_4 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("lightsteelblue2","steelblue" ), name = "Fruit & vegetable intake", labels = c(">=5/day", "<5/day")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_4


gg_legend_4 <- get_legend(gg_split_4)


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

head(Recoded_model_results)
gg_no_legend <- Recoded_model_results %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = new_category), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      "4", "5", "6\n\nSmoking", "7", "8",
      "4", "5", "6\n\nAlcohol\nconsumption","7", "8",
      "4", "5", "6\n\nPhysical \nactivity", "7", "8",
      "4", "5", "6\n\nFruit &\nVegetables","7","8"
    )
  ) +
  labs(    title = "Class-membership probabilities for LCA model (Complete Sample)",
    x = "Health behaviours across waves", 
    y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12,face = "bold",margin = margin(0.3,0,0.3,0, "cm")),
        legend.position = "none",
        axis.title.x = element_text(vjust = -0.75)
  ) 
gg_no_legend

legends <- plot_grid(gg_legend_1, gg_legend_2, gg_legend_3, gg_legend_4, ncol = 1, align = "v", rel_widths = c(1 / 4, 1 / 4, 1 / 4, 1 / 4))
final_plot <- grid.arrange(gg_no_legend, legends, ncol = 2, widths = c(5, 1))
final_plot

ggsave(here("Figures","4404_cases","LCA_6class_Complete_Sample.png"),final_plot,width=30, height=22, dpi=300,units=c("cm"),limitsize = FALSE)



###### Visualising a 7 class model : COMPLETE SAMPLE ######

LCA_output <- readModels("c7_lca_enumerate.out", recursive = TRUE, what = "all")

enum_summary <- LatexSummaryTable(LCA_output, #
                                  keepCols = c(
                                    "Title", "AIC", "BIC", "aBIC", "Entropy", #
                                    "T11_VLMR_PValue"
                                  ), #
                                  sortBy = "Title"
) #

gt(enum_summary) %>% #
  tab_header( #
    title = "Fit Indices"
  ) %>% #
  tab_options( #
    table.width = pct(80)
  ) %>% #
  tab_footnote( #
    footnote = "English Longitudinal Study of Ageing : SNAP behaviours", #
    location = cells_title()
  )

model_results <- LCA_output$parameters$probability.scale #

###

model_results <- model_results %>%
  mutate(Behaviour = c(ifelse(endsWith(param, "SMOKE") == TRUE, "SMOKE",
                              ifelse(endsWith(param, "DRINK") == TRUE, "DRINK",
                                     ifelse(endsWith(param, "FV") == TRUE, "FV",
                                            ifelse(endsWith(param, "PA") == TRUE, "PA",
                                                   NA
                                            )
                                     )
                              )
  )))
head(model_results)


ColourPalleteMulti <- function(df, group, subgroup) {
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep = "~")), df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 100)(nrow(categories))) # Set the top of the colour pallete
  category.end <- (scales::hue_pal(l = 40)(nrow(categories))) # set the bottom
  
  # Build Colour pallette
  colours <- unlist(lapply(
    1:nrow(categories),
    function(i) {
      colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i, 2])
    }
  ))
  return(colours)
}


class_count <- LCA_output$class_counts$modelEstimated %>%
  mutate(percent = percent(proportion, accuracy = .01)) %>%
  dplyr::select(-proportion)

labels <- c(
  "1" = paste0("Inactive, heavy drinkers (", class_count[1, 3], ")"),
  "2" = paste0("High-risk smokers (", class_count[2, 3], ")"),
  "3" = paste0("Low risk yet inactive (", class_count[3, 3], ")"),
  "4" = paste0("Low risk yet heavy drinkers (", class_count[4, 3], ")"),
  "5" = paste0("Abstainer yet inactive (", class_count[5, 3], ")"),
  "6" = paste0("Poor diet and inactive (", class_count[6, 3], ")"),
  "7" = paste0("Low risk (", class_count[7, 3], ")")
)




colours <- ColourPalleteMulti(model_results, "Behaviour", "category")

model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('7','3','4','5','6','1','2'))

##### changing the category column to produce a clear graph
Recoded_model_results <- model_results %>%
  mutate(new_category = ifelse(Behaviour == "SMOKE" & category == 1, 1,
                               ifelse(Behaviour == "SMOKE" & category == 2, 2,
                                      ifelse(Behaviour == "DRINK" & category == 1, 3,
                                             ifelse(Behaviour == "DRINK" & category == 2, 4,
                                                    ifelse(Behaviour == "DRINK" & category == 3, 5,
                                                           ifelse(Behaviour == "DRINK" & category == 4, 6,
                                                                  ifelse(Behaviour == "PA" & category == 4, 7,
                                                                         ifelse(Behaviour == "PA" & category == 3, 8,
                                                                                ifelse(Behaviour == "PA" & category == 2, 9,
                                                                                       ifelse(Behaviour == "PA" & category == 1, 10,
                                                                                              ifelse(Behaviour == "FV" & category == 2, 11,
                                                                                                     ifelse(Behaviour == "FV" & category == 1, 12, 100)
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
  ))
Recoded_model_results$new_category <- as.factor(Recoded_model_results$new_category)
Recoded_model_results$LatentClass_factor <- factor(model_results$LatentClass, levels = c('7','3','4','5','6','1','2'))


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

bar1 <- Recoded_model_results %>%
  ggplot(aes(fill = new_category)) +
  geom_bar(aes(x = param, y = est), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_rep_wrap(~LatentClass_factor, labeller = as_labeller(labels), scales = "fixed", repeat.tick.labels = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text.x = element_text(size = 10))
bar1

g1 <- ggplotGrob(bar1)
gtable_show_layout(g1)
g1_am0 <- g1[c(1:10), -c(8:17)]
grid.newpage()
grid.draw(g1_am0)


g2_am0 <- g1[c(1:10), c(6:11)]
grid.newpage()
grid.draw(g2_am0)

g3_am0 <- g1[c(1:10), c(11:16)]
grid.newpage()
grid.draw(g3_am0)

g4_am0 <- g1[c(10:18), -c(8:17)]
grid.newpage()
grid.draw(g4_am0)

g5_am0 <- g1[c(10:18), c(6:11)]
grid.newpage()
grid.draw(g5_am0)

g6_am0 <- g1[c(10:18), c(11:16)]
grid.newpage()
grid.draw(g6_am0)


data_split_1 <- model_results[model_results$Behaviour %in% "SMOKE", ]
head(data_split_1, 100)

data_split_2 <- model_results[model_results$Behaviour %in% "DRINK", ]
head(data_split_2)

data_split_3 <- model_results[model_results$Behaviour %in% "PA", ]
head(data_split_3)

data_split_4 <- model_results[model_results$Behaviour %in% "FV", ]
head(data_split_4)

gg_split_1 <- data_split_1 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("yellow", "yellow3"), name = "Smoking", labels = c("Non-smoker", "Smoker")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_1

gg_legend_1 <- get_legend(gg_split_1)


gg_split_2 <- data_split_2 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("darkseagreen1", "palegreen3", "mediumseagreen", "seagreen"), name = "Alcohol consumption", labels = c("Abstainer", "Moderate", "Hazardous", "Harmful")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_2

gg_legend_2 <- get_legend(gg_split_2)


gg_split_3 <- data_split_3 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2, 3, 4), values = c("azure1", "paleturquoise2", "turquoise3", "darkcyan"), name = "Physical activity", labels = c("High", "Moderate", "Low", "Sedentary")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_3

gg_legend_3 <- get_legend(gg_split_3)

gg_split_4 <- data_split_4 %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = category), position = "stack", stat = "identity") +
  scale_fill_manual(breaks = c(1, 2), values = c("lightsteelblue2","steelblue" ), name = "Fruit & vegetable intake", labels = c(">=5/day", "<5/day")) +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      
      "Smoking_4", "Smoking_5", "Smoking_6", "Smoking_7", "Smoking_8",
      "Alcohol consumption_4", "Alcohol consumption_5", "Alcohol consumption_6", "Alcohol consumption_7", "Alcohol consumption_8",
      "Physical activity_ 4", "Physical activity_5", "Physical activity_6", "Physical activity_7", "Physical activity_8",
      "Fruit Veg_4", "Fruit Veg_5", "Fruit Veg_6", "Fruit Veg_7", "Fruit Veg_8"
    )
  ) +
  labs(
    title = "Class-membership probabilities for LCA model",
    x = "Health behaviours across waves", y = "Item-class probabilities"
  ) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5), strip.text.x = element_text(size = 10),
    legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), legend.title = element_text(size = 12, face="bold")
  )
gg_split_4


gg_legend_4 <- get_legend(gg_split_4)


colours_new <- c(
  "yellow", "yellow3", "darkseagreen1", "palegreen3", "mediumseagreen", "seagreen","azure1", "paleturquoise2", "turquoise3", "darkcyan", "lightsteelblue2","steelblue" 
)

head(Recoded_model_results)
gg_no_legend <- Recoded_model_results %>%
  ggplot() +
  geom_bar(aes(x = param, y = est, fill = new_category), position = "stack", stat = "identity") +
  scale_x_discrete(
    limits = c(
      "W4_SMOKE", "W5_SMOKE", "W6_SMOKE", "W7_SMOKE", "W8_SMOKE",
      "W4_DRINK",
      "W5_DRINK", "W6_DRINK", "W7_DRINK", "W8_DRINK",
      "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA",
      "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV"
    ),
    labels = c(
      "4", "5", "6\n\nSmoking", "7", "8",
      "4", "5", "6\n\nAlcohol\nconsumption","7", "8",
      "4", "5", "6\n\nPhysical \nactivity", "7", "8",
      "4", "5", "6\n\nFruit &\nVegetables","7","8"
    )
  ) +
  labs(    title = "Class-membership probabilities for LCA model (Complete Sample)",
    x = "Health behaviours across waves", 
    y = "Item-class probabilities"
  ) +
  scale_fill_manual(values = colours_new) +
  facet_wrap(~LatentClass_factor, labeller = as_labeller(labels)) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12,face = "bold",margin = margin(0.3,0,0.3,0, "cm")),
        legend.position = "none",
        axis.title.x = element_text(vjust = -0.75)
  ) 
gg_no_legend

legends <- plot_grid(gg_legend_1, gg_legend_2, gg_legend_3, gg_legend_4, ncol = 1, align = "v", rel_widths = c(1 / 4, 1 / 4, 1 / 4, 1 / 4))
final_plot <- grid.arrange(gg_no_legend, legends, ncol = 2, widths = c(5, 1))
final_plot

ggsave(here("Figures","4404_cases","LCA_7class_Complete_Sample.png"),final_plot,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)







##### We selected the 6 class model after inspecting other candidate models (i.e. 5- and 7-class models)











