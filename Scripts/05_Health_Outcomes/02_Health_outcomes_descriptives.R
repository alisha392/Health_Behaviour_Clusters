
# As Mplus requires that files not have a header row and that the variable names
# be specified within the Mplus input syntax, the prepareMplusData() allows us to do this on R
main_data <- read_csv(here("Data", "Processed_Data", "R_Data", "Health_Outcomes", "Outcomes_Demographics_Behaviours_4to9.csv"))
head(main_data)


main_data <- main_data %>% 
  dplyr::mutate(Female = ifelse(main_data$Sex<0, -99, ifelse(main_data$Sex == 2, 1, 0))) %>% 
  dplyr::select(-2) 

main_data <- main_data[, c(1,11:31,54,2:10,43:53,32:42)]

head(main_data)

main_data[main_data == -99] <- NA
 


Complete_case <- main_data[complete.cases(main_data[23:32]), ]
Missing_case <- main_data[!complete.cases(main_data[23:32]), ]


columns <- colnames(Complete_case[44:54])

# Create an empty data frame to store the results
results <- data.frame(Column = character(), Level = character(), Count = numeric(), Percentage = numeric(), stringsAsFactors = FALSE)

# Loop through each column
for (col in columns) {
  # Calculate table
  tbl <- table(Complete_case[[col]])
  
  # Calculate proportions
  prop <- prop.table(tbl)
  
  # Prepare data frame
  df <- data.frame(Column = col,
                   Level = names(tbl),
                   Count = as.numeric(tbl),
                   Count_Percentage = paste0(as.numeric(tbl), " (", round(as.numeric(prop)*100, 1), ")"))
  
  
  # Append to results
  results <- rbind(results, df)
}

# Print results
print(results)


#### Comparing complete case dataset with the dataset with missing values that was excluded from the study_ Wave 4 disease outcomes###

Complete_case <- main_data[complete.cases(main_data[23:32]), ]

k1 <-tidy(prop.test(x = c(sum(Complete_case$Eye_disorders_W4,na.rm = TRUE),sum(Missing_case$Eye_disorders_W4,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Eye_disorders_W4)),length(na.omit(Missing_case$Eye_disorders_W4))
),alternative = "two.sided"))
k2 <-tidy(prop.test(x = c(sum(Complete_case$Circulatory_disorders_W4,na.rm = TRUE),sum(Missing_case$Circulatory_disorders_W4,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Circulatory_disorders_W4)),length(na.omit(Missing_case$Circulatory_disorders_W4))
),alternative = "two.sided"))
k3 <-tidy(prop.test(x = c(sum(Complete_case$Endocrine_nutritional_metabolic_W4,na.rm = TRUE),sum(Missing_case$Endocrine_nutritional_metabolic_W4,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Endocrine_nutritional_metabolic_W4)),length(na.omit(Missing_case$Endocrine_nutritional_metabolic_W4))
),alternative = "two.sided"))
k4 <-tidy(prop.test(x = c(sum(Complete_case$Musculoskeletal_connective_system_W4,na.rm = TRUE),sum(Missing_case$Musculoskeletal_connective_system_W4,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Musculoskeletal_connective_system_W4)),length(na.omit(Missing_case$Musculoskeletal_connective_system_W4))
),alternative = "two.sided"))
k5 <-tidy(prop.test(x = c(sum(Complete_case$Respiratory_W4,na.rm = TRUE),sum(Missing_case$Respiratory_W4,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Respiratory_W4)),length(na.omit(Missing_case$Respiratory_W4))
),alternative = "two.sided"))
k6 <-tidy(prop.test(x = c(sum(Complete_case$Neoplasms_W4,na.rm = TRUE),sum(Missing_case$Neoplasms_W4,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Neoplasms_W4)),length(na.omit(Missing_case$Neoplasms_W4))
),alternative = "two.sided"))

k8 <-tidy(prop.test(x = c(sum(Complete_case$M_Present_W4,na.rm = TRUE),sum(Missing_case$M_Present_W4,na.rm = TRUE)), n = c(length(na.omit(Complete_case$M_Present_W4)),length(na.omit(Missing_case$M_Present_W4))
),alternative = "two.sided"))
k9 <-tidy(prop.test(x = c(sum(Complete_case$CM_Present_W4,na.rm = TRUE),sum(Missing_case$CM_Present_W4,na.rm = TRUE)), n = c(length(na.omit(Complete_case$CM_Present_W4)),length(na.omit(Missing_case$CM_Present_W4))
),alternative = "two.sided"))



t_test_results <- bind_rows(k1, k2, k3, k4, k5, k6, k8, k9)
kable(head(t_test_results,n=10), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

table <- kable(t_test_results[, c(2:3,1, 5)])%>%
  kable_styling(latex_options = c("striped", "scale_down"))
#### Comparing complete case dataset with the dataset with missing values that was excluded from the study###

Complete_case <- main_data[complete.cases(main_data[23:32]), ]

t1 <-tidy(prop.test(x = c(sum(Complete_case$Eye_disorders,na.rm = TRUE),sum(Missing_case$Eye_disorders,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Eye_disorders)),length(na.omit(Missing_case$Eye_disorders))
),alternative = "two.sided"))
t2 <-tidy(prop.test(x = c(sum(Complete_case$Circulatory_disorders,na.rm = TRUE),sum(Missing_case$Circulatory_disorders,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Circulatory_disorders)),length(na.omit(Missing_case$Circulatory_disorders))
),alternative = "two.sided"))
t3 <-tidy(prop.test(x = c(sum(Complete_case$Endocrine_nutritional_metabolic,na.rm = TRUE),sum(Missing_case$Endocrine_nutritional_metabolic,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Endocrine_nutritional_metabolic)),length(na.omit(Missing_case$Endocrine_nutritional_metabolic))
),alternative = "two.sided"))
t4 <-tidy(prop.test(x = c(sum(Complete_case$Musculoskeletal_connective_system,na.rm = TRUE),sum(Missing_case$Musculoskeletal_connective_system,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Musculoskeletal_connective_system)),length(na.omit(Missing_case$Musculoskeletal_connective_system))
),alternative = "two.sided"))
t5 <-tidy(prop.test(x = c(sum(Complete_case$Respiratory,na.rm = TRUE),sum(Missing_case$Respiratory,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Respiratory)),length(na.omit(Missing_case$Respiratory))
),alternative = "two.sided"))
t6 <-tidy(prop.test(x = c(sum(Complete_case$Neoplasms,na.rm = TRUE),sum(Missing_case$Neoplasms,na.rm = TRUE)), n = c(length(na.omit(Complete_case$Neoplasms)),length(na.omit(Missing_case$Neoplasms))
),alternative = "two.sided"))
t8 <-tidy(prop.test(x = c(sum(Complete_case$M_Present,na.rm = TRUE),sum(Missing_case$M_Present,na.rm = TRUE)), n = c(length(na.omit(Complete_case$M_Present)),length(na.omit(Missing_case$M_Present))
),alternative = "two.sided"))
t9 <-tidy(prop.test(x = c(sum(Complete_case$CM_Present,na.rm = TRUE),sum(Missing_case$CM_Present,na.rm = TRUE)), n = c(length(na.omit(Complete_case$CM_Present)),length(na.omit(Missing_case$CM_Present))
),alternative = "two.sided"))



t_test_results <- bind_rows(t1, t2, t3, t4, t5, t6, t8, t9)
kable(head(t_test_results,n=10), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

table <- kable(t_test_results[, c(2:3,1, 5)])%>%
  kable_styling(latex_options = c("striped", "scale_down"))



##### Checking for multicollinearity #########

#### correlation matrix for strictly the covariates, not outcomes
correlation_matrix <- cor(Complete_case[, 23:32], method = "pearson")
kable(head(correlation_matrix), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

corrplot(correlation_matrix,
  type = "upper", order = "hclust",
  tl.col = "black", tl.srt = 45
)

col <- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = correlation_matrix, col = col, symm = TRUE)

res <- rcorr(as.matrix(correlation_matrix))

corrplot(res$r, type = "upper", tl.col = "black", order = "hclust", p.mat = res$P, sig.level = 0.01, insig = "blank")

corrplot



###### Bar charts for probabilities of multimorbidity and complex multimorbidity #####
