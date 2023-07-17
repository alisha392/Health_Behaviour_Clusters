# This file is created after the scripts in 05_Health_Outcomes have been run

main_data <- read_csv(here("Data", "Processed_Data", "R_Data", "Health_Outcomes", "Outcomes_Demographics_Behaviours_4to9.csv"))
head(main_data)


## Dummy coding Sex with Male as baseline, so Female =1
main_data <- main_data %>% 
  dplyr::mutate(Female = ifelse(main_data$Sex<0, -99, ifelse(main_data$Sex == 2, 1, 0))) %>% 
  dplyr::select(-2) 

main_data <- main_data[, c(1,11:31,54,2:10,43:53,32:42)]
head(main_data)

main_data[main_data == -99] <- NA
Complete_case_data <- main_data[complete.cases(main_data[23:32]), ]
Complete_case_data[is.na(Complete_case_data)]<- -99


main_data <- Complete_case_data 

# List of column names
columns <- c("W4_Smoke", "W5_Smoke", "W6_Smoke", "W7_Smoke", "W8_Smoke", 
             "W4_Drink", "W5_Drink", "W6_Drink", "W7_Drink", "W8_Drink", 
             "W4_PA", "W5_PA", "W6_PA", "W7_PA", "W8_PA", 
             "W4_FV", "W5_FV", "W6_FV", "W7_FV", "W8_FV")

# Create an empty data frame to store the results
results <- data.frame(Column = character(), Level = character(), Count = numeric(), Percentage = numeric(), stringsAsFactors = FALSE)

# Loop through each column
for (col in columns) {
  # Calculate table
  tbl <- table(main_data[[col]])
  
  # Calculate proportions
  prop <- prop.table(tbl)
  
  # Prepare data frame
  df <- data.frame(Column = col,
                   Level = names(tbl),
                   Count = as.numeric(tbl),
                   Percentage = round(as.numeric(prop)*100,1),
                   Count_Percentage = paste0(as.numeric(tbl), " (", round(as.numeric(prop)*100, 1), ")"))

  
  # Append to results
  results <- rbind(results, df)
}

# Print results
print(results)

