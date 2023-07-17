install.packages('tableone')
library(tableone)
install.packages("xlsx")
library(xlsx)

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


j <- main_data[23:32] %>% 
  summarise_all(~sum(is.na(.)))
view(j)

Complete_case <- main_data[complete.cases(main_data[23:32]), ]
Missing_case <- main_data[!complete.cases(main_data[23:32]), ]


Complete_case_descriptives <- sapply(Complete_case[, 23:54], function(x) {
  c(
    "Mean" = mean(x, na.rm = TRUE),
    "Stand dev" = sd(x, na.rm = TRUE),
    "count"=sum(x,na.rm = TRUE)
  )
})
head(Complete_case_descriptives)

Missing_case_descriptives <- sapply(Missing_case[, 23:54], function(x) {
  c(
    "Mean" = mean(x, na.rm = TRUE),
    "Stand dev" = sd(x, na.rm = TRUE),
    "count"=sum(x,na.rm = TRUE)
  )
})
head(Missing_case_descriptives)



table1 <-kable(head(Complete_case_descriptives), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

table2 <- kable(head(Missing_case_descriptives), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))



#### Comparing complete case dataset with the dataset with missing values that was excluded from the study###

t1 <- tidy(prop.test(x = c(sum(Complete_case$Female), sum(Missing_case$Female)), n = c(nrow(Complete_case),nrow(Missing_case)),alternative = "two.sided"))
t2 <- tidy(t.test(Complete_case$Age, Missing_case$Age, paired = FALSE))
t3 <- tidy(prop.test(x = c(sum(Complete_case$Paternal_Occ_Intermediate), sum(Missing_case$Paternal_Occ_Intermediate,na.rm = TRUE)), n = c(nrow(Complete_case),length(na.omit(Missing_case$Paternal_Occ_Intermediate))
),alternative = "two.sided"))
t4 <- tidy(prop.test(x = c(sum(Complete_case$Paternal_Occ_High), sum(Missing_case$Paternal_Occ_High,na.rm = TRUE)), n = c(nrow(Complete_case),length(na.omit(Missing_case$Paternal_Occ_High))
),alternative = "two.sided"))
t5 <- tidy(prop.test(x = c(sum(Complete_case$Self_Occ_Intermediate), sum(Missing_case$Self_Occ_Intermediate,na.rm = TRUE)), n = c(nrow(Complete_case),length(na.omit(Missing_case$Self_Occ_Intermediate))
),alternative = "two.sided"))
t6 <- tidy(prop.test(x = c(sum(Complete_case$Self_Occ_High), sum(Missing_case$Self_Occ_High,na.rm = TRUE)), n = c(nrow(Complete_case),length(na.omit(Missing_case$Self_Occ_High))
),alternative = "two.sided"))
t7 <- tidy(prop.test(x = c(sum(Complete_case$Education_Intermediate), sum(Missing_case$Education_Intermediate,na.rm = TRUE)), n = c(nrow(Complete_case),length(na.omit(Missing_case$Education_Intermediate))
),alternative = "two.sided"))
t8 <- tidy(prop.test(x = c(sum(Complete_case$Education_Degree), sum(Missing_case$Education_Degree,na.rm = TRUE)), n = c(nrow(Complete_case),length(na.omit(Missing_case$Education_Degree))
),alternative = "two.sided"))
t9 <- tidy(prop.test(x = c(sum(Complete_case$Wealth_Second_Tertile), sum(Missing_case$Wealth_Second_Tertile,na.rm = TRUE)), n = c(nrow(Complete_case),length(na.omit(Missing_case$Wealth_Second_Tertile))
),alternative = "two.sided"))
t10 <- tidy(prop.test(x = c(sum(Complete_case$Wealth_Third_Tertile), sum(Missing_case$Wealth_Third_Tertile,na.rm = TRUE)), n = c(nrow(Complete_case),length(na.omit(Missing_case$Wealth_Third_Tertile))
),alternative = "two.sided"))




t_test_results <- bind_rows(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
kable(head(t_test_results,n=19), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

Variables <- names(Complete_case[c(23:32,44:53)])

table <- kable(t_test_results[, c(1:4)])%>%
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



######### Descriptives for the complete case dataset #####
Complete_case_WithoutId <- Complete_case %>% 
  dplyr::select(-c(1,22,31:43))

varsToFactor <- dput(names(Complete_case_WithoutId[-c(22)]))
Complete_case_WithoutId[varsToFactor] <- lapply(Complete_case_WithoutId[varsToFactor], factor)
summary(Complete_case_WithoutId[varsToFactor])
vars <- dput(names(Complete_case_WithoutId))
tableOne <- CreateTableOne(vars = vars, data = Complete_case_WithoutId,includeNA =TRUE)
tab_csv <- print(tableOne,
                 printToggle = FALSE)

write.csv(tab_csv, file = here("Descriptives.csv"))


