setwd("E:/pencil")
df <- read.csv("ДЗ3_superstore_data.csv", stringsAsFactors = FALSE)
head(df)
library(dplyr)
df_filtered <- filter(df, Income > 30000)
df_selected <- select(df_filtered, Id, Year_Birth, Education, Marital_Status, Income, Response)
df_mutated <- mutate(df_selected, Age = 2023 - Year_Birth, Rich_flag = Income > 80000)
education_income <- df_mutated %>%
  group_by(Education) %>%
  summarize(Average_Income = mean(Income, na.rm = TRUE), .groups = "drop")
df_final <- left_join(df_mutated, education_income, by = "Education")
print(df_final)
