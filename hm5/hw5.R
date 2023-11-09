library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
setwd("E:/pencil")
df <- read.csv("ДЗ3_superstore_data.csv", stringsAsFactors = FALSE)
df_filtered <- df %>%
  filter(Income > 30000) %>%
  select(Id, Year_Birth, Education, Marital_Status, Income, Response) %>%
  mutate(
    Age = 2023 - Year_Birth,
    Rich_flag = as.numeric(Income > 80000) 
  )
education_income <- df_filtered %>%
  group_by(Education) %>%
  summarize(Average_Income = mean(Income, na.rm = TRUE), .groups = "drop")
df_final <- left_join(df_filtered, education_income, by = "Education")

# Обработка строк, содержащих "NaN"
df_final <- df_final %>%
  mutate_all(~na_if(., "NaN")) %>% # Преобразование "NaN" в NA
  replace_na(list(
    Age = 0,
    Income = 0,
    Average_Income = 0
  ))
# Объединение столбцов Education и Marital_Status
df_final <- df_final %>%
  unite("Education_Marital", Education, Marital_Status, sep = "_")
# Построение диаграммы «ящик с усами»
boxplot <- ggplot(df_final, aes(x = as.factor(Response), y = Age)) +
  geom_boxplot() +
  labs(x = "Response", y = "Age") +
  theme_minimal()
print(boxplot)
rows_with_9 <- df_final %>%
  filter(str_detect(Age, "9"))
print(rows_with_9)
