setwd("E:/pencil")
# Загрузка данных из CSV-файла
df <- read.csv("ДЗ3_superstore_data.csv", stringsAsFactors = FALSE)
# Подготовка и очистка данных
library(dplyr)
df_filtered <- df %>%
  filter(Income > 30000) %>%
  select(Id, Year_Birth, Education, Marital_Status, Income, Response) %>%
  mutate(Age = 2023 - Year_Birth, Rich_flag = Income > 80000)
education_income <- df_filtered %>%
  group_by(Education) %>%
  summarize(Average_Income = mean(Income, na.rm = TRUE), .groups = "drop")
df_final <- left_join(df_filtered, education_income, by = "Education")
print(head(df_final))
# Загрузка библиотеки для визуализации
library(ggplot2)
library(pivottabler)
# Создание сводной таблицы
pt <- PivotTable$new()
pt$addData(df_final)
pt$addColumnDataGroups("Marital_Status")
pt$addRowDataGroups("Education")
pt$defineCalculation(calculationName = "Total count", summariseExpression = "n()")
pt$evaluatePivot()
print(pt)

# Столбчатая диаграмма
ggplot(df_final, aes(x = Education, fill = as.factor(Rich_flag))) +
  geom_bar() +
  labs(fill = "Rich Flag", x = "Education", y = "Count") +
  theme_minimal()

# Подготовка данных для линейной диаграммы
df_by_year <- df_final %>%
  group_by(Year_Birth) %>%
  summarise(Count = n())

# Линейная диаграмма
ggplot(df_by_year, aes(x = Year_Birth, y = Count)) +
  geom_line() +
  labs(x = "Year of Birth", y = "Number of Observations") +
  theme_minimal()
