install.packages("plotly")
install.packages("leaflet")
install.packages("DT")
install.packages("rpivotTable")

# Подключите библиотеки
library(plotly)
library(leaflet)
library(DT)
library(rpivotTable)

# Укажите путь к вашему датасету и считайте его
setwd("E:/pencil")
df <- read.csv("ДЗ3_superstore_data.csv", stringsAsFactors = FALSE)

# Отфильтруйте данные и создайте столбец "Age"
df_filtered <- filter(df, Income > 30000)
df_selected <- select(df_filtered, Id, Year_Birth, Education, Marital_Status, Income, Response)
df_mutated <- mutate(df_selected, Age = 2023 - Year_Birth, Rich_flag = Income > 80000)

# Вычислите средний доход по образованию
education_income <- df_mutated %>%
  group_by(Education) %>%
  summarize(Average_Income = mean(Income, na.rm = TRUE), .groups = "drop")

# Объедините исходный датасет с данными о среднем доходе по образованию
df_final <- left_join(df_mutated, education_income, by = "Education")

# Постройте точечную диаграмму Age vs Income с помощью Plotly
scatter_plot <- plot_ly(data = df_final, x = ~Age, y = ~Income, type = "scatter", mode = "markers", 
                        marker = list(size = 8, opacity = 0.6))

scatter_plot <- scatter_plot %>% layout(title = "Scatter Plot of Age vs Income",
                                         xaxis = list(title = "Age"),
                                         yaxis = list(title = "Income"))

# Постройте тепловую карту Income_avg_edu с помощью Plotly
heatmap_data <- df_final %>%
  group_by(Education, Marital_Status) %>%
  summarize(Income_avg_edu = mean(Income, na.rm = TRUE), .groups = "drop")

heatmap_plot <- plot_ly(data = heatmap_data, x = ~Education, y = ~Marital_Status, z = ~Income_avg_edu,
                        type = "heatmap", colorscale = "Viridis")

heatmap_plot <- heatmap_plot %>% layout(title = "Heatmap of Income by Education and Marital Status",
                                         xaxis = list(title = "Education"),
                                         yaxis = list(title = "Marital_Status"))

# Постройте карту-дерево с двумя векторами констант с помощью Plotly
tree_map_data <- data.frame(Label = c("Constant1", "Constant2"), Value = c(10, 20))

tree_map_plot <- plot_ly(data = tree_map_data, labels = ~Label, values = ~Value,
                         type = "treemap")

tree_map_plot <- tree_map_plot %>% layout(title = "Tree Map of Constants")

# Постройте географическую карту с координатами Николаевского кладбища в Красноярске с помощью leaflet
map <- leaflet() %>%
  setView(lng = 92.881177, lat = 56.008932, zoom = 15) %>%
  addTiles() %>%
  addMarkers(lng = 92.881177, lat = 56.008932, popup = "Николаевское кладбище в Красноярске")
map

# Выведите всю информацию из датасета в плоскую таблицу с помощью DT
datatable(df_final)

# Выведите информацию из датасета в сводную таблицу с помощью rpivotTable
rpivotTable(df_final, rows = "Education", cols = "Marital_Status")

# Отобразите графики и таблицы
scatter_plot
heatmap_plot
tree_map_plot
map
