library(tidyverse)
library(viridis)
library(ggthemes)
library(gridExtra)
library(ggplot2)
library(dplyr)

setwd("D:/big_data/")
data <- read.csv("france.csv", header = TRUE)

colnames(data) <- c("Year", "City", "Category", "Gold", "Silver", "Bronze", "Total",
                    "Place4", "Place5", "Place6", "Place7", "Place8")

all_data <- filter(data, Category == "All")

places_long <- select(all_data, Year, Gold, Silver, Bronze, Place4:Place8)
places_long <- pivot_longer(places_long, cols = -Year, names_to = "Place", values_to = "Count")
places_long$Place <- factor(
  places_long$Place,
  levels = c("Gold", "Silver", "Bronze", "Place4", "Place5", "Place6", "Place7", "Place8"),
  labels = 1:8
)

#Столбчатая диаграмма по количеству мест с 1-8
ggplot(places_long, aes(x = factor(Year), y = Count, fill = Place)) +
  geom_col(position = "stack", color = "black", size = 0.3) +
  scale_fill_viridis_d(option = "inferno", begin = 0.1, end = 0.9) +
  labs(
    title = "Распределение мест 1-8 на Олимпиадах",
    x = "Год Олимпиады",
    y = "Количество спортсменов",
    fill = "Место"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top")

#Круговая диаграмма по количеству первых мест
ggplot(gold_sum, aes(x = "", y = Gold, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  labs(title = "Доля золотых медалей по Олимпиадам") +
  theme_void(base_size = 15) +
  theme(legend.position = "top")

#Функциональный график тенденции изменения количества призовых мест отдельно по мужчинам и женщинам за последние 30 лет.
gender_data <- filter(data, (Category == "Men" | Category == "Women") & Year >= 1994)

prize_trend <- summarise(
  group_by(gender_data, Year, Category),
  Prize = sum(Gold + Silver + Bronze),
  .groups = "drop"
)

ggplot(prize_trend, aes(x = Year, y = Prize, color = Category)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Men" = "#3498db", "Women" = "#e74c3c")) +
  labs(title = "Динамика призовых мест (мужчины vs женщины)") +
  theme_minimal(base_size = 16)


# Создание датафрейма для сравнения медалей по странам за последние 6 Олимпиад
data <- data.frame(
  Страна = c("США", "Великобритания", "Китай", "Россия", "Германия", "Япония", "Франция"),
  `Последняя Олимпиада` = c(128, 88, 128, 98, 68, 148, 168),
  `Предыдущая Олимпиада` = c(197, 137, 117, 127, 127, 77, 167),
  `Третья с конца` = c(146, 146, 136, 66, 146, 66, 106),
  `Четвертая с конца` = c(325, 175, 205, 165, 185, 135, 165),
  `Пятая с конца` = c(204, 164, 254, 114, 134, 84, 114),
  `Шестая с конца` = c(121, 67, 70, 56, 42, 40, 42)
)

long_data <- tidyr::pivot_longer(data, cols = -Страна, names_to = "Олимпиада", values_to = "Медали")

ggplot(long_data, aes(x = Олимпиада, y = Медали, group = Страна, color = Страна)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Сравнение достижений 7 стран за последние 6 Олимпиад",
       x = "Олимпиады (по убыванию времени)",
       y = "Количество медалей",
       color = "Страна") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Построение финального графика для динамики призовых мест по полу
ggplot(gender_summary, aes(x = Year, y = Prize, color = Category)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Men" = "#3498db", "Women" = "#e74c3c")) +
  labs(title = "Динамика призовых мест по полу") +
  theme_minimal(base_size = 16)
