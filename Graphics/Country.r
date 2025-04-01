library(rvest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Функция для загрузки данных с сайта Numbeo
get_numbeo_data <- function(year) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  webpage <- read_html(url)
  tables <- html_nodes(webpage, "table")

  if (length(tables) < 2) {
    stop("Не удалось найти нужную таблицу на странице: ", url)
  }

  data <- html_table(tables[2], fill = TRUE)[[1]]
  data$Year <- year

  if ("Rank" %in% colnames(data)) {
    data <- data[, !(names(data) %in% c("Rank"))]
  }

  return(data)
}


years <- 2014:2021
numbeo_data <- do.call(rbind, lapply(years, get_numbeo_data))

if ("Rank" %in% colnames(numbeo_data)) {
  numbeo_data <- numbeo_data[, !(names(numbeo_data) %in% c("Rank"))]
}

# Фильтруем данные для выбранных стран
countries <- c("Switzerland", "Sweden", "Indonesia", "Lebanon", "United Arab Emirates")
filtered_data <- numbeo_data[numbeo_data$Country %in% countries, ]

# Указываем путь к папке для сохранения файлов
output_dir <- "D:/HTML/for_the_university/big_data"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if ("Rank" %in% colnames(filtered_data)) {
  filtered_data <- filtered_data[, !(names(filtered_data) %in% c("Rank"))]
}

# Сохраняем данные для каждой страны в отдельный CSV-файл
for (country in countries) {
  country_data <- filtered_data[filtered_data$Country == country, ]

  if (nrow(country_data) > 0) {
    file_path <- file.path(output_dir, paste0(country, ".csv"))
    write.csv(country_data, file_path, row.names = FALSE)
  } else {
    message("Нет данных для страны: ", country)
  }
}

print("Файлы успешно сохранены в папку D:/HTML/for_the_university/big_data")

# Преобразуем данные в длинный формат
long_data <- gather(filtered_data, key = "Index", value = "Value", -Country, -Year)

if ("Rank" %in% unique(long_data$Index)) {
  long_data <- long_data[long_data$Index != "Rank", ]
}

# Создадим папку для графиков, если её нет
plot_dir <- file.path(output_dir, "plots")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# Построение и сохранение отдельных графиков
unique_indexes <- unique(long_data$Index)

for (index in unique_indexes) {
  plot_data <- long_data[long_data$Index == index, ]

  if (nrow(plot_data) > 0) {
    p <- ggplot(plot_data, aes(x = Year, y = Value, color = Country, group = Country)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Изменение", index, "по годам"), x = "Год", y = "Значение индекса")

    print(p)  # Вывод графика в окно

    # Сохранение графика в файл
    ggsave(filename = file.path(plot_dir, paste0(index, ".png")), plot = p, width = 8, height = 5)
  }
}

print("Графики выведены и сохранены в D:/HTML/for_the_university/big_data/plots")


# URL страницы с музеями
museum_url <- "https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/"

webpage <- read_html(museum_url, encoding = "UTF-8")

titles <- html_text(html_nodes(webpage, ".post-list-item-title a"))
links <- html_attr(html_nodes(webpage, ".post-list-item-title a"), "href")
addresses <- html_text(html_nodes(webpage, ".post-list-item-info .addressItem"))

titles <- str_trim(titles)
addresses <- str_trim(addresses)


if (length(addresses) > length(titles)) {
  addresses <- addresses[1:length(titles)]
}

if (length(links) < length(titles)) {
  links <- c(links, rep(NA, length(titles) - length(links)))
}

# Собираем данные в data.frame
museum_data <- data.frame(
  Name = titles,
  Address = addresses,
  Link = links
)

# Выводим данные
View(museum_data)
