# ---------------------------
# 1. Загрузка библиотек и данных
# ---------------------------
library(ggplot2)
library(cluster)
library(factoextra)
library(scatterplot3d)
library(dplyr)
library(psych)
library(ggpubr)
library(tidyr)
library(ConsensusClusterPlus)

data <- read.csv2("D:/HTML/for_the_university/big_data/zzz.csv", sep = ";", dec = ",", na.strings = "-9 999")

convert_numbers <- function(x) {
  if (is.character(x)) {
    x <- gsub(" ", "", x)
    as.numeric(x)
  } else x
}
data[ , -1] <- lapply(data[ , -1], convert_numbers)

data <- na.omit(data)

countries <- data$страна
regions <- data$регион

cat("Фрагмент исходного датасета:\n")
print(head(data, 10))

# ---------------------------
# 2. Дескриптивный анализ данных
# ---------------------------
data_num <- data[ , !(names(data) %in% c("страна", "регион"))]
summary_stats <- describe(data_num)
cat("\nДескриптивная статистика:\n")
print(summary_stats)

num_cols <- colnames(data_num)
par(mfrow = c(2, ceiling(length(num_cols)/2)))
for (col in num_cols) {
  hist(data_num[[col]], main = paste("Гистограмма:", col), xlab = col, col = "skyblue", border = "white")
}
par(mfrow = c(1,1))

# ---------------------------
# 3. Оценка оптимального числа кластеров
# ---------------------------
scaled_data <- scale(data_num)
set.seed(123)

# Метод локтя
fviz_nbclust(scaled_data, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(title = "Метод локтя")

# Метод силуэта
fviz_nbclust(scaled_data, kmeans, method = "silhouette") +
  labs(title = "Метод силуэта")

# Статистика разрыва
gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 50, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) + labs(title = "Статистика разрыва")

# ---------------------------
# 4. Иерархическая кластеризация
# ---------------------------
dist_matrix <- dist(scaled_data)
hc <- hclust(dist_matrix, method = "ward.D2")
cat("\nДендрограмма для иерархической кластеризации:\n")
plot(hc, main = "Дендрограмма", labels = countries, cex = 0.6)
rect.hclust(hc, k = 3, border = 2:4)
hc_clusters <- cutree(hc, k = 3)

# ---------------------------
# 5. Столбчатые диаграммы и боксплоты
# ---------------------------
data$cluster <- as.factor(hc_clusters)
means <- aggregate(data_num, by = list(Кластер = data$cluster), FUN = mean)
means_long <- pivot_longer(means, cols = -Кластер, names_to = "Показатель", values_to = "Значение")

# Столбчатая диаграмма
ggplot(means_long, aes(x = Показатель, y = Значение, fill = Кластер)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Средние значения по кластерам", x = "", y = "Значение")

# Боксплоты
data_long <- pivot_longer(data, cols = all_of(num_cols), names_to = "Показатель", values_to = "Значение")
ggplot(data_long, aes(x = cluster, y = Значение, fill = cluster)) +
  geom_boxplot() +
  facet_wrap(~Показатель, scales = "free") +
  theme_minimal() +
  labs(title = "Боксплоты по кластерам", x = "Кластер", y = "Значение")

# ---------------------------
# 6. K-means кластеризация
# ---------------------------
km <- kmeans(scaled_data, centers = 3, nstart = 50)
data$kmeans <- as.factor(km$cluster)


fviz_cluster(km,
             data = scaled_data,
             labelsize = 8,
             geom = "point",
             ellipse.type = "norm",
             palette = "Set2",
             ggtheme = theme_minimal()) +
  labs(title = "Кластеры по PCA (без подписей)") +
  theme(legend.position = "top")

# С подписями
cluster_plot <- fviz_cluster(km,
                             data = scaled_data,
                             geom = "point",
                             ellipse.type = "norm",
                             palette = "Dark2",
                             ggtheme = theme_minimal()) +
  labs(title = "Кластеры по PCA (с названиями стран)") +
  theme(legend.position = "top")

# Добавляем подписи стран вручную
cluster_plot + geom_text(aes(x = scaled_data[, 1], y = scaled_data[, 2], label = countries), 
                         size = 3, vjust = -0.5, hjust = 0.5)

# ---------------------------
# 7. Scatterplot
# ---------------------------
pairs(data_num, main = "Scatterplot переменных", pch = 21, bg = km$cluster)

# ---------------------------
# 8. 3D Scatterplot
# ---------------------------
scatterplot3d(data_num[,1:3],
              color = km$cluster,
              pch = 19,
              angle = 55,
              main = "3D Scatterplot кластеров",
              xlab = colnames(data_num)[1],
              ylab = colnames(data_num)[2],
              zlab = colnames(data_num)[3])

# ---------------------------
# 9. Алгоритм консенсуса
# ---------------------------
cat("\nПрименение алгоритма консенсуса для кластеризации...\n")

# Преобразуем data_num в матрицу
data_matrix <- as.matrix(data_num)

# Проведение консенсусной кластеризации (для разных значений K)
consensus_res <- ConsensusClusterPlus(
  data_matrix, 
  maxK = 3,  # уменьшено до 3
  reps = 50, 
  pItem = 0.8, 
  pFeature = 1, 
  clusterAlg = "km", 
  distance = "euclidean", 
  seed = 123
)
