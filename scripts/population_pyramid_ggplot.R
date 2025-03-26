
library(ggplot2)

# 年齢ラベル
ggplot_age_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
               "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
               "70-74", "75-79", "80-84", "85-89", "90-94", "95+")

# 男性人口データ例
male_population <- c(157, 230, 269, 282, 262, 293, 258, 334, 355, 454, 477, 425, 556, 527, 560, 494, 310, 198, 112, 29)

# 女性人口データ例
female_population <- c(139, 203, 227, 237, 230, 268, 245, 328, 343, 443, 463, 413, 546, 522, 577, 502, 321, 236, 127, 30)

# データフレーム作成
data <- data.frame(
  age = factor(rep(ggplot_age_labels, 2), levels = rev(ggplot_age_labels)),
  gender = rep(c("Male", "Female"), each = length(ggplot_age_labels)),
  population = c(-male_population, female_population)
)

# ggplot描画
ggplot(data, aes(x = age, y = population, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c("Male" = "#00A0CD", "Female" = "#EE86A7")) +
  labs(title = "Mikkabi Population Pyramid (ggplot version)",
       x = "Age Group", y = "Population") +
  theme_minimal()
