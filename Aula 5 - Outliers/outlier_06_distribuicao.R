library(ggplot2)
library(dplyr)

df <- ggplot2::mpg %>% dplyr::select(categoria = cty   , valor = hwy)

Quantiles <- quantile(df$valor, probs = c(0.25, 0.5, 0.75))
IQR <- Quantiles[3] - Quantiles[1]

cerca_inf <- Quantiles[1] - 1.5*IQR
cerca_sup <- Quantiles[3] + 1.5*IQR


# Scatter plot com outliers destacados
df <- df %>%
  mutate(is_outlier = valor < cerca_inf | valor > cerca_sup)

ggplot(df, aes(x = valor, y = categoria,
               color = is_outlier)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "steelblue",
                                "TRUE"  = "red"),
                     labels = c("Normal", "Outlier")) +
  # scale_size_manual(values  = c("FALSE" = 1.5,
  #                               "TRUE"  = 3)) +
  labs(title = "Identificacao visual de outliers",
       color = NULL, size = NULL) +
  theme_minimal()
