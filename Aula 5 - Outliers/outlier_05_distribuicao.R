library(ggplot2)

df <- ggplot2::mpg %>% dplyr::select(categoria = class, valor = hwy)

Quantiles <- quantile(df$valor, probs = c(0.25, 0.5, 0.75))
IQR <- Quantiles[3] - Quantiles[1]

cerca_inf <- Quantiles[1] - 1.5*IQR
cerca_sup <- Quantiles[3] + 1.5*IQR

# Histograma com destaque para caudas
ggplot(df, aes(x = valor)) +
  geom_histogram(bins = 50, fill = "steelblue",
                 color = "white", alpha = 0.8) +
  geom_vline(xintercept = c(cerca_inf, cerca_sup),
             color = "red", linetype = "dashed",
             linewidth = 0.8) +
  annotate("text", x = cerca_sup, y = Inf,
           label = "Cerca Tukey", vjust = 2,
           color = "red", size = 3) +
  labs(title = "Distribuicao com cercas de Tukey") +
  theme_minimal()

# Density plot: revela bimodalidade e caudas
ggplot(df, aes(x = valor)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  geom_rug(color = "gray40", alpha = 0.3) +
  theme_minimal()
