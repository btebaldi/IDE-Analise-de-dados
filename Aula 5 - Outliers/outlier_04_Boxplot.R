library(ggplot2)

df <- ggplot2::mpg %>% dplyr::select(categoria = class, valor = hwy)

# Boxplot simples com identificacao de outliers
g1 <- ggplot(df, aes(x = categoria, y = valor)) +
  geom_boxplot(outlier.colour = "red",
               outlier.shape  = 16,
               outlier.size   = 2) +
  labs(title    = "Distribuicao por categoria",
       subtitle = "Pontos vermelhos: outliers (Tukey k=1.5)") +
  theme_minimal()

# Boxplot com jitter para ver todos os pontos (n pequeno)
g2 <- ggplot(df, aes(x = categoria, y = valor)) +
  geom_boxplot(alpha = 0.4) +
  geom_jitter(width = 0.2, alpha = 0.5,
              color = "steelblue", size = 1) +
  theme_minimal()


cowplot::plot_grid(g1, g2)
