library(ggplot2)
library(patchwork)

data(faithful)

# Scatter simples: dificil enxergar densidade
p1 <- ggplot(faithful, aes(eruptions, waiting)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  labs(title = "Scatter plot") +
  theme_minimal()

# KDE bivariada: clusters evidentes
p2 <- ggplot(faithful, aes(eruptions, waiting)) +
  geom_density_2d_filled(alpha = 0.85, show.legend = FALSE) +
  geom_point(size = 0.5, color = "white", alpha = 0.3) +
  labs(title = "KDE bivariada") +
  theme_minimal()

p1 + p2
