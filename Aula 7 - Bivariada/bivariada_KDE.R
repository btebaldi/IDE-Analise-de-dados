library(ggplot2)
library(MASS)

data(faithful)  # geiser Old Faithful: erupcoes e tempo de espera

# Opcao 1: geom_density_2d_filled (contorno preenchido)
ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_density_2d_filled(alpha = 0.85) +
  geom_point(size = 0.8, color = "white", alpha = 0.4) +
  labs(title    = "KDE bivariada: Old Faithful",
       subtitle = "Regioes mais escuras = maior concentracao de observacoes",
       x = "Duracao da erupcao (min)",
       y = "Tempo de espera (min)") +
  theme_minimal()

# Opcao 2: geom_density_2d (apenas as linhas de contorno)
ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point(alpha = 0.3, color = "gray50") +
  geom_density_2d(color = "steelblue", linewidth = 0.7) +
  theme_minimal()

# Opcao 3: grade numerica via MASS::kde2d
dens2d <- kde2d(faithful$eruptions, faithful$waiting, n = 100)
image(dens2d, col = hcl.colors(20, "YlOrRd", rev = TRUE))
contour(dens2d, add = TRUE, col = "gray30")
