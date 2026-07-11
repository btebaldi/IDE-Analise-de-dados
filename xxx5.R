library(ggplot2)
library(ggridges)

# Violin + boxplot interno
g1 <- ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_violin(alpha = 0.5) +
 # theme_minimal() +
  theme(legend.position = "none") +
  labs()

# Violin + boxplot interno
g2 <- ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_boxplot(width = 0.1) +
 # theme_minimal() +
  theme(legend.position = "none") +
  labs()

# Ridgeline (varios grupos ordenados)
# ggplot(iris, aes(y = Petal.Length, x = Species, fill = Species)) +
#   geom_density_ridges(alpha = 0.7) +
#   theme_minimal() +
#   labs()


g3 <- ggplot(iris, aes(x = Petal.Length, fill = Species)) +
  geom_density(alpha = 0.7) +
  theme(legend.position = "none") +
  labs()

library(cowplot)

cowplot::plot_grid(g1, g2 ,g3, nrow = 1)
