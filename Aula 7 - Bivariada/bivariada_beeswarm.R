library(ggbeeswarm)

# Strip plot simples (com jitter para evitar sobreposicao)
ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_jitter(width = 0.15, alpha = 0.6, color = "steelblue") +
  theme_minimal()

# Beeswarm: organiza os pontos sem sobreposicao
ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_beeswarm(alpha = 0.6, color = "steelblue", size = 2) +
  theme_minimal()

# Combinacao: violin + beeswarm (melhor dos dois mundos)
ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_violin(alpha = 0.3, fill = "gray80") +
  geom_beeswarm(alpha = 0.7, color = "steelblue") +
  theme_minimal()
