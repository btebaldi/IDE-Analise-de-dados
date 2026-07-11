library(GGally)

data(iris)
ggpairs(iris,
        columns = 1:4,
        aes(color = Species, alpha = 0.6)) +
  theme_minimal()
