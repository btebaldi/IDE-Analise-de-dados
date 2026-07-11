# Para ilustrar seu efeito, utilizamos uma amostra aleatória simulada da
# distribuição normal padrão (representada pelos picos azuis no gráfico de
# dispersão no eixo horizontal).

rm(list = ls())

set.seed(420)
sample_1 <- rnorm(n = 100, mean = 0, sd = 1)
sample_2 <- rnorm(n = 100, mean = 5, sd = 2)


realFunction <- function(x){
  ret <- (dnorm(x, mean = 0, sd = 1) + dnorm(x, mean = 5, sd = 2))/2
  return(ret)
}

sample_full <- c(sample_1, sample_2)


g1 <- data.frame(x = seq(min(sample_full), max(sample_full), 0.01)) |> ggplot() +
  geom_function(fun = realFunction, mapping = aes(x = x)) +
  geom_density(mapping = aes(x = x), data = data.frame(x = sample_full),
               colour = "red") +
  theme_bw() +
  labs(title = "Gaussian")


g2 <- data.frame(x = seq(min(sample_full), max(sample_full), 0.01)) |> ggplot() +
  geom_function(fun = realFunction, mapping = aes(x = x)) +
  geom_density(mapping = aes(x = x), data = data.frame(x = sample_full),
               colour = "red", kernel = "rectangular") +
  theme_bw() +
  labs(title = "Rectangular")


g3 <- data.frame(x = seq(min(sample_full), max(sample_full), 0.01)) |> ggplot() +
  geom_function(fun = realFunction, mapping = aes(x = x)) +
  geom_density(mapping = aes(x = x), data = data.frame(x = sample_full),
               colour = "red", kernel = "triangular") +
  theme_bw() +
  labs(title = "Triangular")


g4 <- data.frame(x = seq(min(sample_full), max(sample_full), 0.01)) |> ggplot() +
  geom_function(fun = realFunction, mapping = aes(x = x)) +
  geom_density(mapping = aes(x = x), data = data.frame(x = sample_full),
               colour = "red", kernel = "cosine") +
  theme_bw() +
  labs(title = "Cosine")


cowplot::plot_grid(g1, g2, g3, g4)

# Escolha de h ------------------------------------------------------------

bw.nrd0(sample_full)


data.frame(x = seq(min(sample_full), max(sample_full), 0.01)) |> ggplot() +
  geom_function(fun = realFunction, mapping = aes(x = x, colour = "real distribution")) +
  geom_density(mapping = aes(x = x, fill= NULL, colour = "h=1"), data = data.frame(x = sample_full), linetype="dashed", bw = 1) +
  geom_density(mapping = aes(x = x, fill= NULL, colour = "h=2"), data = data.frame(x = sample_full), linetype="dashed", bw = 2) +
  geom_density(mapping = aes(x = x, fill= NULL, colour = "h=3"), data = data.frame(x = sample_full), linetype="dashed", bw = 3) +
  geom_density(mapping = aes(x = x, fill= NULL, colour = "h=4"), data = data.frame(x = sample_full), linetype="dashed", bw = 0.5) +
  geom_density(mapping = aes(x = x, fill= NULL, colour = "h=silver"), data = data.frame(x = sample_full), bw = bw.nrd0(sample_full)) +
  theme_bw() + 
  scale_colour_manual(
    name = NULL,
    values = c("h=1"="red",
               "h=2"="blue",
               "h=3"="darkgreen",
               "h=4" = "orange",
               "h=silver" = "darkred",
               "Gauss (h=1)" = "steelblue",
               "Coseno (h=2)" = "darkred",
               "Triangular (h=2)" = "darkgreen",
               "real distribution" = "black"),
  ) +
  theme(legend.position = "bottom") +
  labs()
