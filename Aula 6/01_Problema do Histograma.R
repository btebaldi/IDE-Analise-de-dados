# Setup -------------------------------------------------------------------
rm(list = ls())

library(ggplot2)
library(cowplot)


# hist(diamonds$carat)
# density(diamonds$carat)

mBins <- c(10, 20, 30, 50, 100, 150)

for(i in seq_along(mBins)){
  
  cBin <- mBins[i]
  
  assign(x = sprintf("g%d", i), value = diamonds %>% 
           ggplot() + 
           geom_histogram(aes(x=carat), bins = cBin, fill = "blue", colour = "black", alpha =0.5) +
           theme_bw() + 
           labs(title = NULL, subtitle = sprintf("%d bins", cBin),
                y="Quantidade", x = "carat"))
  
}


cowplot::plot_grid(g1, g2, g3, g4, g5, g6)
cowplot::plot_grid(g1, g3, g4, g6)


"nrd0", "nrd", "ucv", "bcv", "SJ"

diamonds %>% 
  ggplot() + 
  geom_density(aes(x=carat), fill = "blue", alpha =0.3, bw = "ucv") +
  theme_bw() + 
  labs(title = NULL, subtitle = NULL,
       y="Quantidade", x = "carat")


x <- xx <- faithful$eruptions
x[i.out <- sample(length(x), 10)] <- NA

doR <- density(x, bw = 0.15, na.rm = TRUE)
plot(doR)
lines(doR, col = "blue")
points(xx[i.out], rep(0.01, 10))

# The Old Faithful geyser data
d <- density(faithful$eruptions, bw = "sj")
d
plot(d)

plot(d, type = "n")
polygon(d, col = "wheat")


fit <- density(xx)
N <- 1e6
x.new <- rnorm(N, sample(xx, size = N, replace = TRUE), fit$bw)
plot(fit)
lines(density(x.new), col = "blue")
