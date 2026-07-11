rm(list = ls())

library(dplyr)
library(readr)
library(ggplot2)

if(FALSE){
  faithful %>%
    readr::write_csv(file = "faithful.csv")
}



# Data Load ---------------------------------------------------------------

tbl <- readr::read_csv("faithful.csv")

tbl



# Questao 1 ---------------------------------------------------------------

ggplot(tbl) +
  geom_histogram(mapping = aes(x = eruptions),
                 fill = "blue", colour = "black", alpha = 0.4,
                 bins = 5)


ggplot(tbl) +
  geom_histogram(mapping = aes(x = eruptions),
                 fill = "blue", colour = "black", alpha = 0.4,
                 bins = 15)


ggplot(tbl) +
  geom_histogram(mapping = aes(x = eruptions),
                 fill = "blue", colour = "black", alpha = 0.4,
                 bins = 40)



# Questao 2 ---------------------------------------------------------------



ggplot(tbl) +
  geom_density(mapping = aes(x = eruptions),
                 fill = "blue", colour = "black", alpha = 0.4,
                 bw = "nrd0") + 
  labs(title = "KDE Gausiana", 
       subtitle = "Silverman")

ggplot(tbl) +
  geom_density(mapping = aes(x = eruptions),
                 fill = "blue", colour = "black", alpha = 0.4,
                 bw = "SJ") + 
  labs(title = "KDE Gausiana", 
       subtitle = "Sheather-Jones")

ggplot(tbl) +
  geom_density(mapping = aes(x = eruptions),
                 fill = "blue", colour = "black", alpha = 0.4,
                 bw = 0.01) + 
  labs(title = "KDE Gausiana", 
       subtitle = "h=0.01")





# Questao 3 ---------------------------------------------------------------


ggplot(tbl) +
  geom_density(mapping = aes(x = eruptions),
               fill = "blue", colour = "black", alpha = 0.4,
               bw = "nrd0", kernel = "gaussian") + 
  labs(title = "KDE Gausiana", 
       subtitle = "Silverman")


ggplot(tbl) +
  geom_density(mapping = aes(x = eruptions),
               fill = "blue", colour = "black", alpha = 0.4,
               bw = "nrd0", kernel = "rectangular") + 
  labs(title = "KDE rectangular", 
       subtitle = "Silverman")


ggplot(tbl) +
  geom_density(mapping = aes(x = eruptions),
               fill = "blue", colour = "black", alpha = 0.4,
               bw = "nrd0", kernel = "triangular") + 
  labs(title = "KDE triangular", 
       subtitle = "Silverman")


# Questao 4 ---------------------------------------------------------------


dens <- density(tbl$eruptions)

dens_func <- approxfun(dens$x, dens$y)

prob_interval <- integrate(dens_func, lower = 2, upper = 4)

print(prob_interval$value)





