rm(list = ls())

library(ggplot2)

K_gauss ggplot2K_gauss <- function(x, h = 1, mu=0){
  
  1/(h * sqrt(2 * pi)) * exp(-1/2* ((x-mu)^2)/(h^2) )
}


K_gauss(0)


K_uni <- function(x, h = 1, mu=0){
  return((abs(x-mu)<=h) * 1/(2*h))
}

K_tri <- function(x, h = 1, mu=0){
 ret <- (1 - abs(x-mu)/h) * 1/h *(abs(x-mu)<=h)
   return(ret)
}

K_cos <- function(x, h = 1, mu=0){
  return(pi/(4*h) * cos((x-mu)/h* pi/2) *(abs(x-mu)<=h))
}


ggplot(data.frame(x = seq(-4, 4, 0.1)), aes(x)) +
  geom_function(fun = K_gauss, mapping = aes(colour="Gauss (h=1)")) +
  geom_function(fun = K_uni, mapping = aes(colour = "Uniforme (h=1)"), args = list("h" = 1, "mu"=0)) +
  geom_function(fun = K_cos, mapping = aes(colour = "Coseno (h=2)"), args = list("h" = 2, "mu"=0)) +
  geom_function(fun = K_tri, mapping = aes(colour = "Triangular (h=2)"), args = list("h" = 2, "mu"=0)) +
  scale_colour_manual(
    name = NULL,
    values = c("Gauss (h=1)" = "steelblue",
               "Coseno (h=2)" = "darkred",
               "Triangular (h=2)" = "darkgreen",
               "Uniforme (h=1)" = "black"),
  ) +
  theme_bw() + 
  theme(legend.position = "bottom") 

