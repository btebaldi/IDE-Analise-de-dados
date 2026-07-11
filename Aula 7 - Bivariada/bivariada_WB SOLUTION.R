rm(list = ls())
library(readr)
library(ggplot2)

#tbl <- gapminder::gapminder
# tbl %>% 
#   write_csv(file = "gapminder.csv")


tbl <- readr::read_csv("gapminder.csv")


# \part \textbf{Scatter e correlação:} Construa o scatter plot de
# (\texttt{gdpPercap}, \texttt{lifeExp}) para 2007. Calcule $r$ de Pearson e de
# Spearman. Eles diferem? Por quê?

ggplot(tbl) + 
  geom_point(mapping = aes(x= gdpPercap, y= lifeExp))

# \part \textbf{Transformação:} Aplique $\log(\texttt{gdpPercap})$ e refaça o
# scatter. A relação fica mais linear?
ggplot(tbl) + 
  geom_point(mapping = aes(x= log(gdpPercap), y= lifeExp))


# \part \textbf{Violin por grupo:} Produza um violin plot de \texttt{lifeExp}
# por \texttt{continent}. Qual continente apresenta a distribuição mais
# assimétrica ou bimodal?

ggplot(tbl) + 
  geom_violin(mapping = aes(x= continent, y= lifeExp))


# \part \textbf{KDE chart:} Construa uma distribuicao (KDE) de \texttt{log(gdpPercap)}
# no eixo x, \texttt{lifeExp} no eixo y, colorinfo por \texttt{continent}.

ggplot(tbl) + 
  geom_density_2d(mapping = aes(x= log(gdpPercap), y= lifeExp, group = continent,
                                colour = continent), bins =70) +
  facet_wrap(~continent) +
#  geom_point(mapping = aes(x= log(gdpPercap), y= lifeExp, group = continent, colour = continent, alpha = 0.1)) +
  labs()

# \part \textbf{Faceting:} Repita o scatter do item 2, usando
# \texttt{facet\_wrap(~continent)}. A correlação parece similar entre os
# continentes?

