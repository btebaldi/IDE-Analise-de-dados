library(corrplot)

data(mtcars)
matriz_cor <- cor(mtcars)

corrplot(matriz_cor,
         method = "color",
         type   = "upper",
         order  = "hclust",        # agrupa variaveis correlacionadas
         addCoef.col = "black",    # mostra os valores numericos
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE)
