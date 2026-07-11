rm(list = ls())

library(dplyr)

df <- ggplot2::mpg %>% dplyr::select(categoria = class, valor = hwy)

Quantiles <- quantile(df$valor, probs = c(0.25, 0.5, 0.75))
IQR <- Quantiles[3] - Quantiles[1]

cerca_inf <- Quantiles[1] - 1.5*IQR
cerca_sup <- Quantiles[3] + 1.5*IQR

# PASSO 1: sempre criar flag antes de remover
df <- df %>% mutate(flag_outlier = valor < cerca_inf | valor > cerca_sup)


  library(DescTools)
  
  # Winsorisacao nos percentis 1% e 99%
  df <- df %>%
    mutate(valor_wins = Winsorize(valor,
                                  val = quantile(valor, probs = c(0.01, 0.99))))
  
  
  # Comparar distribuicoes
  par(mfrow = c(1, 2))
  boxplot(df$valor,       main = "Original")
  boxplot(df$valor_wins,  main = "Winsorizado (P1-P99)")
  
  # Impacto na media e desvio padrao
  summarise(df,
            media_orig  = mean(valor),
            media_wins  = mean(valor_wins),
            dp_orig     = sd(valor),
            dp_wins     = sd(valor_wins))
  