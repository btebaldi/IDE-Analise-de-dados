rm(list = ls())

library(dplyr)

df <- ggplot2::mpg %>% dplyr::select(categoria = cty   , valor = hwy)

Quantiles <- quantile(df$valor, probs = c(0.25, 0.5, 0.75))
IQR <- Quantiles[3] - Quantiles[1]

cerca_inf <- Quantiles[1] - 1.5*IQR
cerca_sup <- Quantiles[3] + 1.5*IQR

# PASSO 1: sempre criar flag antes de remover
df <- df %>% mutate(flag_outlier = valor < cerca_inf | valor > cerca_sup)

# PASSO 2: inspecionar os outliers
df %>% dplyr::filter(flag_outlier)

# PASSO 3: remover apenas apos investigacao
df_clean <- df %>% dplyr::filter(!flag_outlier)

# PASSO 4: comparar estatisticas antes e depois
summary(df$valor)
summary(df_clean$valor)
