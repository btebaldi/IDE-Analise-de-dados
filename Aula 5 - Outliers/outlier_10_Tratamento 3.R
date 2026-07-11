  library(dplyr)
  library(MASS)   # para boxcox
  
  
  df <- ggplot2::mpg %>% dplyr::select(categoria = class, valor = hwy)
  
  Quantiles <- quantile(df$valor, probs = c(0.25, 0.5, 0.75))
  IQR <- Quantiles[3] - Quantiles[1]
  
  cerca_inf <- Quantiles[1] - 1.5*IQR
  cerca_sup <- Quantiles[3] + 1.5*IQR
  
  # PASSO 1: sempre criar flag antes de remover
  df <- df %>% mutate(flag_outlier = valor < cerca_inf | valor > cerca_sup)
  
  
  # Log-transformacao (mais comum em dados financeiros)
  df <- df %>%
    mutate(log_valor = log1p(valor))   # log(x + 1) evita log(0)
  
  # Box-Cox: estima o lambda otimo
  bc <- boxcox(valor ~ 1, data = df, lambda = seq(-2, 2, 0.1))
  lambda_opt <- bc$x[which.max(bc$y)]
  df <- df %>%
    mutate(valor_bc = (valor^lambda_opt - 1) / lambda_opt)

  
  # Comparar distribuicoes
  par(mfrow = c(1, 2))
  boxplot(df$valor,       main = "Original")
  boxplot(df$valor_bc,  main = "Box-Cox")
  par(mfrow = c(1, 1))
  