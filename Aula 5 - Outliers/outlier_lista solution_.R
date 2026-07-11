rm(list = ls())
library(dplyr)

# df <- nasaweather::atmos
# 
# df.mis <- df[!complete.cases(df),]
# df.com <- df[complete.cases(df),]
# df.com <- df.com %>% slice_sample( n = floor(nrow(df.mis)*3.1))
# 
# rbind(df.com, df.mis) %>% arrange(ozone) %>% readr::write_csv(file = "atmos.csv")

readr::read_csv("atmos.csv") -> df

df %>% summary()

# Gere o mapa de dados ausentes (\textit{Dica:} utilize
# \texttt{naniar::vis\_miss(df)}).
naniar::vis_miss(df)


# Aplique o teste de Little (\texttt{naniar::mcar\_test(df)}).
naniar::mcar_test(df)

# Impute a variável \texttt{ozone} com três estratégias: média global, mediana
# global e \texttt{mice} ($M = 5$, método \texttt{"pmm"}).


# Imputacao multipla com M = 5 datasets
imp <- mice::mice(df,
                  m = 5, # numero de imputacoes
                  method = "pmm", # predictive mean matching
                  seed = 42,
                  printFlag = FALSE)

# Ver os metodos usados por variavel
imp$met


# Extrair um dataset completo (o 1o dos 5)
mice::complete(imp)


