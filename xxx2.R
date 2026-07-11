
# % Código de carregamento — copie e execute no RStudio
# % # Instale o pacote caso ainda não tenha
# % # install.packages("rbcb")

library(rbcb)
library(dplyr)
library(ggplot2)

# Baixa as três séries diretamente da API do Banco Central
ipca  <- get_series(433,  start_date = "2015-01-01", end_date = "2024-12-31")
selic <- get_series(4189, start_date = "2015-01-01", end_date = "2024-12-31")
cambio <- get_series(3695,    start_date = "2015-01-01", end_date = "2024-12-31")

# Renomeia as colunas e une em um único data.frame
names(ipca)[2]   <- "ipca"
names(selic)[2]  <- "selic"
names(cambio)[2] <- "cambio"

macro <- ipca |>
   left_join(selic,  by = "date") |>
   left_join(cambio, by = "date")

glimpse(macro)  # verifique a estrutura antes de plotar

macro %>% 
  ggplot() + 
  geom_line(mapping = aes(x = date, y = ipca)) +
  geom_line(mapping = aes(x = date, y = selic)) + 
  geom_line(mapping = aes(x = date, y = cambio)) + 
  labs()


macro %>% readr::write_csv(file = "IPCA_SELIC_FX.csv")
