# ── Demonstração da Lei dos Grandes Números (LGN) ────────────────────────────
# Experimento: lançamento de uma moeda honesta M vezes.
# Objetivo: mostrar que a proporção acumulada de caras converge para 0,5
# à medida que o número de lançamentos cresce — isso é a LGN em ação.

# Setup -------------------------------------------------------------------
rm(list = ls())          # limpa o ambiente para garantir reprodutibilidade
library(dplyr)
library(ggplot2)

# Parâmetros do experimento -----------------------------------------------
M <- 1000                # número total de lançamentos da moeda

# Simulação ---------------------------------------------------------------
# Cria um tibble com M lançamentos simulados.
# sample() com replace = TRUE simula lançamentos independentes (com reposição),
# o que é a condição correta para modelar uma moeda honesta.
df <- tibble(
  id    = seq_len(M),                                        # índice do lançamento (1 a M)
  Moeda = sample(c("H", "T"), size = M, replace = TRUE)     # resultado: "H" (cara) ou "T" (coroa)
)

# Cálculo das estatísticas acumuladas -------------------------------------
df <- df %>%
  mutate(
    # Converte o resultado em variável binária: 1 = cara, 0 = coroa
    Cara = if_else(Moeda == "H", true = 1, false = 0),
    
    # Soma acumulada de caras até o lançamento i
    # cumsum() retorna um vetor em que o elemento i é a soma de Cara[1] até Cara[i]
    Total_Cara = cumsum(Cara),
    
    # Proporção acumulada de caras: estimativa da probabilidade P(Cara) até o lançamento i
    # Pela LGN, Prob converge para 0,5 (o valor verdadeiro) quando id → ∞
    Prob = Total_Cara / id,
    
    # Banda de incerteza: ±2 erros padrão da proporção estimada
    # O erro padrão de uma proporção p com n observações é sqrt(p*(1-p)/n)
    # Multiplicamos por 2 para obter um intervalo aproximado de 95% de confiança
    # Nota: este intervalo usa a aproximação Normal, válida para n suficientemente grande
    se = 2 * sqrt(Prob * (1 - Prob) / id)
  )

# Visualização ------------------------------------------------------------
# O gráfico mostra como a proporção acumulada (linha preta) oscila bastante
# no início e vai se estabilizando em torno de 0,5 (linha vermelha)
# à medida que o número de lançamentos cresce — ilustração direta da LGN.
# As linhas azuis tracejadas mostram a banda de ±2 EP, que também se estreita
# com n crescente, refletindo a redução da incerteza amostral.
df %>%
  ggplot() +
  
  # Proporção acumulada de caras ao longo dos lançamentos
  geom_line(aes(x = id, y = Prob)) +
  
  # Limite superior da banda de confiança (Prob + 2*EP)
  geom_line(aes(x = id, y = Prob + se),
            linetype = "dashed", colour = "blue", alpha = 0.5) +
  
  # Limite inferior da banda de confiança (Prob - 2*EP)
  geom_line(aes(x = id, y = Prob - se),
            linetype = "dashed", colour = "blue", alpha = 0.5) +
  
  # Linha de referência: valor verdadeiro P(Cara) = 0,5 para uma moeda honesta
  geom_hline(yintercept = 0.5, colour = "red") +
  
  # Rótulos para facilitar a leitura do gráfico
  labs(
    title    = "Lei dos Grandes Números: lançamento de moeda",
    subtitle = "A proporção acumulada converge para 0,5 à medida que n cresce",
    x        = "Número de lançamentos",
    y        = "Proporção acumulada de caras",
    caption  = "Linha vermelha = valor verdadeiro (0,5) | Linhas azuis = ± 2 erros padrão"
  ) +
  theme_minimal()

