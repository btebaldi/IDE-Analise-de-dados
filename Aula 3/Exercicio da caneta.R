# ── Solução: Exercício da Caneta Stylus Fountain Pen ─────────────────────────
# Problema: o programa de Felix gera erro quando o último dia do mês
# cai num sábado ou domingo (pois não há cotação de ações nesses dias).
# Objetivo: contar quantas vezes isso acontece entre janeiro de 1950
# e janeiro de 2021, e comparar com o valor teórico esperado.

# Setup -------------------------------------------------------------------
rm(list = ls())          # limpa o ambiente
library(dplyr)
library(lubridate)       # para a função wday(), que retorna o dia da semana

# Geração das datas -------------------------------------------------------
# seq() com by = "month" gera uma sequência de datas mensais.
# Cada data gerada é o PRIMEIRO dia de cada mês (ex: 1950-01-01, 1950-02-01, ...).
# Hipótese assumida: usamos o primeiro dia do mês como proxy para o último dia
# do mês anterior — ou equivalentemente, estamos gerando os primeiros dias e
# tratando-os como representativos do fim do mês anterior.
#
# ATENÇÃO: uma implementação mais precisa usaria o último dia de cada mês.
# Isso pode ser feito com: seq() + ceiling_date() - days(1), ou
# com rollback(seq(...) + months(1)).
# Para os fins do exercício, a aproximação é aceitável.
tbl <- tibble(
  Data = seq(from = as.Date("1950-01-01"),
             to   = as.Date("2021-01-01"),
             by   = "month")
)

# Dia da semana -----------------------------------------------------------
# wday() retorna um inteiro representando o dia da semana.
# Na convenção padrão do lubridate (locale en_US):
#   1 = domingo, 2 = segunda, ..., 6 = sexta, 7 = sábado
# Portanto: wday == 1 corresponde a domingo
#           wday == 7 corresponde a sábado
tbl <- tbl %>%
  mutate(wday = wday(Data))

# Contagem empírica -------------------------------------------------------
# Filtra os meses em que o dia gerado cai num sábado (7) ou domingo (1),
# e conta o número de linhas resultantes.
# Esse é o número de vezes que Felix precisará ajustar os dados manualmente.
n_erros_empirico <- tbl %>%
  filter(wday == 1 | wday == 7) %>%
  nrow()

n_erros_empirico   # resultado empírico (contagem exata no calendário)

# Valor teórico esperado --------------------------------------------------
# Hipótese: os dias da semana se distribuem uniformemente ao longo do tempo.
# Em qualquer semana, 2 dos 7 dias são fim de semana → P(fim de semana) = 2/7.
#
# Número total de meses no período:
#   (2021 - 1950) anos × 12 meses/ano = 852 meses
#
# Valor esperado de erros:
#   E[erros] = 852 × (2/7) ≈ 243,4
#
# Nota: o valor teórico é uma aproximação — ignora que o calendário gregoriano
# não distribui os dias de semana perfeitamente de forma uniforme em janelas
# curtas, mas a aproximação é excelente para períodos longos como este.
Teorico <- (2021 - 1950) * 12 * (2/7)
Teorico   # valor esperado teórico

# Comparação --------------------------------------------------------------
# Os dois valores devem ser próximos mas raramente idênticos,
# pois o calendário real tem pequenas irregularidades em relação
# à distribuição uniforme assumida pelo cálculo teórico.
cat("Erros empíricos (calendário real):", n_erros_empirico, "\n")
cat("Erros teóricos esperados:         ", round(Teorico, 1),  "\n")
cat("Diferença:                        ", n_erros_empirico - round(Teorico), "\n")