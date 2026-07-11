# ── Quarteto de Anscombe: estatísticas idênticas, distribuições diferentes ───
# O dataset 'anscombe' já vem embutido no R base, sem necessidade de instalar
# pacotes adicionais.

# Setup -------------------------------------------------------------------
rm(list =ls())

library(dplyr)
library(tidyr)
library(ggplot2)

# ── Carregar e inspecionar o dataset ─────────────────────────────────────────
data(anscombe)
print(anscombe)

# O dataset vem em formato "largo": x1,y1 / x2,y2 / x3,y3 / x4,y4
# Vamos reorganizar para formato "longo" (tidy) para facilitar o cálculo
# de estatísticas por grupo e a visualização com facet_wrap


anscombe_long <- anscombe %>%
  pivot_longer(
    cols = everything(),
    names_to  = c(".value", "dataset"),
    names_pattern = "(x|y)(\\d)"
  ) %>%
  mutate(dataset = paste("Dataset", dataset))

head(anscombe_long, 10)

# ── Calcular estatísticas descritivas por dataset ───────────────────────────
# Esta é a parte que comprova o ponto central do exemplo:
# media, variancia e correlacao sao IDENTICAS (ou quase) entre os 4 datasets

estatisticas <- anscombe_long %>%
  group_by(dataset) %>%
  summarise(
    n              = n(),
    media_x        = mean(x),
    media_y        = mean(y),
    variancia_x    = var(x),
    variancia_y    = var(y),
    correlacao_xy  = cor(x, y),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric) & !n, ~round(., 3)))

print(estatisticas)

# ── Ajustar regressão linear simples por dataset ────────────────────────────
# Outra demonstração: o coeficiente da regressão linear tambem e quase
# identico entre os 4 datasets, apesar das distribuicoes serem visualmente
# muito diferentes

modelos <- anscombe_long %>%
  group_by(dataset) %>%
  summarise(
    intercepto = coef(lm(y ~ x))[1],
    inclinacao = coef(lm(y ~ x))[2],
    r_quadrado = summary(lm(y ~ x))$r.squared,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

print(modelos)

# ── Visualização: o ponto central do exemplo ────────────────────────────────
# Aqui fica evidente que, apesar das estatisticas serem praticamente
# identicas, os quatro datasets tem padroes completamente diferentes:
#
# Dataset 1: relacao linear "normal", com ruido tipico
# Dataset 2: relacao claramente nao-linear (curva), regressao linear e
#            inadequada apesar do bom encaixe estatistico
# Dataset 3: relacao linear quase perfeita, mas distorcida por UM outlier
# Dataset 4: a maioria dos pontos tem o mesmo valor de x; um unico ponto
#            com x diferente "cria" toda a correlacao observada

ggplot(anscombe_long, aes(x = x, y = y)) +
  geom_point(color = "steelblue", size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE,
              color = "red", linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~ dataset, nrow = 2) +
  labs(
    title    = "Quarteto de Anscombe",
    subtitle = "Mesma média, variância e correlação — distribuições completamente diferentes",
    x = "x",
    y = "y",
    caption = "A linha vermelha tracejada é a mesma reta de regressão (aproximadamente) em todos os painéis"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    plot.caption     = element_text(size = 8, color = "gray50", hjust = 0)
  )

# ── Tabela resumo formatada para slide/relatório ────────────────────────────
# Junta estatisticas descritivas e parametros da regressao numa unica tabela
# de comparacao, pronta para ser exibida

tabela_resumo <- estatisticas %>%
  left_join(modelos, by = "dataset") %>%
  select(dataset, media_x, media_y, variancia_x, variancia_y,
         correlacao_xy, inclinacao, intercepto, r_quadrado)

print(tabela_resumo)

# Versao formatada em texto, util para impressao no console em aula
cat("\n=== Resumo: estatisticas praticamente identicas entre os 4 datasets ===\n")
for (i in 1:nrow(tabela_resumo)) {
  cat(sprintf(
    "%-12s | media_x=%.2f media_y=%.2f var_x=%.2f var_y=%.2f cor=%.3f | y = %.2f + %.2fx (R2=%.3f)\n",
    tabela_resumo$dataset[i],
    tabela_resumo$media_x[i],
    tabela_resumo$media_y[i],
    tabela_resumo$variancia_x[i],
    tabela_resumo$variancia_y[i],
    tabela_resumo$correlacao_xy[i],
    tabela_resumo$intercepto[i],
    tabela_resumo$inclinacao[i],
    tabela_resumo$r_quadrado[i]
  ))
}

# ── Visualização adicional: residuos por dataset ────────────────────────────
# Reforça visualmente por que a regressao linear e adequada apenas
# para o Dataset 1. Padroes nos residuos revelam os problemas
# que a estatistica sumaria (R2, correlacao) nao capturava.

anscombe_long <- anscombe_long %>%
  group_by(dataset) %>%
  mutate(
    predito = predict(lm(y ~ x)),
    residuo = y - predito
  ) %>%
  ungroup()

ggplot(anscombe_long, aes(x = predito, y = residuo)) +
  geom_point(color = "darkorange", size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "gray40", linetype = "dashed") +
  facet_wrap(~ dataset, nrow = 2) +
  labs(
    title    = "Resíduos da regressão linear por dataset",
    subtitle = "Padrões nos resíduos revelam onde o modelo linear falha",
    x = "Valor predito",
    y = "Resíduo (y observado - y predito)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text       = element_text(face = "bold"),
    plot.title        = element_text(face = "bold", size = 14),
    plot.subtitle     = element_text(size = 10, color = "gray40")
  )
