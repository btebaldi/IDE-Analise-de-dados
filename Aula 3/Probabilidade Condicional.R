# ── Solução: Probabilidade Condicional via Simulação ─────────────────────────
# Exercício: usando o banco de dados de alunos por curso e sexo,
# verificar empiricamente as probabilidades calculadas na tabela de contingência.
# A estratégia é simular 30.000 "escolhas aleatórias" de alunos e comparar
# as frequências relativas observadas com os valores teóricos esperados.

# Setup -------------------------------------------------------------------
rm(list = ls())
library(readr)
library(dplyr)    # necessário para o filter() usado mais abaixo

# Carrega o banco de dados ------------------------------------------------
# O arquivo contém as variáveis Curso e Sexo dos 200 alunos
# conforme a tabela de contingência do slide.
df <- readr::read_csv("Aula 3/cursos.csv")

# Tabela de contingência --------------------------------------------------
# Mostra o cruzamento de Curso x Sexo para conferência.
# O [,-1] remove a primeira coluna (provavelmente um ID ou nome do aluno)
# para que a tabela mostre apenas as variáveis categóricas de interesse.
table(df[, c("curso", "sexo")])

# Simulação: amostragem com reposição -------------------------------------
# Em vez de calcular probabilidades diretamente pela fórmula,
# vamos simular o processo de "escolher um aluno ao acaso" n vezes.
# n grande garante que as frequências relativas convirjam para as
# probabilidades verdadeiras --- isso é a Lei dos Grandes Números em ação.
n <- 30000

# sample() sorteia n índices de linhas do dataset com reposição (replace = TRUE).
# Com reposição significa que o mesmo aluno pode ser escolhido mais de uma vez,
# simulando corretamente escolhas independentes de uma população infinita.
mySample <- sample(1:nrow(df), size = n, replace = TRUE)

# ── Probabilidade marginal: P(Mulher) ────────────────────────────────────────
# Empírico: conta quantas das n escolhas resultaram em "Mulher"
#           e divide pelo total de escolhas
# Teórico:  85 mulheres em 200 alunos → P(Mulher) = 85/200 = 0,425
sum(df$sexo[mySample] == "Mulher") / n   # empírico ≈ 0,425
85 / 200                                                 # teórico  = 0,425

# ── Probabilidade marginal: P(Estatística) ───────────────────────────────────
# Empírico: proporção de escolhas que resultaram no curso de Estatística
# Teórico:  30 alunos de Estatística em 200 → P(Estatística) = 30/200 = 0,15
sum(df$curso[mySample] == "Estatistica") / n   # empírico ≈ 0,15
30 / 200                                                       # teórico  = 0,15

# ── Probabilidade conjunta: P(Mulher ∩ Estatística) ──────────────────────────
# Empírico: proporção de escolhas onde o aluno é Mulher E do curso Estatística
#           simultaneamente. O operador & exige que AMBAS as condições sejam TRUE.
# Teórico:  20 mulheres em Estatística em 200 alunos → P = 20/200 = 0,10
sum((df$sexo[mySample]  == "Mulher") & (df$curso[mySample] == "Estatistica")) / n   # empírico ≈ 0,10
10 / 200                                                         # teórico  = 0,10

# ── Probabilidade condicional: P(Mulher | Estatística) ───────────────────────
# Para calcular P(Mulher | Estatística), precisamos RESTRINGIR o espaço amostral
# apenas às escolhas em que o aluno é do curso de Estatística.
# Isso é exatamente o que filter() faz: cria um novo dataset menor (NewDataset)
# contendo apenas os alunos de Estatística dentro da amostra simulada.
NewDataset <- df[mySample, ] %>%
  filter(curso == "Estatistica")

# Agora calculamos a proporção de mulheres DENTRO desse espaço restrito.
# Empírico: mulheres entre os alunos de Estatística sorteados
# Teórico:  20 mulheres entre os 30 alunos de Estatística → P = 20/30 ≈ 0,667
#
# Note a diferença em relação à probabilidade conjunta:
#   P(Mulher ∩ Estatística) = 10/200 = 0,050   (sobre todos os alunos)
#   P(Mulher | Estatística) = 20/30  ≈ 0,667   (apenas sobre os de Estatística)
# Isso ilustra concretamente que condicionar restringe e renormaliza o espaço.
sum(NewDataset$sexo == "Mulher") / nrow(NewDataset)   # empírico ≈ 0,667
20 / 30                                                # teórico  ≈ 0,667

# ── Verificação da fórmula de Bayes ──────────────────────────────────────────
# Podemos confirmar o resultado condicional pela definição formal:
# P(Mulher | Estatística) = P(Mulher ∩ Estatística) / P(Estatística)
#                         = (10/200) / (30/200)
#                         = 10/30 ≈ 0,333
#
# ATENÇÃO: o valor teórico correto é 20/30, não 10/30.
# 10 é o número de mulheres em Estatística no dataset ORIGINAL (200 alunos).
# A fórmula de Bayes aplicada aos dados originais dá:
# P(Mulher | Estatística) = (20/200) / (30/200) = 20/30 ✓