# Carrega pacotes necessarios para os passos acima
rm(list = ls())

library(dplyr)
library(stringr)
library(MASS)  # para mvrnorm

# ── Geração da base de dados: transações financeiras ────────────────────────
# Simula um dataset de transações com cartão de crédito, contendo variáveis
# numéricas correlacionadas e um pequeno percentual de outliers multivariados
# injetados deliberadamente — situações em que cada variável isoladamente
# parece normal, mas a COMBINAÇÃO das variáveis é anômala.

set.seed(123)

n_normal   <- 950   # observações "normais"
n_outliers <- 50    # observações com padrão anômalo na combinação

# ── Bloco 1: observações normais ─────────────────────────────────────────────
# valor_transacao e tempo_processamento sao positivamente correlacionados:
# transacoes maiores tendem a levar mais tempo para serem processadas.
# Construimos isso via uma matriz de covariancia explicita.


media_normal <- c(valor_transacao     = 250,
                  tempo_processamento = 4.5,
                  num_itens           = 8)

# Matriz de covariancia: define a correlacao entre as variaveis
sigma_normal <- matrix(c(
  2500,   45,    20,     # var(valor)=2500 (dp~50), cov(valor,tempo)=45
  45,    4,    1.5,    # var(tempo)=4 (dp=2), cov(tempo,itens)=1.5
  20,  1.5,    9        # var(itens)=9 (dp=3)
), nrow = 3, byrow = TRUE)

dados_normais <- mvrnorm(n = n_normal,
                         mu    = media_normal,
                         Sigma = sigma_normal) %>%
  as.data.frame() %>%
  setNames(c("valor_transacao", "tempo_processamento", "num_itens"))

# ── Bloco 2: outliers multivariados (combinação anômala) ───────────────────
# Cada variavel individualmente parece plausivel, mas a COMBINACAO e atipica.
# Exemplo: valor alto + tempo de processamento muito baixo
# (tipico de fraude: transacao de alto valor processada quase instantaneamente,
#  sem o tempo de validacao normalmente associado a valores altos)

dados_outliers <- data.frame(
  valor_transacao      = runif(n_outliers, min = 800,  max = 1500),
  tempo_processamento  = runif(n_outliers, min = 0.5,  max = 1.5),
  num_itens            = runif(n_outliers, min = 1,    max = 3)
)

# ── Bloco 3: combinar e embaralhar ──────────────────────────────────────────
df <- bind_rows(
  dados_normais %>% mutate(tipo_real = "normal"),
  dados_outliers %>% mutate(tipo_real = "outlier_injetado")
) %>%
  mutate(
    id_transacao = paste0("TXN", str_pad(row_number(), 5, pad = "0")),
    .before = 1
  ) %>%
  # garantir valores plausiveis (sem negativos)
  mutate(
    valor_transacao     = round(pmax(valor_transacao, 5), 2),
    tempo_processamento = round(pmax(tempo_processamento, 0.1), 2),
    num_itens           = round(pmax(num_itens, 1))
  ) %>%
  sample_frac(1)  # embaralha as linhas para remover qualquer ordem


# ── Verificação rápida da base ───────────────────────────────────────────────
glimpse(df)
summary(df %>% dplyr::select(valor_transacao, tempo_processamento, num_itens))

# Distribuicao do tipo real (apenas para conferencia do analista,
# normalmente essa coluna NAO estaria disponivel num cenario real)
table(df$tipo_real)



# Distancia de Mahalanobis em R
df_num <- df %>% select_if(is.numeric) %>% na.omit()

D2 <- mahalanobis(df_num,
                  center = colMeans(df_num),
                  cov    = cov(df_num))

# Threshold: quantil 97.5% da qui-quadrado com p graus de liberdade
p         <- ncol(df_num)
threshold <- qchisq(0.975, df = p)
outliers_mah <- which(D2 > threshold)

df_num$D2 <- D2
plot(df_num)
df_num %>%
  mutate(outlier = D2 > threshold) %>% 
  ggplot() + 
  geom_point(aes(x = tempo_processamento, y = num_itens, colour = outlier)) +
  scale_color_manual(values = c("FALSE" = "steelblue",
                                "TRUE"  = "red"),
                     labels = c("Normal", "Outlier")) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, colour = NULL)
