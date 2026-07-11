set.seed(42)

# Definir os grupos conforme a tabela
cursos  <- c("Matematica", "Macroeconomia", "Estatistica", "Econometria")
n_hom   <- c(70, 15, 10, 20)   # homens por curso
n_mul   <- c(40, 15, 20, 10)   # mulheres por curso

# Função auxiliar para gerar linhas de um grupo
gerar_grupo <- function(curso, sexo, n) {
  data.frame(
    curso    = curso,
    sexo     = sexo,
    n_obs    = seq_len(n)   # índice temporário, removido depois
  )
}

# Criar esqueleto com curso e sexo
skeleton <- do.call(rbind, mapply(function(cur, nh, nm) {
  rbind(
    gerar_grupo(cur, "Homem",  nh),
    gerar_grupo(cur, "Mulher", nm)
  )
}, cursos, n_hom, n_mul, SIMPLIFY = FALSE))

skeleton$n_obs <- NULL
n <- nrow(skeleton)   # deve ser 200

# ── Variáveis topográficas ────────────────────────────────────────────────────

# Idade: entre 18 e 35, distribuição ligeiramente diferente por curso
idade_media <- c(Matematica    = 22,
                 Macroeconomia = 24,
                 Estatistica   = 23,
                 Econometria   = 25)

skeleton$idade <- round(
  mapply(function(cur) {
    rnorm(1, mean = idade_media[cur], sd = 2.5)
  }, skeleton$curso)
)
skeleton$idade <- pmax(18, pmin(35, skeleton$idade))   # clipar entre 18–35

# Período: 1 a 8, mais avançado para cursos como Econometria
skeleton$periodo <- sapply(skeleton$curso, function(cur) {
  base <- c(Matematica = 3, Macroeconomia = 4,
            Estatistica = 3, Econometria = 5)
  p <- round(rnorm(1, mean = base[cur], sd = 1.5))
  pmax(1, pmin(8, p))
})

# IRA (Índice de Rendimento Acadêmico): escala 0–10
# Estatística e Matemática tendem a ter IRAs ligeiramente menores
ira_media <- c(Matematica    = 6.8,
               Macroeconomia = 7.2,
               Estatistica   = 6.5,
               Econometria   = 7.0)

skeleton$ira <- round(
  mapply(function(cur) {
    rnorm(1, mean = ira_media[cur], sd = 1.2)
  }, skeleton$curso), 2
)
skeleton$ira <- pmax(0, pmin(10, skeleton$ira))

# Cidade de origem: probabilidades diferentes por curso
cidades <- c("Sao Paulo", "Rio de Janeiro", "Belo Horizonte",
             "Curitiba", "Outras")

prob_cidade <- list(
  Matematica    = c(0.45, 0.20, 0.15, 0.10, 0.10),
  Macroeconomia = c(0.40, 0.25, 0.15, 0.10, 0.10),
  Estatistica   = c(0.50, 0.15, 0.15, 0.10, 0.10),
  Econometria   = c(0.35, 0.25, 0.20, 0.10, 0.10)
)

skeleton$cidade_origem <- sapply(skeleton$curso, function(cur) {
  sample(cidades, 1, prob = prob_cidade[[cur]])
})

# Renda familiar mensal (R$): log-normal para refletir assimetria real
renda_media_log <- c(Matematica    = 8.5,
                     Macroeconomia = 9.0,
                     Estatistica   = 8.7,
                     Econometria   = 9.2)

skeleton$renda_familiar <- round(
  mapply(function(cur) {
    exp(rnorm(1, mean = renda_media_log[cur], sd = 0.6))
  }, skeleton$curso)
)

# Modalidade de ingresso
skeleton$ingresso <- sample(
  c("ENEM", "Vestibular", "Transferencia", "Convenio"),
  n, replace = TRUE,
  prob = c(0.55, 0.30, 0.10, 0.05)
)

# Bolsista
skeleton$bolsista <- rbinom(n, 1,
                            prob = ifelse(skeleton$renda_familiar < 3000, 0.70, 0.15))
skeleton$bolsista <- factor(skeleton$bolsista,
                            levels = c(0, 1),
                            labels = c("Nao", "Sim"))

# Turno
skeleton$turno <- sample(c("Integral", "Noturno"), n,
                         replace = TRUE, prob = c(0.65, 0.35))

# ── Organizar e nomear o dataframe final ─────────────────────────────────────
df <- skeleton
rownames(df) <- NULL

# ── Validação ─────────────────────────────────────────────────────────────────
cat("Tabela de contingência (deve coincidir com o slide):\n")
print(table(df$curso, df$sexo))

cat("\nDimensões do dataset:", nrow(df), "linhas x", ncol(df), "colunas\n")
cat("Variáveis:", paste(names(df), collapse = ", "), "\n")

df %>% rename(idc_rend_academico = ira) %>% readr::write_csv(file = "cursos.csv")
