library(dplyr)
library(ggplot2)

# Dataset de Berkeley disponivel no R base
data(UCBAdmissions)
df <- as.data.frame(UCBAdmissions)

# ── Analise AGREGADA (sem estratificar) ──────────────────
df %>%
  group_by(Gender) %>%
  summarise(total      = sum(Freq),
            admitidos  = sum(Freq[Admit == "Admitted"]),
            taxa_adm   = admitidos / total) %>%
  arrange(desc(taxa_adm))
# Resultado: Homens 44.5%, Mulheres 30.4%
# Conclusao errada: homens sao mais admitidos

# ── Analise ESTRATIFICADA por departamento ────────────────
df %>%
  group_by(Dept, Gender) %>%
  summarise(total     = sum(Freq),
            admitidos = sum(Freq[Admit == "Admitted"]),
            taxa_adm  = admitidos / total,
            .groups   = "drop") %>%
  pivot_wider(names_from  = Gender,
              values_from = c(total, admitidos, taxa_adm))
# Resultado: na maioria dos deptos, mulheres tem taxa maiorlibrary(dplyr)
library(ggplot2)

# Dataset de Berkeley disponivel no R base
data(UCBAdmissions)
df <- as.data.frame(UCBAdmissions)

# ── Analise AGREGADA (sem estratificar) ──────────────────
df %>%
  group_by(Gender) %>%
  summarise(total      = sum(Freq),
            admitidos  = sum(Freq[Admit == "Admitted"]),
            taxa_adm   = admitidos / total) %>%
  arrange(desc(taxa_adm))
# Resultado: Homens 44.5%, Mulheres 30.4%
# Conclusao errada: homens sao mais admitidos

# ── Analise ESTRATIFICADA por departamento ────────────────
df %>%
  group_by(Dept, Gender) %>%
  summarise(total     = sum(Freq),
            admitidos = sum(Freq[Admit == "Admitted"]),
            taxa_adm  = admitidos / total,
            .groups   = "drop") %>%
  pivot_wider(names_from  = Gender,
              values_from = c(total, admitidos, taxa_adm))
# Resultado: na maioria dos deptos, mulheres tem taxa maior







library(ggplot2)
library(dplyr)

data(UCBAdmissions)
df <- as.data.frame(UCBAdmissions)

df_taxa <- df %>%
  group_by(Dept, Gender) %>%
  summarise(taxa = sum(Freq[Admit == "Admitted"]) / sum(Freq),
            .groups = "drop")

# Grafico estratificado: a verdade por departamento
p1 <- ggplot(df_taxa, aes(x = Dept, y = taxa, fill = Gender)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = "Taxa de admissao POR DEPARTAMENTO",
       subtitle = "Em 4 de 6 deptos, mulheres tem taxa maior ou igual",
       x = "Departamento", y = "Taxa de admissao", fill = NULL) +
  theme_minimal()

# Grafico agregado: a ilusao
df_agg <- df %>%
  group_by(Gender) %>%
  summarise(taxa = sum(Freq[Admit == "Admitted"]) / sum(Freq))

p2 <- ggplot(df_agg, aes(x = Gender, y = taxa, fill = Gender)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = "Taxa de admissao AGREGADA",
       subtitle = "Parece que homens sao mais admitidos",
       x = NULL, y = "Taxa de admissao") +
  theme_minimal()

library(patchwork)
p2 + p1


