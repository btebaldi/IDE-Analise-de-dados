# Setup -------------------------------------------------------------------

rm(list =ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(forcats)
library(tidyr)

# Data Load Cancelamentos -------------------------------------------------

tbl_cancel <- read_excel("RELACAO CANCELADOS.xlsx", 
                                 sheet = "Cancelamentos")

m_lbl <- c("2023_1", "2023_2", "2024_1", "2023_2", "2024_2", "2025_1", "2025_2", "2026_1")
m_lvl <- c("2023/ 1", "2023/ 2", "2024/1", "2023/2", "2024/2", "2025/1", "2025/2", "2026/1")

tbl_cancel <- tbl_cancel %>% 
  mutate(Ingresso = factor(Ingresso, levels = m_lvl, labels = m_lbl, ordered = TRUE))


tbl_cancel <- tbl_cancel %>% 
  mutate(Curso = factor(Curso,
                        levels = c("CIÊNCIA DE DADOS", "ECONOMIA DE NEGÓCIOS", "FINANÇAS CORPORATIVAS", "INVESTIMENTO", "DATA SCIENCE"),
                        labels = c("Data Science Aplicada", "Economia de Negócios", "Finanças Corporativas", "Investimentos", "Data Science Aplicada")))



# Data Load Ingresso ------------------------------------------------------

tbl_ingresso <- read_excel("RELACAO CANCELADOS.xlsx", 
                         sheet = 2, range = "A13:F17")

colnames(tbl_ingresso) <- c("Enfase", "D2024_1", "D2024_2", "D2025_1", "D2025_2", "D2026_1")

tbl_ingresso <- tbl_ingresso %>% 
  mutate(Enfase = factor(Enfase,
                        levels = c("Data Science Aplicada​", "Economia de Negócios​", "Finanças Corporativas​", "Investimentos​"),
                        labels = c("Data Science Aplicada", "Economia de Negócios", "Finanças Corporativas", "Investimentos")))


tbl_ingresso <- tbl_ingresso %>% 
  pivot_longer(cols = -Enfase, names_prefix = "D", names_to = "Ingresso", values_to = "AnoCurso") %>% 
  group_by(Ingresso) %>% 
  mutate(Ano = sum(AnoCurso))




padronizar_motivo <- function(motivo) {
  
  motivo_limpo <- motivo %>%
    str_to_lower() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_squish()
  
  case_when(
    # Migração para outro curso/programa
    str_detect(motivo_limpo, "mpe|mestrado|anpec|direito|administracao|outro curso|outra instituicao|pos graduacao") ~
      "Outro curso/programa",
    
    # Conflito profissional / falta de tempo
    str_detect(motivo_limpo, "emprego|trabalho|agenda|rotina|tempo|dedicar|atuacao profissional") ~
      "Falta de tempo",
    
    # Financeiro
    str_detect(motivo_limpo, "financeiro|custear|desempregado") ~
      "Financeiro",
    
    # Adaptação ao curso / metodologia
    str_detect(motivo_limpo, "nao se adaptou|metodologia|formato|online|expectat") ~
      "Adaptação ao curso / metodologia",
    
    # Mudança de país / exterior
    str_detect(motivo_limpo, "exterior|outro pais") ~
      # "Mudança de país / exterior",
      "Outro curso/programa",
    
    # Saúde / família
    str_detect(motivo_limpo, "saude|familia") ~
      "Saúde / família",
    
    # Motivos pessoais
    str_detect(motivo_limpo, "particulares") ~
      "Motivos pessoais",
    
    # TRUE ~ "Outros"
    TRUE ~ "Outro curso/programa",
  )
}


tbl_cancel <- tbl_cancel %>% 
  mutate(MotivoClasse = padronizar_motivo(motivo))

tbl_cancel %>% 
  readr::write_csv(file = "BaseDados.csv")

unique(tbl_cancel$MotivoClasse)

# Data Re ---------------------------------------------------------------

tbl_cancel %>% 
  group_by(Curso, Ingresso, MotivoClasse) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(id_cols = c("Curso", "Ingresso"), names_from = "MotivoClasse", values_from = Total, values_fill = 0) %>% 
  pivot_longer(cols = -c("Curso", "Ingresso"), names_to = "MotivoClasse", values_to = "Total") %>% 
  left_join(tbl_ingresso, by = c("Curso"="Enfase", "Ingresso"="Ingresso"), ) %>% 
  group_by(Ingresso, MotivoClasse) %>% 
  summarise(Total = sum(Total), Ano  = mean(Ano)) %>% 
  mutate(ShareAno = Total/Ano) %>% 
  ggplot() + 
  # geom_col(aes(x = Ingresso, y = Total), position = "dodge") +
  geom_col(aes(x = Ingresso, y = Total), position = "dodge") +
  # scale_x_discrete(drop = FALSE) +
  facet_wrap(~MotivoClasse) +
  labs()


tbl_cancel %>% 
  filter(MotivoClasse == "Migração para outro curso/programa") %>% 
  select(motivo, MotivoClasse) %>% print(n=10)


tbl_cancel %>% 
  filter(MotivoClasse == "Outros") %>% 
  select(Ingresso, motivo, MotivoClasse) %>% print(n=10)
  


