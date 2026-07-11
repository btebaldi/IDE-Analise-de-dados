rm(list = ls())
library(ggmosaic)

data(Titanic)
df_titanic <- as.data.frame(Titanic)

# Tabela de frequencia cruzada
tabela <- xtabs(Freq ~ Class + Survived, data = df_titanic)
print(tabela)
prop.table(tabela, margin = 1)   # proporcoes por linha

# Grafico mosaico: area proporcional a frequencia
ggplot(df_titanic) +
  geom_col(aes(y = Freq, x = product(Class), fill = Survived)) +
  theme_minimal() +
  labs(title = "Sobrevivencia por classe no Titanic")


library(tidyr)
df_titanic %>% 
  group_by(Survived, Class) %>% 
  summarise(n = sum(Freq)) %>% 
  #pivot_wider(id_cols = Class, names_from = Survived, values_from = n) %>% 
ggplot() +
  geom_col(aes(y = n, x = Class, fill = Survived), position = "stack") +
  geom_hline(yintercept = 325) +
  theme_minimal() +
  labs(title = "Sobrevivencia por classe no Titanic")
