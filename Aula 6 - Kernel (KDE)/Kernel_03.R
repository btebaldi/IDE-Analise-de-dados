library(ggplot2)

set.seed(1)
idade <- rgamma(500, shape = 4, scale = 10)  # idade: suporte > 0

# KDE diretamente em idade: vaza para valores negativos
dens_idade <- density(idade, bw = "SJ")

ggplot(data.frame(x = dens_idade$x, y = dens_idade$y), aes(x, y)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_area(data = subset(data.frame(x = dens_idade$x, y = dens_idade$y),
                          x < 0),
            aes(x, y), fill = "red", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "KDE de idade: vazamento para x < 0",
       subtitle = "Area vermelha = densidade impossivel (idade negativa)") +
  theme_minimal()

# Correcao: KDE em log(idade), depois retorna a escala original
log_idade <- log(idade)
dens_log  <- density(log_idade, bw = "nrd0")

# Transformar a grade de volta para a escala original
x_original <- exp(dens_log$x)
# Ajuste de densidade pelo Jacobiano: f(x) = f(log(x)) / x
y_corrigida <- dens_log$y / x_original

ggplot(data.frame(x = x_original, y = y_corrigida), aes(x, y)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  labs(title = "KDE corrigida via log-transformacao",
       subtitle = "Sem vazamento: densidade = 0 para x <= 0") +
  theme_minimal()
