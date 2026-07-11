# Exemplo: se $x = \{1, 2, 2, 3, 100\}$,
# o outlier 100 eleva tanto $s$ que os outros parecem normais

%>% %>% xx <- c(1,2,2,3,100)
%>% %>% 
# padroniza: (x - media) / dp
z_scores <- scale(xx)

outliers_z <- which(abs(z_scores) > 3)
print(outliers_z)

# Z-score modificado em R
mad_val  <- mad(xx, constant = 1)
z_mod    <- 0.6745 * (xx - median(xx)) / mad_val
outliers_zmod <- which(abs(z_mod) > 3.5)
