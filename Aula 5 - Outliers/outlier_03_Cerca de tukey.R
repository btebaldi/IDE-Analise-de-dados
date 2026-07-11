xx <- c(1,2,2,3,100)


  # Cercas de Tukey em R
  Q1  <- quantile(xx, 0.25)
  Q3  <- quantile(xx, 0.75)
  IQR_val <- Q3 - Q1
  
  cerca_inf <- Q1 - 1.5 * IQR_val
  cerca_sup <- Q3 + 1.5 * IQR_val
  
  outliers_iqr <- xx[xx < cerca_inf | xx > cerca_sup]
  outliers_iqr
  