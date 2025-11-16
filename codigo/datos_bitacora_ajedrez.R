# datos_bitacora_ajedrez.R
# Hecho por:
# Anthonny Flores (C32875)
# Andres Zuñiga Mora (C38733)
# Amy Chen Wu (C32203)
# Leonardo Vega Aragon (C38313)

# Librerias 
library(ggplot2)
library(dplyr)

# Funcion para ploter distribucion de variables cuantitativas 
plot.quantitative.distribution <- function(df, quant.vars) {
  for (var in quant.vars) {
    ggplot(df, aes_string(x = '""', y = var)) +
      geom_boxplot(outlier.shape = NA, fill = "lightblue") +
      geom_jitter(width = 0.2, height = 0, color ="red") +
      ggtitle(paste("Distribución de", var)) +
      xlab("") +
      ylab(var) +
      theme_minimal() -> p
    print(p)
  }
}

# Funcion para ploter relacion entre variables cuantitativas
plot.quantitative.relationship <- function(df, var1, var2) {
  ggplot(df, aes_string(x = var1, y = var2)) +
    geom_jitter(width = 0.2, height = 0, color = "blue") +
    geom_smooth(method = "loess", se = TRUE, color = "red") +
    ggtitle(paste("Relación entre", var1, "y", var2, "(LOESS)")) +
    xlab(var1) +
    ylab(var2) +
    theme_minimal() -> p
  print(p)
}

# Funcion para ploter distribucion de variables categoricas
plot.categorical.distribution <- function(df, cat.vars) {
   for (var in cat.vars) {
     ggplot(df, aes_string(x = var)) +
       geom_bar(fill = "skyblue", color = "black") +
       ggtitle(paste("Distribución de", var)) +
       theme_minimal() -> p
     print(p)
   }
}
