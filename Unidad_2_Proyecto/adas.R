# Datos correctos
x <- c(1, 2, 3, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
y <- c(812.52, 822.50, 1211.50, 1348.00, 1301.00, 2567.50, 2526.50, 2755.00, 4390.50, 5581.50, 5548.00, 6086.00, 5764.00, 8903.00)

# Modelo de regresión lineal
modelo_lineal <- lm(y ~ x)
summary(modelo_lineal)

# Cálculo de s^2, R^2 y PRESS para el modelo lineal
s2_lineal <- sum(residuals(modelo_lineal)^2) / df.residual(modelo_lineal)
R2_lineal <- summary(modelo_lineal)$r.squared
hii_lineal <- lm.influence(modelo_lineal)$hat
PRESS_lineal <- sum((residuals(modelo_lineal) / (1 - hii_lineal))^2)

# Modelo cuadrático
modelo_cuadratico <- lm(y ~ x + I(x^2))
summary(modelo_cuadratico)

# Cálculo de s^2, R^2 y PRESS para el modelo cuadrático
s2_cuadratico <- sum(residuals(modelo_cuadratico)^2) / df.residual(modelo_cuadratico)
R2_cuadratico <- summary(modelo_cuadratico)$r.squared
hii_cuadratico <- lm.influence(modelo_cuadratico)$hat
PRESS_cuadratico <- sum((residuals(modelo_cuadratico) / (1 - hii_cuadratico))^2)

# Resultados
list(
  lineal = list(s2 = s2_lineal, R2 = R2_lineal, PRESS = PRESS_lineal),
  cuadratico = list(s2 = s2_cuadratico, R2 = R2_cuadratico, PRESS = PRESS_cuadratico)
)
