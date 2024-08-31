################
# ANALISIS DE VARIANZA
#################
# Carga de datos
y = c(5.9, 6.1, 6.3, 6.1, 6.0, 6.3, 6.6, 6.4, 6.4, 6.5, 4.8, 5.2, 5.0, 4.7, 5.1, 6.0, 6.2, 6.1, 5.8)
x = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4) #codificando X como numerica (es categorica)

# convertir X en categorica
x = as.factor(x) # funcion as.factor

# Analisis exploratorio ANOVA
library(PASWR)
oneway.plots(y, x)

# Modelo ANOVA
modelo = aov(y ~ x) # funcion ANOVA
summary(modelo)

# Comprobacion de supuestos
# Primer paso: Obtener los residuos estandarizados
library(MASS)
r = stdres(modelo) # funcion para calcular residuos del ANOVA

# Normalidad
shapiro.test(r)

# Homocedasticidad (levene)
library(lawstat)
levene.test(y, x)

# Independencia
Box.test(r, lag=1, type="Ljung-Box")

######################
# Comparaciones multiples
# Nota: solo se realiza si la HO del anova se rechaza

library(asbio)
pairw.anova(x = x, y = y, method ="bonf") # Bonferroni
pairw.anova(x = x, y = y, method ="tukey") # Tukey
pairw.anova(x = x, y = y, method ="scheffe") # Scheffe


