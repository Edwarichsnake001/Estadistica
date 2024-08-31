###########################################
# RESOLUCION PRUEBA TERCER PARCIAL - ESTADISTICA A0501
# NOMBRES Y APELLIDOS COMPLETOS: MATEO NICOLAY ROGERON MAILA
# NRC: 13477
# FECHA: 2024/08/26
###########################################
# INSTRUCCIONES:
# El cuestionario consta de 7 ejercicios, los cuales debe 
# responder usando R Studio y este archivo. El examen dura 
# 60 minutos, al finalizar el mismo, debe enviar este archivo 
# script de R con la resoluci?n al aula virtual.

# NOTAS IMPORTANTES
  
# Si no env?a el archivo de R con la resoluci?n del cuestionario, 
# la nota final ser? de CERO.

# Si usted responde a una pregunta en el cuestionario, pero no 
# env?a la resoluci?n de la misma en el archivo .R, su nota se 
# PENALIZARA con UN punto menos a la nota final por cada pregunta
# mal contestada, por intento de fraude acad?mico.

# Si usted responde a una pregunta en el cuestionario, pero la 
# resoluci?n de la misma en el archivo .R no coincide con la 
# respuesta, su nota se PENALIZARA con UN punto menos a la nota 
# final  por cada pregunta mal contestada, por intento de fraude
# acad?mico.

# Buena suerte.

###########################################

###########################################
# RESOLUCION PREGUNTA 1

################
# REGRESION LINEAL SIMPLE
#################

x <- c(10, 10, 10, 10, 10, 50, 50, 50, 50, 50)
y <- c(13, 18, 16, 15, 20, 86, 90, 88, 88, 92)

n <- length(x)  # total de pares ordenados (X, Y)

# GRAFICO DE DISPERSION
plot(x, y, main="Diagrama de dispersión", xlab="Presión (x)", ylab="Lectura de la escala (y)")

# COVARIANZA Y VARIANZA
Sxy <- sum((x - mean(x)) * (y - mean(y))) / n
S2x <- sum((x - mean(x))^2) / n

# ESTIMACION DE LOS BETAS
Beta1 <- Sxy / S2x  # Pendiente
Beta0 <- mean(y) - Beta1 * mean(x)  # Intercepto

Yproy <- Beta0 + Beta1 * x

# GRAFICO DE LA RECTA DE REGRESION
plot(x, y, main="Regresión Lineal Simple", xlab="Presión (x)", ylab="Lectura de la escala (y)")
abline(Beta0, Beta1, col="red")

resid <- y - Yproy

RSL1 <- lm(y ~ x)
summary(RSL1)


# INDIQUE EL LITERAL DE SU RESPUESTA AQUI: A

###########################################
# RESOLUCION PREGUNTA 2

################
# ANALISIS DE VARIANZA
#################
# Carga de datos
y <- c(5.2,4.7,8.1,6.2,3.0,9.1,7.1,8.2,6.0,9.1,3.2,5.8,2.2,3.1,7.2,2.4,3.4,4.1,1.0,4.0,7.1,6.6,9.3,4.2,7.6)
x <- factor(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5))

library(PASWR)
oneway.plots(y, x)


modelo <- aov(y ~ x)
summary(modelo)

# Independencia
Box.test(r, lag=1, type="Ljung-Box")

######################

# Comparaciones múltiples (Solo si se rechaza H0 en ANOVA)
library(asbio)
pairw.anova(x = x, y = y, method ="tukey") # Método de Tukey 


# INDIQUE EL LITERAL DE SU RESPUESTA AQUI: C




