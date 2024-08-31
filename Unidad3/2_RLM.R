################
# REGRESION LINEAL MULTIPLE
#################
# CARGAR LOS DATOS
y = c(40,35,30,20,25) # 
x1 = c(100,90,80,75,70) # 
x2 = c(35,32,28,20,30) # 
n = 5 # total de observaciones
p = 3 # total de coeficientes betas a estimar

# Construir las matrices
Y = matrix(y, nrow = 5) # vector Y
Y
X = matrix(c(rep(1,n),x1,x2), ncol = p) # matriz de diseño
X

# Construir el sistema de ecuaciones normales A*beta = b
t(X) # transpuesta de X

A = t(X) %*% X # Ojo: multiplicacion matricial = %*%
A

b = t(X) %*% Y 
b

# Estimar el vector Beta
Ainv = solve(A) # matriz inversa de A
Ainv

Beta = Ainv %*% b # estimacion por MCO del vector Beta
Beta

# Proyecciones de Y
Yproy = X %*% Beta
Yproy

# Residuos
resid = Y - Yproy
resid

# sumatorias de residuos
sum(resid) # muy cercano a cero!

SCR = sum(resid^2)
SCR

t(resid)%*%resid # SCR de forma matricial

# Estimacion de la varianza del error (varianza de los residuos)
sigma2 = SCR / (n-p)
sigma2

sigma = sqrt(sigma2)
sigma

# Matriz de proyeccion H
H = X %*% Ainv %*% t(X) # matriz de proyeccion H
H

H %*% Y # vector de proyecciones de Y

# Matriz generadora de residuos
diag(n)
M = (diag(n) - H) # matriz identidad de tamaño n = diag(n)
M

M %*% Y # vector de residuos

t(H) %*% M

#########
# Calculo automatico con la funcion linear model

RLM1 = lm (y ~ x1 + x2) # ojo, aqui no se ponen las matrices, sino los vectores de datos

RLM1$coefficients # COEFICIENTES BETAS
RLM1$fitted.values #PROYECCIONES
RLM1$residuals# RESIDUOS

summary(RLM1)

# Matriz de covarianzas del vector Beta
sigma2
Ainv

covar = sigma2 * Ainv # producto escalar por matriz (*)
covar

################
# Pruebas t individuales
Beta
covar

# errores tipicos de cada beta
err.tip1 = sqrt(diag(covar)[1]) # error tipico de beta_0
err.tip2 = sqrt(diag(covar)[2]) # error tipico de beta_1
err.tip3 = sqrt(diag(covar)[3]) # error tipico de beta_2
err.tip1
err.tip2
err.tip3

# estadisticos de contrastes
T.b1 = Beta[1]/err.tip1 # T del beta 0
T.b2 = Beta[2]/err.tip2 # T del beta 1
T.b3 = Beta[3]/err.tip3 # T del beta 2
T.b1
T.b2
T.b3

# P Valores de cada beta
Pvalor.b1 = 2*(pt(T.b1, n-p))
Pvalor.b2 = 2*(1 - pt(T.b2, n-p))
Pvalor.b3 = 2*(1 - pt(T.b3, n-p))
Pvalor.b1 # un poco mayor al 5% - Beta_0 se anula
Pvalor.b2 # casi igual al 5% - Beta_1 se anula
Pvalor.b3 # mayor al 5% - Beta_2 se anula

# PRUEBA F GLOBAL
SCT = sum((Y - mean(Y))^2)
SCT

SCReg = sum((Yproy - mean(Y))^2)
SCReg

SCR

SCReg + SCR # debe ser igual a la SCT

F.test = (SCReg /(p-1)) / (SCR / (n-p))
F.test

Pvalor.F = 1 - pf(F.test,(p-1),(n-p))
Pvalor.F # es menor al 5%, Y si depende del modelo RLM

# COEFICIENTES DE DETERMINACION
R2 = 1 - SCR/SCT
R2

R2adj = 1 - (SCR/(n-p)) / (SCT/(n-1))
R2adj

# PROYECCION EN UN PUNTO DE PREDICCION
x0 = c(1, 95, 25)
x0
Beta
Y0 = x0 %*% Beta
Y0 # proyeccion en el punto X0

# Intervalos de prediccion para la media (Yproy_0)
LIC.Y0 = Y0 - qt(0.975, n-p) * sigma  * sqrt(t(x0) %*% Ainv %*% x0)
LSC.Y0 = Y0 + qt(0.975, n-p) * sigma  * sqrt(t(x0) %*% Ainv %*% x0)
LIC.Y0
LSC.Y0

# Intervalos de prediccion puntual de Y0 
LIC.P = Y0 - qt(0.975, n-p) * sigma  * sqrt(1 + t(x0) %*% Ainv %*% x0)
LSC.P = Y0 + qt(0.975, n-p) * sigma  * sqrt(1 + t(x0) %*% Ainv %*% x0)
LIC.P
LSC.P

# PREDICCION AUTOMATICA (funcion predict)

# IC para la media condicionada
predict(RLM1, newdata = data.frame(x1 = 95, x2 = 25), interval = "confidence", level = 0.95)

# IC para la prediccion puntual
predict(RLM1, newdata = data.frame(x1 = 95, x2 = 25), interval = "prediction", level = 0.95)

# RESUMEN DEL OBJETO LM
summary(RLM1)

# DIAGNOSTICO DEL MODELO (similar a la RLS)
library(lmtest) #este paquete contiene los contrastes de diagnosticos
# debemos tener previamente construido el objeto "lm" (regresion lineal multiple: RLM1)

# Comprobar linealidad (test RESET)
reset(RLM1) # Sale un error en el Pvalor (tamaño muestral es muy pequeño n=5, p=3)

# Comprobar homocedasticidad (test Harrison Mc Cabe)
hmctest(RLM1) #Sale un error en el Pvalor (tamaño muestral es muy pequeño n=5, p=3)

# Comprobar normalidad (test Shapiro wilk)
shapiro.test(resid) #P valor casi igual al 5%, No se puede concluir!! (muy pocos datos)

# Comprobar independencia (Test de Durbin Watson)
dwtest(RLM1) # P valor mayor a 5%, H0 de independencia se cumple

#analisis gráfico
plot(RLM1)

# Diagnostico de colinealidad (factor de inflación de varianza)
library(car)
vif(RLM1) # ambos vif son menores a 5, no existe multicolinealidad (OK)

# MODELO LINEALIZABLE - POLINOMIAL DE GRADO 2
# time = tiempo de la corrida de produccion (minutos)
# size = tamaño de la corridad de produccion (unidades)

time = c(195,215,243,162,185,231,234,166,253,196)
size = c(175,189,344,88,114,338,271,173,284,277)

# generar la variable de grado 2
size2 = size^2

# Modelo: time = Beta_0 + Beta_1 * size + Beta_2 * size2 + E (RLM con p = 3)
# ajuste atomatico

RLM2 = lm (time ~ size + size2) #modelo polinomial de grado 2

RLM2$coefficients # COEFICIENTES BETAS
RLM2$fitted.values #PROYECCIONES
RLM2$residuals# RESIDUOS

summary(RLM2)


# MODELO EXPONENCIAL: time = B_0 * e ^(B_1 * size)

lntime = log(time) # logaritmo natural de tiempo de corridas

RLE = lm (lntime ~ size)

summary(RLE)

Beta0 = exp(5.0008351) # obteniendo el Beta Cero original
Beta0

RLE$fitted.values # proyecciones de log(Y)
Yproy = exp(RLE$fitted.values) # proyecciones de la variable Y


plot(size, time)
points(size, Yproy, col = 2)
