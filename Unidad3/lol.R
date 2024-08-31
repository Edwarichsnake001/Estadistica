Oi = c(14,18,32,20)

n= sum(Oi)

Ei = c(n/4,n/4,n/4,n/4)

Ei

Q = sum((Oi-Ei)^2 / Ei)
Q

Pvalor = 1-pchisq(Q,3)
Pvalor

#Como el valor p es menor al 5% (0.05). entonces las calificaciones no se distribuyen de forma uniforme.

#EJ 10.84

?dhyper

p0 = dhyper(x=0,5,3,3)
p1 = dhyper(x=1,5,3,3)
p2 = dhyper(x=2,5,3,3)
p3 = dhyper(x=3,5,3,3)

Oi = c(1,31,55,25)

p0+p1+p2+p3 #la suma debe ser uno

prob = c(p0,p1,p2,p3)
prob

#las frecuencias esperadas Ei, se obtinen multiplicando las probabilidades teóricas de Ei

Ei = 112*prob
Ei

Q = sum((Oi-Ei)^2 / Ei)
Q

Pvalor = 1- pchisq(Q,3)
Pvalor

## como el p valor supera el 5%. H0 no se rechaza.


##Ejercicio 10.93

Oi = matrix(c(162,310,258,280,
            118,196,193,175,
            451,996,458,390,
            18,25,10,19), ncol = 4)

colnames(Oi) = c("Asalto", "Robo", "Hurto", "Homicidio")
rownames(Oi) = c("D1","D2","D3","D4")
Oi

#Usando la funcion chisq.test de r 

chisq.test(Oi)

#Al ser menor el pvalor a 0.01 enonces el crimen si depende del lugar de la ciudad



prueba = chisq.test(Oi)
prueba

prueba$expected

prueba$residuals
prueba$statistic

prueba$p.value

addmargins(Oi)

Oi <- matrix(c(72, 63, 47, 40,   # Algo mejor
               144, 135, 100, 105, # Igual
               84, 102, 53, 55),   # No tan bueno
             ncol = 3, byrow = FALSE)

colnames(Oi) <- c("Algo mejor", "Igual", "No tan bueno")
rownames(Oi) <- c("1980 Enero", "1980 Mayo", "1980 Septiembre", "1981 Enero")
Oi

# Realizar la prueba de Chi-cuadrado
prueba <- chisq.test(Oi)
prueba

# Mostrar las frecuencias esperadas
prueba$expected

# Mostrar los residuos
prueba$residuals

# Mostrar el valor del estadístico Chi-cuadrado
prueba$statistic

# Mostrar el p-valor
prueba$p.value

