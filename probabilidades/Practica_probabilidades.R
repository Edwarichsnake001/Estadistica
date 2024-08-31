#PRACTICA DE PROBABILIDADES
#TECNICAS DE CONTEO

?choose #calcular combinaciones

permut = function(n,r){factorial(n)/factorial(n-r)} #construyendo funcion

combin = function(n,r){factorial(n)/(factorial(r)*factorial(n-r))}

permut(29,3)

combin(29,3)

library(ISLR)
?Auto

#tabla de contingencia

attach(Auto)

tabla = table(origin,year)

addmargins(tabla)

round(addmargins(prop.table(tabla)),3) #prop.table = tabla de probabilidades

round(addmargins(prop.table(tabla, 1)),3) #fila
round(addmargins(prop.table(tabla, 2)),3) #columna
