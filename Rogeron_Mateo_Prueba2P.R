###########################################
# RESOLUCION PRUEBA SEGUNDO PARCIAL - ESTADISTICA A0501
# NOMBRES Y APELLIDOS COMPLETOS: Mateo Nicolay Rogeron Maila
# NRC: 13477
# FECHA: 2024/07/15
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
# RESOLUCION PREGUNTA 1
# Datos proporcionados
n = 10
x_bar = 5
varianza = 0.25

s = sqrt(varianza)

SE = s / sqrt(n)

SE

t_value = qt(0.975, df = n-1)  
IC_lower = x_bar - t_value 
IC_upper = x_bar + t_value 

IC_lower
IC_upper


# INDIQUE EL LITERAL DE SU RESPUESTA AQUI: A
  
###########################################
# RESOLUCION PREGUNTA 2

#z estamos hablando de un chi cuadrado:
n2 = 80
d = 10

p_hat = d / n2

SE = sqrt(p_hat * (1 - p_hat) / n2)
#opcion a descartada. Quedan b y c


#z debe tener un valor de confianza igual al 95%

confianza = 0.95
confianza2 = 0.99
sigma = 1 - confianza
alpha = 1 - confianza2

# Valor Z
Z1 = qnorm(1 - sigma / 2)

Z2 = qnorm(1-alpha/2)

#NOTA: la respuesta sería B si la v.c es igual a 99% pero la directriz fue usar en todas un nivel de confianza de 95%

lower_bound = p_hat - Z * SE
upper_bound = p_hat + Z * SE


# INDIQUE EL LITERAL DE SU RESPUESTA AQUI: C.

###########################################
# RESOLUCION PREGUNTA 3

# INDIQUE EL LITERAL DE SU RESPUESTA AQUI:

###########################################
# RESOLUCION PREGUNTA 4
# INDIQUE EL LITERAL DE SU RESPUESTA AQUI:

###########################################
# RESOLUCION PREGUNTA 5
# INDIQUE EL LITERAL DE SU RESPUESTA AQUI:



#Si se quiere probar si es igual o distinto de 0, es una prueba bilateral. Mediante el sistema de hipótesis

#Poblaciones normales, con varaianzas conocidas

#t_tesdt

n1=12
prom1 = 37900
s1 = 5100

n2 = 12
prom2 = 39800
s2 = 5900

var.c = ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1 +n2 - 2)

desv.c = sqrt(var.c)

t_prom = (prom1 - prom2) / (desv.c * sqrt(1/n1 + 1/n2))


t_prom = (prom1 - prom2) / (desv.c * sqrt(1/n1 + 1/n2))
t_prom

#probabilidad de ver la discrepancia
pt(t_prom, n1+n2-2)

pvalor= 2 * pt(t_prom, n1+n2-2)
pvalor






