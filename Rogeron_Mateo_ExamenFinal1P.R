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
# Cada respuesta del cuestionario debe tener su resoluci?n 
# en este archivo de R. Las resoluciones deben realizarse 
# ?nicamente usando c?lculos manuales (no debe usar las funciones 
# de R).

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

s <- sqrt(varianza)

SE <- s / sqrt(n)

SE

t_value <- qt(0.975, df = n-1)  
IC_lower <- x_bar - t_value 
IC_upper <- x_bar + t_value 

IC_lower
IC_upper


# INDIQUE EL LITERAL DE SU RESPUESTA AQUI: A
  
###########################################
# RESOLUCION PREGUNTA 2

#z estamos hablando de un chi cuadrado:
n2 = 80
m = rnorm(n2, mean =3, sd = sqrt(4))

mu = mean(m)
sigma = sd(m)

n2 = 80
z = (m - mu) / sigma



# INDIQUE EL LITERAL DE SU RESPUESTA AQUI: C

###########################################
# RESOLUCION PREGUNTA 3

# INDIQUE EL LITERAL DE SU RESPUESTA AQUI:

###########################################
# RESOLUCION PREGUNTA 4
# INDIQUE EL LITERAL DE SU RESPUESTA AQUI:

###########################################
# RESOLUCION PREGUNTA 5
# INDIQUE EL LITERAL DE SU RESPUESTA AQUI:


