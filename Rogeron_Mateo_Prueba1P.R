###########################################
# RESOLUCION PRUEBA PRIMER PARCIAL - ESTADISTICA A0501
# NOMBRES Y APELLIDOS COMPLETOS: MATEO NICOLAY ROGERON MAILA
# NRC: 13477
# FECHA: 2024/05/29
###########################################
# NOTA: # La prueba dura 1 hora (60 minutos) y consta de 10 preguntas aleatorias
# Las preguntas de la prueba se encuentran en  aula virtual.
# Debe resolver en este archivo script y registrar sus respuestas en dicha prueba.
# Al finalizar su prueba, debe guardar el archivo script con sus datos, de la
# siguiente manera: "Apellido_Nombre_Prueba1P.R"
# Luego debe subir este archivo al aula virtual, en un plazo maximo de 10 minutos
# de finalizada la prueba. Si no envia el archivo script, su nota sera de CERO.
# No es necesario copiar los enunciados de los ejercicios.
# Debe interpretar todos los resultados y respuestas usando comentarios.
# Trate en lo posible de NO REDONDEAR los calculos

###########################################
# RESOLUCION PREGUNTA 1

#-->La probabilidad de un evento se obtiene a partir de una partición del espacio muestral

###########################################
# RESOLUCION PREGUNTA 2

#-->Son mutuamente excluyentes

###########################################
# RESOLUCION PREGUNTA 3

tiempo = Envios$Real
dias = Envios$Scheduled
tipo = Envios$Type
status = Envios$Status
min(dias)
max(dias)

plot(dias, tiempo)

table(dias == 4, tiempo)


###########################################
# RESOLUCION PREGUNTA 4

Envios$Scheduled

table(tiempo, dias)

sum(dias<=2, tiempo)/(6)

3/2106



###########################################
# RESOLUCION PREGUNTA 5
Envios$Type

table(status == "Shipping on time", tipo == "Debit")
table(status, tipo)

sum(status=="Shipping on time")

30/92

###########################################
# RESOLUCION PREGUNTA 6

table(status)


###########################################
# RESOLUCION PREGUNTA 7

###########################################
# RESOLUCION PREGUNTA 8

plot(dias, tiempo)
min(status)
max(status)
###########################################
# RESOLUCION PREGUNTA 9

###########################################
# RESOLUCION PREGUNTA 10

