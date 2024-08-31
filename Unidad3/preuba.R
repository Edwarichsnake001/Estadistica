##Prueba sorpresa

##Literal 16.1

# Prueba de signo para comprobar si la media del tiempo de espera es mayor a 20 minutos

# Datos de tiempo de espera (en minutos) de los pacientes
tiempo_espera <- c(17, 15, 20, 20, 32, 28, 12, 26, 25, 25, 35, 24)

# Probar si la media del tiempo de espera es mayor a 20 minutos
resultado <- SIGN.test(tiempo_espera, md=20, alternative="greater", conf.level=0.95)

# Mostrar el resultado
print(resultado)

## x = 7 CON UN VALOR P DE 0.1719; AL SER MAYOR AL NIVEL DE SIGNIFICANCIA DE 0.05, LA H0 SE NO SE RECHAZA
##SIGNIFICANDO QUE EL TIEMPO DE ESPERA NO ES MAYOR A 20 MINUTOS

##LITERAL 16.10

antes <- c(66, 80, 69, 52, 75)
despues <- c(71, 82, 68, 56, 73)

# Prueba de Wilcoxon para datos pareados
resultado <- wilcox.test(despues, antes, paired = TRUE, alternative = "greater", conf.level = 0.95)

# Mostrar el resultado
print(resultado)


#H0 NO SE RECHAZA

##16.16

con_tratamiento <- c(2.1, 5.3, 1.4, 4.6, 0.9)
sin_tratamiento <- c(1.9, 0.5, 2.8, 3.1)


resultado <- wilcox.test(con_tratamiento, sin_tratamiento, alternative = "greater", conf.level = 0.95)

# Mostrar el resultado
print(resultado)


##############################################
# CONTRASTES NO PARAMETRICOS
##############################################

# Prueba de Kruskal-Wallis para comparar los tiempos de operación de tres tipos de calculadoras

# Datos de tiempos de operación en horas para las calculadoras A, B y C
calculadora_A <- c(4.9, 6.1, 4.3, 4.6, 5.2)
calculadora_B <- c(5.5, 5.4, 6.2, 5.8, 5.5, 5.2, 4.8)
calculadora_C <- c(6.4, 6.8, 5.6, 6.5, 6.3, 6.6)

# Realizar la prueba de Kruskal-Wallis
resultado <- kruskal.test(list(calculadora_A, calculadora_B, calculadora_C))

# Mostrar el resultado
print(resultado)

# Interpretación del resultado:
if (resultado$p.value < 0.01) {
  cat("Rechazamos la hipótesis nula: Los tiempos de operación para las tres calculadoras son significativamente diferentes.\n")
} else {
  cat("No se rechaza la hipótesis nula: No hay suficiente evidencia para concluir que los tiempos de operación sean diferentes.\n")
}


