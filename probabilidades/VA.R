#PARACTICA DE V.A DESCRETA

#
#
x = c (0, 1, 2, 3, 4, 5)

fx = c(0.1, 0.15, 0.2, 0.17,0.18, 0.20)

#verificar condicion fx

sum(fx) 

#grafico de f(x)
plot(x, fx, type="h",ylim = c(0,max(fx)))

#A partor de fx se puede ontener su comportamiento probabilidtico
#cual es la probabilidad que en la proxima hora suceda:
# 1. un solo fallo p(x=1)
fx[2]
# 2. entre dos a 4 fallos p(x=2 o x=3 o x=4)
fx[3]+fx[4]+fx[5]
# 3. No mas ed 3 fallos: (x=0, x=1, x=2)
fx[1] + fx[2] + fx[3]
# 4. No se presenten 5 fallos
1 - fx[6]
# Calcular la esperazna matemática
em = sum(x*fx)

abline(v = em, col =2)
#lo mas probable es que en una hora determinada existan entre 2 y 3 fallas



#si por cada falla el costo de perdida de conexion es de 2 usd
#y el costo de reconexion es de 50 usd por gora calcule el costo medio total por las fallas

y = 2*x+50

sum(y*fx) #usando definicion de la em
2*em + 50 #usando propiedades de la em

#todos positivos, suma uno

#usando la definicion

sigma2= sum((x-em)^2 * fx)
sigma2

EX2 = sum(x^2 * fx)
EX2

sigma = sqrt(sigma2)
sigma


