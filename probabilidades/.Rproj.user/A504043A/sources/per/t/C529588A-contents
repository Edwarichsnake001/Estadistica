#PRACTICA DE VARIABLES ALEATORIAS CONTINUAS

#la probabilidad de que x sea igial a un valor es 0, por lo que la probabilidad sea P(a<x<=b) o viceverse

#f(x) = 3/16 (3-x^2), -1 <= x <= 1 o sino 

#grafico funcion densicdad
#esperanza matematica --- valor mas probable


fdp = function(x){
  (3/16)*(3-x^2)
}
  
curve(fdp, -1, 1)

#el grafico indica dque la funcion de probabilidad es positiva
#verificar si el area bajo la curva es igual a 1

?integrate
integrate(fdp,1,-1)

#si no queremos el error
integrate(fdp,-1,1)$value #demostramos que la integral de f(x) para -1<=x<=1 es igual 1

#calcular la probabilidad de que x sea menoa a 1/2

prob = integrate(fdp,-1,1/2)$value
prob

#calcular la probabilidad de que  |x|>0.8

prob2 = 1 - integrate(fdp,-0.8,0.8)$value
prob2

#esperanza matemática
x.fdp = function(x){
  (3/16)*(3-x^2)
}

mu = integrate(x.fdp, -1,1)$value

#varianza
sigma.fdp = function(x) {
  (x-mu)^2 * 3/16 * (3-x^2)
}

sigma2 = integrate(sigma.fdp, -1,1)$value
sigma2




x2.fdp = function(x){
  (x)^2 * 3/16 * (3-x^2)
}

EX2 = integrate(x2.fdp,-1,1)$value 
EX2


#funcion de distribucion acumulada

Fx = function(x){
  3/16 * ((-x)^3/3 + 3*x + 8/3)
}

Fx(1/2)


#P(-0.8 <= X <= 0.8)
Fx(0.8) - Fx(-0.8)
1 - (Fx(0.8) - Fx(-0.8))








