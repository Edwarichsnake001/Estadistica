##EJERCICIOS PROBABILIDAD DISCRETA

#Binominal (5.9 Walpole)

n=15
p=0.25
# 	a) P(de 3 a 6 ponchaduras) = P(3 <= x <= 6) = P(x=3) + p(x=4)+p(x=5) + p(x=6)

#calculo normal
p3= choose(15,3)*(0.25^3)*(0.75^12)
p3

p4= choose(15,4)*(0.25^4)*(0.75^11)
p4

p5= choose(15,5)*(0.25^5)*(0.75^10)
p5

p6=choose(15,6)*(0.25^6)*(0.75^9)
p6

p3+p4+p5+p6

#usando funcones de R

dbinom(x=3,size=15,p=0.25)
dbinom(x=4,size=15,p=0.25)
dbinom(x=5,size=15,p=0.25)
dbinom(x=6,size=15,p=0.25)

sum(dbinom(x = 3:6, size = 15, p = 0.25))

pbinom(6, size = 15, p = 0.25) - pbinom(2,size = 15,p=0.25)

p0=dbinom(x=0,size=15,p=0.25)
p1=dbinom(x=1,size=15,p=0.25)
p2=dbinom(x=2,size=15,p=0.25)
p3=dbinom(x=3,size=15,p=0.25)

#literal c --> P(x >5) = p(6)
1 - (p0+p1+p2+p3+p4+p5)
sum(dbinom(x = 6:15, size = 15, p = 0.25))



#EJERCICIO 2 --> distribucion hipergeometrica

N = 10
M = 7
n2 = 4
x = 4

p4 = choose(7,4) * choose(3,0) / choose(10, 4)
p4

#funciones de R
dhyper(x, M, N-M, n2)


#literal b

p0= choose(3,0)*choose(7,4) / choose(10,4)
p0

p1=choose(3,1)*choose(7,3) / choose(10,4)
p1

p2=choose(3,2)*choose(7,2) / choose(10,4)
p2

p0+p1+p2

##N y n2 se mantienen
N=10
n2=4
M=3
x=2
sum(dhyper(0:x,M,N-M,n2))
phyper(x,M,N-M,n2)


#EJERCICIO 3

lamda = 3
x=5
p5 = (exp(-lamda)*lamda^x)/factorial(x)
p5

#literal b

p0 = (exp(-lamda)*lamda^0)/factorial(0)
p0

p1 = (exp(-lamda)*lamda^1)/factorial(1)
p1

p2 = (exp(-lamda)*lamda^2)/factorial(2)
p2

p0+p1+p2

sum(dpois(0:2,lamda))
ppois(2, lamda)


##continua
#literal A --\x<8.8
A=7
B=10
fx = function(x){
  (x/x)/(B-A)
}
prob = integrate(fx, 7, 8.8)$value
prob


#Notal en las distribuciones continuas no intera la f(x) sino la acumulada
?punif

f8=punif(8.8,7,10)
f7 = punif(7,7,10)
prob = f8 - f7
prob

#Literak B P(7.4 <x <9.5)

prop = integrate(fx, 7.4, 9.5)$value
prop


##Literal C al menos 8.5 --> P(X > 8.5) = 
prop = integrate(fx, 8.5, 10)$value
prob

f10 = punif(8.5,7,10)
1-f10


#Ejercicio 2
lambda = 1/4
fx = function(x){
  lambda*exp(-lambda*x)
}
curve (fx, 0, 10)

abline(v=3, col=2)


p3 = integrate(fx,0,3)$value
p3

pexp(3, lambda)

p = p3
n=6

1-pbinom(3,6,p3)


#EJERCICIO 3


lambda = 1/5

pexp(5, lambda)


mu = 0
sigma = 1

fx = function(x){
  (1/sqrt(2*pi*sigma^2))*exp(-(x-mu)^2/2*sigma^2)
}
curve(fx,4,-4)


abline(v=1.43, col = 2)
integrate(fx, -Inf, 1.43)$value

pnorm(1.43, mu, sigma)

#a la derecha de z=-0.89


##Binominal negativa

x=6
k=4
p=0.55

p1 = choose(x-1,k-1)*p^k*(1-p)^(x-k)
p1

p2 = choose(4-1,k-1)*p^k*(1-p)^(4-k)
p2

p3 = choose(5-1,k-1)*p^k*(1-p)^(5-k)
p3

p4 = choose(7-1,k-1)*p^k*(1-p)^(7-k)
p4

n2 = sum(p1,p2,p3,p4)
n2

x0 = 3
x1 = 4
x2 = 5
p = 0.55
k = 3


p5 = choose(x0 - 1,k-1)*p^k*(1-p)^(x0-k)
p5

p6 = choose(x1 - 1,k-1)*p^k*(1-p)^(x1-k)
p6

p7 = choose(x2 - 1,k-1)*p^k*(1-p)^(x2-k)
p7

n = sum(p5,p6,p7)
n



