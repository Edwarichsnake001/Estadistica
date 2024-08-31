library(ISLR)

datos = Auto
mpg = datos$mpg

#GRAFICOS DESCRIPTIVOS

min(mpg)
max(mpg)

stripchart(mpg, method = "stack", at =0)

?hist #funcion para hacer histogramas

hist(mpg) #por defecto se utiliza el metodo de Stugges para el numero de clases

hist(mpg, breaks = seq(0,50,1))
histo = hist(mpg, breaks = c(0,10,20,30,40,50),
    main = "HISTOGRAMA: millas/galon",
    col = "blue", ylab = "Frecuencia)")

n= sum(histo$counts)
fr = 100 * histo$counts/n
fr

#OJIVA DE FRECUENCIA

Facum = cumsum (histo$counts)
Facum

plot(histo$mids, Facum, type = "l")
plot(histo$mids, 100 * Facum/n, type = "l",
     main = "ojiva de frecuencia",col = "red", xlab = "Marcas de clase",
     ylim = c(0,100), ylab = "Frecuencuas relat. acum (x)")











