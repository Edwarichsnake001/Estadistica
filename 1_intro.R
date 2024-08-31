
### vectors, data, matrices, subsetting
x=c(2,7,5) #creando un vector por concatenacion
x #tipeamos el nombre del objeto para ver su contenidos
y=seq(from=4,length=3,by=3) #creando un vector a partir de una secuencia
?seq #pidiendo ayuda de la funcion seq
y #devolvemos el contenido del objeto
x+y #las operaciones de hacen elemento por elemento
x/y
x^y
x[2] #seleccionamos el elemento de indice 2
x[2:3] #seleccionamos de manera conjunta dos elementos del vector
x[-2] #eliminamos un elemento del vector
x[-c(1,2)] #eliminamos varios elementos del vector
z=matrix(seq(1,12),4,3) #creamos una matriz
z
z[3:4,2:3] #seleccionamos una parte de una matriz
z[,2:3] #seleccionamos varias columnas consecutivas
z[,1] #seleccionamos una columna y se devuelve como vector
z[,1,drop=FALSE] #seleccionamos una columna y se mantiene como matriz
dim(z) #dimensiones de la matriz
ls() #objetos en memoria (espacio de trabajo)
rm(y) #removemos un objeto de memoria
ls()

### Reading data in packages
library(ISLR) #cargamos una libreria
Auto #base de datos a utilizar incluida en el paquete
names(Auto)
dim(Auto)
class(Auto)
summary(Auto)
plot(Auto$cylinders,Auto$mpg)
plot(Auto$cyl,Auto$mpg)

### Importing data (from excel)
library(readxl) # para otros formatos, buscar la libreria
data = read_xlsx("BDD_produccion.xlsx")
data$Tiempo
summary(data$Tiempo) # resumen dato numerico
data$Linea #variable codificada
summary(data$Linea) # no es corretco
data$Linea = as.factor(data$Linea)
data$Linea #variable como factor
summary(data$Linea) # resumen dato categorico correcto
plot(data$Linea, data$Tiempo)
