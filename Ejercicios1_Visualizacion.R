#Ejercicio 1. Nombre de Variables
#Auria Lucía Jiménez Gutiérrez
library(dslabs)
data(heights)
names(heights)



#Ejercicio 2 Tipos de variables
#Auria Lucía Jiménez Gutiérrez
library(dslabs)
data(heights)
head(heights)
class(heights$sex)
 ##Opción 2: Categórica


#Ejercicio 3. Valores numéricos
#Auria Lucía Jiménez Gutiérrez
library(dslabs)
data(heights)
x <- heights$height
vx <- (unique(x))
length(vx)


#Ejercicio 4. Tablas
#Auria Lucía Jiménez Gutiérrez
library(dslabs)
data(heights)
x <- heights$height
tab <- table(x)
tab

#Ejercicio 5. Indicaciones de variables
#Auria Lucía Jiménez Gutiérrez
library(dslabs)
data(heights)
x <- heights$height
tab <- table(x)
sum(tab==1)


#Ejercicio 6. Tipos de datos - alturas
#Auria Lucía Jiménez Gutiérrez
library(dslabs)
data(heights)
x <- heights$height
tab <- table(x)
sum(tab==1)
#La opción correcta sería
#Opción 1: Es más efectivo considerar las alturas como numéricas dada la cantidad de valores únicos que observamos y el hecho de que si seguimos recolectando datos se observarán aún más.
