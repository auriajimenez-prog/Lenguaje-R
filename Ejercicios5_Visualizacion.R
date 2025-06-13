#Ejercicio 1. Explorando el conjunto de datos Galton: promedio y media
#Auria Lucía Jiménez Gutiérrez
library(HistData)
data(Galton)
Galton
x <- Galton$child
mean(x)
median(x)

#Ejercicio 2. Explorando el conjunto de datos Galton DE y MAD
#Auria Lucía Jiménez Gutiérrez
library(HistData)
data(Galton)
Galton
x <- Galton$child
sd(x)
mad(x)


#Ejercicio 3. Impacto del error en el promedio
#Auria Lucía Jiménez Gutiérrez

library(HistData)
data(Galton)
x <- Galton$child
p <- mean(x)
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
p_e <- mean(x_with_error)
dif <- p_e - p
dif


#Ejercicio 4. Impacto del error en la DE
#Auria Lucía Jiménez Gutiérrez

library(HistData)
data(Galton)
x <- Galton$child
p <- sd(x)
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
p_e <- sd(x_with_error)
dif <- p_e - p
dif

#Ejercicio 5. Impacto del error en la mediana
#Auria Lucía Jiménez Gutiérrez

library(HistData)
data(Galton)
x <- Galton$child
p <- median(x)
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
p_e <- median(x_with_error)
dif <- p_e - p
dif

#Ejercicio 6. Impacto del error en la MAD
#Auria Lucía Jiménez Gutiérrez

library(HistData)
data(Galton)
x <- Galton$child
p <- mad(x)
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
p_e <- mad(x_with_error)
dif <- p_e - p
dif

#Ejercicio 7. Utilidad del Analisis exploratorio de datos (EDA)
#Auria Lucía Jiménez Gutiérrez

##Opción 3: Un diagrama de caja, histograma o gráfico qq revelaría claramente un valor atípico.

#Ejercicio 8. Utilizando EDA para explorar cambios
#Auria Lucía Jiménez Gutiérrez

error_avg <- function(k){
  x <- c(seq(5,10,0.5))
  x[1]<- k
  p <- mean(x)
  return (p)
}
error_avg(10000)
error_avg(-10000)
