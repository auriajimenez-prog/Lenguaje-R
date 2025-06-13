#Ejercicio 1. Proporciones
#Auria Lucía Jiménez Gutiérrez

library(dslabs)
data(heights)
x <- c(heights$height)
xf <- x > 69 & x <= 72     
xf
mean(xf)


##Ejercicio 2. Distribuciones promedio y estandar
#Auria Lucía Jiménez Gutiérrez

library(dslabs)
data(heights)
x <- c(heights$height)
avg <- mean(x)
stdev <- sd(x)
prop <- pnorm(72, avg, stdev) - pnorm(69, avg, stdev)
prop


#Ejercicio 3. Aproximaciones
#Auria Lucía Jiménez Gutiérrez

library(dslabs)
data(heights)
x <- c(heights$height)
approx <- x > 79 & x <= 81     
approx <- mean(approx)

avg <- mean(x)
stdev <- sd(x)
prop <- pnorm(81, avg, stdev) - pnorm(79, avg, stdev)
prop
propp <- approx/prop
propp

#Ejercicio 4. Jugadores de siete pies y la NBA
#Auria Lucía Jiménez Gutiérrez
library(dslabs)
data(heights)
x <- c(heights$height/12)
x
avg <- mean(x)
stdev <- sd(x)
prop <- 1- pnorm(7, avg, stdev) 


#Ejercicio 5. Estimación del número de jugadores de siete pies 
#Auria Lucía Jiménez Gutiérrez

library(dslabs)
data(heights)
heights
h <- heights |> filter(sex=="Male")
x <- c(h$height/12)
avg <- mean(x)
stdev <- sd(x)
p <- 1-pnorm(7, avg, stdev)
round(p * 10^9)

#Ejercicio 6. ¿Cuantos jugadores de siete pies hay en la NBA?
#Auria Lucía Jiménez Gutiérrez

library(dslabs)
data(heights)
heights
h <- heights |> filter(sex=="Male")
x <- c(h$height/12)
avg <- mean(x)
stdev <- sd(x)
p <- 1-pnorm(7, avg, stdev)
n <- round(p * 10^9)
10/n


#Ejercicio 7. Altura de Lebron James
#Auria Lucía Jiménez Gutiérrez
p <- 1 - pnorm(7*12+8, 69, 3)
N <- p * 10^9
150/N

#Ejercicio 8. Interpretación
#Auria Lucía Jiménez Gutiérrez

##Opción 4: Como se ve en el ejercicio 3, la aproximación normal tiende a sobreestimar los valores extremos. Es posible que haya menos jugadores de siete pies de lo que predijimos.
