#Ejercicio 1. Distribuciones-1
#Auria Lucía Jiménez Gutiérrez

library(dplyr)
library(dslabs)
data(murders)

proporcion <- murders %>% 
  filter(region == "North Central") %>% 
  nrow() / nrow(murders)

round(proporcion / 0.05) * 0.05 * 100  # 25%

##Opción 3: 25%


#Ejercicio 2. Distribuciones-2
#Auria Lucía Jiménez Gutiérrez

library(dplyr)
data(murders)
library(ggplot2)
ggplot(murders, aes(x = region)) + 
  geom_bar() + 
  labs(title = "Distribución de estados por región")

##Opción 2: El gráfico muestra solo cuatro números con un diagrama de barras.

#Ejercicio 3. Función de Distribución Acumulativa Empirica (eCDF)
#Auria Lucía Jiménez Gutiérrez
data("heights")
l <-heights |> filter(sex=="Male")
x <- heights |> filter(sex=="Male" & height<75) 
#p <- nrow(ecdf(x$height))/nrow(ecdf(l$height))*100
p <- round(nrow(x)/nrow(l)*100)
print(paste("Porcentaje de hombres <75 pulgadas:", round(p), "%"))

#Opción 2: 95%


#Ejercicio 4. eCDF Alturas masculinas
#Auria Lucía Jiménez Gutiérrez
data("heights")
boxplot(l)
x <- heights |> filter(sex=="Male" & height<69) |> nrow()
y <- heights |> filter(sex=="Male" & height>69) |> nrow()
x
y

#Opción 3: 69 pulgadas

#Ejercicio 5. eCDF de Tasas de Asesinatos
#Auria Lucía Jiménez Gutiérrez
data("murders")
murders
x <- murders |> filter((total/ population * 100000)>10) 
nrow(x)

##Opción 1: 1

#Ejercicio 6. eCDF de Tasas de Asesinatos -2
#Auria Lucía Jiménez Gutiérrez
data("murders")
murders
x <- murders |> filter((total/ population * 100000)>=5)
nrow(x)

##Opción 2: La mayoría de los estados tienen tasas de asesinato por debajo de 2 por cada 100,000.
##Opción 4: Con la excepción de 4 estados, las tasas de asesinato están por debajo de 5 por cada 100,000.

#Ejercicio 7. Histogramas
#Auria Lucía Jiménez Gutiérrez
data("heights")
nrow(heights)
library(ggplot2)
alturas <- c(heights |> filter(sex=="Male" & height>=62.5 & height<=65.5))
ggplot(heights,aes(x=alturas))+
  geom_histogram(aes(y=..density..),binwidth=5, fill="lightblue",color="black",alpha=0.6)+
  geom_density(color="blue", size=1)+
  labs(title="Histograma de variable altura", x="Altura (cm)", y="Frecuencia")+
theme_minimal()

x <- heights |> filter(sex=="Male" & height>=62.5 & height<=65.5)
nrow(x)

##Opción 3: 58

#Ejercicio 8. Histogramas-2
#Auria Lucía Jiménez Gutiérrez
data("heights")
x <- heights |> filter(sex=="Male" & height < 60) |> nrow()
y <- heights |> filter(sex=="Male") |> nrow()
p <- x/y*100

##Opción 1: 1%

#Ejercicio 9. Gráficos de densidad
#Auria Lucía Jiménez Gutiérrez
data("murders")
murders
x <- murders |> filter(population > 10000000)
p <- nrow(x)/nrow(murders)
p

##Opción 2: 0.15


#Ejercicio 10. Gráficos de densidad-2
#Auria Lucía Jiménez Gutiérrez
library(ggplot2)
murders
datos <- murders$population

# Gráfico 1: Escala lineal, suavizado por defecto
p1 <- ggplot(data.frame(x = datos), aes(x)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  ggtitle("Escala lineal")

# Gráfico 2: Escala lineal, suavizado insuficiente
p2 <- ggplot(data.frame(x = datos), aes(x)) + 
  geom_density(bw = 0.1, fill = "red", alpha = 0.5) +  # bw pequeño
  ggtitle("Suavizado de menos")

# Gráfico 3: Escala logarítmica, sobresuavizado
p3 <- ggplot(data.frame(x = datos), aes(x)) + 
  geom_density(bw = 1.5, fill = "green", alpha = 0.5) +  # bw grande
  scale_x_log10() + 
  ggtitle("Sobresuavizado (escala log)")

library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

##Opción 4: Son el mismo conjunto de datos, pero el primero no tiene el eje x en escala logarítmica, el segundo suaviza poco y el tercero suaviza demasiado.
