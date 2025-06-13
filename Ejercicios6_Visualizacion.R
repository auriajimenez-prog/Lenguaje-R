#Ejercicio 1. Fundamentos de ggplot2
#Auria Lucía Jiménez Gutiérrez

library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)
p <- ggplot(data = murders)
p <- ggplot(murders)
p <- murders %>% ggplot()
class(p)

#Ejercicio 2. Impresión
#Auria Lucía Jiménez Gutiérrez

x <- 2

print(x)
p <- ggplot(murders)
p

###Opción 2: Un gráfico en blanco

#Ejercicio 3. Pipes
#Auria Lucía Jiménez Gutiérrez
data("heights")
p <- heights |> ggplot()


#Ejercicio 4. Capas
#Auria Lucía Jiménez Gutiérrez
data(murders)
murders
##Opción 3: total and population


#Ejercicio 5. Geom_point 1
#Auria Lucía Jiménez Gutiérrez

murders |> ggplot(aes(x =population , y = total)) +
   geom_point()


#Ejercicio 6. Geom_point 2
#Auria Lucía Jiménez Gutiérrez

murders |> ggplot(aes(x =total , y = population)) +
    geom_point()

#Ejercicio 7. Geom_point text
#Auria Lucía Jiménez Gutiérrez

murders %>% ggplot(aes(population, total)) +
    geom_label()

##Opción 1: Necesitamos mapear un carácter a cada punto a través del argumento label en aes.

#Ejercicio 8. Geom_point text
#Auria Lucía Jiménez Gutiérrez

murders %>% ggplot(aes(population, total, label = abb)) +
  geom_label()

#Ejercicio 9. Geom_point colors
#Auria Lucía Jiménez Gutiérrez

murders %>% ggplot(aes(population, total, label = abb)) +
  geom_label(color="blue")

##Opción 4: Usando el argumento color en geom_label porque queremos que todos los colores sean azules, así que no necesitamos mapear colores.

#Ejercicio 10. Geom_point colors 2
#Auria Lucía Jiménez Gutiérrez

murders |> ggplot(aes(population, total, label= abb)) +
    geom_label(color="blue")


#Ejercicio 11. Geom_labels by region
#Auria Lucía Jiménez Gutiérrez
murders |> ggplot(aes(population, total, group=region, color=region)) +
  geom_point()

##Opción 2: Mapeando los colores a través del argumento color de aes porque cada etiqueta necesita un color diferente.



#Ejercicio 12. Geom_labels colors
#Auria Lucía Jiménez Gutiérrez

murders %>% ggplot(aes(population, total, label = abb, color=region)) +
  
  geom_label()

#Ejercicio 13. Log-scale
#Auria Lucía Jiménez Gutiérrez

p <- murders |> ggplot(aes(population, total, label = abb, color = region)) +
    geom_label()

p + scale_x_log10() + scale_y_log10()


#Ejercicio 14. Titulos
#Auria Lucía Jiménez Gutiérrez

library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)
p<- murders |> ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()

p + scale_x_log10() + scale_y_log10()
p + ggtitle("Gun murder data")

#Ejercicio 15. Histogramas
#Auria Lucía Jiménez Gutiérrez
data("heights")
heights
##Opción 3: height

#Ejercicio 16. Un segundo ejemplo
#Auria Lucía Jiménez Gutiérrez

library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
p<- heights |> ggplot(aes(x=height)) 


#Ejercicio 17. Histograma 2
#Auria Lucía Jiménez Gutiérrez

library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
p<- heights |> ggplot(aes(x=height)) 
p + geom_histogram()

#Ejercicio 18. Ancho de bins del Histograma 
#Auria Lucía Jiménez Gutiérrez
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
p<- heights |> ggplot(aes(x=height)) 
p + geom_histogram()
p + geom_histogram(binwidth = 1)


#Ejercicio 19. Gráfico de densidad suavizada
#Auria Lucía Jiménez Gutiérrez
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
p <- heights |>
  ggplot(aes(height)) 

p + geom_density()

#Ejercicio 20. Dos Gráficos de densidad suavizada
#Auria Lucía Jiménez Gutiérrez  
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
p <- heights |>
  ggplot(aes(height, group = sex)) 

p + geom_density()

#Ejercicio 21. Dos Gráficos de densidad suavizada 2
#Auria Lucía Jiménez Gutiérrez 

library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
p <- heights |>
  ggplot(aes(height, color = sex)) 

p + geom_density()

#Ejercicio 22. Dos Gráficos de densidad suavizada 3
#Auria Lucía Jiménez Gutiérrez 

library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
p <- heights |>
  ggplot(aes(height, color = sex)) 

p + geom_density(alpha = 0.2)