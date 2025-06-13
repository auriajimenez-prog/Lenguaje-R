#Ejercicio 1. Longitudes de vectores
#Auria Lucía Jiménez Gutiérrez
library(dslabs)
data(heights)
indexm <- heights$sex == "Male"
male <- heights$height[indexm]
indexf <- heights$sex=="Female"
female <- heights$height[indexf]
length(male)
length(female)


#Ejercicio 2. Percentiles
#Auria Lucía Jiménez Gutiérrez
male <- c(69,80,72,71,76)
female <- c(63,68,65,67,69)
female_percentiles <- quantile(female, seq(0.1, 0.9, 0.2))
male_percentiles <- quantile(male, seq(0.1, 0.9, 0.2))
df <- data.frame(female = female_percentiles, male = male_percentiles)
df

#Ejercicio 3. Interpretando Boxplots - 1
#Auria Lucía Jiménez Gutiérrez

#¿Qué continente tiene el país con el mayor tamaño de población?
  
 # Instrucciones
#Respuesta es
#Opción 3: Asia


#Ejercicio 4. Interpretando Boxplots - 2
#Auria Lucía Jiménez Gutiérrez

#¿Qué continente tiene la mediana de población más grande?
#Respuesta es
#Opción 1: Africa


#Ejercicio 5. Interpretando Boxplots - 3
#Auria Lucía Jiménez Gutiérrez

#Al millón más cercano, ¿cuál es el tamaño mediano de la población para África?
#Respuesta es
#Opción 5: 1 Millones

#Ejercicio 6. Cuartiles bajos
#Auria Lucía Jiménez Gutiérrez

#informa aproximadamente qué proporción de países en Europa tienen poblaciones por debajo de 14 millones:
#Respuesta es
#Opción 1: 0.75

#Ejercicio 7. Rango Inter-Cuantil (IQR)
#Auria Lucía Jiménez Gutiérrez

#¿qué continente mostrado a continuación tiene el rango intercuartílico más grande para log(población)?
#Respuesta es

#Opción 3: Asia




library(dplyr)
datos %>%
  group_by(continent) %>%
  summarise(max_population = max(population)) %>%
  arrange(desc(max_population))
