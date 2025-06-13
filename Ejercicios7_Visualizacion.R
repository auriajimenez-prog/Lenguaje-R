#Ejercicio 7. Explorando el conjunto de datos Gapminder


#Ejercicio 1. Centro Nacional de Estadísticas de Salud
library(NHANES)

data(NHANES)

library(dslabs)

data(na_example)

mean(na_example)

sd(na_example)

mean(na_example, na.rm = TRUE)

sd(na_example, na.rm = TRUE)

##El código funciono obtuve el promedio y la desviación estandar

#Ejercicio 2. Presión arterial 1
#Auria Lucia Jiménez Gutiérrez
library(NHANES)
library(dslabs)
library(dplyr)
data(NHANES)
NHANES
tab <- NHANES |> filter(AgeDecade == " 20-29" & Gender=="female")
head(tab)

#Ejercicio 3. Presión arterial 2
#Auria Lucia Jiménez Gutiérrez


tab <- NHANES[complete.cases(NHANES$BPSysAve), ] |> 
  filter(AgeDecade == " 20-29" & Gender=="female") |>
  summarise(average = mean(BPSysAve), standard_deviation = sd(BPSysAve), na.rm=TRUE) 
tab

#Ejercicio 4. Resumiendo promedios
#Auria Lucia Jiménez Gutiérrez

tab <- NHANES |>
  filter(complete.cases(BPSysAve)) |>
  filter(AgeDecade == " 20-29" & Gender == "female") |>
  summarise(average = mean(BPSysAve, na.rm = TRUE)) |>
  pull(average)
tab

#Ejercicio 5. Minimo y maximo
#Auria Lucia Jiménez Gutiérrez
tab <- NHANES |>
  filter(complete.cases(BPSysAve)) |>
  filter(AgeDecade == " 20-29" & Gender == "female") |>
  summarise(minbp = min(BPSysAve, na.rm = TRUE),
            maxbp = max(BPSysAve, na.rm = TRUE)) 
tab

#Ejercicio 6. Group by
#Auria Lucia Jiménez Gutiérrez

library(dplyr)

tab <- NHANES |>
  filter(Gender == "female" & !is.na(BPSysAve)) |>  # Filtramos mujeres y eliminamos NA en BPSysAve
  group_by(AgeDecade) |>  # Agrupamos por década de edad
  summarize(
    average = mean(BPSysAve, na.rm = TRUE),  # Media (na.rm=TRUE por redundancia)
    standard_deviation = sd(BPSysAve, na.rm = TRUE)  # Desviación estándar
  )

tab

#Ejercicio 7. Group by exmaple 2
#Auria Lucia Jiménez Gutiérrez
library(dplyr)

tab <- NHANES |>
  filter(Gender == "male" & !is.na(BPSysAve)) |>  # Filtramos hombres y eliminamos NA en BPSysAve
  group_by(AgeDecade) |>  # Agrupamos por década de edad
  summarize(
    average = mean(BPSysAve, na.rm = TRUE),  # Media (na.rm=TRUE por redundancia)
    standard_deviation = sd(BPSysAve, na.rm = TRUE)  # Desviación estándar
  )

tab

#Ejercicio 8. Group by exmaple 3
#Auria Lucia Jiménez Gutiérrez
library(dplyr)
tab <- NHANES |>
  filter(!is.na(BPSysAve)) |>  # Filtramos hombres y eliminamos NA en BPSysAve
  group_by(AgeDecade, Gender) |>  # Agrupamos por década de edad
  summarize(
    average = mean(BPSysAve, na.rm = TRUE),  # Media (na.rm=TRUE por redundancia)
    standard_deviation = sd(BPSysAve, na.rm = TRUE)  # Desviación estándar
  )

tab

#Ejercicio 9. Arrange
#Auria Lucia Jiménez Gutiérrez

tab <- NHANES |>
  filter(Gender == "male" & AgeDecade == " 40-49" & !is.na(BPSysAve)) |>
  group_by(Race1) |>
  summarize(
    average = mean(BPSysAve, na.rm = TRUE),
    standard_deviation = sd(BPSysAve, na.rm = TRUE)
  ) |>
  arrange(average)  # Ordena de menor a mayor según la media
tab