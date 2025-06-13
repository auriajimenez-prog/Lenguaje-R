##Ejercicio 1. Personalización de gráficos - Observa y aprende
#Auria Lucia Jiménez Gutiérrez
library(dslabs)
library(ggplot2)
data("us_contagious_diseases")
state <- dat$state
rate <- dat$count/dat$population*10000*52/dat$weeks_reporting
dat <- us_contagious_diseases |>
  filter(year == 1967 & disease=="Measles" & !is.na(population)) |>
  mutate(rate = rate)
dat |> ggplot(aes(x = reorder(state, rate), y = rate)) +
  geom_bar(stat="identity") +
  coord_flip()


##Ejercicio 2. Personalización de gráficos - Redefiniendo
#Auria Lucia Jiménez Gutiérrez
library(dslabs)
library(ggplot2)
data("us_contagious_diseases")

# Definir 'dat' con mutate() y reordenar estados por tasa en un solo paso
dat <- us_contagious_diseases |>
  filter(year == 1967 & disease == "Measles" & !is.na(population)) |>
  mutate(
    rate = count / population * 10000 * 52 / weeks_reporting,
    state = reorder(state, rate)  # <- Esta es la línea clave que reordena
  )


# Gráfico (ya con estados ordenados por tasa)
ggplot(dat, aes(x = state, y = rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Estado", y = "Tasa (por 10,000 personas)") +
  theme_minimal()


##Ejercicio 3. Mostrando los datos y presonalizando el grafico 
#Auria Lucia Jiménez Gutiérrez

library(dplyr)

library(ggplot2)

library(dslabs)

data("murders")

murders %>% mutate(rate = total/population*100000) %>%
  
  group_by(region) %>%
  
  summarize(avg = mean(rate)) %>%
  
  mutate(region = factor(region)) %>%
  
  ggplot(aes(region, avg)) +
  
  geom_bar(stat="identity") +
  
  ylab("Murder Rate Average")


##Respuesta: Opción 3: No muestra todos los datos. No vemos la variabilidad dentro de una región y es posible que los estados más seguros no estén en el Oeste.


##Ejercicio 4. Haciendo un gráfico de caja
#Auria Lucia Jiménez Gutiérrez


dat <- murders |> 
  mutate(
    rate = total / population * 100000,
    region = reorder(region, rate, FUN = mean)  # Ordenar regiones por tasa media
  )

ggplot(dat, aes(x = region, y = rate)) +
  geom_boxplot() +
  geom_point(color = "blue", size = 3) +
  coord_flip() +
  labs(x = "Región", 
       y = "Tasa de homicidios (por 100,000 personas)",
       title = "Tasas de homicidios por región") +
  theme_minimal()

