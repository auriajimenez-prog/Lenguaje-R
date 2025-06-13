#Ejercicio 1. Esperanza de Vida vs Fertilidad parte 1
#Auria Lucia Jiménez Gutiérrez

##x=fertility y=infant_mortality
library(dslabs)
data("gapminder")
gapminder
gapminder_2012 <- gapminder |> filter(year==2012 & continent=="Africa")
gapminder_2012  |>
  ggplot(aes(x=fertility, y=life_expectancy))+
  geom_point(alpha=0.7, size=3)+
  labs(title="Relación entre Tasa de fertilidad y Esperanza de vida en Africa en el 2012",
       x="Tasa de fertilidad",
       y="Esperanza de vida")+
  theme_minimal()+
  theme(legend.position="right")+
  geom_smooth(method="lm", color="black")+
  annotate("text", x=7, y=50, label="Linea de regresión", color="black", size =4)

#Ejercicio 2. Esperanza de Vida vs Fertilidad parte 2 Coloreando tu gráfico
#Auria Lucia Jiménez Gutiérrez

library(dslabs)
data("gapminder")
gapminder
gapminder_2012 <- gapminder |> filter(year==2012 & continent=="Africa")
gapminder_2012  |>
  ggplot(aes(x=fertility, y=life_expectancy, color="pink"))+
  geom_point(alpha=0.7, size=2)+
  labs(title="Relación entre Tasa de fertilidad y Esperanza de vida en Africa en el 2012",
       x="Tasa de fertilidad",
       y="Esperanza de vida")+
  theme_minimal()+
  theme(legend.position="right")+
  geom_smooth(method="lm", color="purple")+
  annotate("text", x=7, y=50, label="Linea de regresión", color="lightblue", alpha=0.5, size =4)

#Ejercicio 3. Esperanza de Vida vs Fertilidad parte 3 seleccionando pais y región
#Auria Lucia Jiménez Gutiérrez

library(dslabs)
data("gapminder")df <- gapminder |> 
  filter(year==2012 & continent=="Africa" & fertility <=3 & life_expectancy>=70) |>
  select(country, region)

df

#Ejercicio 4. Esperanza de Vida y la Guerra de Vietnam parte 1
#Auria Lucia Jiménez Gutiérrez

library(dslabs)
data("gapminder")
mycountries <- c("United Stated", "Vietnam")
tap <- gapminder |> 
  filter(year>=1960 & year<=2010 & country %in% mycountries) 

tab


#Ejercicio 5. Esperanza de Vida y la Guerra de Vietnam parte 2
#Auria Lucia Jiménez Gutiérrez

library(dslabs)
data("gapminder")
mycountries <- c("United States", "Vietnam")
tap <- gapminder |> 
  filter(year>=1960 & year<=2010 & country %in% mycountries) 

p <- tap |>
  ggplot(aes(x=year, y=life_expectancy, color=country))+
  geom_line(alpha=0.7, size=2)+
  labs(title="Esperanza de vida vs año para Vietnam y Estados Unidos 1960-2010",
       x="Años",
       y="Esperanza de vida")+
  theme_minimal()+
  theme(legend.position="right")
print(p)


#Ejercicio 6. Esperanza de Vida en Camboya
#Auria Lucia Jiménez Gutiérrez

library(dslabs)
data("gapminder")
gapminder |> 
  filter(year>=1960 & year<=2010 & country =="Cambodia") |>
  ggplot(aes(x=year, y=life_expectancy))+
  geom_line(alpha=0.7, size=2)+
  labs(title="Esperanza de vida en Camboya 1960-2010",
       x="Años",
       y="Esperanza de vida")+
  theme_minimal()+
  theme(legend.position="right")

#Ejercicio 7. Dolares por día parte 1
#Auria Lucia Jiménez Gutiérrez

daydollars <- gapminder |> 
   mutate(dollars_per_day = gdp / population / 365) |>
   filter(continent =="Africa" & year ==2010, !is.na(dollars_per_day)) 
daydollars


#Ejercicio 8. Dolares por día parte 2
#Auria Lucia Jiménez Gutiérrez
library(dslabs)
library(ggplot2)
data("gapminder")
daydollars <- gapminder |> 
  mutate(dollars_per_day = gdp / population / 365) |>
  filter(continent =="Africa" & year ==2010, !is.na(dollars_per_day)) |>
  ggplot(aes(x=dollars_per_day))+
  geom_density(fill="skyblue", alpha=0.5)+
  labs(title="Histograma de variable Dolar por día", x="Dolar por día", y="Frecuencia")+
  theme_minimal() +
  scale_x_continuous(trans = "log2")
daydollars

#Ejercicio 9. Dolares por día parte 3 Multiples graficos de densidad
#Auria Lucia Jiménez Gutiérrez

library(dslabs)
library(ggplot2)
data("gapminder")
daydollars <- gapminder |> 
  mutate(dollars_per_day = gdp / population / 365) |>
  filter(continent =="Africa" & year ==2010 | year ==1970, !is.na(dollars_per_day)) |>
  ggplot(aes(x=dollars_per_day))+
  geom_density(fill="skyblue", alpha=0.5)+
  labs(title="Histograma de variable Dolar por día", x="Dolar por día", y="Frecuencia")+
  theme_minimal() +
  scale_x_continuous(trans = "log2")+
  facet_grid(year~.)
daydollars

#Ejercicio 10. Dolares por día parte 4 Grafico de densidad apilado
#Auria Lucia Jiménez Gutiérrez

library(dslabs)
library(ggplot2)
data("gapminder")
daydollars <- gapminder |> 
  mutate(dollars_per_day = gdp / population / 365) |>
  filter(continent =="Africa" & year ==2010 | year ==1970, !is.na(dollars_per_day)) |>
  ggplot(aes(x=dollars_per_day, fill = region))+
  geom_density(fill="skyblue", alpha=0.5, bw=0.5, position = "stack")+
  labs(title="Histograma de variable Dolar por día", x="Dolar por día", y="Frecuencia")+
  theme_minimal() +
  scale_x_continuous(trans = "log2")+
  facet_grid(year~.)
daydollars


#Ejercicio 11. Grafico de dispersión de mortalidad infantil parte 1
#Auria Lucia Jiménez Gutiérrez

library(dslabs)
library(ggplot2)
data("gapminder")
gapminder_Africa_2010 <- gapminder |> 
  mutate(dollars_per_day = gdp / population / 365) |>
  filter(continent =="Africa" & year ==2010, !is.na(dollars_per_day)) |>
  ggplot(aes(x=dollars_per_day, y=infant_mortality,color = region))+
  geom_point(fill="skyblue", alpha=0.5, bw=0.5, position = "stack")+
  labs(title="Diagrama de distribución de Mortalidad infantil vs Dolar por día", 
       x="Dolar por día", 
       y="Mortalidad infantil")+
  theme_minimal() 
gapminder_Africa_2010


#Ejercicio 12. Grafico de dispersión de mortalidad infantil parte 2 Eje logaritmico
#Auria Lucia Jiménez Gutiérrez

library(dslabs)
library(ggplot2)
data("gapminder")
gapminder_Africa_2010 <- gapminder |> 
  mutate(dollars_per_day = gdp / population / 365) |>
  filter(continent =="Africa" & year ==2010, !is.na(dollars_per_day)) |>
  ggplot(aes(x=dollars_per_day, y=infant_mortality,color = region))+
  geom_point(fill="skyblue", alpha=0.5, bw=0.5, position = "stack")+
  labs(title="Diagrama de distribución de Mortalidad infantil vs Dolar por día", 
       x="Dolar por día", 
       y="Mortalidad infantil")+
  theme_minimal() +
  scale_x_continuous(trans = "log2")
gapminder_Africa_2010


#Ejercicio 13. Grafico de dispersión de mortalidad infantil parte 3 Añadiendo etiquetas2 Eje logaritmico
#Auria Lucia Jiménez Gutiérrez

library(dslabs)
library(ggplot2)
data("gapminder")
gapminder_Africa_2010 <- gapminder |> 
  mutate(dollars_per_day = gdp / population / 365) |>
  filter(continent =="Africa" & year ==2010, !is.na(dollars_per_day)) |>
  ggplot(aes(x=dollars_per_day, y=infant_mortality,color = region))+
  geom_point(fill="skyblue", alpha=0.5, bw=0.5, position = "stack")+
  geom_text(aes(label=country),vjust=0.1, color="black", size=2)+
  labs(title="Diagrama de distribución de Mortalidad infantil vs Dolar por día", 
       x="Dolar por día", 
       y="Mortalidad infantil")+
  theme_minimal() +
  scale_x_continuous(trans = "log2")
gapminder_Africa_2010

#Ejercicio 14. Grafico de dispersión de mortalidad infantil parte 4 Comparación de gráficos de dispersión 
#Auria Lucia Jiménez Gutiérrez

library(dslabs)
library(ggplot2)
data("gapminder")
gapminder_Africa_2010 <- gapminder |> 
  mutate(dollars_per_day = gdp / population / 365) |>
  filter(continent =="Africa" & year ==2010 | year ==1970, !is.na(dollars_per_day)) |>
  ggplot(aes(x=dollars_per_day, y=infant_mortality, color = region))+
  geom_point(fill="skyblue", alpha=0.5, position = "stack")+
  geom_text(aes(label=country),vjust=0.1, color="black", size=2)+
  labs(title="Diagrama de distribución de Mortalidad infantil vs Dolar por día", 
       x="Dolar por día", 
       y="Mortalidad infantil")+
  theme_minimal() +
  scale_x_continuous(trans = "log2")+
  facet_grid(year~.)
gapminder_Africa_2010

