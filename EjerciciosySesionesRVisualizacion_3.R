##sesion 3

#Ejercicio 1
library (gapminder)
library(dplyr)
library(ggplot2)

gapminder

#filtrar los datos para Estados Unidos y México en años (1960 a 2010)
years <- 1960:2010
countries <- c("United States", "Mexico", "Argentina","Colombia","Niger", "France", "Russia","Uganda")

gapminder_fitered <- gapminder %>%
  filter(year %in% years, country %in% countries)

gapminder_fitered

#Graficamos la evolución de la esperanza de vida
ggplot(gapminder_fitered, aes(x=year))+
  geom_line(aes(y=lifeExp, color=country), alpha=0.5)+
  facet_wrap(~continent, scales="free_y")+#CREA ESCALAS PARA CADA CONTINENTE scales="free_y"
  labs(title="Comparación de esperanza de vida", 
       x="Año",
       y="Esperanza de vida")+
  theme_minimal()


#Ejercicio 2

mexico_data <- gapminder %>%
  filter(country=="Mexico" & year >= 2000 & year <=2020)
correlation <- cor(mexico_data$pop, mexico_data$gdpPercap * mexico_data$pop /1e9)

ggplot(mexico_data, aes(x=pop, y=gdpPercap * pop / 1e9))+
  geom_point(color="blue", size=3)+
  geom_smooth(method="lm", color="red", linetype="dashed")+
  labs(title="Relación entre población y PIB de México", 
       x="Población",
       y="PIB total en miles millones")


#Ejercicio 3
##x=fertility y=infant_mortality
library(dslabs)
data("gapminder")
gapminder_2010 <- gapminder %>% filter(year==2010)

gapminder_2010 
gapminder_2010  %>%
  ggplot(aes(x=fertility, y=infant_mortality, color=continent))+
  geom_point(alpha=0.7, size=3)+
  labs(title="Relación entre tasa de fertilidad y mortalidad infantil",
       x="Tasa de fertilidad",
       y="Mortalidad infaltil")+
  theme_minimal()+
  theme(legend.position="right")+
  scale_color_manual(values=c("#1f78b4","#33a02c","#e31a1c", "#ff7f00", "#6a3d9a"))+
  geom_smooth(method="lm", color="black")+
  annotate("text", x=7, y=50, label="Linea de regresión", color="black", size =4)



#Ejercicio sin gapminder

#Secuencia de ángulo de 0 a 2*pi
theta <- seq(0, 2*pi, length.out=1000)
theta

r= 1-sin(theta)
#Convertir a coordenadas cartesianas
df <- data.frame(x=r*cos(theta),
                 y=r*sin(theta))
ggplot(df,aes(x,y))+
  geom_path(color="red", size=2)+
  coord_fixed()
