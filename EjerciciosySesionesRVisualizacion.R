library(ggplot2)

datos <-data.frame(altura=c(156,158,159,162,168,175,178,176))
#Histograma
ggplot(datos,aes(x=altura))+
  geom_histogram(binwidth = 4,fill="lightblue", color="black", alpha=0.7)+
  labs(title="Histograma de variable altura", x="Altura (cm)", y="Frecuencia")
theme_minimal()

#Suavizado
ggplot(datos,aes(x=altura))+
  geom_density(fill="skyblue", alpha=0.5)+
  labs(title="Histograma de variable altura", x="Altura (cm)", y="Frecuencia")
theme_minimal()

#Histograma + suavidad
ggplot(datos,aes(x=altura))+
  geom_histogram(aes(y=..density..),binwidth=5, fill="lightblue",color="black",alpha=0.6)+
  geom_density(color="blue", size=1)+
  labs(title="Histograma de variable altura", x="Altura (cm)", y="Frecuencia")
theme_minimal()


#ejercicio 1 demostración de puntuaciones de calificaciones

puntuaciones <- rnorm(100,mean=75, sd=10)#Generando 100 valores pseudoaleatorios con distribución normal
probabilidad <- 1-pnorm(80,mean=mean(puntuaciones),sd=sd(puntuaciones))
probabilidad


#ejercicio 2
edad1 <- rnorm(5,mean=35,sd=15)
edad2 <- rnorm(5,mean=40,sd=10)
edades <- c(edad1,edad2)

#Ejercicio3
nomf <-c("Maria", "Lucia", "Lizeth", "Alejandra")
nomM <- c("Jose","Alain","Victor","Juan")
nombres <- c(nomf,nomM)

#Ejercicio 4
library(dslabs)
data(mtcars)
puntuaciones <- mtcars$mpg

q1 <- quantile(puntuaciones, 0.25)
q1
q2 <- quantile(puntuaciones,0.5)
q2
median(puntuaciones)
q3 <- quantile(puntuaciones,0.75)
q3
cat("Q1(Primer cuartil):",q1,"\n","Q2(Mediana):",q2,"\n","Q3(Tercer cuartil):",q3,"\n")

#Boxplot
boxplot(puntuaciones,main="Boxplot de mpg(millas por galón)",
        ylab="Millas por galón",
        col="pink",
        border="darkblue")

#Ejercicio5
tiempos_algoritmo1 <- rnorm(100, mean=10, sd=2)
tiempos_algoritmo2 <- rnorm(100, mean=15, sd=3)
media_algoritmo1 <- mean(tiempos_algoritmo1)
sd_algoritmo1 <- sd(tiempos_algoritmo1)
media_algoritmo2 <- mean(tiempos_algoritmo2)
sd_algoritmo2 <- sd(tiempos_algoritmo2)

cat("Tiempo medio de ejecucion del algoritmo 1:", media_algoritmo1, "\nDesviación estándar algoritmo 1: ",sd_algoritmo1,  "\nTiempo medio de ejecucion del algoritmo 2:", media_algoritmo2,"\nDesviación estándar algoritmo 2: ",sd_algoritmo2)


#Ejercicio 6
set.seed(123)
v6 <- rnorm(1000, mean = 50, sd = 10)
cat("Media de la distribución normal: ",mean(v6),)
v6
media6  <- mean(v6)
sd6 <- sd(v6)
cat("Mediana de la distribucion normal: ", media6, ",Desviacion estandar de la distribución normal: ", sd6, "\n")


#Ejercicio 7
set.seed(456)
v7 <- rbinom(1000,size=10,prob=0.3)
media7 <- mean(v7)
var7 <- var(v7)
cat("Media: ", media7, "\nVarianza: ",var7)



#-------------------------
#Diferencia entre print() y cat()

x <-"Hola\nmundo"
print(x)
cat(x)

data <- 3.14159
print("El valor de pi es: ",data)
cat("El valor de pi es: ",data)

#------------------------------
# load the dataset
library(dslabs)
data(heights)

# make a table of category proportions
prop.table(table(heights$sex))



#uso de pnorm para calcular probabilidades
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
1 - pnorm(70.5, mean(x), sd(x))
#trazar distribución de alturas exactas en datos
plot(prop.table(table(x)), xlab = "a = Altura en pulgadas", ylab = "Pr(x = a)")

#probabilidades en datos reales en rangos de longitud 1 que contienen un entero
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

#probabilidades en la aproximación normal coinciden bien
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

#probabilidades en datos reales en otros rangos no coinciden tan bien con la aprox normal
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

# install.packages("devtools")
devtools::install_github("r-lib/conflicted")

##esclalas etiquetas y colores
# definir p
library(tidyverse)
library(dslabs)
data(murders)
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))

# escalar en logaritmo base 10 el eje x y el eje y
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

# escalar eficientemente en logaritmo los ejes
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Población en millones (escala logarítmica)") +
  ylab("Número total de asesinatos (escala logarítmica)") +
  ggtitle("Asesinatos por Armas de Fuego en EE.UU. en 2010")


# definir la tasa media de asesinatos
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

# línea básica con la tasa media de asesinatos para el país
p <- p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r)) # la pendiente es por defecto de 1

# cambiar la línea a discontinua y gris oscuro, línea debajo de los puntos
p +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)


p <- p + scale_color_discrete(name = "Región") # capitalizar el título de la leyenda

##Paquetes adicionales
# tema utilizado para gráficos en el libro de texto y el curso
library(dslabs)
ds_theme_set()

# temas de ggthemes
library(ggthemes)
p + theme_economist() # estilo de la revista The Economist
p + theme_fivethirtyeight() # estilo del sitio web FiveThirtyEight
# cargar librerías
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# definir la intersección
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# crear el gráfico, combinando todos los elementos
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Población en millones (escala logarítmica)") +
  ylab("Número total de asesinatos (escala logarítmica)") +
  ggtitle("Asesinatos por Armas de Fuego en EE.UU. en 2010") +
  scale_color_discrete(name = "Región") +
  theme_economist()


##otros ejemplos
# cargar datos de alturas
library(tidyverse)
library(dslabs)
data(heights)

# definir p
p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))
# histogramas básicos
p + geom_histogram()
p + geom_histogram(binwidth = 1)

# histograma con relleno azul, contorno negro, etiquetas y título
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Alturas masculinas en pulgadas") +
  ggtitle("Histograma")

p + geom_density()
p + geom_density(fill = "blue")

# gráfico QQ básico
p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()

# gráfico QQ contra una distribución normal con la misma media/desviación estándar que los datos
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

# gráfico QQ de datos escalados contra la distribución normal estándar
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()
# definir gráficos p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

# organizar gráficos uno al lado del otro en 1 fila, 3 columnas
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)



