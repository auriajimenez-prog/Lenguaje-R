#Sesion 2
#Ejemplo barra resaltada en histograma
#sembrar semilla para obtener mismo resultados
set.seed(123)

#Crear  una distribucion normal de 100 numeros aleatorios
datos <- rnorm(100)
#Crear el histograma
histograma <- hist(datos, plot=FALSE)
histograma
hist(datos)
histograma$breaks[-1]
histograma$breaks[-length(histograma$breaks)]

#identificar barras que deseamos resaltar
valor_objetivo <-0
indice_rojo <- which(valor_objetivo>=histograma$breaks[-length(histograma$breaks)]
                    & valor_objetivo < histograma$breaks[-1])
#creamos un vector que repite el color gris tantas veces como barras tengamos
colores <- rep("gray",length(histograma$counts))
colores[indice_rojo] <- "red" #resaltas

#Resaltar segunda barra
colores[2] <- "pink" #resaltas

colores[indice_rojo+1] <- "blue" #resaltas


#graficar nuestro histograma
plot(histograma, col=colores, main="Histograma con una barra resaltada", xlab="Valor", ylab="Frecuencia")

#--------------Inicia clase----------------
#Ejercicio1. Demostración 1

library(dslabs)
library(ggplo2)
data("mtcars")
mtcars
ggplot(mtcars,aes(x=cyl,y=mpg, fill=factor(cyl)))+
  geom_boxplot()+
  labs(title="Boxplor de Millas por Galón en mtcars"
       x="Numero de Cilindros", 
       y="Millas por Galón")+
  scale_fill_manual(values=c("4"="skyblue","6"="salmon","8"="lightgreen"))+
  theme_minimal()  
  
#Ejercicio 2. 
  library(dslabs)
  library(ggplo2)
  data("mtcars")
  mtcars
  ggplot(mtcars,aes(x=factor(cyl), fill=factor(cyl)))+
    geom_bar()+
    labs(title="Número de modelos por cilindros en mtcars",
       x="Numero de Cilindros", 
       y="Cantidad de Modelos por cilindros")+
    scale_fill_manual(values=c("4"="red","6"="blue","8"="orange"))+
    theme_minimal() +
    geom_text(stat = "count", aes(label= ..count..), vjust = -0.5)

  
#Ejercicio 3
plot(Nile)
class(Nile) #ts = serie de tiempo
nile_df <- data.frame(time=time(Nile),valor=as.vector(Nile))
ggplot(nile_df, aes(x=time, y=valor))+
         geom_line()+
  labs(title="Niveles anuales del Rio Nilo (1871-1970", 
       x="Año",
       y="Nivel del Río")+
  theme_gray()


#Ejercicio 4
library(dslabs)
library(ggplot2)
data("mtcars")
?geom_smooth
ggplot(mtcars, aes(x=hp, y=mpg))+
  geom_point(color="blue", size=2)+
  geom_smooth(method="lm",se=TRUE, color="red",size=0.5)+
  labs(title="Gráfico de Dispersión con Línea de Tendencia",
       x="Potencia del Motor",
       y="Eficiencia del combustible (mpg)")+
  theme_light()

#  Ejercicio Actividad individual

library(dplyr)
library(ggplot2)
data("economics")
economics
ggplot(economics, aes(x=date))+
  geom_area(aes(y=psavert, fill="Evolución de Ingreso"), alpha=0.7)+
  geom_area(aes(y=uempmed, fill="Gasto"), alpha=0.7)+
  scale_fill_manual(values=c("Evolución de Ingreso"="skyblue", "Gasto"="red"))+
  labs(title="Evolución del ingreso y gasto trimestral",
       x="Fecha",
       y="Ingreso y Gasto")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1))

#Grafico final
#Tenemos 59000 ganamos y pasamos a 65000
valores <- c(59000, 65000-59000)
etiquetas <-c("Valor original", "Aumento")

#Calcular los porcentajes
porcentajes <- round(valores / sum(valores)*100,1)
etiqueta_porcentaje <- paste(etiquetas,"-",porcentajes,"%")

#crear un grafico de pastel
pie(valores,
    labels=etiqueta_porcentaje,
    col=c("skyblue","orange"),
    main="Distribución de nuevo valor de computadora")

#Con ggplot
data <- data.frame(
  categoria=c("Valor original", "AUmento"),
  valor=c(59000,65000-59000)
)
data
ggplot(data,aes(x=" ",y=valor,fill=categoria))+
  geom_bar(stat="identity", width = 1)+
  coord_polar("y")+
  labs(title="Distribución de nuevo valor de computadora")
