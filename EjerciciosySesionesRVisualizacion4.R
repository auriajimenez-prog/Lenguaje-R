#Sesion #4 Visialización
#Demostración 1. grafico de barras que muestre la eficiencia de combustible

library (ggplot2)
library(dslabs)
data ("mtcars")
mtcars

mtcars_filtrado <- mtcars[complete.cases(mtcars$mpg), ]
grafico <- ggplot(mtcars_filtrado, aes(x=rownames(mtcars_filtrado), y=mpg, fill=factor(cyl)))+
  geom_bar(stat ="identity", position="dodge", color="black")+
  geom_hline(yintercept =10, linetype="dashed", color="purple", size=2)+
  geom_text(aes(label=mpg),vjust=0.5, color="black", size=3)+
  labs(title="Millas por Galón por Modelo",
       x = "Modelo de Automovil",
       y = "Millas por Galón",
       fill="Cilindros")+
  theme_minimal()+
  theme(axis.text.x =element_text(angle=45, hjust=1))+
scale_fill_manual(values=c("red","blue","green"))

print(grafico)


#Ejercicio 2 Crear un grafico 

library(dslabs)
data("mtcars")

grafico2 <- ggplot(mtcars,aes(x=reorder(rownames(mtcars_filtrado),-mpg), y=mpg, fill=factor(cyl)))+
  geom_bar(stat="identity", position="dodge",color="black")+
  geom_text(aes(label=mpg), vjust=0.6, color="black", size=3)+ #etiquetar barras
  labs(title="Grafico de barras ordenado por millas por galón",
       x="Modelo de Automovil",
       y="Millas por galón",
       fill="Cilindros")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=50, hjust=1)) #rotar etiquetas en el eje x

print(grafico2)


#Demostracion 3

ggplot (mtcars,aes(x=reorder(rownames(mtcars_filtrado),-mpg), y=mpg, fill=factor(cyl)))+
  geom_bar(stat="identity", position="dodge",color="black")+
  geom_text(aes(label=mpg),vjust=0.8, color="white", size=3)+
  labs(title="Grafico de barras ordenado por millas por galón",
     x="Modelo de Automovil",
     y="Millas por galón",
     fill="Cilindros")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.major = element_blank(),#Eliminar lineas de la cuadricula principal
        panel.grid.minor=element_blank(), #Eliminar lineas de la cuadricula secundaria
        panel.background=element_blank(), #Eliminar fondo del panel
        axis.line=element_line(color="black"), #Añadir linea de ejes
        legend.position="bottom",#mover leyenda a laparte inferior
        legend.title=element_text(size=10, face="bold"),#estilo de titulo de leyenda
        legend.text=element_text(size=8, color="black")) #rotar etiquetas en el eje x



#Demostración finale

install.packages("gridExtra")

library(gridExtra)
library(dslabs)
library(ggplot2)
library("iris")
iris
plot_sepals <- iris |>
  ggplot(aes(x=Species, y=Sepal.Length, fill=Species))+
  geom_bar(stat="identity", position="dodge", alpha=0.7)+
  labs(title="Comparación de longitud de Sepalos por especie",
       x="Especie",
       y="Longitud de Sepalos")+
  theme_minimal()

plot_sepals

plot_petals <- iris |>
  ggplot(aes(x=Species, y=Petal.Length, color=Species))+
  geom_jitter(alpha=0.7 )+
  labs(title="Distribución de longitud de Petalos por especie",
       x="Especie",
       y="Longitud de Petalos")+
  theme_minimal()
plot_petals

grid.arrange(plot_sepals,plot_petals, ncol=1)
