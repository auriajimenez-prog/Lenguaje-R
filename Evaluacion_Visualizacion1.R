#Evaluacion Supervivencia Titanic

options(digits = 3) # reportar 3 cifras significativas
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
titanic

class(titanic$Survived)

#Variable	Tipo
#Survived	Categórica no ordinal
#Pclass	Categórica ordinal
#Sex	Categórica no ordinal
#Age	Continua
#SibSp	Discreta
#Parch	Discreta
#Fare	Continua


#Pregunta 2
library(ggplot2)
library(dplyr)

# Cargar y preparar los datos
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

# 1. Gráfico de densidad básico por sexo
ggplot(titanic, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de edad por sexo",
       x = "Edad", y = "Densidad") +
  theme_minimal()

# 2. Gráfico de densidad con facetas por sexo
ggplot(titanic, aes(x = Age)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~Sex) +
  labs(title = "Distribución de edad por sexo (facetas)",
       x = "Edad", y = "Densidad") +
  theme_minimal()

# 3. Gráfico de densidad apilado (counts en eje y)
ggplot(titanic, aes(x = Age, y = ..count.., fill = Sex)) +
  geom_density(position = "stack", alpha = 0.7) +
  labs(title = "Distribución de edad por sexo (recuentos)",
       x = "Edad", y = "Recuento") +
  theme_minimal()

# 4. Gráfico de densidad con líneas verticales para rangos clave
ggplot(titanic, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = c(17, 18, 35, 40), linetype = "dashed", color = "red") +
  labs(title = "Distribución de edad con rangos clave",
       x = "Edad", y = "Densidad") +
  theme_minimal()



##Pregunta 3
library(ggplot2)
library(dplyr)

# Cargar y preparar los datos
titanic <- titanic_train %>%
  select(Survived, Sex) %>%
  mutate(Survived = factor(Survived, levels = c(0, 1), labels = c("No", "Sí")),
         Sex = factor(Sex))

# 1. Gráfico de barras apiladas (default)
ggplot(titanic, aes(x = Sex, fill = Survived)) +
  geom_bar() +
  labs(title = "Supervivencia por sexo (apilado)",
       x = "Sexo", y = "Conteo") +
  theme_minimal()

# 2. Gráfico de barras agrupadas
ggplot(titanic, aes(x = Sex, fill = Survived)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Supervivencia por sexo (agrupado)",
       x = "Sexo", y = "Conteo") +
  theme_minimal()

# 3. Gráfico de proporciones
ggplot(titanic, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de supervivencia por sexo",
       x = "Sexo", y = "Proporción") +
  theme_minimal()


##Pregunta 4
library(ggplot2)
library(dplyr)

# Preparar los datos
titanic <- titanic_train %>%
  select(Survived, Age) %>%
  filter(!is.na(Age)) %>%
  mutate(Survived = factor(Survived, labels = c("Murió", "Sobrevivió")))

# Crear gráfico de densidad
ggplot(titanic, aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2, position = "identity") +
  labs(title = "Distribución de edad por estado de supervivencia",
       x = "Edad", y = "Recuento") +
  theme_minimal() +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"))

sum(titanic$Age>=70 &titanic$Age<=80 & titanic$Survived=="Murió")
sum(titanic$Age>=70 &titanic$Age<=80 & titanic$Survived=="Sobrevivió")
titanic
0.33
0.58
0.64
0.62
0.85


##Pregunta 6
library(ggplot2)
library(dplyr)

# Preparar datos
titanic_fare <- titanic_train %>%
  filter(Fare > 0) %>%  # Eliminar tarifas cero
  mutate(Survived = factor(Survived, labels = c("No sobrevivió", "Sobrevivió")),
         log2Fare = log2(Fare))  # Transformación log2

# Crear boxplot con jitter
ggplot(titanic_fare, aes(x = Survived, y = log2Fare, fill = Survived)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, width = 0.2) +
  labs(title = "Distribución de tarifas (log2) por estado de supervivencia",
       x = "", y = "Tarifa (log2)") +
  theme_minimal()


##Pregunta 7
library(ggplot2)
library(dplyr)

# Gráfico 1: Barras básicas de clase rellenadas por supervivencia
ggplot(titanic_train, aes(x = Pclass, fill = Survived)) +
  geom_bar() +
  labs(title = "Recuento de supervivencia por clase")

# Gráfico 2: Proporciones relativas por clase
ggplot(titanic_train, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = position_fill()) +
  labs(title = "Proporción de supervivencia por clase")

# Gráfico 3: Supervivencia rellenada por clase
ggplot(titanic_train, aes(x = Survived, fill = Pclass)) +
  geom_bar(position = position_fill()) +
  labs(title = "Composición de clase por estado de supervivencia")

sum(titanic_fare$Survived=="Sobrevivió" & titanic_fare$Pclass==1)
sum(titanic_fare$Survived=="No sobrevivió" & titanic_fare$Pclass==1)
sum(titanic_fare$Survived=="Sobrevivió" & titanic_fare$Pclass==2)
sum(titanic_fare$Survived=="No sobrevivió" & titanic_fare$Pclass==2)
sum(titanic_fare$Survived=="Sobrevivió" & titanic_fare$Pclass==3)
sum(titanic_fare$Survived=="No sobrevivió" & titanic_fare$Pclass==3)

##Pregunta 10
library(ggplot2)
library(dplyr)

titanic_train %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, y = ..count.., fill = factor(Survived))) +
  geom_density(alpha = 0.5) +
  facet_grid(Sex ~ Pclass) +
  labs(title = "Distribución de edad por supervivencia, sexo y clase",
       x = "Edad", y = "Recuento") +
  scale_fill_discrete(name = "Sobrevivió", labels = c("No", "Sí")) +
  theme_minimal()
