
#Pregunta 2
# 1. Magnitud media
mean_magnitude <- mean(stars$magnitude, na.rm = TRUE)

# 2. Desviación estándar de la magnitud
sd_magnitude <- sd(stars$magnitude, na.rm = TRUE)

# Mostrar resultados redondeados a 2 decimales
cat(sprintf("1. Magnitud media: %.2f\n", mean_magnitude))
cat(sprintf("2. Desviación estándar: %.2f\n", sd_magnitude))

#Pregunta 2
# Gráfico de densidad
ggplot(stars, aes(x = magnitude)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = c(-15, 0, 20), linetype = "dashed", color = "red") + # Líneas de referencia
  labs(title = "Distribución de la magnitud absoluta estelar",
       x = "Magnitud absoluta (mayor luminosidad → valores más negativos)",
       y = "Densidad") +
  theme_minimal()


#Pregunta 3
stars
ggplot(stars, aes(x = temp)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = c(-15, 0, 20), linetype = "dashed", color = "red") + # Líneas de referencia
  labs(title = "Distribución de la temperatura de las estrellas",
       x = "Temperatura",
       y = "Densidad") +
  theme_minimal()

#Pregunta 4
library(dslabs)
library(ggplot2)
data("stars")

ggplot(stars, aes(x = temp, y = magnitude)) +
  geom_point(alpha = 0.5) +
  scale_y_reverse() +  # Invertir eje Y (valores más negativos arriba)
  scale_x_reverse() + # Temperaturas más altas a la izquierda
  labs(title = "Diagrama HR: Magnitud vs Temperatura",
       x = "Temperatura (K, invertida)",
       y = "Magnitud absoluta (invertida)") +
  theme_minimal()

library(dslabs)
library(ggplot2)
data("stars")

ggplot(stars, aes(x = log10(temp), y = magnitude)) +
  geom_point(alpha = 0.6) +
  scale_y_reverse() +  # Magnitudes bajas (brillantes) arriba
  scale_x_reverse() +  # Temperaturas altas a la izquierda
  labs(x = "log10(Temperatura) [K] (invertido)", 
       y = "Magnitud absoluta (invertida)",
       title = "Diagrama HR con ejes transformados") +
  theme_minimal()


library(dslabs)
data("stars")

# 1. Definir características de enanas blancas:
# - Alta temperatura (temp > 8000 K)
# - Baja luminosidad (magnitude > 10)
enanas_blancas <- stars %>%
  filter(temp > 8000 & magnitude > 10)

# 2. Contar el número de enanas blancas
num_enanas_blancas <- nrow(enanas_blancas)
num_enanas_blancas




library(dslabs)
data("stars")

# Filtrar gigantes (fuera de secuencia principal y no enanas blancas)
gigantes <- stars %>%
  filter(
    between(magnitude, -5, 0) &  # Luminosidad intermedia-alta
      between(temp, 3000, 10000)    # Rango típico de gigantes
  )

# Calcular temperatura media
mean_temp_gigantes <- mean(gigantes$temp, na.rm = TRUE)
mean_temp_gigantes




# 1. Validación para Van Maanen's Star
stars %>% 
  filter(temp > 5000) %>% 
  arrange(desc(magnitude)) %>% 
  select(star, temp, magnitude) %>% 
  head(1)

# 2. Validación para supergigantes
stars %>% 
  arrange(temp) %>% 
  filter(magnitude < 0) %>% 
  select(star, temp, magnitude) %>% 
  head(5)  # Muestra Betelgeuse y Antares como las 2 primeras

# 3. El Sol (si estuviera en el dataset)
data.frame(star = "Sun", temp = 5772, magnitude = 4.83) %>% 
  mutate(type = case_when(
    between(temp, 3000, 10000) & between(magnitude, 4, 6) ~ "Secuencia principal",
    TRUE ~ "Otros"
  ))

