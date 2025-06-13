library (dslabs)
data("temp_carbon")
temp_carbon

temp_carbon |> filter(!is.na(carbon_emissions)) |>
  select(year) |>
  max()

temp_carbon |> .$year |>
  max()
temp_carbon |> filter(!is.na(carbon_emissions)) |>
  max(year)

temp_carbon |> filter(!is.na(carbon_emissions)) |>
  max(.$year)

temp_carbon |> filter(!is.na(carbon_emissions)) |>
  pull(year) |>
  max()

temp_carbon |> filter(!is.na(carbon_emissions)) |>
  .$year |>
  max()

temp_carbon |> filter(!is.na(carbon_emissions)) |>
  select(year) |>
  max()

-0.11   -0.48
0.82    1.18



library(dslabs)
library(dplyr)

data("temp_carbon")

# Filtrar años con datos válidos y obtener emisiones
carbon_data <- temp_carbon %>%
  filter(!is.na(carbon_emissions))

primer_año <- min(carbon_data$year)  # 1880
ultimo_año <- 2018  # Asegúrate de que tu dataset incluya este año

# Extraer valores
emisiones_1880 <- carbon_data %>% 
  filter(year == 1880) %>% 
  pull(carbon_emissions)  # 356 millones de toneladas

emisiones_2018 <- carbon_data %>% 
  filter(year == 2018) %>% 
  pull(carbon_emissions)  # Valor específico en tu dataset

# Calcular incremento
veces_mayor <- emisiones_2018 / emisiones_1880
veces_mayor




library(dslabs)
library(dplyr)

data("temp_carbon")

# Filtrar años con datos de temperatura válidos (no NA)
temp_data <- temp_carbon %>%
  filter(!is.na(temp_anomaly))

# Obtener el año más antiguo y más reciente
primer_año <- min(temp_data$year)  # Normalmente 1880
ultimo_año <- max(temp_data$year)  # En tu caso, 2018

# Extraer anomalías de temperatura
anomalia_primer_año <- temp_data %>%
  filter(year == primer_año) %>%
  pull(temp_anomaly)

anomalia_ultimo_año <- temp_data %>%
  filter(year == ultimo_año) %>%
  pull(temp_anomaly)

# Calcular aumento en °C
aumento_celsius <- anomalia_ultimo_año - anomalia_primer_año
aumento_celsius




library(ggplot2)
library(dplyr)

# Cargar y preparar datos
data("temp_carbon", package = "dslabs")
temp_data <- temp_carbon %>% 
  filter(!is.na(temp_anomaly))

# Crear gráfico con línea de referencia del siglo XX
p <- ggplot(temp_data, aes(x = year, y = temp_anomaly)) +
  # Línea principal de anomalías
  geom_line(color = "firebrick", linewidth = 1.2) +
  
  # Línea horizontal azul para la media del siglo XX (y=0)
  geom_hline(aes(yintercept = 0), color = "blue") +
  
  # Etiqueta explicativa para la línea azul
  geom_text(aes(x = min(year), y = 0.1, label = "Media siglo XX"), 
            color = "blue", hjust = -0.1, vjust = -1, size = 3.5) +
  
  # Formateo del gráfico
  labs(title = "Anomalía Térmica Global (1880-2018)",
       subtitle = "Desviación respecto a la media del siglo XX (1901-2000)",
       x = "Año",
       y = "Anomalía de Temperatura (°C)",
       caption = "Fuente: Dataset temp_carbon de dslabs") +
  
  scale_x_continuous(breaks = seq(1880, 2020, 20)) +
  scale_y_continuous(breaks = seq(-0.5, 1, 0.25)) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )
p+ ylab("Anomalia de temp (C)")+
  ggtitle("Anomalia de temp relativas siglo XX")+
  geom_text(aes(x=2000, y=005, label="Media del siglo"), col="blue")
# Mostrar gráfico
print(p)



library(dslabs)
library(dplyr)

data("temp_carbon")

# Filtrar años con anomalía positiva y obtener el primero
primer_año_sobre_media <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%       # Eliminar NA
  filter(temp_anomaly > 0) %>%           # Anomalías positivas
  arrange(year) %>%                      # Ordenar cronológicamente
  slice(1) %>%                           # Obtener el primer registro
  pull(year)                             # Extraer el año

primer_año_sobre_media




library(dslabs)
library(dplyr)

data("temp_carbon")

# Filtrar años con anomalía negativa y obtener el último
ultimo_año_bajo_media <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%       # Eliminar valores NA
  filter(temp_anomaly < 0) %>%           # Anomalías negativas
  arrange(desc(year)) %>%                # Ordenar de mayor a menor año
  slice(1) %>%                           # Obtener el último registro
  pull(year)                             # Extraer el año

ultimo_año_bajo_media


library(dslabs)
library(dplyr)

data("temp_carbon")

# Filtrar y encontrar el primer año con anomalía > 0.5°C
primer_ano_sobre_05 <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%      # Eliminar valores faltantes
  filter(temp_anomaly > 0.5) %>%        # Anomalías mayores a 0.5°C
  arrange(year) %>%                     # Ordenar cronológicamente
  slice(1) %>%                          # Obtener el primer registro
  pull(year)                            # Extraer el año

primer_ano_sobre_05


ggplot(temp_carbon, aes(x = year)) +
  geom_line(aes(y = global_anomaly, color = "Global"), linewidth = 1.2) +
  geom_line(aes(y = land_anomaly, color = "Continentes"), linetype = "dashed") +
  geom_line(aes(y = ocean_anomaly, color = "Océanos"), linetype = "dotted") +
  scale_color_manual(values = c("Global" = "black", "Continentes" = "firebrick", "Océanos" = "blue")) +
  labs(title = "Anomalías térmicas: Global vs. Regiones",
       x = "Año", y = "Anomalía (°C)", color = "Región") +
  theme_minimal()



greenhouse_gases %>% 
  ggplot(aes(x=year, y=concentration))+
  geom_line()+
  facet_grid(gas ~., scales="free")+
  geom_vline(xintercept = 1850)+
  ylab("Concentration")+
  ggtitle("concetracion de gases")




library(ggplot2)

greenhouse_gases %>%
  ggplot(aes(x = year, y = concentration, color = gas)) +
  geom_line(linewidth = 1) +
  facet_grid(gas ~ ., scales = "free_y") +
  scale_x_continuous(labels = function(x) paste0(abs(x), " BP"))+
  ylab("Concentration (ppm/ppb)") +
  ggtitle("Concentración de Gases (Período Antiguo)") +
  theme_minimal()





library(dslabs)
library(dplyr)

data("greenhouse_gases")

# Filtrar datos de CO₂ y encontrar los años clave
co2_data <- greenhouse_gases %>%
  filter(gas == "co2") %>%
  select(year, concentration) %>%
  arrange(year)

# 1. Encontrar el último año con nivel "estable" (~275 ppmv ±5)
ultimo_año_estable <- co2_data %>%
  filter(concentration <= 280) %>%  # Umbral cercano a 275 ppmv
  slice_max(year) %>%
  pull(year)

# 2. Obtener el año cuando se alcanzó el nivel actual (~415 ppmv)
año_actual <- 2018
nivel_actual <- co2_data %>%
  filter(year == año_actual) %>%
  pull(concentration)

# 3. Calcular duración en años
duracion_años <- año_actual - ultimo_año_estable

# Resultados
list(
  ultimo_año_estable = ultimo_estable,
  año_actual = año_actual,
  concentracion_actual = nivel_actual,
  duracion_años = duracion_años
)



library(ggplot2)

ggplot(co2_data, aes(x = year, y = concentration)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 275, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 415, linetype = "dashed", color = "black") +
  annotate("text", x = -800000, y = 275, label = "Nivel preindustrial (275 ppmv)", 
           color = "blue", vjust = -1) +
  annotate("text", x = 2000, y = 415, label = "Nivel actual (415 ppmv)", 
           color = "black", vjust = -1) +
  labs(title = "Aumento de CO₂ desde la era preindustrial",
       x = "Año", y = "Concentración de CO₂ (ppmv)") +
  theme_minimal()
