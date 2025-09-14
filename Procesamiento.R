
library(tidyverse)
library(readxl)

Datos_completos_parcela_1 <- read_excel("~/MEGA/MEGAsync Imports/Doctorado en Chile/Proyecto de doctorado/Instalacion de sensores/Datos-unidos/Datos-completos-parcela-1_2.xlsx")

# Glimpse : "mirar" ---------------------------------------------------------
glimpse(Datos_completos_parcela_1)

dim(Datos_completos_parcela_1) # filas x columnas

# Head y summary ----------------------------------------------------------
head(Datos_completos_parcela_1)

summary(Datos_completos_parcela_1)

#Conocer los valores de SHF y TT_C cuando hay radiacion#

Datos_completos_parcela_1 %>%
  filter(NR_Avg > 0) %>%
  select(TIMESTAMP, `Temperatura del aire (Celcius)`, TT_C_Avg, `Velocidad del viento (m/s)`, `Direccion del viento (grados)`)

# EDA ---------------------------------------------------------------------

Datos_completos_parcela_1_procesado <- Datos_completos_parcela_1 %>%
  filter(!is.na(`Temperatura del aire (Celcius)`) & !is.na(TT_C_Avg) & !is.na(`Velocidad del viento (m/s)`) & !is.na(`Direccion del viento (grados)`) & !is.na(H) )

# EDA visual

#H

ggplot(Datos_completos_parcela_1_procesado, aes(x = TIMESTAMP, y = H)) +
  geom_line(linewidth = 0.8, color = 4) +
  labs(title = '', y = "Flujo de calor sensible H (w/m2)", x = 'Tiempo')  +
  theme_bw()

#Temperatura del aire

ggplot(Datos_completos_parcela_1_procesado, aes(x = TIMESTAMP, y = `Temperatura del aire (Celcius)`)) +
  geom_line(linewidth = 0.8,  color = 2) +
  labs(title = '', y = "Temperatura del aire (°C)", x = 'Tiempo')  +
  theme_bw()

#Temperatura del suelo

ggplot(Datos_completos_parcela_1_procesado, aes(x = TIMESTAMP, y = TT_C_Avg)) +
  geom_line(linewidth = 0.8,  color = 5) +
  labs(title = '', y = "Temperatura del suelo (°C)", x = 'Tiempo')  +
  theme_bw()

#Velocidad del viento

ggplot(Datos_completos_parcela_1_procesado, aes(x = TIMESTAMP, y = `Velocidad del viento (m/s)`)) +
  geom_line(linewidth = 0.8,  color = 6) +
  labs(title = '', y = 'Velocidad del viento (m/s)', x = 'Tiempo')  +
  theme_bw()

#`Direccion del viento (grados)`

ggplot(Datos_completos_parcela_1_procesado, aes(x = TIMESTAMP, y = `Direccion del viento (grados)`)) +
  geom_line(linewidth = 0.8,  color = 6) +
  labs(title = '', y = "Direccion del viento (grados)", x = 'Tiempo')  +
  theme_bw()

TukeyHSD(ajuste_anova)

# Modelo lineal múltiple con todas las variables
modelo_completo <- lm(H ~ `Temperatura del aire (Celcius)` + TT_C_Avg +
                        `Velocidad del viento (m/s)` + `Direccion del viento (grados)`,
                      data = Datos_completos_parcela_1_procesado)

# ANOVA del modelo completo
anova_result <- anova(modelo_completo)
print(anova_result)

# Resultados más detallados
summary(modelo_completo)





