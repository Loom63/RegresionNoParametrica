library(dplyr)
library(tidyr)
library(readr)
options(scipen = 999)

#Cargar los datos de la encuesta
EPHPM_JULIO_2025 <- read_csv("EPHPM JULIO 2025.csv")

#Selección de las variables importantes
Relacion<-EPHPM_JULIO_2025$RELA_J
Hogar<-EPHPM_JULIO_2025$HOGAR
Nivel<-EPHPM_JULIO_2025$nivel
Ingreso_Hogar<-EPHPM_JULIO_2025$ytothg

#crear un data frame con las variables
datos<-data.frame(Hogar,Relacion,Ingreso_Hogar,Nivel)


#filtrar las observaciones que cumplen nuestros supuestos
datos_12 <- datos %>%
  filter(
    (Relacion == 1 & lead(Relacion) == 2) |
      (Relacion == 2 & lag(Relacion) == 1)
  )
head(datos_12)

####################################################

#Tratamiento final de nuestros datos

# Asegurar que Relacion sea numérico
df<-datos_12
df$Relacion <- as.integer(df$Relacion)

# Ver estructura
str(df)

# --- Transformación ---
# 1. Extraer el ingreso por hogar (valor de la fila con Relacion = 1)
ingreso_hogar <- df %>%
  filter(Relacion == 1) %>%
  select(Hogar, Ingreso_Hogar)

# 2. Pivotar la columna Nivel para crear Nivel_1 y Nivel_2
nivel_ancho <- df %>%
  select(Hogar, Relacion, Nivel) %>%
  pivot_wider(
    names_from = Relacion,
    values_from = Nivel,
    names_prefix = "Nivel_"   # Para que las columnas se llamen Nivel_1 y Nivel_2
  )

# 3. Unir las dos tablas por Hogar
resultado <- nivel_ancho %>%
  left_join(ingreso_hogar, by = "Hogar") %>%
  relocate(Ingreso_Hogar, .after = Hogar)   # Colocar Ingreso después de Hogar

DATA<-na.omit(resultado)


#Muestra resultante que necesitaremos

Muestra <- DATA %>%
  rename(
    ID_Hogar = Hogar,
    Ingreso = Ingreso_Hogar,
    Nivel_Jefe = Nivel_1,
    Nivel_Pareja = Nivel_2
  )

# Ver resultado
head(Muestra)

