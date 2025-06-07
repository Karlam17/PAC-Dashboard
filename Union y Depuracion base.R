# Limpiar el entorno 
rm(list = ls())

# Cargar paquetes necesarios
library(readxl)
library(dplyr)
library(tidyr)
library(stringdist)
library(writexl)
library(purrr)
library(tibble)
library(countrycode)

# Crear carpeta de salida si no existe (opcional)
dir.create("data", showWarnings = FALSE)

# Paso 1: Lista de archivos a unir desde la carpeta del proyecto
archivos <- list.files("data/", pattern = "\\.xlsx$", full.names = TRUE)

# Paso 2: Asignar nombres a cada variable 
variables <- c("Mortalidad_infantil",  "Tasa_fertilidad", "Pib_pcap",
               "Pobreza", "Esperanza_vida", "Esperanza_femenina", "Esperanza_masculino") 

# Paso 3: Leer y convertir cada archivo a formato largo
datos_largos <- map2(archivos, variables, function(archivo, variable) {
  df <- read_excel(archivo)
  colnames(df)[1] <- "Pais"
  df_long <- pivot_longer(df, -Pais, names_to = "Anio", values_to = variable)
  return(df_long)
})

# Paso 4: Estandarizar nombres de países usando el primero como referencia
paises_ref <- unique(datos_largos[[1]]$Pais)

estandarizar_nombres <- function(paises_vector, paises_ref) {
  sapply(paises_vector, function(p) {
    distancias <- stringdist(tolower(p), tolower(paises_ref), method = "jw")
    paises_ref[which.min(distancias)]
  }, USE.NAMES = FALSE)
}

datos_estandarizados <- map(datos_largos, function(df) {
  df$Pais <- estandarizar_nombres(df$Pais, paises_ref)
  return(df)
})

# Paso 5: Unir todas las bases por País y Año
base_panel <- reduce(datos_estandarizados, full_join, by = c("Pais", "Anio"))

# Paso 6: Asegurar formato de año y ordenar
base_panel$Anio <- as.integer(as.numeric(base_panel$Anio))
base_panel <- arrange(base_panel, Pais, Anio)

# Paso 7: Exportar base combinada
write_xlsx(base_panel, "data/base_panel_final.xlsx")

# Paso 8: Sumarizar por país y año
vars_deseadas <- c("Mortalidad_infantil", "Tasa_fertilidad", "Pib_pcap", "Pobreza",
                   "Esperanza_vida", "Esperanza_femenina", "Esperanza_masculino")

vars_existentes <- intersect(vars_deseadas, names(base_panel))

base_panel <- base_panel %>%
  group_by(Pais, Anio) %>%
  summarise(across(all_of(vars_existentes), ~ coalesce(.[!is.na(.)][1], NA_real_)),
            .groups = "drop")

# Paso 9: Revisar valores perdidos
colSums(is.na(base_panel))
sapply(base_panel, function(x) mean(is.na(x)) * 100)

na_info <- tibble(
  Variable = names(base_panel),
  NA_Conteo = colSums(is.na(base_panel)),
  Porcentaje_NA = sapply(base_panel, function(x) mean(is.na(x)) * 100)
) %>% arrange(desc(Porcentaje_NA))

print(na_info)

# Función para ver faltantes por columna
faltantes_por_columna <- function(df, columna) {
  df %>%
    filter(is.na(.data[[columna]])) %>%
    select(Pais, Anio, all_of(columna))
}

# Ver datos faltantes (en pestañas si estás en RStudio)
View(faltantes_por_columna(base_panel, "Mortalidad_infantil"))
View(faltantes_por_columna(base_panel, "Tasa_fertilidad"))
View(faltantes_por_columna(base_panel, "Pib_pcap"))
View(faltantes_por_columna(base_panel, "Esperanza_vida"))
View(faltantes_por_columna(base_panel, "Esperanza_femenina"))
View(faltantes_por_columna(base_panel, "Esperanza_masculino"))
View(faltantes_por_columna(base_panel, "Pobreza"))

# Paso 10: Excluir países no deseados
paises_excluir <- c("Andorra", "Dominica", "Holy See", "Liechtenstein", "Marshall Islands",
                    "Monaco", "Nauru", "Palau", "San Marino", "St. Kitts and Nevis", "Tuvalu",
                    "Hong Kong, China")

base_panel <- base_panel %>%
  filter(!Pais %in% paises_excluir)

# Guardar base limpia
write_xlsx(base_panel, "data/base_panel_sin_paises.xlsx")

# Paso 11: Agregar continente
base_panel <- read_excel("data/base_panel_sin_paises.xlsx")

base_panel$Continente <- countrycode(base_panel$Pais,
                                     origin = "country.name",
                                     destination = "continent")

# Guardar base final
write_xlsx(base_panel, "data/base_con_continentes.xlsx")