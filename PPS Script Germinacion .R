
############################################################################################################
#                      Instrucciones para cargar el conjunto de datos a analizar
############################################################################################################

# Eliminar todos los objetos del entorno de trabajo de R, si es que hace falta hacerlo.
rm(list=ls(all=TRUE))

# Cargar conjunto de datos llamado "datos_abra_pampa", que contiene los registros de temperatura media
#  diaria y precipitación de la EMC de Abra Pampa, desde el 01 de julio de 1969 hasta el 30 de junio
#  de 2007. Precaución: considerar que el conjunto de datos tiene faltantes.

# Para cargar el archivo si está en el directorio de trabajo de R (carpeta por defecto).
#load("datos_abra_pampa.Rdata")

# Para cargar un archivo ubicado en cualquier carpeta (se abre un cuadro de Windows)
load(file.choose())
ls() # Se debe visualizar el nombre del conjunto "datos"

# Para visualizar las diez primeras filas del tibble
print(datos, n=10)


############################################################################################################
#                      SCRIPTS PARA ANALIZAR LOS PERIODOS ÓPTIMOS DE GERMINACIÓN
############################################################################################################

# Última actualización: 23-10-2024

# ============================= Script para identificar los periodos válidos =============================

##  ACLARACIONES IMPORTANTES - LEER ANTES DE USAR EL SCRIPT

##  Los datos de entrada deben proporcionarse como un data.frame llamado 'datos' de tres columnas.
#   Las columnas deben tener el siguiente orden y denominación:
#    'fecha', correspondiente a la fecha (con formato año-mes-día: yyyy/mm/dd), 
#    'temp', correspondiente a la temperatura media diaria (°C)
#    'precip', correspondiente a la precipitación diaria (mm)

##  Condiciones que evalúa la función:

# - La temperatura media mensual y la precipitación acumulada mensual deben ser superiores a
#      los umbrales pre-establecidos. Por defecto: 10°C y 50 mm/mes.

# - Un día es considerado como lluvioso si el valor de precipitación supera un valor umbral
#      pre-establecido. Por defecto, 5 mm/día.

# - Un periodo es considerado válido si se cumple que:

#      - El periodo húmedo posee un nro mínimo de días y la precipitación acumulada
#         supera un valor umbral pre-establecido. Por defecto, 3 días y 30 mm/periodo.

#      - El nro. de días del periodo seco que sigue a continuación del periodo húmedo
#         debe ser inferior a un valor máximo pre-establecido. Por defecto, 5 días.

## Todas la condiciones anteriores están definidas como argumentos en la función y son modificables.
#   Los valores por defecto se conrresponden con los indicados para el análisis de la
#   germinación del pasto llorón (según bibliografía consultada)

## La salida de la función es un data.frame con las siguientes columnas:

#   1.- Ciclo anual (ej. 2003-2004) ---> desde 01 julio a 30 de junio
#   2.- Mes en el que inicia el periodo en cuestión
#   3.- Fecha de inicio del periodo lluvioso
#   4.- Fecha de fin del periodo lluvioso
#   5.- Fecha de inicio del periodo seco
#   6.- Fecha de fin del periodo seco
#   7.- Número de días lluviosos del periodo
#   8.- Número de días secos del periodo
#   9.- Precipitación acumulada en el periodo lluvioso

# - La función calcula el "ciclo anual" porque, en la Puna, el ciclo de crecimiento vegetal está
#    determinado por las condiciones de temperatura y precipitación, y se extiende desde mediados 
#    del segundo semestre de un año hasta mediados el primer semestre del año siguiente (Oct-Abr).
#    No tiene sentido, desde un punto de vista biológico, analizar los datos por año calendario.

# - Debido a lo anterior, es recomendable que el conjunto de datos inicie el 1 de julio de un año
#    y finalice el 30 de junio de otro año posterior.

# - La función no ha sido diseñada para tratar con datos ausentes, el conjunto de datos debe
#    estar completo. No obstante, fue probado con datos faltantes y funciona, pero se debe
#    considerar que puede haber errores en los resultados ( => Se debe verificar! ).

# - El valor mínimo permitido de 'umbral_dia_lluvioso' es 0.1 mm y el de 'min_dias_humedo'
#     es 2 días


# ---------------------------------------------------------------------------------
# --- Crear la función identificar_periodos() --- 

## Cargar los paquetes requeridos. Instalarlos, si es necesario.
if(!require(lubridate)) {install.packages("lubridate")} # Para trabajar con fechas
if(!require(dplyr)) {install.packages("dplyr")}         # Para manipulación de datos
if(!require(tidyr)) {install.packages("tidyr")}

#require(lubridate)
#require(dplyr)
#require(tidyr)

## Inicio de la creación de la función

identificar_periodos <- function(datos,         # Datos de entrada (nombre del data.frame)
                                 umbral_temp_media = 10,            # Umbral de temperatura media mensual
                                 umbral_precip_mensual = 50,        # Umbral de precip. acumulada mensual
                                 umbral_dia_lluvioso = 5,           # U|mbral de precip. de día lluvioso
                                 umbral_precip_periodo_humedo = 30, # Umbral de precip. acum. en periodo húmedo
                                 max_dias_secos = 5,                # Nro. máximo de días secos a posteriori
                                 min_dias_humedo = 3) {             # Nro. mínimo de días del periodo húmedo
  
  # Agregar columnas de "año" y "mes"
  datos$año <- year(datos$fecha)
  datos$mes <- month(datos$fecha)
  
  # Calcular temperatura media y precipitación acumulada mensual
  datos_mensuales <- datos %>%
    group_by(año, mes) %>%
    summarise(
      temp_promedio = mean(temp),           # Temperatura media mensual
      precip_total = sum(precip)            # Precipitación acumulada mensual
    )
  
  # Agregar columna "año_mes"
  datos_mensuales$año_mes <- with(datos_mensuales, paste0(año, "_", mes))
  
  ## Identificar y filtrar los meses que cumplen la condición de temp. media y precip. acum. mensual
  meses_validos <- datos_mensuales %>%
    filter(temp_promedio > umbral_temp_media & precip_total > umbral_precip_mensual)
  
  ## Identificar todos los periodos lluviosos y secos
  # Identificar si un día es lluvioso o seco. El resultado es TRUE/FALSE (lluvioso/seco)
  datos$es_lluvioso <- datos$precip >= umbral_dia_lluvioso
  # Identificar secuencias de periodos lluviosos y secos con función rle():
  # "values" indica si es lluvioso o seco (TRUE/FALSE) y "lengths" indica la duración del periodo
  periodos <- rle(datos$es_lluvioso)
  
  ## Evaluar todos los periodos (lluviosos y secos) y determinar cuáles cumplen las condiciones
  # Índice inicial
  indice_inicio <- 1
  # Crear lista vacía para almacenar los periodos válidos
  periodos_validos <- list()
  
  # El ciclo for() recorre todos los periodos húmedos/secos y evalua las condiciones
  #   pre-establecidas, si un periodo cumple las condiciones, lo guarda en la lista
  #  'periodos_validos', sino, pasa al siguiente.
  
  for (i in 1:length(periodos$lengths)) {
    # Calcular el índice final
    indice_fin <- indice_inicio + periodos$lengths[i] - 1
    
    # Si el periodo es lluvioso y dura >= min_dias_humedo, evaluar precipitación acumulada
    if (periodos$values[i] && periodos$lengths[i] >= min_dias_humedo) {
      periodo_lluvioso <- datos[indice_inicio:indice_fin, ]
      
      # Si la precipitación acumulada es >= umbral indicado, evaluar el periodo seco siguiente
      if (sum(periodo_lluvioso$precip) >= umbral_precip_periodo_humedo) {
        
        # Si el i-ésimo periodo analizado no es el último, y el periodo siguiente es seco
        #  calcular los índices de inicio y fin del periodo seco
        if (i < length(periodos$lengths) && !periodos$values[i+1]) {
          # Calcular del índice inicial del periodo seco
          inicio_seco <- indice_fin + 1
          # Calcular del índice final del periodo seco
          fin_seco <- inicio_seco + periodos$lengths[i+1] - 1
          # Extraer filas del periodo seco, según índices inicial y final
          periodo_seco <- datos[inicio_seco:fin_seco, ]
          
          # Si el periodo seco dura <= el máximo de días secos permitido, calcular variables
          #  y guardarlas en la lista
          if (nrow(periodo_seco) <= max_dias_secos) {
            # Guardar la información en la lista de periodos válidos
            periodos_validos[[length(periodos_validos) + 1]] <- list(
              # Fecha de inicio del periodo lluvioso
              inicio_lluvioso = periodo_lluvioso$fecha[1],
              # Fecha de fin del periodo lluvioso
              fin_lluvioso = periodo_lluvioso$fecha[nrow(periodo_lluvioso)],
              # Fecha de inicio del periodo seco
              inicio_seco = periodo_seco$fecha[1],
              # Fecha de fin del periodo seco
              fin_seco = periodo_seco$fecha[nrow(periodo_seco)],
              # Número de días lluviosos
              dias_lluviosos = nrow(periodo_lluvioso),
              # Número de días secos
              dias_secos = nrow(periodo_seco),
              # Precipitación acumulada en el periodo lluvioso
              precip_total = round(sum(periodo_lluvioso$precip),1)
            )
          }
        }
      }
    }
    
    # Actualizar el índice inicial para la próxima iteración del ciclo for()
    indice_inicio <- indice_fin + 1
  }
  
  # Convertir la lista de periodos válidos en un data.frame
  resultado <- do.call(rbind, lapply(periodos_validos, as.data.frame))
  
  # Filtrar periodos que cumplen con la condición de temp. media y precip. acum. mensual 
  resultado <- resultado %>%
    filter(paste0(year(inicio_lluvioso), "_", month(inicio_lluvioso)) %in% meses_validos$año_mes)
  
  # Agregar columnas de "ciclo anual" y "mes"
  resultado <- resultado %>%
    mutate(
      ciclo_anual = case_when(
        month(inicio_lluvioso) >= 7 ~ paste0(year(inicio_lluvioso), "-", year(inicio_lluvioso) + 1),
        month(inicio_lluvioso) < 7 ~ paste0(year(inicio_lluvioso) - 1, "-", year(inicio_lluvioso))
      )
    ) %>%
    mutate(mes = month(inicio_lluvioso)
    ) %>%
    select(ciclo_anual, mes, everything())  # Mover "ciclo_anual" y "mes" al inicio
  
  # Mostrar resultado
  return(resultado)
  
}

# --- FIN función identificar_periodos() ---

# -------------------------------------------------------------------

## Correr la función identificar_periodos() para identificar los periodos válidos
#   Se asume que el data.frame se llama 'datos'. La salida es un data.frame llamado 'resultado'

resultado <- identificar_periodos(datos)
head(resultado, 10) # Muestra las 10 primeras filas
#print(resultado)    # Muestra todo el data.frame 'resultado'

## Variables (columnas) del data.frame 'resultado':
#
#   1. ciclo_anual
#   2. mes
#   3. inicio_lluvioso
#   4. fin_lluvioso
#   5. inicio_seco
#   6. fin_seco
#   7. dias_lluviosos,
#   8. dias_secos
#   9. precip_total

## Si se desea cambiar los argumentos por defecto, por ejemplo:

ejemplo <- identificar_periodos(datos,
                                umbral_temp_media = 8,             # Umbral de temperatura media mensual
                                umbral_precip_mensual = 30,        # Umbral de precip. acumulada mensual
                                umbral_dia_lluvioso = 5,           # Umbral de precip. de día lluvioso
                                umbral_precip_periodo_humedo = 15, # Umbral de precip. acum. en periodo húmedo
                                max_dias_secos = 7,                # Nro. máximo de días secos a posteriori
                                min_dias_humedo = 2)               # Nro. mínimo de días del periodo húmedo

head(ejemplo, 20) # Mostrar 20 primeras filas
tail(ejemplo, 20) # Mostrar 20 últimas filas
#print(ejemplo)   # Mostrar tabla completa

# Nota: el valor mínimo permitido de 'umbral_dia_lluvioso' es 0.1 mm y el de 'min_dias_humedo' es 2 días

# Eliminar ejemplo
rm(ejemplo)


# =================== Scripts para calcular estadísticas sobre los periodos válidos ========================

## Script 1
#  Para calcular y tabular la cantidad de periodos ocurridos por mes y ciclo anual. Si hay meses y
#   ciclos ausentes debido a que no hay datos, los valores son ceros. También, para calcular las
#   frecuencias de los periodos por ciclo anual.

## Script 2
#  Para calcular el promedio, el desvio estandar, el valor mínimo y el máximo de las precipitaciones
#   acumuladas por periodo. También, para calcular las frecuencias por intervalos de clase.

## Script 3
#  Para calcular las frecuencias, absoluta y relativa, del número de días del periodo
#   húmedo.

## Script 4
#  Para calcular las frecuencias, absoluta y relativa, del número de días del periodo
#   seco.

## Script 5
#  Para calcular la fecha promedio, el desvio estandar (en días), la fecha más temprana y la
#   más tardía, de ocurrencia del primer periodo húmedo de cada ciclo anual, para toda la
#   serie de ciclos anuales.


# --------------------------------------------------------------------------------------------------
# --- Script 1: Cantidad de periodos válidos por mes y ciclo anual ---

# Cargar los paquetes necesarios
library(dplyr)
library(tidyr)
library(lubridate)

## Función para calcular la cantidad de periodos válidos por mes y ciclo anual

# Nota: la función requiere, como argumentos de entrada, los data.frames 'datos' y 'resultado'

n_periodos <- function(datos, resultado){
  
  # Crear la secuencia completa de ciclos anuales y meses a partir del archivo de datos original
  datos <- datos %>%
    mutate(
      año = year(fecha),
      mes = month(fecha),
      ciclo_anual = if_else(mes >= 7, paste0(año, "-", año + 1), paste0(año - 1, "-", año))
    )
  
  # Generar todas las combinaciones posibles de ciclo_anual y mes
  todos_los_meses <- datos %>%
    select(ciclo_anual, mes) %>%       # Seleccionar variables
    distinct() %>%                     # Equivalente a unique()
    complete(ciclo_anual, mes = 1:12)  # Para asegurarse de tener todos los meses (1 a 12)
  
  # Contar la cantidad de periodos válidos por ciclo anual y mes a partir de los resultados
  #  y unirla con la tabla todos_los_meses (para incluir en las tabla los ciclos anuales
  #  en los que no hay datos)
  tabla_periodos <- resultado %>%
    group_by(ciclo_anual, mes) %>%
    summarise(cantidad = n(), .groups = 'drop') %>%               # Calcular nro periodos por ciclo anual
    right_join(todos_los_meses, by = c("ciclo_anual", "mes")) %>% # Unir tablas
    replace_na(list(cantidad = 0)) %>%                            # Reemplazar NAs por ceros
    arrange(ciclo_anual, mes)                                     # Reordenar por ciclo anual y mes
  
  # Convertir la tabla de formato largo a formato ancho (ubicar meses como columnas)
  tabla_periodos_ancha <- tabla_periodos %>%
    pivot_wider(
      names_from = mes,                # Los nombres de las columnas serán los meses
      values_from = cantidad,          # Los valores serán las cantidades de periodos
      names_prefix = "Mes_"            # Prefijo para las columnas de meses
    )
  
  # Reordenar los meses de 7 a 12 y de 1 a 6, de acuerdo con los ciclos anuales
  tabla_periodos_ancha <- tabla_periodos_ancha[,c(1, 8:13, 2:7)]
  
  # Calcular los totales por ciclo anual
  tabla_periodos_ancha$total_ciclo <- NA  
  tabla_periodos_ancha$total_ciclo <- apply(tabla_periodos_ancha[,2:13], 1, sum)
  
  # Mostrar la tabla final con los periodos contados por ciclo y mes
  return(as.data.frame(tabla_periodos_ancha))
}


## Calcular y previsualizar la cantidad de periodos válidos por mes y ciclo anual
periodos <- n_periodos(datos, resultado)
head(periodos, 10) # Muestra los 10 primeros datos solamente
#print(periodos)    # Muestra la tabla completa (todo el data.frame)

# Cálculo de frecuencias absoluta (nro. obs.) y relativa (%) del nro de periodos
frec.abs.period <- table(periodos$total_ciclo)
frec.rel.period <- round(prop.table(frec.abs.period)*100,1)

# Mostrar tablas de frecuencias
print(frec.abs.period) # Frec. absoluta
print(frec.rel.period) # Frec. relativa

## Gráfico del nro total de periodos por ciclo anual

# Gráfico frecuencias absolutas
barplot(frec.abs.period, ylim=c(0,20),
        main="Periodos por ciclo anual",
        xlab="Nro. de periodos por ciclo anual",
        ylab="Frecuencia absoluta (nro. obs.)")

# Gráfico frecuencias relativas
barplot(frec.rel.period,
        main="Periodos por ciclo anual",
        xlab="Nro. de periodos por ciclo anual",
        ylab="Frecuencia relativa (%)")

## Guardar gráfico como .png
png("frec_rel_periodos_por_ciclo_anual.png", width=100, height=100, units="mm", res=300)
barplot(frec.rel.period,
        main="Periodos por ciclo anual",
        xlab="Nro. de periodos por ciclo anual",
        ylab="Frecuencia relativa (%)")
dev.off()



# --------------------------------------------------------------------------------------------------
## --- Script 2: Estadísticas descriptivas de la precipitación acumulada por periodo ---

# Función para calcular estadísticas descriptivas de la precipitación acumulada por periodo

precip_acum <- function(precip_total) {
  media = round(mean(precip_total, na.rm = TRUE),1)
  ds = round(sd(precip_total, na.rm = TRUE),1)   # desvío estándar
  n = length(precip_total)
  ee = round(ds/sqrt(n), 1)                      # error estándar
  min = round(min(precip_total, na.rm = TRUE),1)
  max = round(max(precip_total, na.rm = TRUE),1)
  estadisticas_precip <- c(media=media, ee=ee, n=n, min=min, max=max)
  return(estadisticas_precip)
}

# Calcular y mostrar las estadísticas
est_precip_tot <- precip_acum(resultado$precip_total)
print(est_precip_tot)

## Gráficos

## Boxplot
boxplot(resultado$precip_total, 
        main="Precipitación acumulada por periodo",
        ylab="Precipitación acumulada (mm/periodo)", xlab="")
points(x=1, y=est_precip_tot["media"], pch=19, col="red") # Añade punto con la media

## Histograma - Frecuencia absoluta
hist(resultado$precip_total, ylim=c(0,12),
     main="Precipitación acumulada por periodo",
     ylab="Frecuencia absoluta (nro. obs.)", xlab="Intervalos de clase (mm)")

## Histograma - Frecuencia relativa
# Cálculo de los porcentajes y modificación de los datos del histograma,
#  para que se muestren como porcentajes y no como conteos
hist_datos <- hist(resultado$precip_total, plot=F)
print(hist_datos)
conteos <- hist_datos$counts
porc <- round(conteos/sum(conteos)*100,1)
print(porc) # [1] 24.1 37.9 24.1  6.9  3.4  3.4
hist_datos$counts <- porc
rm(conteos, porc)
print(hist_datos$counts)
# Gráficar histograma
plot(hist_datos, ylim=c(0,40),
     main="Precipitación acumulada por periodo",
     ylab="Frecuencia relativa (%)", xlab="Intervalos de clase (mm)")

## Guardar gráfico como .png
png("frec_rel_precip_acum_periodo_lluvioso.png", width=100, height=100, units="mm", res=300)
plot(hist_datos, ylim=c(0,40),
     main="Precipitación acumulada por periodo",
     ylab="Frecuencia relativa (%)", xlab="Intervalos de clase (mm)")
dev.off()


# --------------------------------------------------------------------------------------------------
## --- Script 3: Estadísticas descriptivas del número de días lluviosos por periodo ---

# Función para calcular estadísticas descriptivas del número de días lluviosos

n_dias_lluviosos <- function(dias_lluviosos){
  media = round(mean(dias_lluviosos, na.rm = TRUE),1)
  ds = round(sd(dias_lluviosos, na.rm = TRUE),1)      # desvío estándar
  n = length(dias_lluviosos)
  ee = round(ds/sqrt(n), 1)                           # error estándar
  min = round(min(dias_lluviosos, na.rm = TRUE),1)
  max = round(max(dias_lluviosos, na.rm = TRUE),1)
  estadisticas_dias_lluviosos <- c(media=media, ee=ee, n=n, min=min, max=max)
  return(estadisticas_dias_lluviosos)
}

# Calcular y mostrar las estadísticas
n_dias_lluvia <- n_dias_lluviosos(resultado$dias_lluviosos)
print(n_dias_lluvia)

# Calcular tablas de frecuencias absoluta y relativa
d_lluvia.frec.abs <- table(resultado$dias_lluviosos)
d_lluvia.frec.rel <- prop.table(d_lluvia.frec.abs)*100
d_lluvia.frec.rel <- round(d_lluvia.frec.rel,1)

# Mostrar tablas de frecuencias
print(d_lluvia.frec.abs) # Frec. absoluta
print(d_lluvia.frec.rel) # Frec. relativa

# Gráfico de barras - frecuencia absoluta
barplot(d_lluvia.frec.abs,
        main="Duración del periodo lluvioso",
        ylab="Frecuencia absoluta (nro. obs.)", xlab="Nro. de días del periodo lluvioso")

# Gráfico de barras - frecuencia relativa
barplot(d_lluvia.frec.rel, ylim=c(0,50),
        main="Duración del periodo lluvioso",
        ylab="Frecuencia relativa (%)", xlab="Nro. de días del periodo lluvioso")

## Guardar gráfico como .png
png("frec_rel_duracion_periodo_lluvioso.png", width=100, height=100, units="mm", res=300)
barplot(d_lluvia.frec.rel, ylim=c(0,50),
        main="Duración del periodo lluvioso",
        ylab="Frecuencia relativa (%)", xlab="Nro. de días del periodo lluvioso")
dev.off()

# --------------------------------------------------------------------------------------------------
## --- Script 4: Estadísticas descriptivas del número de días secos por periodo ---

# Función para calcular estadísticas descriptivas del número de días secos

n_dias_secos <- function(dias_secos){
  media = round(mean(dias_secos, na.rm = TRUE),1)
  ds = round(sd(dias_secos, na.rm = TRUE),1) # desvío estándar
  n = length(dias_secos)
  ee = ds/sqrt(n)                            # error estándar
  min = round(min(dias_secos, na.rm = TRUE),1)
  max = round(max(dias_secos, na.rm = TRUE),1)
  estadisticas_dias_secos <- c(media=media, ee=ee, n=n, min=min, max=max)
  return(estadisticas_dias_secos)
}

# Calcular y mostrar las estadísticas descriptivas
n_dias_seco <- n_dias_secos(resultado$dias_secos)
print(n_dias_seco)

# Calcular tablas de frecuencias absoluta y relativa
d_seco.frec.abs <- table(resultado$dias_secos)
d_seco.frec.rel <- prop.table(d_seco.frec.abs)*100
d_seco.frec.rel <- round(d_seco.frec.rel,1)

# Mostrar tablas de frecuencias
print(d_seco.frec.abs) # Frec. absoluta
print(d_seco.frec.rel) # Frec. relativa

# Gráfico de barras - frecuencia absoluta
barplot(d_seco.frec.abs,
        main="Duración del periodo seco",
        ylab="Frecuencia absoluta (nro. obs.)", xlab="Nro. de días del periodo seco")

# Gráfico de barras - frecuencia relativa
barplot(d_seco.frec.rel, ylim=c(0,50),
        main="Duración del periodo seco",
        ylab="Frecuencia relativa (%)", xlab="Nro. de días del periodo seco")

## Guardar gráfico como .png
png("frec_rel_duracion_periodo_seco.png", width=100, height=100, units="mm", res=300)
barplot(d_seco.frec.rel, ylim=c(0,50),
        main="Duración del periodo seco",
        ylab="Frecuencia relativa (%)", xlab="Nro. de días del periodo seco")
dev.off()

# --------------------------------------------------------------------------------------------------
## --- Script 5: Calcular la fecha promedio de ocurrencia del primer periodo húmedo ---

# Cargar los paquetes necesarios
library(dplyr)
library(lubridate)

# Crear la secuencia completa de ciclos anuales a partir del archivo de datos original
datos <- datos %>%
  mutate(
    año = year(fecha),
    mes = month(fecha),
    ciclo_anual = if_else(mes >= 7, paste0(año, "-", año + 1), paste0(año - 1, "-", año))
  )

# Función para calcular el día del ciclo anual (desde el 1ro de julio)
calcular_dia_del_ciclo <- function(fecha, ciclo_anual) {
  # El ciclo comienza el 1 de julio (día 1)
  inicio_ciclo <- as.Date(paste0(substring(ciclo_anual, 1, 4), "-07-01"))
  return(as.numeric(fecha - inicio_ciclo) + 1)
}

# Identificar el primer periodo húmedo de cada ciclo anual
primeros_periodos <- resultado %>%
  group_by(ciclo_anual) %>%
  summarise(primer_inicio_lluvioso = min(inicio_lluvioso, na.rm = TRUE), .groups = 'drop')

# Calcular el día del ciclo anual en que ocurre el primer periodo húmedo
primeros_periodos <- primeros_periodos %>%
  mutate(dia_del_ciclo = mapply(calcular_dia_del_ciclo, primer_inicio_lluvioso, ciclo_anual))

# Calcular el promedio de los días de ocurrencia del primer periodo
promedio_dia <- round(mean(primeros_periodos$dia_del_ciclo, na.rm = TRUE),0)
ds_dia <- round(sd(primeros_periodos$dia_del_ciclo, na.rm = TRUE),0)
minimo_dia <- round(min(primeros_periodos$dia_del_ciclo, na.rm = TRUE),0)
maximo_dia <- round(max(primeros_periodos$dia_del_ciclo, na.rm = TRUE),0)


# Función para convertir un día del ciclo anual a una fecha (día y mes)
convertir_dia_a_fecha <- function(dia, año_inicial) {
  # El ciclo comienza el 1 de julio
  inicio_ciclo <- as.Date(paste0(año_inicial, "-07-01"))
  fecha_resultante <- inicio_ciclo + days(dia - 1)
  return(fecha_resultante)
}

# Convertir el promedio de días a una fecha (asumiendo el ciclo comienza el 1 de julio de
#  un año no bisiesto)
fecha_promedio <- convertir_dia_a_fecha(promedio_dia, 2000)  # Se usa un año cualquiera, ej. 2000
fecha_mas_temprana <- convertir_dia_a_fecha(minimo_dia, 2000)
fecha_mas_tardia <- convertir_dia_a_fecha(maximo_dia, 2000)

# Extraer el día y mes del promedio
dia_promedio <- day(fecha_promedio)
mes_promedio <- month(fecha_promedio, label = TRUE, abbr = FALSE)
inicio_promedio <- paste0(dia_promedio ," de ", mes_promedio)

# Extraer el día y mes de la fecha más temprana en que ha ocurrido un periodo húmedo
dia_temprana <- day(fecha_mas_temprana)
mes_temprana <- month(fecha_mas_temprana, label = TRUE, abbr = FALSE)
inicio_temprano <- paste0(dia_temprana," de ", mes_temprana)

# Extraer el día y mes de la fecha más tardía en que ha ocurrido un periodo húmedo
dia_tardia <- day(fecha_mas_tardia)
mes_tardia <- month(fecha_mas_tardia, label = TRUE, abbr = FALSE)
inicio_tardio <- paste0(dia_tardia," de ", mes_tardia)

# Guardar resultado en un data frame
result_fecha_inicio <- data.frame(fecha_inicio_promedio=inicio_promedio,
                                  desvio_estandar_dias=ds_dia,
                                  fecha_mas_temprana=inicio_temprano,
                                  fecha_mas_tardia=inicio_tardio)

# Mostrar tabla de resultados (traspuesta)
print(t(result_fecha_inicio))


# ==================================================================================================

## ------ Gráficar los periodos vs los ciclos anuales -------

## Cálculos adicionales para hacer los gráficos

# Añadir a la tabla de "resultado" las fechas calculadas como días desde el 1 de julio

# Variables de la tabla 'resultado'
names(resultado)
#[1] "ciclo_anual"     "mes"             "inicio_lluvioso" "fin_lluvioso"   
#[5] "inicio_seco"     "fin_seco"        "dias_lluviosos"  "dias_secos"     
#[9] "precip_total" 

# Función para calcular el día del ciclo anual
calcular_dia_del_ciclo <- function(fecha, ciclo_anual) {
  # El ciclo comienza el 1 de julio (día 1)
  inicio_ciclo <- as.Date(paste0(substring(ciclo_anual, 1, 4), "-07-01"))
  return(as.numeric(fecha - inicio_ciclo) + 1)
}

# Crear una copia de la tabla para no alterar, por error, la de origen.
resultado2 <- resultado

# Crear nuevas variables para poder construir el gráfico: 
#  inicio_lluvioso_d, fin_lluvioso_b, inicio_seco_d, fin_seco_d  

resultado2 <- resultado2 %>% 
  mutate(inicio_lluvioso_d = calcular_dia_del_ciclo(inicio_lluvioso, ciclo_anual),
         fin_lluvioso_d = calcular_dia_del_ciclo(fin_lluvioso, ciclo_anual),
         inicio_seco_d = calcular_dia_del_ciclo(inicio_seco, ciclo_anual),
         fin_seco_d = calcular_dia_del_ciclo(fin_seco, ciclo_anual)) %>%
  as.data.frame()

# Verificar la correcta creación de las variables
head(resultado2)

## Crear gráficos de los periodos válidos vs los ciclos anuales

# Cargar el paquete necesario {ggplot2}. Si no esta instalado, instalar.
if(!require(ggplot2, quietly=TRUE)){install.packages("ggplot2")}
library(ggplot2, quietly=TRUE)

# Crear gráfico de periodos (periodo húmedo y seco juntos, sin diferenciar)
g1 <- ggplot(resultado2, aes(x = ciclo_anual, y = 1:366)) +
  geom_segment(aes(x = ciclo_anual, xend = ciclo_anual,
                   y = inicio_lluvioso_d, yend = fin_seco_d+1), # (*)
               linewidth = 3) +
  geom_hline(yintercept=200, linetype=2) + # Añade línea horizontal (**)
  labs(x = "Ciclo anual", y = "Días desde el 1 de julio") +
  scale_x_discrete(limits=rev) +           # Invierte el orden de las etiquetas del eje x
  scale_y_continuous(breaks = seq(0, 366, by = 10)) + # Ajusta eje y, entre 0 y 366, de a 10 u.
  coord_flip()                                        # Traspone los ejes del gráfico
#print(g1)

# Crear gráfico de periodos, diferenciando periodo húmedo (azul) y seco (rojo)
g2 <- ggplot(resultado2, aes(x = ciclo_anual, y = 1:366)) +
  geom_segment(aes(x = ciclo_anual, xend = ciclo_anual, 
                   y = inicio_lluvioso_d, yend = fin_lluvioso_d+1), # (*)
               col="blue", linewidth = 3) +
  geom_segment(aes(x = ciclo_anual, xend = ciclo_anual,
                   y = inicio_seco_d, yend = fin_seco_d+1), # (*)
               col="red", linewidth = 3) +
  geom_hline(yintercept=200, linetype=2) + # Añade línea horizontal (**)
  labs(x = "Ciclo anual", y = "Días desde el 1 de julio") +
  scale_x_discrete(limits=rev) +           # Invierte el orden de las etiquetas del eje x
  scale_y_continuous(breaks = seq(0, 366, by = 10)) + # Ajusta eje y, entre 0 y 366, de a 10 u.
  coord_flip()                                        # Traspone los ejes del gráfico 
print(g2)

# Guardar gráfico g2 como .png
ggsave("periodos_vs_ciclo_anual.png", g2, width = 160, height = 100, units = "mm", dpi = 300)


# Aclaraciones: 
#   (*) Se debe sumar 1 para que se muestre correctamente, ya que, si el periodo tiene solo
#       un día, la fecha de inicio y de fin es la misma; luego, el segmento tendrá longitud
#       cero y no se mostrará en el gráfico.
#  (**) Queda vertical al trasponer el gráfico


#                      ------------- FIN ANALISIS PERIODOS VALIDOS -------------

############################################################################################################


#                 --------- SCRIPT PARA CONSTRUIR GRAFICO DE PRECIPITACION ---------

## --- Script para la clasificación de la precipitación diaria por categorías ---

## El siguiente script clasifica la precipitación diaria en diferentes categorías:
#
#   - Sin precipitación (precip < 5 mm)
#   - Precipitación de 5 a 10 mm
#   - Precipitación de 10 a 20 mm
#   - Precipitación mayor a 20 mm
#
#  Luego, con esa información se elabora un gráfico de precipitaciones (por categoría) vs ciclo anual

# Cargar paquetes
library(dplyr)
library(ggplot2)
library(lubridate)

# Cargar el archivo "datos_abra_pampa.RData" (se abre un cuadro de Windows)
load(file.choose())
head(datos)

# Filtrar por fecha, entre 01-07-1969 y 30-06-2007
datos <- datos %>%
  filter(fecha > "1969-06-30" & fecha < "2007-06-30")

# Generar las variables: ciclos anuales, año, mes
datos <- datos %>%
  mutate(
    ciclo_anual = case_when(
      month(fecha) >= 7 ~ paste0(year(fecha), "-", year(fecha) + 1),
      month(fecha) < 7 ~ paste0(year(fecha) - 1, "-", year(fecha))),
    año = year(fecha),
    mes = month(fecha)) %>%
  select(ciclo_anual, año, mes, fecha, precip) %>%
  as.data.frame()

head(datos)

# Crear variable categórica cat_pp, según rangos de precipitación
datos <- datos %>%
  mutate(
    cat_pp = case_when(
      precip < 5 ~ "Sin Precip.",
      precip >= 5 & precip < 10 ~ "5-10 mm",
      precip >= 10 & precip < 20 ~ "10-20 mm",
      precip >= 20 ~ "> 20 mm"))

head(datos, 10)
tail(datos, 10)

# Calcular las frecuencias (como medio de verificación)
table(datos$cat_pp)

# Filtrar sólo los días en que hubo precipitación
datos2 <- datos %>% 
  filter(cat_pp == "5-10 mm" | cat_pp == "10-20 mm" | cat_pp == "> 20 mm")
head(datos2)
table(datos2$cat_pp)

# Transformar la variable 'cat_pp' en factor y ordenar los niveles del factor
datos2$cat_pp <- factor(datos2$cat_pp, levels=c("5-10 mm", "10-20 mm", "> 20 mm"))
levels(datos2$cat_pp) # Verificación del orden de los niveles del factor

## Calcular día del ciclo anual (nro de días desde el 1 de julio de cada ciclo)

# Función para calcular el día del ciclo anual
calcular_dia_del_ciclo <- function(fecha, ciclo_anual) {
  # El ciclo comienza el 1 de julio (día 1)
  inicio_ciclo <- as.Date(paste0(substring(ciclo_anual, 1, 4), "-07-01"))
  return(as.numeric(fecha - inicio_ciclo) + 1)
}

# Cálculo de los días del ciclo anual a partir de las fechas
datos2 <- datos2 %>%
  mutate( dia_ciclo = calcular_dia_del_ciclo(fecha, ciclo_anual))

# Filtrar datos desde el día 120 hasta el día 280 (aprox. 01-nov a 31-mar)
datos2 <- datos2 %>%
  filter(dia_ciclo >= 120 & dia_ciclo <=280)

## Crear gráfico de precipitación según categorías, por ciclo anual

g3 <- ggplot(datos2, aes(x = ciclo_anual, y = 120:280)) +
  geom_segment(aes(x = ciclo_anual, xend = ciclo_anual, 
                   y = dia_ciclo, yend = dia_ciclo + 1,
                   color = factor(cat_pp)),  # Asignación del color según cat_pp
               linewidth = 3) +
  labs(x = "Ciclo anual", y = "Días desde el 1 de julio") +
  scale_x_discrete(limits=rev) +  # Invierte el orden de las etiquetas del eje x
  scale_y_continuous(breaks = seq(120, 280, by = 10)) +  # Ajusta eje y, entre 120 y 280, de a 10 u.
  scale_color_manual(values = rev(hcl.colors(n=3, "Viridis"))) +  # Paleta de colores personalizada
  coord_flip() +  # Traspone los ejes del gráfico
  theme(legend.title = element_blank(), # Sin título de leyenda
        legend.position = "bottom")      # Leyenda ubicada abajo del gráfico

# Mostrar gráfico
print(g3)

# Guardar gráfico g3 como .png
ggsave("precip_vs_ciclo_anual.png", g3, width = 180, height = 200, units = "mm", dpi = 300)


#                --------------- FIN ANALISIS PRECIPITACION --------------

############################################################################################################ 
