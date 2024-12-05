####README####

# El siguiente archivo se encuentra organizado en BLOQUES
# Con el objetivo de facilitar su seleccion y ejecuccion
# Es recomendable desplegar en el entorno R studio el "outline" del documento (CTRL + SHIFT + O)
# Esto facilita la visualizacion y desplazamiento a travez de los diferentes bloques 

# Para la seleccion de los bloques se puede utilzar el boton izquierdo del raton y arrastrar 
# O puede mantener apretando la tecla SHIFT y navegar con las teclas de direccion 
# Para correr el script seleccionado puede optar por utilizar el apartado "correr"en R studio 
# O la combinacion del teclado (CTRL + ENTER)



#MODULOS O BLOQUES DE CODIGO ORDENADOS SEGUN PRIORIDAD DE EJECUCCION. 

##### 1) INSTALACION y ACTIVACION DE DEPENDENCIAS ######

rm(list=ls(all=TRUE))

install.packages("openxlsx2")
install.packages("purrr")
install.packages("lubridate")
install.packages("cellranger")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("rstatix")
install.packages("dplyr")


library(openxlsx2)  
library(purrr)      
library(lubridate) 
library(cellranger)
library(ggplot2)
library(tidyr)
library(rstatix)
library(dplyr)





##### 2) IMPORTACION  #####

# Seleccionar Carpeta Raiz donde se encuentran las planillas mensuales
# agrupadas por año en subcarpetas.

# Importa los datos contenidos en la hoja "Datos Diarios" para cada planilla
# meteorologica mensual INTA, como resultado crea un dataframe llamado "datos"
# Despliega en consola diferentes previualizaciones del contenido en el dataframe "datos"




ruta <- choose.dir()


archivos <- list.files(path = ruta,
                       pattern = "^[1-2](.*).xlsx$",
                       full.names = TRUE,
                       recursive = TRUE)


hoja <- c("Datos Diarios")


cols <- cell_cols("B:BL") 
cols <- seq(cols[[1]][2], cols[[2]][2], by=1) 


filas <- c(16,22,28,50,56,62,84,90,96,118,124,130,152,158,164,186,192,198,220,226,232,254,260,266,288,294,300,
           322,328,334,356,362,368,390,396,402,424,430,436,458,464,470,492,498,504,526,532,538,560,566,572,594,600,
           606,628,634,640,662,668,674,696,702,708,730,736,742,764,770,776,798,804,810,832,838,844,866,872,878,900,
           906,912,934,940,946,968,974,980,1002,1008,1014,1036,1042,1048)



n_dias <- function(archivo){
  año_mes <- substr(basename(archivo), 1, 7)
  año <- substr(año_mes, 1, 4)
  mes <- substr(año_mes, 6, 7)
  fecha <- paste(año, mes, "01", sep = "-")
  dias_mes <- days_in_month(as.Date(fecha))
  return(dias_mes)
} 

n_filas <- function(n_dias) {
  if(n_dias==31) { n_filas <- filas
  } else if(n_dias==30) { n_filas <- filas[1:90] 
  } else if(n_dias==29) { n_filas <- filas[1:87] 
  } else { n_filas <- filas[1:84]
  } 
  return(n_filas)
} 



importar_filas <- function(archivo) {
  año <- substr(basename(archivo), 1, 4)
  mes <- substr(basename(archivo), 6, 7)
  dias <- n_dias(archivo)
  nro.filas <- n_filas(dias)
  AÑO <- as.integer(rep(año, times=dias*3))
  MES <- as.integer(rep(mes, times=dias*3))
  DIA <- as.integer(rep(1:dias, each=3))
  HORA <- as.integer(rep(c("09", "15", "21"), times=dias))
  FECHA <- make_datetime(year=AÑO, month=MES, day=DIA)
  FECHA.HORA <- make_datetime(year=AÑO, month=MES, day=DIA, hour=HORA)
  datos <- read_xlsx(xlsxFile=archivo,
                     sheet=hoja,
                     colNames = FALSE,
                     rowNames = FALSE, 
                     detectDates = FALSE,
                     skipEmptyRows = TRUE,
                     skipEmptyCols = FALSE, 
                     rows = nro.filas,
                     cols = cols,
                     check.names = FALSE,
                     sep.names = ".") 
  datos <- cbind(FECHA.HORA, FECHA, AÑO, MES, DIA, HORA, datos)
  
  return(datos)
} 


datos <- map(archivos, ~importar_filas(.x))

datos <- do.call(rbind, datos)

## Verificaciones

basename(archivos) 
head(datos, 3)     
tail(datos, 3)     
str(datos)         
names(datos)       
dim(datos)         



#### 3) FILTRADO ####

# Filtrado de columnas vacías

# Crea vector nombres con los nombres de cada columna de la hoja "datos diarios"
# Agrega el prefijo "eliminar" en las columnas que descea remover
# Finalmente obtiene un data frame "datos 2" filtrado y lo previsualiza en consola 



nombres <- c(       
  "fecha.hora",                # [1,] "FECHA.HORA"
  "fecha",                     # [2,] "FECHA"     
  "año",                       # [3,] "AÑO"       
  "mes",                       # [4,] "MES"       
  "dia",                       # [5,] "DIA"       
  "hora",                      # [6,] "HORA"      
  "eliminar_dia",              # [7,] "B" - verificación día planilla          
  "eliminar_dia",              # [8,] "C" - verificación hora planilla          
  "termometro_pres_atm_g",     # [9,] "D"         
  "barometro_mm_Hg",           #[10,] "E"         
  "barometro_mm_Hg_0g_cCg",    #[11,] "F"         
  "barometro_hPa_0g_cCg",      #[12,] "G"         
  "barografo_hPa",             #[13,] "H"         
  "tendencia_y_valor",         #[14,] "I"         
  "termometro_seco_g",         #[15,] "J"         
  "termometro_humedo_g",       #[16,] "K"         
  "bulbo",                     #[17,] "L"         
  "termografo_g",              #[18,] "M"         
  "temp_maxima_g",             #[19,] "N"         
  "temp_mimima_g",             #[20,] "O"         
  "termometro_seco_g_CC",      #[21,] "P"         
  "termometro_humedo_g_CC",    #[22,] "Q"         
  "tension_vapor_hPa",         #[23,] "R"         
  "humedad_relativa_porc",     #[24,] "S"         
  "eliminar_humedad_relativa_porc2",    #[25,] "T"         
  "eliminar_humedad_relativa_porc2",    #[26,] "U"         
  "temp_pto_rocio_g",          #[27,] "V"         
  "eliminar_temp_pto_rocio_g2",         #[28,] "W"         
  "eliminar_temp_pto_rocio_g3",         #[29,] "X"         
  "eliminar_temp_pto_rocio_g4",         #[30,] "Y"         
  "viento_dir_texto",          #[31,] "Z"         
  "viento_veloc_beaufort",     #[32,] "AA"        
  "eliminar_viento_veloc_beaufort2",    #[33,] "AB"        
  "viento_dir_cifrado",        #[34,] "AC"        
  "eliminar_viento_dir_cifrado2",       #[35,] "AD"        
  "viento_veloc_nudos",        #[36,] "AE"        
  "eliminar_viento_veloc_nudos2",       #[37,] "AF"        
  "eliminar_viento_veloc_nudos3",       #[38,] "AG"        
  "nubosidad_total",           #[39,] "AH"        
  "est_nub_A_forma",           #[40,] "AI"        
  "est_nub_A_grado",           #[41,] "AJ"        
  "est_nub_M_forma",           #[42,] "AK"        
  "est_nub_M_grado",           #[43,] "AL"        
  "est_nub_B_forma",           #[44,] "AM"        
  "est_nub_B_grado",           #[45,] "AN"        
  "alt_base_nub_baja",         #[46,] "AO"        
  "visibilidad",               #[47,] "AP"        
  "eliminar_visibilidad2",              #[48,] "AQ"        
  "est_suelo",                 #[49,] "AR"        
  "eliminar_alt_base_nub_baja2",        #[50,] "AS"        
  "eliminar_alt_base_nub_baja3",        #[51,] "AT"        
  "tiempo_presente",           #[52,] "AU"        
  "eliminar_tiempo_presente2",          #[53,] "AV"        
  "pres_atm_nivel_mar_hPa_alt",  #[54,] "AW"        
  "eliminar_pres_atm_nivel_mar_hPa_alt2", #[55,] "AX"        
  "eliminar_pres_atm_nivel_mar_hPa_alt3", #[56,] "AY"        
  "eliminar_pres_atm_nivel_mar_hPa_alt4", #[57,] "AZ"        
  "alt_nieve_cm",              #[58,] "BA"        
  "eliminar_alt_nieve_cm2",             #[59,] "BB"        
  "eliminar_alt_nieve_cm3",             #[60,] "BC"        
  "higrografo",                #[61,] "BD"        
  "eliminar_higrografo2",               #[62,] "BE"        
  "tiempo_pasado",             #[63,] "BF"        
  "precipitacion_mm",          #[64,] "BG"        
  "eliminar_precipitacion_mm2",          #[65,] "BH"        
  "eliminar_precipitacion_mm3",          #[66,] "BI"        
  "heliofanografo",            #[67,] "BJ"        
  "observaciones",             #[68,] "BK"        
  "iniciales_obs"              #[69,] "BL"
) 


names(datos) <- nombres

datos2 <- datos[, !grepl("^eliminar", names(datos))] 

head(datos2, 3)
tail(datos2, 3)



#### En este Paso ocurre un segundo filtrado donde el dataframe "datos2" se reduce
# solo a las columnas de interes para la investigacion 
# en este caso:

# Columna 2 = Fecha
# Columna 3 = Año
# Columna 4 = Mes
# Columna 5 = Dia
# Columna 6 = Hora
# Columna 28 = Nubosidad
# Columna 37 = Estado del suelo 
# Columna 43 = Precipitacion



DATOS.9am <- datos2 [,c(2:6,28,37,43)]



# Finalmente se obtiene un nuevo data frame filtrado llamado "DATOS.9am"
# El mismo contiene los valores de precipitacion de cada dia a las 9 am 
# pero no elimina 0 ni NA  


#Filtrados Extra 

datos <- datos[!is.na(datos$precipitacion_mm) & datos$precipitacion_mm != 0, ]  #Deja filtrados solo valores > 0

lluvias <- datos

datos_sinNA <- DATOS.9am[!is.na(DATOS.9am$precipitacion_mm),]                   #Filtra todos los NA y deja solo los 0



# Explicacion de cada dataframe que se cargara en la variable subconj.


subconj <- DATOS.9am      #datos de precipitacion diarios, con  NA y 0 
subconj <- datos_sinNA    #datos de precipitacion diarios, sin  NA con 0
subconj <- lluvias        #datos de precipitacion diarios, sin NA y sin 0
subconj <- DATOS_MDK     #datos corregidos 




















### 4) TEST DE RANGO FIJO  #####

# USUARIO
# El usuario define el rango mínimo (rango_min <- X) y rango máximo (rango_max <- Y) 
# de precipitación modificando los valores X e Y.

# QUE HACE EL SCRIPT
# El script crea una paleta de colores para resaltar elementos como valores de 
# precipitacion y los rangos maximos y minimos.
# El script grafica las fechas con sus valores de precipitación y lo umbrales maximos 
# y minimos definidos

# RESULTADO 
# Grafico de precipitaciones diarias donde el eje X corresponde a fechas y el eje Y 
# a valores de precipitacion en milimetros
# Posibilita visualizar la distribucion y magnitud de las lluvias y e identificar valores sospechosos


rango_min <- 0   #(X)
rango_max <- 60  #(Y)


colores <- c("#3366CC", "#FF9900", "#CC0033")


ggplot(DATOS.9am, aes(x = as.Date(fecha), y = precipitacion_mm)) +
  geom_line(size = 1.5) +
  geom_point(color = "green" ,size = 1.5, alpha = 0.7) +
  geom_hline(yintercept = c(rango_min, rango_max),
             color = "purple", linetype = "dashed",
             size = 1) +
  scale_y_continuous(limits = c(-30, 90), expand = c(0, 0),
                     sec.axis = sec_axis(~., name = "Precipitación (mm)")) +
  labs(x = "Fecha", y = NULL,
       title = "Análisis de Precipitación Diaria",
       subtitle = "Control de Rango fijo") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "gray70", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line.y.right = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.title.y.right = element_text(color = "blue", size = 14),
        axis.text.y.right = element_text(color = "blue", size = 12),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = "white"),
        legend.key.size = unit(1.5, "lines"),
        panel.background = element_rect(fill = "gray95"),
        panel.border = element_rect(color = "gray50", fill = NA),
        plot.margin = margin(30, 30, 30, 30)) +
  scale_color_manual(values = colores) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date(c("1980-01-01", "1981-01-01", "1982-01-01")),
             color = "#FF3366", linetype = "dotted", size = 1.2, alpha = 0.5) +
  geom_text(aes(x = as.Date("1982-06-01"), y = 70, label = "Limite Superior\nDatos Sospechosos"),
            color = "purple", size = 5, fontface = "bold") +
  geom_hline(yintercept = seq(0, 200, by = 25),
             color = "gray70", linetype = "dotted", size = 0.5) +
  geom_vline(xintercept = seq(as.Date("1980-01-01"), as.Date("1983-01-01"), by = "1 year"),
             color = "gray70", linetype = "dotted", size = 0.5)




### 5) TEST DE RANGO VARIABLE ########

#OUTLIERS

### Identificar outliers y extremos con las funciones identify_outliers() del paquete {rstatix}
# La función is_outlier() verifica outliers
# La función is_extreme() verifica outliers extremos
# La función identify_outliers() identifica ambos




outliers <- lluvias
#outliers <- datos_sinNA  ????

outliers %>%
  identify_outliers(precipitacion_mm)

outliers_export <- outliers %>% identify_outliers(precipitacion_mm)


outliers %>%
  group_by(año) %>%
  identify_outliers(precipitacion_mm)

outliers_year_export <- outliers %>% group_by(año) %>% identify_outliers(precipitacion_mm)

boxplot(lluvias$precipitacion_mm~lluvias$año, lluvias)



#CORRECCIONES EDUARDO 
### A PARTIR DE AQUI planteamos los nuevos boxplots que incluyan los meses faltantes 

boxplot(DATOS_MDK$precipitacion_mm~DATOS_MDK$mes, DATOS_MDK)

boxplot(DATOS.9am$precipitacion_mm~DATOS.9am$mes, DATOS.9am)

#Mes es una variable numerica por lo que al convertirla en factores rellenamos con los meses que faltan 


DATOS_MDK$mes <- factor(DATOS_MDK$mes, levels = 1:12)

boxplot(precipitacion_mm ~ mes, data = DATOS_MDK)

# variantes esteticas
boxplot(precipitacion_mm ~ mes, data = DATOS_MDK, col = rainbow(12))

boxplot(precipitacion_mm ~ mes, data = DATOS_MDK, col = rainbow(12),
        xlab = "Months", ylab = "Precipitation (mm)",
        main = "Box plot precipitaciones mensuales")
boxplot(precipitacion_mm ~ mes, data = DATOS_MDK, col = rainbow(12),
        xlab = "Months", ylab = "Precipitation (mm)",
        main = "Box plot precipitaciones mensuales", grid = TRUE)
boxplot(precipitacion_mm ~ mes, data = DATOS_MDK, col = rainbow(12),
        xlab = "Months", ylab = "Precipitation (mm)",
        main = "Box plot precipitaciones mensuales", pch = 19)


library(RColorBrewer)

#Set 3 funciona bien al parecer 
boxplot(precipitacion_mm ~ mes, data = DATOS_MDK, col = brewer.pal(12, "Set1"),
        xlab = "Months", ylab = "Precipitation (mm)",
        main = "Box plot precipitaciones mensuales")

boxplot(precipitacion_mm ~ mes, data = DATOS_MDK, col = brewer.pal(12, "Set2"),
        xlab = "Months", ylab = "Precipitation (mm)",
        main = "Box plot precipitaciones mensuales")

boxplot(precipitacion_mm ~ mes, data = DATOS_MDK, col = brewer.pal(12, "Set3"),
        xlab = "Months", ylab = "Precipitation (mm)",
        main = "Box plot precipitaciones mensuales")


boxplot(precipitacion_mm ~ mes, data = DATOS_MDK, col = brewer.pal(12, "Set3"),
        xlab = "Months", ylab = "Precipitation (mm)",
        main = "Box plot precipitaciones mensuales", notch = TRUE)

boxplot(precipitacion_mm ~ mes, data = DATOS_MDK, col = brewer.pal(12, "Set3"),
        xlab = "Months", ylab = "Precipitation (mm)",
        main = "Box plot precipitaciones mensuales", border = "gray")

boxplot(precipitacion_mm ~ mes, data = DATOS_MDK, col = brewer.pal(12, "Set3"),
        xlab = "Months", ylab = "Precipitation (mm)",
        main = "Box plot precipitaciones mensuales", range = 1.5)

### Cambiar etiquetas eje x por tres primeras letras del mes correspondiente


DATOS_MDK$mes <- factor(DATOS_MDK$mes, levels = 1:12)


short_month_labels <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")


levels(DATOS_MDK$mes) <- short_month_labels


boxplot(precipitacion_mm ~ mes, data = DATOS_MDK, col = brewer.pal(12, "Set3"),
        xlab = "Mes", ylab = "Precipitación (mm)")


axis(side = 1, at = 1:12, labels = short_month_labels)


###### grafico multiple con las precipitaciones acumuladas por mes 

ggplot(subconj, aes(x = as.factor(mes), y = precipitacion_mm)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(x = "Meses", y = "Precipitación (mm/día)") +
  facet_wrap(~ año)


# FUNCIONAAAA
library(tidyr)
library(ggplot2)

complete_data <- complete(subconj, mes = 1:12, año)

ggplot(complete_data, aes(x = factor(mes, levels = 1:12), y = precipitacion_mm)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(x = "Meses", y = "Precipitación (mm/día)") +
  scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"), breaks = 1:12) +
  facet_wrap(~ año)



library(tidyr)
library(ggplot2)


complete_data <- complete(subconj, mes = 1:12, año)


blue_palette <- scales::viridis_pal(option = "A", direction = -1)(100)

ggplot(complete_data, aes(x = factor(mes, levels = 1:12), y = precipitacion_mm, fill = precipitacion_mm)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_gradientn(colors = blue_palette, na.value = "white") +
  labs(x = "Meses", y = "Precipitación (mm/día)") +
  scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"), breaks = 1:12) +
  facet_wrap(~ año)


#_______________________________________________________________________________________________

#### 6) TEST DE CONTINUIDAD TEMPORAL #####


# Load necessary library
library(dplyr)
library(ggplot2)


# Load the dataset
datos <- DATOS.9am

# Filter out rows corresponding to hours 15 and 21
datos_filtered <- datos %>%
  filter(!(datos[, 5] %in% c(15, 21)))  # Using column index to access the hour column


DATOS.9am <- datos_filtered


# Assuming DATOS.9am is already in your environment
data <- DATOS.9am

# Select relevant columns and filter for non-zero precipitation
data <- data %>%
  select(fecha, precipitacion_mm) %>%
  mutate(fecha = as.Date(fecha)) %>%  # Ensure fecha is in Date format
  filter(precipitacion_mm > 0)  # Exclude zero precipitation values

# Initialize a data frame to store results
consecutive_days <- data.frame(Start_Date = as.Date(character()),
                               End_Date = as.Date(character()),
                               Precipitation_mm = numeric(),
                               Consecutive_Days = integer(),
                               stringsAsFactors = FALSE)

# Initialize variables to find consecutive precipitation values
current_start <- data$fecha[1]
current_precip <- data$precipitacion_mm[1]
count <- 1

for (i in 2:nrow(data)) {
  # Check if the current value is the same and if it's a consecutive day
  if (data$precipitacion_mm[i] == current_precip && 
      data$fecha[i] == data$fecha[i - 1] + 1) {
    count <- count + 1
  } else {
    # If we have a sequence longer than 1, record it
    if (count > 1) {
      consecutive_days <- rbind(consecutive_days, 
                                data.frame(Start_Date = current_start,
                                           End_Date = data$fecha[i - 1],
                                           Precipitation_mm = current_precip,
                                           Consecutive_Days = count,
                                           stringsAsFactors = FALSE))
    }
    # Reset for the new sequence
    current_start <- data$fecha[i]
    current_precip <- data$precipitacion_mm[i]
    count <- 1
  }
}

# Check if the last sequence needs to be recorded
if (count > 1) {
  consecutive_days <- rbind(consecutive_days, 
                            data.frame(Start_Date = current_start,
                                       End_Date = data$fecha[nrow(data)],
                                       Precipitation_mm = current_precip,
                                       Consecutive_Days = count,
                                       stringsAsFactors = FALSE))
}

# Print the results
print(consecutive_days)


# Plot the results with fewer dates on the x-axis
ggplot(consecutive_days, aes(x = Start_Date, y = Consecutive_Days, fill = as.factor(Precipitation_mm))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Consecutive Days with Same Precipitation Values",
       x = "Start Date",
       y = "Number of Consecutive Days",
       fill = "Precipitation (mm)") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "8 weeks") +  # Changed to 2 weeks
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#### 7) CORRECCION DE DATAFRAME #######


DATOS_MDK <- DATOS.9am
DATOS_MDK <- lluvias


CORRECCION <- 4.0


row_index <- which(DATOS_MDK$fecha == "1979-01-25")
row_index

DATOS_MDK$precipitacion_mm[row_index] <- CORRECCION
DATOS_MDK

DATOS.9am <- DATOS_MDK







#_______________________________________________________________________________________________

####  8) ESTADISTICAS DESCRIPTIVAS - colocar depues de hacer la corrección de 40 mm #####

# USUARIO 
# 
# QUE HACE EL SCRIPT 
# 
# RESULTADOS


# USANDO BASE DE DATOS DATOS.9am con NA Y CON 0 
# Crea "matriz" resumen_mes (Minimo, 1st Quartil, Mediana, Media, 3rd Quartil, Maximo y N°NA's)
# de cada mes para todo el conjunto de datos
# Crea "matriz" resumen_año : (Minimo, 1st Quartil, Mediana, Media, 3rd Quartil, Maximo y N°NA's)
# de cada año para todo el conjunto de datos
# Crea "data frame" Matrix_agregada que es el resultado de unir "resumen_mes" y "resumen_año"



# A PARTIR DE AQUI YA ESTAN REMOVIDOS LOS NA'S PERO NO LOS 0 
# Crea "matriz" suma_mes_año : contiene las precipitaciones acumuladas para cada mes de cada año del 
# conjunto de datos 
# Crea "ppcion_anual": =  Precipitacion anual acumulada que equivale a 
# la sumatoria de precipitaciones acumuladas mensuales para cada año. 
# Crea "ppcion_media_mensual" : Media de todos los "Eneros", "Feb" ... del conjunto d edatos.
# Promedio para cada mes de las precipitaciones acumuladas mensuales regitradas en todos los años. 

# Crea un Grafico multiple que contiene precipitacion mensual acumulada para cada año 
# Reorganizado para que agrupe los meses de mayor precipitacion en el centro 



subconj <- DATOS.9am      #datos de precipitacion diarios, con  NA y 0 
subconj <- DATOS_MDK     #datos corregidos 


var <- "precipitacion_mm"


resumen <- summary(subconj[, var])
resumen

aux1 <- tapply(subconj[, var], subconj$mes, summary)
resumen_mes <- do.call(rbind, aux1)
resumen_mes
rm(aux1)

aux2 <- tapply(subconj[, var], subconj$año, summary)
resumen_año <- do.call(rbind, aux2)
resumen_año
rm(aux2)

aggregate(subconj[, var], list(subconj$mes, subconj$año), summary)

Matrix_agregada <- aggregate(subconj[, var], list(subconj$mes, subconj$año), summary)


na.rm <- TRUE 


suma_mes_año <- tapply(subconj[, var],
                       list(subconj$mes, subconj$año),
                       sum, na.rm=na.rm)

suma_mes_año

ppcion_anual <- apply(suma_mes_año, 2, sum, na.rm=na.rm)
ppcion_anual


ppcion_media_mensual <- apply(suma_mes_año, 1, mean, na.rm=na.rm) |> round(digits=1)
ppcion_media_mensual

rm(na.rm)

complete_data <- complete(DATOS.9am, mes = 1:12, año)

month_levels <- c(7:12, 1:6)

ggplot(complete_data, aes(x = factor(mes, levels = month_levels), y = precipitacion_mm)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(x = "Meses", y = "Precipitación (acumulada)") +
  scale_x_discrete(labels = c("Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "Ene", "Feb", "Mar", "Abr", "May", "Jun"), breaks = month_levels) +
  facet_wrap(~ año)



### Alternativa box plot sin NA ni 0 

boxplot(subconj[, var]~mes, data= datos_sinNA,
        main="Box-plot por mes", 
        ylab="Precipitación (mm/día)")




#### ) HISTOGRAMA 
# SIN 0 Y SIN NA 


class(subconj$fecha)


subconj$fecha<- as.Date(datos$fecha, format = "%d-%m-%Y")


subconj <- subconj[apply(subconj != 0, 1, all), ]


valid_precip <- subconj$precipitacion[!is.na(subconj$precipitacion)]
data_range <- max(valid_precip) - min(valid_precip)
print(data_range)
num_breaks <- ceiling(data_range / 0.5)
print(num_breaks)


# Crear el histograma de precipitación
hist(subconj$precipitacion,
     main = "Histograma de Precipitación", 
     xlab = "Precipitación",
     ylab = "Frecuencia",
     breaks = num_breaks)

#Crear histograma sin BREAKS
hist(subconj$precipitacion,
     main = "Histograma de Precipitación", 
     xlab = "Precipitación",
     ylab = "Frecuencia",
)





  
