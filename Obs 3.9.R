### a)  IMPORTACION ARCHIVOS "Resumen Diario" #####


install.packages("readxl")

library(readxl)

?readxl
?read_excel

ej.1 <- read_excel(file.choose()) 
ej.2 <- read_excel(ruta, range="Sheet1!B2:D4") 


Ej.1 <- as.data.frame(ej.1)



rm(list=ls(all=TRUE))

AÑOS <- c(1978:1983); AÑOS
ruta <- c("G:/My Drive/PPS/OBSTACLE 3/Data complete/")


R.DIA <- list()

for(año in AÑOS){
  ruta1 <- paste0(ruta, año, "/") # Ruta de acceso para un año específico
  lista <- list.files(ruta1, pattern = ".xlsx", full.names = F)[1:12] # "lista" contiene los nombres de los archivos 1 a 12
  MESES <- list()
  
  for(i in 1:12){
    y <- as.numeric(substr(lista[i], 1, 4)) # Extrae "año" desde el nombre del archivo
    m <- substr(lista[i], 6, 7)		  # Extrae "mes" desde el nombre del archivo
    d <- as.numeric()
    # --- Cálculo del nro de dias del mes ("d") ---
    m.31 <- c("01","03","05","07","08","10","12")	# Meses con 31
    m.30 <- c("04","06","09","11")		# Meses con 30
    if(m %in% m.31) {d=31} else {
      if(m %in% m.30) {d=30} else {
        if(y%%400==0 || (y%%4==0 && y%%100!=0)) {d=29} else {d=28} # Fórmula para año bisiesto
      }
    } # Fin cálculo nro días del mes
    import <- readxl::read_excel(path=paste0(ruta1,lista[i]), range=paste0("Resumen Diario!E2:O",d+1), col_names=FALSE)
    import <- as.data.frame(import) # "import" es un "tibble", y se transforma a df
    #names(import) <- c(paste("TA", c("1.00m","0.50m","0.05m"), sep="_"), paste("TS", rep(c("0.05m","0.10m","0.20m","0.30m","0.40m","0.50m","1.00m"), each=3), rep(c("9hs","15hs","21hs"), times=7), sep="_"))
    AMD <- data.frame(Año=rep(y,times=d), Mes=rep(m,times=d), Dia=c(1:d))
    Fecha <- as.Date(paste(AMD$Año, AMD$Mes, AMD$Dia, sep="/"))
    MESES[[i]] <- cbind(Fecha, AMD, import)
    rm(y, m, d, m.31, m.30, import, AMD, Fecha)
  } # Fin for(i)
  
  R.DIA[[año]] <- do.call(rbind, MESES) # "MESES" es (y debe ser) una lista, y se transforma en data.frame con do.call()
  rm(i,MESES,lista)
  
} # Fin for(año)

rm(año, ruta1)
R.DIA  <- do.call(rbind, R.DIA) # "R.DIA" es una lista y se transforma en data.frame
# save(R.DIA, file=paste0(ruta,"R.DIA.RData"))
# rm(AÑOS, ruta)
str(R.DIA)
ls()



### b) IMPORTACION ARCHIVOS alternativa para hoja "datos diarios" #####

rm(list=ls(all=TRUE))

install.packages("openxlsx2")
install.packages("purrr")
install.packages("lubridate")
install.packages("cellranger")


rm(list=ls(all=TRUE))


library(openxlsx2)  #read_xlsx()
library(purrr)      # map() y list_rbind()
library(lubridate)  # leap_year() 'año bisiesto', days_in_month() 'nro días del mes', y make_datetime() 'crear fecha-hora' 
library(cellranger) # cell_cols()


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


### --- Filtrado de columnas vacías

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




### c) FILTRADO ####

R.DIA

# Filtrado de variables del data.frame "R.DIA", sólo deja las columnas de hora fecha y precipitacion 

R.DIA <- R.DIA[,c(1:4,15)]

R.DIA$precipitacion <- R.DIA$...11      

R.DIA <- R.DIA[,c(1:4,6)]

R.DIAprueba <- R.DIA[c(1:546,578:2191),]


### d) GUARDADO DATA FRAME°S DOS TIPOS DE IMPORTACION ####

DATOS_pp <- R.DIAprueba
write.csv(DATOS_pp, file = "OBS 3.1 pp.csv",row.names = FALSE)
DATOS_pp <- read.csv("G:/My Drive/PPS/OBSTACLE 3/OBS 3.1 pp.csv")               #CRUDO 
  
DATOS_nub <- datos2[,c(1:6,28,37,43)]                           

write.csv(DATOS_nub, file = "OBS 3.1.csv nub", row.names = FALSE)
DATOS_nub <- read.csv("G:/My Drive/PPS/OBSTACLE 3/OBS 3.1.csv")                 #CRUDO 

R.DIA <- DATOS_pp

write.csv(lluvias, file = "Eventos lluviosos 1978-1983",row.names = FALSE)
lluvias <-read.csv("G:/My Drive/PPS/OBSTACLE 3/Eventos lluviosos 1978-1983.csv")    #sin NA sin 0

write.csv(filtrado, file = "DATOS.9am.sinNA",row.names = FALSE)
DATOS.9am <- read.csv("G:/My Drive/PPS/OBSTACLE 3/DATOS.9am.sinNA.csv")       #sin 15 y 21 hs con 0 y  con NA

write.csv(datos_sinNA, file = "datos_sinNA", row.names = FALSE)
datos_sinNA <- read.csv("G:/My Drive/PPS/OBSTACLE 3/datos_sinNA.csv")         #sin 15 y 21hs con 0 y SIN NA

write.csv(merged_df, file = "Promedio_nubosidad", row.names = FALSE)
promedio_nubosidad <- read.csv("G:/My Drive/PPS/OBSTACLE 3/datos_sinNA.csv")         #sin 15 y 21hs con 0 y SIN NA

write.csv(total_na, file = "total_na", row.names = FALSE)
total_na <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/total_na") 

write.csv(total_n, file = "total_n", row.names = FALSE)
total_n <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/total_n") 

write.csv(porcentaje_NA, file = "porcentaje_NA", row.names = FALSE)
porcentaje_NA <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/porcentaje_Na") 

write.csv(outliers_export, file = "outliers_export", row.names = FALSE)
outliers_export <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/outliers_export") 

write.csv(outliers_year_export, file = "outliers_year_export", row.names = FALSE)
outliers_year_export <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/outliers_year_export") 

write.csv(outlier_ind_converted, file = "hamptel filter", row.names = FALSE)
outlier_ind_converted <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/hamptel filter")

write.csv(df, file = "nubosidad_precipitacion", row.names = FALSE)
df <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/nubosidad_precipitacion")

write.csv(resumen, file = "resumen", row.names = FALSE)                         #Al parecer no puedo exportar como CSV por ser "double"
resumen <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/resumen")

write.csv(resumen_mes, file = "resumen_mes", row.names = FALSE)
resumen_mes <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/resumen_mes")

write.csv(resumen_año, file = "resumen_año", row.names = FALSE)
resumen_año <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/resumen_año")

write.csv(Matrix_agregada, file = "Matrix agregada", row.names = FALSE)
Matrix_agregada <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/Matrix_agregada")

write.csv(suma_mes_año, file = "suma_mes_año", row.names = FALSE)
suma_mes_año <- read.csv("H:/My Drive/PPS FINAL WORK/Tablas export/suma_mes_año")




### e) DETECCION DE DATOS FALTANTES (NA) ####

# loop para que enliste los Nas con su correspondiente fecha 

#filtrar NA correspondientes a las 15 y 21 hs para precipitacion, solo valen los NA de las 9 AM


hora <- c(15,21)
filtro_hora1521 <- DATOS$hora %in% hora
filtrado1521 <- DATOS
filtrado<- filtrado1521[!filtro_hora1521,]
filtrado_OP<- filtrado1521[filtro_hora1521,]
DATOS <- filtrado


hora <- c(15)
filtro_hora15 <- DATOS$hora==hora
filtrado15 <- DATOS
filtrado15[filtro_hora15,]



hora <- c(21)
filtro_hora21 <- DATOS$hora==hora
filtrado21 <- DATOS 
filtrado21[filtro_hora21,]


#crear lista de NA
na_list <- list()
for (i in 1:nrow(filtrado)) {
  if (is.na(filtrado$precipitacion_mm[i])) {
    na_list[[as.character(filtrado$fecha.hora[i])]] <- "NA"
  }
}
print(na_list)

#Crear lista de 0s

zero_list <- list()
for (i in 1:nrow(filtrado)) {
  if (filtrado$precipitacion_mm [i] == 0){
    zero_list[as.character(filtrado$fecha.hora[i])] <- '0'  
  }
}


## --- Exploración de datos faltantes 'NA' ---

#subconj <- subset(DATOS.9am, filtro_fechas & filtro_hora, select=c(2:6,43))
#head(subconj)
#str(subconj)



subconj <- DATOS.9am

var <- "precipitacion_mm"

sumaNA <- function(x) sum(is.na(x)) # Cantidad de NAs = 231
sumaN <- function(x) sum(!is.na(x)) # Cantidad de datos (N), omite los NAs =1960

#sumaNA + sumaN = Total de obs = 2191


total_na <- tapply(subconj[, var], list(subconj$año, subconj$mes), sumaNA)
total_na
total_n  <- tapply(subconj[, var], list(subconj$año, subconj$mes), sumaN)
total_n

dias_mes <- total_na + total_n
dias_mes


#Porcentaje de NA's

porcentaje_NA <- (total_na / dias_mes * 100) |> round(digits=1)

#Remover valores NA y 0 

datos <- datos[!is.na(datos$precipitacion_mm) & datos$precipitacion_mm != 0, ]  #Deja filtrados solo valores > 0

lluvias <- datos

datos_sinNA <- DATOS.9am[!is.na(DATOS.9am$precipitacion_mm),]                   #Filtra todos los NA y deja solo los 0


#Pequeña herramienta para determinar el tipo de elemento en una lista o data frame


for (element in na_list) {
  print(typeof(element))
  
}


for(element in lluvias){
  print(typeof(element))
}


#CLIMDEX
install.packages("climdex.pcic")
install.packages(install.packages("RClimDex-1.9-3.tar.gz", repos=NULL, type="source"))

### f) EXPLORACION PRELIMINAR DEL DATAFRAME ####
## --- Cálculo de resúmenes estadísticos ---

subconj <- DATOS.9am      #datos de precipitacion diarios, con  NA y 0 
subconj <- datos_sinNA    #datos de precipitacion diarios, sin  NA con 0
subconj <- lluvias        #datos de precipitacion diarios, sin NA y sin 0
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


## Resumen por mes y año (con y sin NAs)

na.rm <- FALSE

na.rm <- TRUE 

# Acumulado por mes y año
suma_mes_año <- tapply(subconj[, var],
                       list(subconj$mes, subconj$año),
                       sum, na.rm=na.rm)
suma_mes_año

suma_mes_año_sNA <- suma_mes_año


# Acumulado por año
apply(suma_mes_año, 2, sum, na.rm=na.rm)

# Promedio por mes
apply(suma_mes_año_sNA, 1, mean, na.rm=na.rm) |> round(digits=1)

rm(na.rm)


## --- Gráficos de exploración de datos ---


opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))

plot(subconj[, var]~fecha, data=subconj, type="h",
     main="Precipitación diaria",
     ylab="Precipitación (mm/día)")

boxplot(subconj[, var]~mes, data=subconj,
        main="Box-plot por mes", 
        ylab="Precipitación (mm/día)")

hist(subconj[, var], main="Histograma",
     xlab="Clases de precipitación (mm/día)",
     ylab="Frecuencia")

plot(density(subconj[, var], na.rm=T), main="Densidad")

par(opar)

# 


ggplot(subconj, aes(x = as.factor(mes), y = precipitacion_mm)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(x = "Meses", y = "Precipitación (mm/día)") +
  facet_wrap(~ año)


### Alternativa box plot sin NA ni 0 

boxplot(subconj[, var]~mes, data= datos_sinNA,
        main="Box-plot por mes", 
        ylab="Precipitación (mm/día)")




### g) TEST DE NORMALIDAD + FIT DISTR ####

class(subconj$fecha)

# Cargar los datos y convertir la columna de fechas al formato de fecha de R
subconj$fecha<- as.Date(datos$fecha, format = "%d-%m-%Y")


#define rango max y min + calcula numero de breaks
data_range <- max(subconj$precipitacion) - min(subconj$precipitacion)
num_breaks <- ceiling(data_range / 0.5)
num_breaks <- ceiling(data_range / 1)
num_breaks <- ceiling(data_range / 2)
num_breaks <- ceiling(data_range / 3)            # A partir de aqui el grafico es el mismo hasta "R5"
num_breaks <- ceiling(data_range / 4)
num_breaks <- ceiling(data_range / 5)
num_breaks <- ceiling(data_range / 10)


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



#Configuracion de Variables 

Precipitacion_test <- (subconj$precipitacion)
Precipitacion_test

#PRUEBA DE NORMALIDAD SHAPIRO-WILK 

shapiro.test(Precipitacion_test)

#Pueba KOLMOGOROV - SMIRNOV 

ks.test(Precipitacion_test, pnorm,mean=mean(Precipitacion_test),sd=sd(Precipitacion_test))


#PRUEBA ANDERSON-DARLING 

install.packages("nortest")
library(nortest)
resultado_anderson <- ad.test(subconj$precipitacion)
print(resultado_anderson)


ad.test(subconj$precipitacion, "norm")

?ad.test

#FIT DISTRIBUTION 

install.packages(fitdistrplus)
library(fitdistrplus)

#EXAMPLE DATASET = GAMMA DIST
dataset <- rgamma(100, shape = 2, rate = 1)

#LLUVIAS - Spoiler, descarto todo menos gamma
dataset <- lluvias$precipitacion_mm

fit_gamma <- fitdist(dataset, "gamma")
fit_uniform <- fitdist(dataset, "unif")
fit_normal <- fitdist(dataset, "norm")


summary(fit_gamma)
summary(fit_uniform)
summary(fit_normal)


test <- gofstat(list(fit_gamma, fit_uniform, fit_normal),      # Valor del estadístico, no de la probabilidad)!!!
                fitnames = c("Gamma", "Uniforme", "Normal"))
test
test[c(11,7,9)]


windows(); plot(fit_gamma);              # title("Distribución Gamma")
windows(); plot(fit_uniform); # title("Distribución Uniforme")
windows(); plot(fit_normal);  # title("Distribución Normal")

plot(fit_gamma)
plot(fit_uniform)
plot(fit_normal)





### 1) TEST DE RANGO FIJO + GRAFICO #####


library(ggplot2)

# Definir el rango mínimo y máximo de precipitación
rango_min <- 0
rango_max <- 60

# Crear una paleta de colores para resaltar diferentes elementos
colores <- c("#3366CC", "#FF9900", "#CC0033")

# Graficar las fechas con las precipitaciones
ggplot(R.DIA, aes(x = R.DIA[,c(1)], y = R.DIA[,c(5)])) +
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



### 2) TEST DE RANGO VARIABLE ########

#DESVIACIONES RESPECTO AL RANGO INTERCUARTIL PARA VENTANAS MENSUALES

#OUTLIERS

### Identificar outliers y extremos con las funciones identify_outliers() del paquete {rstatix}

# There are two categories of outlier: (1) outliers and (2) extreme points:
#   Values above Q3 + 1.5xIQR or below Q1 - 1.5xIQR are considered as outliers. 
#   Values above Q3 + 3xIQR or below Q1 - 3xIQR are considered as extreme points (or extreme outliers). 

# La función is_outlier() verifica outliers
# La función is_extreme() verifica outliers extremos
# La función identify_outliers() identifica ambos


install.packages("rstatix")
install.packages("dplyr")

library(rstatix)
library(dplyr)

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



#HAMPTEL FILTER#   -----> Trabaja con la MEDIANA (es mas adecuada cuando no tenemos distribucion normal)

# Hampel filter, consists of considering as outliers the values outside the interval ('I')
# formed by the median, plus or minus 3 median absolute deviations ('MAD'):

#   I = [median−3*MAD ; median+3*MAD]

# where 'MAD' is the median absolute deviation and is defined as the median of the absolute deviations
# from the data’s median:

#   MAD = median(|Xi−X|)

# For this method we first set the interval limits thanks to the median() and mad() functions:

subconj <- lluvias
#subconj <- datos_sinNA  #nada nada ?

lower_bound <- median(subconj$precipitacion_mm) - 3 * mad(subconj$precipitacion_mm, constant = 1)
lower_bound

upper_bound <- median(subconj$precipitacion_mm) + 3 * mad(subconj$precipitacion_mm, constant = 1)
upper_bound

outlier_ind <- which(subconj$precipitacion_mm < lower_bound | subconj$precipitacion_mm > upper_bound)
outlier_ind



#Indices utilizados en el conjunto para identificar OUTLIERS con la MEDIANA y MAD

#[1]   1   7  15  16  44  49  54  55  57  58  63  70  71  85 109 119 132 146 160 166 171 172 183 192 198 205


outlier_ind_converted <- subconj[outlier_ind,]                       #Funciona !!!!






### 2.1) VALORES EXTREMOS DE PPCION (CUANTILES DE DISTRIB GAMMA) ####
# prcp (i.j) > Q (p.j)
#i = dia  , j= año  , p= percentil extremo (0,975 / 0,99 / 0,995 )



gamma_params <- fitdistr(lluvias$precipitacion_mm, "gamma")


percentiles <- c(0.975, 0.99, 0.995)


extreme_values <- qgamma(percentiles, shape = gamma_params$estimate[1], scale = gamma_params$estimate[2])


for (i in seq_along(percentiles)) {
  cat("Extreme value at", percentiles[i] * 100, "percentile:", extreme_values[i], "\n")
}


# Assuming df is your data frame and extreme_threshold is the threshold you want to use
#Extreme value at 97.5 percentile: 0.9500109 
#Extreme value at 99 percentile: 1.151182 
#Extreme value at 99.5 percentile: 1.30156



above_threshold_data <- lluvias[lluvias$precipitacion_mm > 0.9500109, ]



hist(lluvias$precipitacion_mm, main="Precipitation Histogram", xlab="Precipitation", ylab="Frequency")
abline(v=0.9500109, col="red", lwd=2)

boxplot(lluvias$precipitacion_mm, main="Precipitation Box Plot", ylab="Precipitation")
abline(h=0.9500109, col="red", lwd=2)


plot(lluvias$fecha, lluvias$precipitacion_mm, type="l", main="Precipitation Time Series", xlab="Date", ylab="Precipitation")
points(lluvias$fecha[lluvias$precipitacion_mm > 0.9500109], lluvias$precipitacion_mm[lluvias$precipitacion_mm > 0.9500109], col="red", pch=20)




### 3) TEST CONTINUIDAD TEMPORAL  ####
### 3.1) PERSISTENCIA DE VALORES CTES POR MAS DE 3 DIAS CONSECUTIVOS ####


data_TCT <- R.DIA
data_TCT

#chequear nombres de columnas y cambiar por "precipitacion"

colnames(data_TCT)
colnames(data_TCT)[colnames(data) == "...11"] <- "precipitacion"
colnames(data_TCT)
data_TCT



#determinar valores faltantes NA 

missing_dates <- data_TCT$Fecha[is.na(data_TCT$precipitacion)]
print(missing_dates)

### REVIEW COMO SAC NA
sin_NA <- data_TCT(!is.na(data_TCT[,5]))
sin_0 <- 


data_TCT$suspect <- FALSE


for (i in 3:nrow(data_TCT)) {
  if (!is.na(data_TCT$precipitacion[i]) && !is.na(data_TCT$precipitacion[i-1]) && !is.na(data_TCT$precipitacion[i-2]) &&
      data_TCT$precipitacion[i] == data_TCT$precipitacion[i-1] && data_TCT$precipitacion[i-1] == data_TCT$precipitacion[i-2]) {
    data_TCT$suspect[i] <- TRUE
  }
}


consecutive_days <- rle(data$precipitacion)$lengths


occurrences <- table(consecutive_days)


barplot(occurrences, xlab = "Number of Consecutive Days", ylab = "Frequency", main = "Persistence of Precipitation Values")


#Alternativa : 


colnames(R.DIA)
colnames(R.DIA)[colnames(R.DIA) == "...11"] <- "precipitacion"
colnames(R.DIA)


# Paso 1: Filtrar los días con precipitación diferente de 0 y sin valores NA
df_filtered <- subset(R.DIA, precipitacion != 0 & !is.na(precipitacion))

# Paso 2: Crear una columna adicional para indicar si la precipitación es igual a la del día anterior
df_filtered$repeticion <- c(0, diff(df_filtered$precipitacion) == 0)

# Paso 3: Calcular la longitud de la secuencia de días de repetición
secuencia <- rle(df_filtered$repeticion)
longitudes <- secuencia$lengths[secuencia$values]

# Paso 4: Crear un gráfico de barras
barplot(table(longitudes), xlab = "Longitud de secuencia", ylab = "Frecuencia de ocurrencias")




### 3.2) PERSISTENCIA EXTREMA(DEFINIDA CLIMATOLOGICAMENTE) DIAS SIN PPCION  ###############

install.packages("rstatix")
install.packages("dplyr")

library(rstatix)
library(dplyr)


# umbral =  PARA CADA MES

# x axys = meses
# y axys = largo de secuencia (dias)



library(dplyr)

df <- datos_sinNA

df$fecha <- as.Date(df$fecha)

df <- df %>%
  mutate(month = format(fecha, "%m")) %>%
  group_by(month) %>%
  arrange(fecha) %>%
  mutate(sequence_length = cumsum(precipitacion_mm == 0)) %>%
  ungroup()


boxplot(df$sequence_length ~ df$month, xlab = "Mes", ylab = "Sequence Length without Precipitation", main = "Monthly Sequence Length Boxplot")



boxplot(lluvias$precipitacion_mm~lluvias$año, lluvias)
boxplot(lluvias$precipitacion_mm~lluvias$mes)
boxplot(lluvias$precipitacion_mm~lluvias$dia)
boxplot(largo_secuencia_dry~lluvias$mes)
boxplot(largo_secuencia_wet~lluvias$mes)

### 4) TEST DE CONSISTENCIA ENTRE VAR #####
# (PPCION = 0, NUB ≠ 0)
# dias en los que la ppcion fue mayor a 0,1 y la nubosidad = 0


# PRIMERA VERSION 
 
 library(dplyr)
 

 daily_avg_cloud_cover <- lluvias  %>%
   group_by(fecha) %>%
   summarize(avg_cloud_cover = mean(nubosidad_total))
 
 #solo para chequear que sean mayor a 0,1, lo minimo agendado por inta es de de 0,5 mm
 days_with_precipitation <-lluvias
 
 days_with_precipitation <- lluvias %>%
   filter(precipitacion_mm > 0.1)
 
 #identif sospechosos
 suspect_data <- days_with_precipitation %>%
   left_join(daily_avg_cloud_cover, by = "fecha") %>%
   filter(avg_cloud_cover == 0)
 

 print(suspect_data)
 
 

 # ALTERNATIVA 
 
 if (!require("dplyr")) install.packages("dplyr")
 if (!require("lubridate")) install.packages("lubridate")
 library(dplyr)
 library(lubridate)
 
 
 df_daily_precip <- DATOS_nub %>%
   mutate(fecha = as.Date(fecha)) %>%
   group_by(fecha) %>%
   summarize(daily_precip = sum(precipitacion_mm, na.rm = TRUE))

 df_daily_cloud <- df %>%
   mutate(fecha = as.Date(fecha)) %>%
   group_by(fecha) %>%
   summarize(daily_cloud = mean(nubosidad_total))
 
 
 suspect_days <- df_daily_precip %>%
   filter(daily_precip > 0.1) %>%
   semi_join(df_daily_cloud, by = "fecha") %>%
   filter(daily_cloud == 0)
 
 suspect_days
 
 
 
#ALTERNATIVA 2
 
 library(dplyr)
 
 df <- merged_df
 
 df$fecha.hora <- as.POSIXct(df$fecha.hora)
 
 df <- df %>%
   arrange(fecha.hora) %>%
   group_by(año, mes) %>%
   mutate(prev_nubosidad_total = lag(nubosidad_total, order_by = fecha.hora))  # ,n=3  si hace falta volver mas tiempo 
 
 df <- df %>%
   mutate(average_prev_day = (avg_nubosidad_total + prev_nubosidad_total) / 2)
 
 df$average_prev_day <- ifelse(is.na(df$average_prev_day), 0, df$average_prev_day)   #IFELSE POR SI QUIERO AGREGAR ALGO EN LOS NA
 
 
 
 #####ALTERNATIVA 3 = DEFINITIVA FUNCIONANDO
 
 
 library(dplyr)
 
 df <- merged_df
 
 df$fecha.hora <- as.POSIXct(df$fecha.hora)
 
 df <- df %>%
   arrange(fecha.hora) %>%
   group_by(año, mes) %>%
   mutate(prev_nubosidad_total = lag(avg_nubosidad_total, order_by = fecha.hora, n=3))  # ,n=3  si hace falta volver mas tiempo 
 
 #
 df <- df %>%
   mutate(average_prev_day = (nubosidad_total + prev_nubosidad_total) / 2)
 #
 df$average_prev_day <- ifelse(is.na(df$average_prev_day), 0, df$average_prev_day)   #IFELSE POR SI QUIERO AGREGAR ALGO EN LOS NA
 
 
 df
 
 df <- df %>%
   mutate(SUSPECT = ifelse(precipitacion_mm > 0.1 & (nubosidad_total == 0 | avg_nubosidad_total == 0), "SUSPECT", ""))
 
 # Create a new column "SUSPECT" based on the specified conditions
 df <- df %>%
   mutate(SUSPECT = ifelse(precipitacion_mm > 0.1 & (nubosidad_total == 0 | avg_nubosidad_total == 0), "SUSPECT", "PASS"))
 
 
 
 
 

 
### 5) ESTADISTICAS DESCRIPTIVAS ####

#Max mensual , Minimo mensual, Media mensual, Mediana, Moda , Desvio estandar , Varianza , coeficiente de variacio, 
# percentil 25, 50 , 75 
# Decil 5 , 95
# Asimetria, curtosis, desviacion absoluta de la mediana 


RESUMEN <- function(DATOS, cifras=2){ 
  datos <- DATOS[which(!is.na(DATOS))] # Remueve NAs para los cálculos
  
  Media <- mean(datos)
  DS  <- sd(datos)
  CV <- abs(DS/Media)*100
  Min <- min(datos)
  Perc.5  <- quantile(datos, probs=c(0.05), names=F)
  Perc.25 <- quantile(datos, probs=c(0.25), names=F)
  Perc.50 <- quantile(datos, probs=c(0.50), names=F)
  Perc.75 <- quantile(datos, probs=c(0.75), names=F)
  Perc.95 <- quantile(datos, probs=c(0.95), names=F)
  Max <- max(datos)
  N <- length(!is.na(datos))
  NAs <- sum(as.numeric(is.na(DATOS)))
  
# Impresión resultados:
  Resumen <- c(Media=Media,
               DS=DS,
               CV=CV,
               Min=Min,
               Perc.5=Perc.5,
               Perc.25=Perc.25, # Cuartil inferior
               Perc.50=Perc.50, # Mediana
               Perc.75=Perc.75, # Cuartil superior
               Perc.95=Perc.95,
               Max=Max,
               N=N,
               Faltantes=NAs)
  return(round(Resumen,cifras))
} 

# Fin función "RESUMEN"
#impresion funcion "RESUMEN"

summary_results <- RESUMEN(DATOS, Cifras = 2)
print(summary_results)





### 6) AMPLIACION ESTADISTICAS DESCRIPTIVAS POR MES Y POR AÑO ####

#Elegir la variable a la que se le calcularán las estadísticas (trabajar sobre el data.frame GEO_3):

nombres <- names(GEO_3) # Para saber el nombre de una variable, según nro de columna
nombres[8]  # "TS_0.05m_9hs"

# --- Estadísticas por mes ---

# Cálculo para 1 mes en particular (ej. febrero), de la var. ubicada en col.8
RESUMEN(subset(GEO_3[,8], GEO_3$Mes=="02"))

# Similar al anterior, pero para todos los meses en simultáneo
aux <- tapply(GEO_3[,8], GEO_3$Mes, RESUMEN)
aux2 <- do.call(rbind, aux); rm(aux)
result.1 <- t(aux2); rm(aux2)
result.1

# --- Estadísticas por estación del año ---

## Creación de un factor auxiliar "Estación"

# Se consideraron las estaciones del siguiente modo:
# Verano:    Dic, Ene, Feb
# Otoño:     Mar, Abr, May
# Invierno:  Jun, Jul, Ago
# Primavera: Sep, Oct, Nov

GEO_3$Estación <- NA
GEO_3$Estacion <- as.character(GEO_3$Estacion)

for(i in 1:dim(GEO_3)[1]){
  if(GEO_3$Mes[i] %in% c("12","01","02")) {GEO_3$Estacion[i] <- "V"} else {
    if(GEO_3$Mes[i] %in% c("03","04","05")) {GEO_3$Estacion[i] <- "O"} else {
      if(GEO_3$Mes[i] %in% c("06","07","08")) {GEO_3$Estacion[i] <- "I"} else {GEO_3$Estacion[i] <- "P"}
    }
  } # Fin if()
} # Fin for()

GEO_3$Estacion <- as.factor(GEO_3$Estacion)
levels(GEO_3$Estacion)  # Levels: I O P V  ---> los niveles están desordenados
GEO_3$Estacion <- factor(GEO_3$Estacion, levels=levels(GEO_3$Estacion)[c(3,4,2,1)]) # Ordena los niveles
levels(GEO_3$Estacion) # Levels: P V O I ---> Niveles ordenados
#GEO_3$Estacion

# Cálculo para 1 estacion en particular (ej. verano "V"), de la var. ubicada en col.8
RESUMEN(subset(GEO_3[,8], GEO_3$Estacion=="V"))

# Similar al anterior, pero para todas las estaciones en simultáneo, de la var. ubicada en col.8
aux <- tapply(GEO_3[,8], GEO_3$Estacion, RESUMEN)
aux2 <- do.call(rbind, aux); rm(aux)
result.2 <- t(aux2); rm(aux2)
result.2

# --- Estadísticas por año ---

result.3 <- as.matrix(RESUMEN(GEO_3[,8]))
result.3



# --- Estadísticas por decáda (10 días) ---

# Decádas: períodos de aproximadamente 10 días. Cada mes del año tiene 3 decádicos. El 
# 1er y 2do decádico siempre consta de 10 días. El 3er decádico puede tener 8, 9 (febrero),
# 10 u 11 días dependiendo del mes

# Cálculo del factor auxiliar "Decadico" 

GEO_3$Decadico <- NA
GEO_3$Decadico <- as.character(GEO_3$Decadico)

for(i in 1:dim(GEO_3)[1]){
  if(GEO_3$Dia[i]<=10) {GEO_3$Decadico[i] <- "I"} else {
    if(GEO_3$Dia[i]>10 & GEO_3$Dia[i]<=20) {GEO_3$Decadico[i] <- "II"} else {GEO_3$Decadico[i] <- "III"}
  } # Fin if()
} # Fin for()

GEO_3$Decadico <- as.factor(GEO_3$Decadico)

aux <- tapply(GEO_3[,8], GEO_3[,c(30,3)], RESUMEN) ## OJO: el orden de los factores 3 y 30 afecta la presentación de los resultados (orden de las columnas)
aux2 <- do.call(rbind, aux); rm(aux)
result.4 <- t(aux2); rm(aux2)
result.4

aux <- expand.grid(c("01","02","03","04","05","06","07","08","09","10","11","12"), c("I","II","III"))
mes.dec <- do.call(paste, c(aux, sep="_")); rm(aux)
colnames(result.4) <- sort(mes.dec)
result.4

aux <- t(result.4)
plot(aux[,1], type="l", col=1, lty=1)


### 8) Analisis de dispersion de series temporales representacion grafica Box plots#### 

boxplot(datos[,5]~datos[,3])




### 10) SPI ####

#### FUNCIONA!! ojo con sobreescribir datos_sinNA

library(dplyr)
library(lubridate)


datos_sinNA$fecha <- as.Date(datos_sinNA$fecha)

DATOS.9am$fecha <- as.Date(DATOS.9am$fecha)                                     #ALT

datos_sinNA <- datos_sinNA %>%
  mutate(year = year(fecha), month = month(fecha))

DATOS.9am <- DATOS.9am %>%                                                  #ALT
  mutate(year = year(fecha), month = month(fecha))


monthly_precipitation <- DATOS.9am_NA %>%
  group_by(year, month) %>%
  summarise(total_precipitation = sum(precipitacion_mm, na.rm = TRUE))

monthly_precipitation_NA <- DATOS.9am %>%
  group_by(year, month) %>%
  summarise(total_precipitation = sum(precipitacion_mm, na.rm = FALSE))      #ALT


print(monthly_precipitation)
print(monthly_precipitation_NA)

monthly_precipitation <- monthly_precipitation[,3]


print(precipitacion_media_mensual)

spi1 <- spi(monthly_precipitation$total_precipitation,1,)
spi1
spi3 <- spi (monthly_precipitation$total_precipitation,3,)
spi3
spi12 <- spi (monthly_precipitation$total_precipitation,12,)
spi12



spi
spi
spei()




### VARIANTES SPI ####

library(dplyr)


all_months <- expand.grid(año = unique(datos$año), mes = 1:12)


datos$fecha <- as.Date(datos$fecha)


precipitacion_media_mensual <- datos %>%
  group_by(año, mes) %>%
  summarize(precipitacion_media = mean(precipitacion_mm, na.rm = TRUE)) %>%
  
  right_join(all_months, by = c("año", "mes")) %>%
  
  mutate(precipitacion_media = ifelse(is.na(precipitacion_media), 0, precipitacion_media))


datos$fecha <- as.Date(datos$fecha)


library(SPEI)
library(dplyr)


precipitacion_media_mensual <- datos_sinNA %>%                                  #no corresponde media mensual sino acumulada 
  group_by(año, mes) %>%
  summarize(precipitacion_media = mean(precipitacion_mm, na.rm = TRUE))

print(precipitacion_media_mensual)



precipitacion_mensual_acumulada <- datos_sinNA %>%                                  #acumulada no anda
  group_by(año, mes) %>%
  summarize(precipitacion_acumulada = sum(precipitacion_mm, na.rm = TRUE))

print(precipitacion_media_mensual)



### 11) SPEI ####

spei1 <- spei(precipitacion_media_mensual$precipitacion_media,1)
spei1
spei3 <- spei(precipitacion_media_mensual$precipitacion_media,3)
spei3












#### Dataframe CORREGIDO ####

DATOS_MDK <- DATOS.9am
DATOS_MDK <- lluvias


CORRECCION <- 4.0


row_index <- which(DATOS_MDK$fecha == "1979-01-25")
row_index

DATOS_MDK$precipitacion_mm[row_index] <- CORRECCION
