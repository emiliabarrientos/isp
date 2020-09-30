##### Validadores #####
#Valentina Andrade

#1. Cargar librerias
pacman::p_load(dplyr,
               tidyverse,
               chron,
               tidyr)
#2. Cargar base de datos
load(file = "informe_resultados/data/ISP_suf.RData")
ISP <- read_sav(file= "data/ISP.sav")

# Criterios de validacion ------------------------------
# A.  Fecha inicio de la encuesta 
# Encuesta iniciadas el 26 de mayo 
ISP_suf$date <- as.Date(ISP_suf$date)
str(ISP_suf$date)
# B. Tiempo de respuesta
#Al menos 5 minutos 
ISP_t <- ISP_suf %>%  
  separate(date_modified, c("date2","hour2", NA), sep = "([ ])") 
str(ISP_t$date2)
ISP_t$date2 <- as.Date(ISP_t$date2, "%m/%d/%y")
ISP_t$hour <- hms::as.hms(ISP_t$hour)
ISP_t$hour2 <- hms::as.hms(ISP_t$hour2)
ISP_t %>% mutate(diff = hour2 - hour) %>% select(diff) %>% mutate(diff = abs(diff*0.0166667)) %>% filter(diff > 1)
#8.899

ISP_t <- ISP_suf %>%   separate(date_modified, c("date2","hour2", NA), sep = "([ ])") 
ISP_t$date <- as.Date(ISP_t$date, "%m/%d/%y")
ISP_t$date <- as.Date(ISP_t$date2, "%m/%d/%y")
ISP_t$hour <- hms::as.hms(ISP_t$hour)
ISP_t$hour2 <- hms::as.hms(ISP_t$hour2)
ISP_t %>% mutate(diff = hour2 - hour) %>% select(diff) %>% mutate(diff = abs(diff*0.0166667)) %>% filter(diff > 1)

#Todos se demoran mas de 1 minuto hasta el primer modulo

#C. Grupos fuera de la muestra
# B1 (sector) se eliminarán los No aplica (sector privado)
names(ISP_t)
#ISP_val <- ISP_t %>% filter(b1 !=85)

#D. Grupos minimos
#B6 (contrato) por ser grupo mínimo
table(ISP_val$b6)
#Son 30 subcontratados. En teoria seria lo mejor eliminarlos. 

#E.  B1 (sector)
table(ISP_val$b1)
# ISP_val <- ISP_val %>% filter(b1 !="FFAA y Orden")
# ISP_FFAA <- ISP_suf %>% filter(b1 =="FFAA y Orden")
table(ISP_FFAA$b3)

ISP_val <- ISP_suf
# Guardar 
save(ISP_val, file = "informe_resultados/data/ISP_val.RData")
