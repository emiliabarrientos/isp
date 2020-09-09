### Grilla de suficiencia  ####
# Valentina Andrade

#0. Cargar base de datos y librerias
library(tidyverse)
load("informe_resultados/data/ISP_proc.RData")

## Criterios para definir una observacion como completa 
#1. LLegar al modulo de modalidad de trabajo 
sum(is.na(ISP_proc$d5_mod)) #683

ISP_suf <- filter(ISP_proc, !is.na(d5_mod))

#2. Fecha inicio de la encuesta 
ISP_suf <- ISP_suf %>% separate(date_created, c("date","hour", NA), sep = "([ ])")
ISP_suf$date <- as.Date(ISP_suf$date, "%m/%d/%y")
# Encuesta iniciadas el 26 de mayo - finalizadas hasta el 17 de Junio. 

#3. Guardar
save(ISP_suf, file = "informe_resultados/data/ISP_suf.RData")
ISP_privisoria <- ISP_suf %>% select(-c(respondent_id:ip_address,mail))
save(ISP_privisoria, file = "informe_resultados/data/ISP_privisoria.RData")
