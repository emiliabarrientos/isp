####### Unir cuestionarios #############
library(dplyr)
library(haven)
library(tidyverse)
library(readxl)
library(tidyverse)
library(compare)

# Combinar cuestionarios español y portugués

# 1.Cargar librerias ----
pacman::p_load(haven,dplyr, tidyverse,lubridate)

#2. Cargar base de datos -----
ISP_es <- read_sav(file= "informe_resultados/data/Condiciones Laborales en Pandemia - Región Interamericana - ISP.sav")
ISP_p <- read_sav(file= "informe_resultados/data/Condições de Trabalho em Pandemia - Região Interamericana - ISP.sav")

# 3. Recodificar error
ISP_p$q0068 <- as.numeric(ISP$q0068)
ISP_p$q0068_other <- as.character(ISP$q0068_other)
ISP_p <- ISP_p %>% mutate(q0068 = case_when(q0068 == 0 ~ 3, TRUE ~ q0068)) 

# Merge
names(ISP_es)
sym_diff <- function(a,b) setdiff(union(a,b), intersect(a,b))
sym_diff(names(ISP_es), names(ISP_p))

# Se eliminarán variables que se agregaron arbitrariamente 
ISP_p<- ISP_p %>% select(-c(q0068_other, p0012, p0013, q0031_0004, q0041_0004))

# Ro binds
ISP <- bind_rows(ISP_es, ISP_p)

# 3. Guardar
save(ISP, file = "informe_resultados/data/ISP_merge.RData")
