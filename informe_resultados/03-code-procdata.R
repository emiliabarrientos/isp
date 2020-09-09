##### Procesamiento de variables #####

# 1.Cargar librerias ----
pacman::p_load(haven,dplyr, tidyverse,lubridate)

#2. Cargar base de datos -----
ISP <- read_sav(file= "informe_resultados/data/Condiciones Laborales en Pandemia - Región Interamericana - ISP.sav")

#3. Explorar base de datos ----
dim(ISP)
names(ISP) #228 variables

## 0. Identificadores ----
# "CollectorNm"
table(ISP$CollectorNm, useNA = "ifany")
# Se eliminara

# "respondent_id"
table(ISP$respondent_id, useNA = "ifany")
str(ISP$respondent_id)


# "collector_id"
table(ISP$collector_id, useNA = "ifany")
#Se eliminara

# "date_created" 
table(ISP$date_created, useNA = "ifany")

# "date_modified"
table(ISP$date_modified, useNA = "ifany")
#Se elimina

# "ip_address"
table(ISP$ip_address, useNA = "ifany")

# "email_address"
table(ISP$email_address, useNA = "ifany")
#se elimina

# "first_name"   
table(ISP$first_name, useNA = "ifany")
#se elimina

# "last_name"
table(ISP$last_name, useNA = "ifany")
# se limina

# "custom_1"     
table(ISP$custom_1, useNA = "ifany")
#se elimina

# Eliminar variables
ISP <- ISP %>% select(- c(custom_1, last_name, first_name, email_address, collector_id, CollectorNm))
# Transformar vectores
sapply(ISP, class) # Ver tipo
ISP[1] <- sapply(ISP[1],as.numeric) # id a numerica

## A. Modulo Sociodemograficas ----
# a0_pais ("q0001") - Pais----
table(ISP$q0001)
ISP$a0_pais <- as.character(ISP$q0001)
ISP$a0_pais <- car::recode(ISP$a0_pais, recodes = c("1='Argentina';2='Brasil'; 3='Colombia'; 4='Costa Rica';
                                                    5='Ecuador';6='México';7='Perú'"))

table(ISP$a0_pais)
# a1_sexo ("q0001") - Sexo --------------------
table(ISP$q0002)
ISP$q0002 <- as.factor(ISP$q0002)
ISP$a1_sexo <- car::recode(ISP$q0002, recodes = c("1='Masculino';2='Femenino';3='Otro'"), as.factor = T)

# a2_edad ("q0005") Edad   -------------        
table(ISP$q0005)
ISP$a2_edad <- as.numeric(ISP$q0005)
ISP <- ISP %>% mutate(edad_cat = case_when(a2_edad < 36 ~ "Jóvenes",
                                           a2_edad > 36 ~ "Adultos",
                                           TRUE ~ NA_character_))
table(ISP$edad_cat)
ISP %>% select(a2_edad, edad_cat)
# a3_educ ("q0006") Educacion ----------------------
table(ISP$q0006)
ISP$q0006 <- as.factor(ISP$q0006)
ISP$a3_educ <- car::recode(ISP$q0006, recodes = c("1='Sin estudios';2='Basica';3='Media';4='Tecnico Profesional';5='Universitaria';6='Posgrado'"), as.factor = T,
                           levels = c("Sin estudios","Basica","Media","Tecnico Profesional","Universitaria", "Posgrado"))
levels(ISP$a3_educ) #verificar
table(ISP$a3_educ)


# Etnia (table(ISP$q0003))
table(ISP$q0003)
ISP$q0003 <- as.factor(ISP$q0003)
ISP$a5_etnia <- car::recode(ISP$q0003, recodes = c("1='Si';2='No'"), as.factor = T)
table(ISP$a5_etnia)

# a6.1, a6.2 y a6.3 (q0021, q0022,, q0023) - Familia ------------------ 
ISP$a6.1 <- as.numeric(ISP$q0007)

# Filtro
table((ISP$q0008))
ISP$a6.2 <- as.factor(ISP$q0008)
ISP$a6.2 <- car::recode(ISP$a6.2, recodes = c("1='Soy el principal sonten económico de mi hogar';2='Comparto con otra persona el sontén económico de mi hogar';3='Otra persona es el principal sostén del hogar'"), as.factor = T,
                               levels = c('Soy el principal sonten económico de mi hogar','Comparto con otra persona el sontén económico de mi hogar','Otra persona es el principal sostén del hogar'))

table(ISP$a6.2)

# a6.2Filtro
table((ISP$q0009))
ISP$a6.2.filtro <- as.factor(ISP$q0009)
ISP$a6.2.filtro <- car::recode(ISP$a6.2.filtro, recodes = c("1='Mi esposo (a) o pareja con la que vivo';
                                                            2='Mi esposo (a) o pareja con la que no vivo';
                                                            3='Ex esposo (a) o ex pareja';
                                                            4='Otra persona (padre, madre, hijos (as), otro familiar, otra persona)'"), as.factor = T,
                               levels = c('Mi esposo (a) o pareja con la que vivo','Mi esposo (a) o pareja con la que no vivo','Ex esposo (a) o ex pareja','Otra persona (padre, madre, hijos (as), otro familiar, otra persona)'))

table((ISP$a6.2.filtro))

# a6.3
ISP$a6.3 <- as.numeric(ISP$q0010)
ISP$a6.4 <- as.numeric(ISP$q0011)
ISP$a6.5 <- as.numeric(ISP$q0012)

table((ISP$q0013))
# B. Modulo Empleo ------------------------------------------------------

# b1 - (q0013) Servicio que trabaja --------------
ISP$b1 <- as.factor(ISP$q0013)
ISP$b1 <- car::recode(ISP$b1, recodes = c("1='Administración Central';2='Gobiernos Regionales';3='Gobiernos Locales/Municipales';4='Poder Judicial';5='Poder Legislativo';6='Salud';7='Agua';8='Energía'; 9= 'Educación'; 10='Otro'"), as.factor = T,
                      levels = c('Administración Central','Gobiernos Regionales','Gobiernos Locales/Municipales','Poder Judicial','Poder Legislativo','Salud','Agua','Energía','Educación','Otro'))

table(ISP$q0013)
table(ISP$b1)


# D. Flexibilidad ------
#Construir desde la d1 a las d4.7
table(ISP$q0022_0001)

## D1 Autonomia ------
## d1.1 (q0022_0001) - velocidad  ----
table(ISP$q0022_0001)
ISP$d1.1 <- as.factor(ISP$q0022_0001)
ISP$d1.1 <- car::recode(ISP$d1.1, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
levels(ISP$d1.1)
table(ISP$d1.1)
## d1.1 (q0022_0002) - cantidad -----
table(ISP$q0022_0002)
ISP$d1.2 <- as.factor(ISP$q0022_0002)
ISP$d1.2 <- car::recode(ISP$d1.2, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
table(ISP$d1.2)
## q0022_0003 - orden (d1.3)-----
table(ISP$q0022_0003)
ISP$d1.3 <- as.factor(ISP$q0022_0003)
ISP$d1.3 <- car::recode(ISP$d1.3, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
table(ISP$d1.3)
## d1. (q0022_0004) - descanso -----
table(ISP$q0022_0004)
ISP$d1.4 <- as.factor(ISP$q0022_0004)
ISP$d1.4 <- car::recode(ISP$d1.4, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
table(ISP$d1.4)

## D2 - Temporal  --------------------

## d2.1 (q0023_0001) - extension  ----
table(ISP$q0023_0001)
ISP$d2.1 <- as.factor(ISP$q0023_0001)
ISP$d2.1 <- car::recode(ISP$d2.1, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
table(ISP$d2.1)

## d2.2 (q0023_0002) - fines de semana  ----
table(ISP$q0023_0002)
ISP$d2.2 <- as.factor(ISP$q0023_0002)
ISP$d2.2 <- car::recode(ISP$d2.2, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
table(ISP$d2.2)

## D3 salarial -----
## d3 (q0024) - extension -------
table(ISP$q0024)
ISP$d3 <- as.factor(ISP$q0024)
ISP$d3 <- car::recode(ISP$d3, recodes= c("1='Totalmente fijo';2='La mayor parte fijo';3='La mayor parte variable';4='Totalmente variable';NA=NA"), as.factor = T,
                      levels= c("Totalmente fijo", "La mayor parte fijo", "La mayor parte variable", "Totalmente variable"))
table(ISP$d3)

## D4 cambio de flexibilidad ----
## d4.1 (q0025_0001) - velocidad ----
table(ISP$q0025_0001)
ISP$d4.1 <- as.factor(ISP$q0025_0001)
ISP$d4.1 <- car::recode(ISP$d4.1, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))
table(ISP$q0025_0001)
table(ISP$d4.1)
## d4.2 (q0025_0002) - cantidad ----
ISP$d4.2 <- as.factor(ISP$q0025_0002)
ISP$d4.2 <- car::recode(ISP$d4.2, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))
table(ISP$q0025_0002)
table(ISP$d4.2)

## d4.3 (q0025_0003) - orden  ----
ISP$d4.3 <- as.factor(ISP$q0025_0003)
ISP$d4.3 <- car::recode(ISP$d4.3, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))
table(ISP$q0025_0003)
table(ISP$d4.3)

## d4.4 (q0025_0004) - descanso ----

ISP$d4.4 <- as.factor(ISP$q0025_0004)
ISP$d4.4 <- car::recode(ISP$d4.4, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))

table(ISP$q0025_0004)
table(ISP$d4.4)

## d4.5 (q0025_0005) - extra hora ----
ISP$d4.5 <- as.factor(ISP$q0025_0005)
ISP$d4.5 <- car::recode(ISP$d4.5, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))

table(ISP$q0025_0005)
table(ISP$d4.5)

## d4.6 (q0025_0006) - fines de semana  ----
ISP$d4.6 <- as.factor(ISP$q0025_0006)
ISP$d4.6 <- car::recode(ISP$d4.6, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))

table(ISP$q0025_0006)
table(ISP$d4.6)

## d4.7 (q0025_0007) - salarial ----
ISP$d4.7 <- as.factor(ISP$q0025_0007)
ISP$d4.7 <- car::recode(ISP$d4.7, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))

table(ISP$q0025_0007)
table(ISP$d4.7)

########## Modalidad de trabajo ###################
# 4. Combinar variables --------
table(ISP$q0026)

# q0038

ISP %>% subset(!is.na(q0028_0001)&!is.na(ISP$q0038_0001)) %>% select(respondent_id,q0028_0001,q0038_0001,q0026)

ISP<-ISP %>% mutate(q38_1=case_when(q0026==1 ~ q0028_0001, q0026==2 ~ q0038_0001, TRUE ~ NA_real_),
                    q38_2=case_when(q0026==1 ~ q0028_0002, q0026==2 ~ q0038_0002, TRUE ~ NA_real_),
                    q38_3=case_when(q0026==1 ~ q0028_0003, q0026==2 ~ q0038_0003, TRUE ~ NA_real_),
                    q38_4=case_when(q0026==1 ~ q0028_0004, q0026==2 ~ q0038_0004, TRUE ~ NA_real_))


table(ISP$q0026,ISP$q38_1)
# q0039

ISP<-ISP %>% mutate(q39_1=case_when(q0026==1 ~ q0029_0001, q0026==2 ~ q0039_0001, TRUE ~ NA_real_),
                    q39_2=case_when(q0026==1 ~ q0029_0002, q0026==2 ~ q0039_0002, TRUE ~ NA_real_),
                    q39_3=case_when(q0026==1 ~ q0029_0003, q0026==2 ~ q0039_0003, TRUE ~ NA_real_))


table(ISP$q0026,ISP$q39_1)

# q0040

ISP<-ISP %>% mutate(q40_1=case_when(q0026==1 ~ q0030_0001, q0026==2 ~ q0040_0001, TRUE ~ NA_real_),
                    q40_2=case_when(q0026==1 ~ q0030_0002, q0026==2 ~ q0040_0002, TRUE ~ NA_real_),
                    q40_3=case_when(q0026==1 ~ q0030_0003, q0026==2 ~ q0040_0003, TRUE ~ NA_real_),
                    q40_4=case_when(q0026==1 ~ q0030_0004, q0026==2 ~ q0040_0004, TRUE ~ NA_real_),
                    q40_5=case_when(q0026==1 ~ q0030_0005, q0026==2 ~ q0040_0005, TRUE ~ NA_real_))

table(ISP$q0026,ISP$q40_1)
# q0041

ISP<-ISP %>% mutate(q41_1=case_when(q0026==1 ~ q0031_0001, q0026==2 ~ q0041_0001, TRUE ~ NA_real_),
                    q41_2=case_when(q0026==1 ~ q0031_0002, q0026==2 ~ q0041_0002, TRUE ~ NA_real_),
                    q41_3=case_when(q0026==1 ~ q0031_0003, q0026==2 ~ q0041_0003, TRUE ~ NA_real_))

table(ISP$q0026,ISP$q41_1)

# q0042

ISP<-ISP %>% mutate(q42_1=case_when(q0026==1 ~ q0032_0001, q0026==2 ~ q0042_0001, TRUE ~ NA_real_),
                    q42_2=case_when(q0026==1 ~ q0032_0002, q0026==2 ~ q0042_0002, TRUE ~ NA_real_),
                    q42_3=case_when(q0026==1 ~ q0032_0003, q0026==2 ~ q0042_0003, TRUE ~ NA_real_),
                    q42_4=case_when(q0026==1 ~ q0032_0004, q0026==2 ~ q0042_0004, TRUE ~ NA_real_),
                    q42_5=case_when(q0026==1 ~ q0032_0005, q0026==2 ~ q0042_0005, TRUE ~ NA_real_),
                    q42_6=case_when(q0026==1 ~ q0032_0006, q0026==2 ~ q0042_0006, TRUE ~ NA_real_),
                    q42_7=case_when(q0026==1 ~ q0032_0007, q0026==2 ~ q0042_0007, TRUE ~ NA_real_),
                    q42_8=case_when(q0026==1 ~ q0032_0008, q0026==2 ~ q0042_0008, TRUE ~ NA_real_))

table(ISP$q0026,ISP$q42_1)

# q0043
ISP$q0033<-as.numeric(as.factor(as.numeric(ISP$q0033)))
ISP$q0043<-as.numeric(as.factor(as.numeric(ISP$q0043)))

ISP<-ISP %>% mutate(q43=case_when(q0026==1 ~ q0033, q0026==2 ~ q0043, TRUE ~ NA_real_))
table(ISP$q0026,ISP$q43)

# q0044

ISP$q0044_0001<-as.numeric(as.factor(as.numeric(ISP$q0044_0001)))
ISP$q0044_0002<-as.numeric(as.factor(as.numeric(ISP$q0044_0002)))

ISP<-ISP %>% mutate(q44_1=case_when(q0026==1 ~ q0034_0001, q0026==2 ~ q0044_0001, TRUE ~ NA_real_),
                    q44_2=case_when(q0026==1 ~ q0034_0002, q0026==2 ~ q0044_0002, TRUE ~ NA_real_))

table(ISP$q0026,ISP$q0044_0001)
table(ISP$q0026,ISP$q44_1)

# q0045
ISP$q0035<-as.numeric(as.factor(as.numeric(ISP$q0035)))
ISP$q0045<-as.numeric(as.factor(as.numeric(ISP$q0045)))

ISP<-ISP %>% mutate(q45=case_when(q0026==1 ~ q0035, q0026==2 ~ q0045, TRUE ~ NA_real_))
table(ISP$q0026,ISP$q45)

# q0046

ISP<-ISP %>% mutate(q46=case_when(q0026==1 ~ q0036, q0026==2 ~ q0046, TRUE ~ NA_real_))
table(ISP$q0026,ISP$q0046)
table(ISP$q0026,ISP$q46)


# q0027
ISP$q0027<-as.numeric(as.factor(as.numeric(ISP$q0027)))
ISP$q0037<-as.numeric(as.factor(as.numeric(ISP$q0037)))

ISP<-ISP %>% mutate(q27=case_when(q0026==1 ~ q0027, q0026==2 ~ q0036, TRUE ~ NA_real_))
table(ISP$q0026,ISP$q27)
table(ISP$q0026,ISP$q0027)

names(ISP$q0038)


## Variables finales son 
## ##q27; q38_1; ##q39_1 ##q40_1 ##q41_1 ##q42 ##q43 ##q44_1 ##q45; q46


## d5_mod (q0026) - Teletrabajo -----------------
table(ISP$q0026)
ISP$d5_mod <- as.factor(ISP$q0026)
ISP$d5_mod <- car::recode(ISP$d5_mod, recodes= c("1='Teletrabajo total';2='Teletrabajo parcial';3='Trabajo normal';NA=NA"), as.factor = T,
                          levels= c("Teletrabajo total", "Teletrabajo parcial", "Trabajo normal"))
levels(ISP$d5_mod)
table(ISP$d5_mod)

### E. - Condiciones de trabajo, familia y salud  ----
## e1 (q27) - personas en hogar  -----
table(ISP$q27)
ISP$e1 <- as.numeric(ISP$q27)
summary(ISP$e1)

## e2 (q38) - uso herramientas (e2) -----
## e2.1 (q38_1) - computador  -----
table(ISP$q38_1)
ISP$e2.1 <- as.factor(ISP$q38_1)
ISP$e2.1 <- car::recode(ISP$e2.1, recodes= c("1='Exclusivo para el trabajo';2='Compartida en el hogar';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Exclusivo para el trabajo", "Compartida en el hogar", "No la utilizo"))
table(ISP$e2.1)

## q0028_2 - celular (e2.2) -----
table(ISP$q38_2)
ISP$e2.2 <- as.factor(ISP$q38_2)
ISP$e2.2 <- car::recode(ISP$e2.2, recodes= c("1='Exclusivo para el trabajo';2='Compartida en el hogar';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Exclusivo para el trabajo", "Compartida en el hogar", "No la utilizo"))

table(ISP$e2.2)

## q0028_3 - conexion internet (e2.3) -----
table(ISP$q38_3)
ISP$e2.3 <- as.factor(ISP$q38_3)
ISP$e2.3 <- car::recode(ISP$e2.3, recodes= c("1='Exclusivo para el trabajo';2='Compartida en el hogar';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Exclusivo para el trabajo", "Compartida en el hogar", "No la utilizo"))
table(ISP$e2.3)

## q0028_4 - escritorio mesa (e2.4) -----
table(ISP$q38_4)
ISP$e2.4 <- as.factor(ISP$q38_4)
ISP$e2.4 <- car::recode(ISP$e2.4, recodes= c("1='Exclusivo para el trabajo';2='Compartida en el hogar';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Exclusivo para el trabajo", "Compartida en el hogar", "No la utilizo"))

table(ISP$e2.4)


## q0029 - propiedad herramientas (e3) -----
## q0029_1 - computador (e3.1) -----
table(ISP$q39_1)
ISP$e3.1 <- as.factor(ISP$q39_1)
ISP$e3.1 <- car::recode(ISP$e3.1, recodes= c("1='Servicio';2='Propias';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Servicio", "Propias", "No la utilizo"))
table(ISP$e3.1)

## q0029_2 - celular (e3.2) -----
table(ISP$q39_2)
ISP$e3.2 <- as.factor(ISP$q39_2)
ISP$e3.2 <- car::recode(ISP$e3.2, recodes= c("1='Servicio';2='Propias';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Servicio", "Propias", "No la utilizo"))
table(ISP$e3.2)

## q0029_3 - conexion internet (e3.3) -----
table(ISP$q39_3)
ISP$e3.3 <- as.factor(ISP$q39_3)
ISP$e3.3 <- car::recode(ISP$e3.3, recodes= c("1='Servicio';2='Propias';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Servicio", "Propias", "No la utilizo"))

table(ISP$e3.3)

## q0030 - calidad herramientas (e4) -----
## q0030_1 - computador (e4.1) -----
table(ISP$q40_1)
ISP$e4.1 <- as.factor(ISP$q40_1)
ISP$e4.1 <- car::recode(ISP$e4.1, recodes= c("1='Muy mala';2='Mala';3='Regular';4='Buena'; 5='Muy buena';6='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Muy mala", "Mala","Regular","Buena","Muy buena", "No la utilizo"))
table(ISP$e4.1)

## q0030_2 - celular (e4.2) -----
table(ISP$q40_2)
ISP$e4.2 <- as.factor(ISP$q40_2)
ISP$e4.2 <- car::recode(ISP$e4.2, recodes= c("1='Muy mala';2='Mala';3='Regular';4='Buena'; 5='Muy buena';6='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Muy mala", "Mala","Regular","Buena","Muy buena", "No la utilizo"))
table(ISP$e4.2)
## q0030_3 - conexion internet (e4.3) -----
table(ISP$q40_3)
ISP$e4.3 <- as.factor(ISP$q40_3)
ISP$e4.3 <- car::recode(ISP$e4.3, recodes= c("1='Muy mala';2='Mala';3='Regular';4='Buena'; 5='Muy buena';6='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Muy mala", "Mala","Regular","Buena","Muy buena", "No la utilizo"))
table(ISP$e4.3)
## q0030_4 - silla (e4.4) -----
table(ISP$q40_4)
ISP$e4.4 <- as.factor(ISP$q40_4)
ISP$e4.4 <- car::recode(ISP$e4.4, recodes= c("1='Muy mala';2='Mala';3='Regular';4='Buena'; 5='Muy buena';6='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Muy mala", "Mala","Regular","Buena","Muy buena", "No la utilizo"))
table(ISP$e4.4)
## q0030_5 - escritorio mesa (e4.5) -----
table(ISP$q40_5)
ISP$e4.5 <- as.factor(ISP$q40_5)
ISP$e4.5 <- car::recode(ISP$e4.5, recodes= c("1='Muy mala';2='Mala';3='Regular';4='Buena'; 5='Muy buena';6='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Muy mala", "Mala","Regular","Buena","Muy buena", "No la utilizo"))
table(ISP$e4.5)

## q0031 - protocolos (e5) -----
## q0031_1 - organizar trabajo (e5.1) -----
table(ISP$q41_1)
ISP$e5.1 <- as.factor(ISP$q41_1)
ISP$e5.1 <- car::recode(ISP$e5.1, recodes= c("1='Si';2='No';NA=NA"), as.factor = T,
                        levels= c("Si", "No"))
table(ISP$e5.1)
## q0031_2 - seguridad e higiene (e5.2) -----
table(ISP$q41_2)
ISP$e5.2 <- as.factor(ISP$q41_2)
ISP$e5.2 <- car::recode(ISP$e5.2, recodes= c("1='Si';2='No';NA=NA"), as.factor = T,
                        levels= c("Si", "No"))

table(ISP$e5.2)

## q0031_3 - accidentes (e5.3) -----
table(ISP$q41_3)
ISP$e5.3 <- as.factor(ISP$q41_3)
ISP$e5.3 <- car::recode(ISP$e5.3, recodes= c("1='Si';2='No';NA=NA"), as.factor = T,
                        levels= c("Si", "No"))
table(ISP$e5.3)

## Plataformas ------------------
## q0032 - plataformas (e6.1)------
table(ISP$q42_8, ISP$d5_mod)

## q0032_1 - teams (e6.1_teams) -----
table(ISP$q42_1, useNA = "ifany")
ISP$e6.1_teams <- as.factor(ISP$q42_1)
ISP$e6.1_teams <- car::recode(ISP$e6.1_teams, recodes= c("1='Si';NA='No'"), as.factor = T,
                              levels= c("Si", "No"))
table(ISP$e6.1_teams)

## q0032_2 - zoom (e6.1_zoom) -----
table(ISP$q42_2)
ISP$e6.1_zoom <- as.factor(ISP$q42_2)
ISP$e6.1_zoom <- car::recode(ISP$e6.1_zoom, recodes= c("1='Si';NA='No'"), as.factor = T,
                             levels= c("Si", "No"))
table(ISP$e6.1_zoom)

## q0032_3 - meet (e6.1_meet) -----
table(ISP$q42_3)
ISP$e6.1_meet <- as.factor(ISP$q42_3)
ISP$e6.1_meet <- car::recode(ISP$e6.1_meet, recodes= c("1='Si';NA='No'"), as.factor = T,
                             levels= c("Si", "No"))
## q0032_4 - intranet (e6.1_intranet) -----
table(ISP$q42_4)
ISP$e6.1_intranet <- as.factor(ISP$q42_4)
ISP$e6.1_intranet <- car::recode(ISP$e6.1_intranet, recodes= c("1='Si';NA='No'"), as.factor = T,
                                 levels= c("Si", "No"))
## q0032_5 - whats app (e6.1_whatsapp) -----
table(ISP$q42_5)
ISP$e6.1_whatsapp <- as.factor(ISP$q42_5)
ISP$e6.1_whatsapp <- car::recode(ISP$e6.1_whatsapp, recodes= c("1='Si';NA='No'"), as.factor = T,
                                 levels= c("Si", "No"))
## q0032_6 - yammer (e6.1_yammer) -----
table(ISP$q42_6)
ISP$e6.1_yammer <- as.factor(ISP$q42_6)
ISP$e6.1_yammer <- car::recode(ISP$e6.1_yammer, recodes= c("1='Si';NA='No'"), as.factor = T,
                               levels= c("Si", "No"))
table(ISP$e6.1_yammer)

## q0032_7 - duo (e6.1_duo) -----
table(ISP$q42_7)
ISP$e6.1_duo <- as.factor(ISP$q42_7)
ISP$e6.1_duo <- car::recode(ISP$e6.1_duo, recodes= c("1='Si';NA='No'"), as.factor = T,
                            levels= c("Si", "No"))
## q0032_8 - otro (e6.1_otro) -----
table(ISP$q42_8)
ISP$e6.1_otro <- as.factor(ISP$q42_8)
ISP$e6.1_otro <- car::recode(ISP$e6.1_otro, recodes= c("1='Si';NA='No'"), as.factor = T,
                             levels= c("Si", "No"))
table(ISP$e6.1_otro)
## q43_71 - tiempo plataforma (e6.2)
table(ISP$q43)
ISP$e6.2<-as.numeric(ISP$q43)
summary(ISP$e6.2)

## e6.3 (q0034) - problemas  -----
## q44_1 - vpn  (e6.3_vpn)----
table(ISP$q44_1)
ISP$e6.3_vpn <- as.factor(ISP$q44_1)
ISP$e6.3_vpn <- car::recode(ISP$e6.3_vpn, recodes= c("1= 'No aplica';2='Nunca';3='Rara vez';4='Algunas veces';5='Casi siempre';6='Siempre';NA=NA"), as.factor = T,
                            levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre", "No aplica"))

table(ISP$e6.3_vpn)

## q44_2 - desconexion  (e6.3_desc)----
table(ISP$q44_2)
ISP$e6.3_desc <- as.factor(ISP$q44_2)
ISP$e6.3_desc <- car::recode(ISP$e6.3_desc, recodes= c("1= 'No aplica';2='Nunca';3='Rara vez';4='Algunas veces';5='Casi siempre';6='Siempre';NA=NA"), as.factor = T,
                             levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre", "No aplica"))

table(ISP$e6.3_desc)

##q45 - En un día de trabajo, ¿cuántas horas contínuas puede dedicarle sin interrupciones? -------
table(ISP$q45)
ISP$e6.4<-as.numeric(ISP$q45)
summary(ISP$e6.4)

# 46 (e7) -------
table(ISP$q46)
ISP$e7 <- as.factor(ISP$q46)
ISP$e7 <- car::recode(ISP$e7, recodes= c("1='Espacio individual'; 2= 'Espacio compartido';NA='No'"), as.factor = T,
                             levels= c("Espacio individual", "Espacio compartido"))
table(ISP$e7)


## F. Condiciones de trabajo habitual (normales y teleparcial) ---------------

## Merge questionaries
# q0042

ISP<-ISP %>% mutate(q0047_0001=case_when(q0026==2   ~ q0047_0001, q0026==3 ~ q0050_0001, TRUE ~ NA_real_),
                    q0047_0002=case_when(q0026==2 ~ q0047_0002, q0026==3 ~ q0050_0002, TRUE ~ NA_real_),
                    q0047_0002=case_when(q0026==2 ~ q0047_0003, q0026==3 ~ q0050_0003, TRUE ~ NA_real_),
                    q0047_0004=case_when(q0026==2 ~ q0047_0004, q0026==3 ~ q0050_0004, TRUE ~ NA_real_),
                    q0047_0005=case_when(q0026==2 ~ q0047_0005, q0026==3 ~ q0050_0005, TRUE ~ NA_real_),
                    q0047_0006=case_when(q0026==2 ~ q0047_0006, q0026==3 ~ q0050_0006, TRUE ~ NA_real_),
                    q0047_0007=case_when(q0026==2 ~ q0047_0007, q0026==3 ~ q0050_0007, TRUE ~ NA_real_),
                    q0047_0008=case_when(q0026==2 ~ q0047_0008, q0026==3 ~ q0050_0008, TRUE ~ NA_real_),
                    q0047_0009=case_when(q0026==2 ~ q0047_0009, q0026==3 ~ q0050_0009, TRUE ~ NA_real_),
                    q0047_0010=case_when(q0026==2 ~ q0047_0010, q0026==3 ~ q0050_0010, TRUE ~ NA_real_),
                    q0047_0011=case_when(q0026==2 ~ q0047_0011, q0026==3 ~ q0050_0011, TRUE ~ NA_real_),
                    q0047_0012=case_when(q0026==2 ~ q0047_0012, q0026==3 ~ q0050_0012, TRUE ~ NA_real_),
                    q0047_0013=case_when(q0026==2 ~ q0047_0013, q0026==3 ~ q0050_0013, TRUE ~ NA_real_),
                    q0047_0014=case_when(q0026==2 ~ q0047_0014, q0026==3 ~ q0050_0014, TRUE ~ NA_real_),
                    q0047_0015=case_when(q0026==2 ~ q0047_0015, q0026==3 ~ q0050_0015, TRUE ~ NA_real_))

table(ISP$q0047_0010, ISP$q0026)


## F1 (q0047) - medidas prevencion  ----
table(ISP$q0047_0001, ISP$d5_mod)

## q0047_1  (f1.1) -----
table(ISP$q0047_0001)
ISP$f1.1 <- as.factor(ISP$q0047_0001)
ISP$f1.1 <- car::recode(ISP$f1.1, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
table(ISP$f1.1)
## q0047_2 -  (f1.2) -----
table(ISP$q0047_0002)
ISP$f1.2 <- as.factor(ISP$q0047_0002)
ISP$f1.2 <- car::recode(ISP$f1.2, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_3 - (f1.3) -----
table(ISP$q0047_0003)
ISP$f1.3 <- as.factor(ISP$q0047_0003)
ISP$f1.3 <- car::recode(ISP$f1.3, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_4 - (f1.4) -----
table(ISP$q0047_0004)
ISP$f1.4 <- as.factor(ISP$q0047_0004)
ISP$f1.4 <- car::recode(ISP$f1.4, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_5 - (f1.5) -----
table(ISP$q0047_0005)
ISP$f1.5 <- as.factor(ISP$q0047_0005)
ISP$f1.5 <- car::recode(ISP$f1.5, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
table(ISP$f1.5)
## q0047_6 - (f1.6) -----
table(ISP$q0047_0006)
ISP$f1.6 <- as.factor(ISP$q0047_0006)
ISP$f1.6 <- car::recode(ISP$f1.6, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_7 - (f1.7) -----
table(ISP$q0047_0007)
ISP$f1.7 <- as.factor(ISP$q0047_0007)
ISP$f1.7 <- car::recode(ISP$f1.7, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_8 - (f1.8) -----
table(ISP$q0047_0008)
ISP$f1.8 <- as.factor(ISP$q0047_0008)
ISP$f1.8 <- car::recode(ISP$f1.8, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_9 - (f1.9) -----
table(ISP$q0047_0009)
ISP$f1.9 <- as.factor(ISP$q0047_0009)
ISP$f1.9 <- car::recode(ISP$f1.9, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_10 - (f1.10) -----
table(ISP$q0047_0010)
ISP$f1.10 <- as.factor(ISP$q0047_0010)
ISP$f1.10 <- car::recode(ISP$f1.10, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_11 - (f1.11) -----
table(ISP$q0047_0011)
ISP$f1.11 <- as.factor(ISP$q0047_0011)
ISP$f1.11 <- car::recode(ISP$f1.11, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_12 - (f1.12) -----
table(ISP$q0047_0012)
ISP$f1.12 <- as.factor(ISP$q0047_0012)
ISP$f1.12 <- car::recode(ISP$f1.12, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_13 - (f1.13) -----
table(ISP$q0047_0013)
ISP$f1.13 <- as.factor(ISP$q0047_0013)
ISP$f1.13 <- car::recode(ISP$f1.13, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_14 - (f1.14) -----
table(ISP$q0047_0014)
ISP$f1.14 <- as.factor(ISP$q0047_0014)
ISP$f1.14 <- car::recode(ISP$f1.14, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0047_15 - (f1.15) -----
table(ISP$q0047_0015)
ISP$f1.15 <- as.factor(ISP$q0047_0015)
ISP$f1.15 <- car::recode(ISP$f1.15, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))

## F2 (q0048) - Daños  (corregir) ----
table(ISP$q0048_0003, ISP$d5_mod)

ISP<-ISP %>% mutate(q0048_0001=case_when(q0026==2   ~ q0048_0001, q0026==3 ~ q0051_0001, TRUE ~ NA_real_),
                    q0048_0002=case_when(q0026==2 ~ q0048_0002, q0026==3 ~ q0051_0002, TRUE ~ NA_real_),
                    q0048_0002=case_when(q0026==2 ~ q0048_0003, q0026==3 ~ q0051_0003, TRUE ~ NA_real_),
                    q0048_0004=case_when(q0026==2 ~ q0048_0004, q0026==3 ~ q0051_0004, TRUE ~ NA_real_),
                    q0048_0005=case_when(q0026==2 ~ q0048_0005, q0026==3 ~ q0051_0005, TRUE ~ NA_real_),
                    q0048_0006=case_when(q0026==2 ~ q0048_0006, q0026==3 ~ q0051_0006, TRUE ~ NA_real_),
                    q0048_0007=case_when(q0026==2 ~ q0048_0007, q0026==3 ~ q0051_0007, TRUE ~ NA_real_))

table(ISP$q0048_0007, ISP$q0026)


## q0048_1  (f2.1) -----
table(ISP$q0048_0001)
ISP$f2.1 <- as.factor(ISP$q0048_0001)
ISP$f2.1 <- car::recode(ISP$f2.1, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
table(ISP$f2.1)
## q0048_2 -  (f2.2) -----
table(ISP$q0048_0002)
ISP$f2.2 <- as.factor(ISP$q0048_0002)
ISP$f2.2 <- car::recode(ISP$f2.2, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
table(ISP$f2.2)
## q0048_3 - (f2.3) -----
table(ISP$q0048_0003)
ISP$f2.3 <- as.factor(ISP$q0048_0003)
ISP$f2.3 <- car::recode(ISP$f2.3, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
## q0048_4 - (f2.4) -----
table(ISP$q0048_0004)
ISP$f2.4 <- as.factor(ISP$q0048_0004)
ISP$f2.4 <- car::recode(ISP$f2.4, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))

## q0048_5 - (f2.5) -----
table(ISP$q0048_0005)
ISP$f2.5 <- as.factor(ISP$q0048_0005)
ISP$f2.5 <- car::recode(ISP$f2.5, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))

table(ISP$f2.5)
## q0048_6 - (f2.6) -----
table(ISP$q0048_0006)
ISP$f2.6 <- as.factor(ISP$q0048_0006)
ISP$f2.6 <- car::recode(ISP$f2.6, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))

table(ISP$f2.6)
## q0048_7 - (f2.7) -----
table(ISP$q0048_0007)
ISP$f2.7 <- as.factor(ISP$q0048_0007)
ISP$f2.7 <- car::recode(ISP$f2.7, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))


## F3 (q0049) - incumplimientos  ----
table(ISP$q0049_0005, ISP$d5_mod)

ISP<-ISP %>% mutate(q0049_0001=case_when(q0026==2 ~ q0049_0001, q0026==3 ~ q0052_0001, TRUE ~ NA_real_),
                    q0049_0002=case_when(q0026==2 ~ q0049_0002, q0026==3 ~ q0052_0002, TRUE ~ NA_real_),
                    q0049_0002=case_when(q0026==2 ~ q0049_0003, q0026==3 ~ q0052_0003, TRUE ~ NA_real_),
                    q0049_0004=case_when(q0026==2 ~ q0049_0004, q0026==3 ~ q0052_0004, TRUE ~ NA_real_),
                    q0049_0005=case_when(q0026==2 ~ q0049_0005, q0026==3 ~ q0052_0005, TRUE ~ NA_real_))

table(ISP$q0048_0007, ISP$q0026)

## q0049_1  (f3.1) -----
table(ISP$q0049_0001)
ISP$f3.1 <- as.factor(ISP$q0049_0001)
ISP$f3.1 <- car::recode(ISP$f3.1, recodes= c("1='Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))

table(ISP$q0049_0001)
table(ISP$f3.1)

## q0049_2 -  (f3.2) -----
table(ISP$q0049_0002)
ISP$f3.2 <- as.factor(ISP$q0049_0002)
ISP$f3.2 <- car::recode(ISP$f3.2, recodes= c("1='Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))

## q0049_3 - (f3.3) -----
table(ISP$q0049_0003)
ISP$f3.3 <- as.factor(ISP$q0049_0003)
ISP$f3.3 <- car::recode(ISP$f3.3, recodes= c("1='Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))
## q0049_4 - (f3.4) -----
table(ISP$q0049_0004)
ISP$f3.4 <- as.factor(ISP$q0049_0004)
ISP$f3.4 <- car::recode(ISP$f3.4, recodes= c("1='Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))

## q0049_5 - (f3.5) -----
table(ISP$q0049_0005)
ISP$f3.5 <- as.factor(ISP$q0049_0005)
ISP$f3.5 <- car::recode(ISP$f3.5, recodes= c("1='Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))


table(ISP$q0049_0005)

## G. Trabajo de Cuidados/Familia/Conflictos -----------------

## G1 (q0077) - permisos y cuidados tarea domestica (g1) -----
table(ISP$q0053, ISP$d5_mod)
ISP$g1 <- as.factor(ISP$q0053)
ISP$g1 <- car::recode(ISP$g1, recodes= c("1='Si'; 2='No'"), as.factor = T,
                      levels = c("Si", "No"))
levels(ISP$g1)
table(ISP$g1)
## G2 (q0077) - tarea domestica (g1) -----
table(ISP$q0054, ISP$d5_mod)
ISP$g2 <- as.factor(ISP$q0054)
ISP$g2 <- car::recode(ISP$g2, recodes= c("1='Soy la/el principal responsable y hago la mayor parte de las tareas del hogar';2='Hago más o menos la mitad de las tareas del hogar';3='Hago más o menos la cuarta parte de las tareas del hogar';4='Solo hago tareas puntuales';5='No hago ninguna o casi ninguna de estas tareas';NA=NA"), as.factor = T,
                      levels= c("Soy la/el principal responsable y hago la mayor parte de las tareas del hogar",
                                "Hago más o menos la mitad de las tareas del hogar",
                                "Hago más o menos la cuarta parte de las tareas del hogar",
                                "Solo hago tareas puntuales",
                                "No hago ninguna o casi ninguna de estas tareas"))
levels(ISP$g2)
table(ISP$g2)

## G3 - Tiempo de trabajo
ISP$g3.1 <- as.numeric(ISP$q0055)
ISP$g3.2 <- as.numeric(ISP$q0056)
summary(ISP$g3.1)
summary(ISP$g3.2)


#G4 - tareas de cuidados ----
##q0057 g4.1 --
table(ISP$q0057_0001)
ISP$g4.1 <- as.factor(ISP$q0057_0001)
ISP$g4.1 <- car::recode(ISP$g4.1, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))


table(ISP$g4.1)

##q0057 g4.2 --
table(ISP$q0057_0002)
ISP$g4.2 <- as.factor(ISP$q0057_0002)
ISP$g4.2 <- car::recode(ISP$g4.2, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))


table(ISP$g4.2)

##q0057 g4.3 --
table(ISP$q0057_0003)
ISP$g4.3 <- as.factor(ISP$q0057_0003)
ISP$g4.3 <- car::recode(ISP$g4.3, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))


table(ISP$g4.3)

##q0057 g4.4 --
table(ISP$q0057_0004)
ISP$g4.4 <- as.factor(ISP$q0057_0004)
ISP$g4.4 <- car::recode(ISP$g4.4, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))


table(ISP$g4.4)

##q0057 g4.5 --
table(ISP$q0057_0005)
ISP$g4.5 <- as.factor(ISP$q0057_0005)
ISP$g4.5 <- car::recode(ISP$g4.5, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))


table(ISP$g4.5)

##q0057 g4.6 --
table(ISP$q0057_0006)
ISP$g4.6 <- as.factor(ISP$q0057_0006)
ISP$g4.6 <- car::recode(ISP$g4.6, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))


table(ISP$g4.6)


##G5 (q0058) - limite trabajo (g2)------
## q0058_1 - (g5.1)----
table(ISP$q0058_0001)
ISP$g5.1 <- as.factor(ISP$q0058_0001)
ISP$g5.1 <- car::recode(ISP$g5.1, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))
table(ISP$g5.1)
## q0058_2 - (g5.2)----
table(ISP$q0058_0002)
ISP$g5.2 <- as.factor(ISP$q0058_0002)
ISP$g5.2 <- car::recode(ISP$g5.2, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))
## q0058_3 - (g5.3)----
table(ISP$q0058_0003)
ISP$g5.3 <- as.factor(ISP$q0058_0003)
ISP$g5.3 <- car::recode(ISP$g5.3, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))
# G6 -- redes de apoyo ------
## Antes
## q0059_1 - (g6.1_abuelo)----
table(ISP$q0059_0001)
ISP$g6.1_abuelo <- as.factor(ISP$q0059_0001)
ISP$g6.1_abuelo <- car::recode(ISP$g6.1_abuelo, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))

table(ISP$g6.1_abuelo)

## q0059_1 - (g6.1_otro)----
table(ISP$q0059_0002)
ISP$g6.1_otro <- as.factor(ISP$q0059_0002)
ISP$g6.1_otro <- car::recode(ISP$g6.1_otro, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                               levels= c("Si", "No"))

table(ISP$g6.1_otro)

## q0059_3 - (g6.1_guarderia)----
table(ISP$q0059_0003)
ISP$g6.1_guarderia <- as.factor(ISP$q0059_0003)
ISP$g6.1_guarderia <- car::recode(ISP$g6.1_guarderia, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                               levels= c("Si", "No"))

table(ISP$g6.1_guarderia)

## q0059_4 - (g6.1_escuela)----
table(ISP$q0059_0004)
ISP$g6.1_escuela <- as.factor(ISP$q0059_0004)
ISP$g6.1_escuela <- car::recode(ISP$g6.1_escuela, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                               levels= c("Si", "No"))

table(ISP$g6.1_escuela)

## q0059_1 - (g6.1_tcp)----
table(ISP$q0059_0005)
ISP$g6.1_tcp <- as.factor(ISP$q0059_0005)
ISP$g6.1_tcp <- car::recode(ISP$g6.1_tcp, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                               levels= c("Si", "No"))

table(ISP$g6.1_tcp)

## q0059_6 - (g6.1_ninguno)----
table(ISP$q0059_0006)
ISP$g6.1_ninguno <- as.factor(ISP$q0059_0006)
ISP$g6.1_ninguno <- car::recode(ISP$g6.1_ninguno, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                               levels= c("Si", "No"))

table(ISP$g6.1_ninguno)

## Despues G6.2----
## q0060_1 - (g6.2_abuelo)----
table(ISP$q0060_0001)
ISP$g6.2_abuelo <- as.factor(ISP$q0060_0001)
ISP$g6.2_abuelo <- car::recode(ISP$g6.2_abuelo, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                               levels= c("Si", "No"))

table(ISP$g6.2_abuelo)

## q0060_1 - (g6.2_otro)----
table(ISP$q0060_0002)
ISP$g6.2_otro <- as.factor(ISP$q0060_0002)
ISP$g6.2_otro <- car::recode(ISP$g6.2_otro, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                             levels= c("Si", "No"))

table(ISP$g6.2_otro)

## q0060_3 - (g6.2_guarderia)----
table(ISP$q0060_0003)
ISP$g6.2_guarderia <- as.factor(ISP$q0060_0003)
ISP$g6.2_guarderia <- car::recode(ISP$g6.2_guarderia, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                                  levels= c("Si", "No"))

table(ISP$g6.2_guarderia)

## q0060_4 - (g6.2_escuela)----
table(ISP$q0060_0004)
ISP$g6.2_escuela <- as.factor(ISP$q0060_0004)
ISP$g6.2_escuela <- car::recode(ISP$g6.2_escuela, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                                levels= c("Si", "No"))

table(ISP$g6.2_escuela)

## q0060_1 - (g6.2_tcp)----
table(ISP$q0060_0005)
ISP$g6.2_tcp <- as.factor(ISP$q0060_0005)
ISP$g6.2_tcp <- car::recode(ISP$g6.2_tcp, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                            levels= c("Si", "No"))

table(ISP$g6.2_tcp)


## q0060_6 - (g6.2_ninguno)----
table(ISP$q0060_0006)
ISP$g6.2_ninguno <- as.factor(ISP$q0060_0006)
ISP$g6.2_ninguno <- car::recode(ISP$g6.2_ninguno, recodes= c("1= 'Si';NA='No'"), as.factor = T,
                                levels= c("Si", "No"))

table(ISP$g6.2_ninguno)

##G7 (q0061) - limite trabajo (g7)------
## q0061_1 - (g7.1)----
table(ISP$q0061_0001)
ISP$g7.1 <- as.factor(ISP$q0061_0001)
ISP$g7.1 <- car::recode(ISP$g7.1, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))
table(ISP$g7.1)
## q0061_2 - (g7.2)----
table(ISP$q0061_0002)
ISP$g7.2 <- as.factor(ISP$q0061_0002)
ISP$g7.2 <- car::recode(ISP$g7.2, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))
## q0061_3 - (g7.3)----
table(ISP$q0061_0003)
ISP$g7.3 <- as.factor(ISP$q0061_0003)
ISP$g7.3 <- car::recode(ISP$g7.3, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))

## q0061_3 - (g7.4)----
table(ISP$q0061_0004)
ISP$g7.4 <- as.factor(ISP$q0061_0004)
ISP$g7.4 <- car::recode(ISP$g7.4, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))

## q0061_3 - (g7.5)----
table(ISP$q0061_0005)
ISP$g7.5 <- as.factor(ISP$q0061_0005)
ISP$g7.5 <- car::recode(ISP$g7.5, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))

table(ISP$g7.5)

##g8 (q0062) - VIOLENCIA DE GENERO (g8)------
## q0062_1 - (g8.1)----
table(ISP$q0062_0001)
ISP$g8.1 <- as.factor(ISP$q0062_0001)
ISP$g8.1 <- car::recode(ISP$g8.1, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))
table(ISP$g8.1)
## q0062_2 - (g8.2)----
table(ISP$q0062_0002)
ISP$g8.2 <- as.factor(ISP$q0062_0002)
ISP$g8.2 <- car::recode(ISP$g8.2, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))
## q0062_3 - (g8.3)----
table(ISP$q0062_0003)
ISP$g8.3 <- as.factor(ISP$q0062_0003)
ISP$g8.3 <- car::recode(ISP$g8.3, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))

## q0062_3 - (g8.4)----
table(ISP$q0062_0004)
ISP$g8.4 <- as.factor(ISP$q0062_0004)
ISP$g8.4 <- car::recode(ISP$g8.4, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))

## q0062_3 - (g8.5)----
table(ISP$q0062_0005)
ISP$g8.5 <- as.factor(ISP$q0062_0005)
ISP$g8.5 <- car::recode(ISP$g8.5, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))

table(ISP$g8.5)

## q0062_6 - (g8.6)----
table(ISP$q0062_0005)
ISP$g8.6 <- as.factor(ISP$q0062_0006)
ISP$g8.6 <- car::recode(ISP$g8.6, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))

table(ISP$g8.6)

## q0062_7 - (g8.7)----
table(ISP$q0062_0007)
ISP$g8.7 <- as.factor(ISP$q0062_0007)
ISP$g8.7 <- car::recode(ISP$g8.7, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))

table(ISP$g8.7)

## q0062_8 - (g8.8)----
table(ISP$q0062_0008)
ISP$g8.8 <- as.factor(ISP$q0062_0008)
ISP$g8.8 <- car::recode(ISP$g8.8, recodes= c("1='Siempre';2='La mayoría de las veces';3='Algunas veces';4='Solo unas pocas veces';5='Nunca';6='No aplica';NA=NA"), as.factor = T,
                        levels= c("Siempre","La mayoría de las veces","Algunas veces","Nunca", "No aplica"))

table(ISP$g8.8)


--------------
## H. Salud Laboral (ámbos modulos)
# H.1 (q0063) - Intensidad ---------
# h1.1 (q0063_0001) - rapidez ------- 
table(ISP$q0063_0001)
ISP$h1.1 <- as.factor(ISP$q0063_0001)
ISP$h1.1 <- car::recode(ISP$h1.1, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
levels(ISP$h1.1)
# h1.2 (q0063_0002) - acumulacion  ------- 
ISP$h1.2 <- as.factor(ISP$q0063_0002)
ISP$h1.2 <- car::recode(ISP$h1.2, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.3 (q0063_0003) - acumulacion  ------- 
ISP$h1.3 <- as.factor(ISP$q0063_0003)
ISP$h1.3 <- car::recode(ISP$h1.3, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.4 (q0063_0004) - acumulacion  ------- 
ISP$h1.4 <- as.factor(ISP$q0063_0004)
ISP$h1.4 <- car::recode(ISP$h1.4, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.5 (q0063_0005) - acumulacion  ------- 
ISP$h1.5 <- as.factor(ISP$q0063_0005)
ISP$h1.5 <- car::recode(ISP$h1.5, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.6 (q0063_0006) - acumulacion  ------- 
ISP$h1.6 <- as.factor(ISP$q0063_0006)
ISP$h1.6 <- car::recode(ISP$h1.6, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.7 (q0063_0007) - acumulacion  ------- 
ISP$h1.7 <- as.factor(ISP$q0063_0007)
ISP$h1.7 <- car::recode(ISP$h1.7, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))

# H.2 (q0064_0001 a q0064_0005) - Salud Mental  ---------
# h2.1 (q0064_0001) - nervios ------- 
table(ISP$q0064_0001)
ISP$h2.1 <- as.factor(ISP$q0064_0001)
ISP$h2.1 <- car::recode(ISP$h2.1, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
levels(ISP$h2.1)
# h2.2 (q0064_0002) - animo  ------- 
ISP$h2.2 <- as.factor(ISP$q0064_0002)
ISP$h2.2 <- car::recode(ISP$h2.2, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h2.3 (q0064_0003) - tranquilo   ------- 
ISP$h2.3 <- as.factor(ISP$q0064_0003)
ISP$h2.3 <- car::recode(ISP$h2.3, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h2.4 (q0064_0004) - triste  ------- 
ISP$h2.4 <- as.factor(ISP$q0064_0004)
ISP$h2.4 <- car::recode(ISP$h2.4, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h2.5 (q0064_0005) - feliz  ------- 
ISP$h2.5 <- as.factor(ISP$q0064_0005)
ISP$h2.5 <- car::recode(ISP$h2.5, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))

# H.3 (q0064_0006 a q0064_0008) - Salud Física  ---------
# h3.1 (q0063_0006) - dolor  ------- 
ISP$h3.1 <- as.factor(ISP$q0065_0001)
ISP$h3.1 <- car::recode(ISP$h3.1, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
table(ISP$h3.1)
# h3.2 (q0063_0007) - percepcion dolor  ------- 
ISP$h3.2 <- as.factor(ISP$q0065_0002)
ISP$h3.2 <- car::recode(ISP$h3.2, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h3.3 (q0063_0008) - palpitaciones  ------- 
ISP$h3.3 <- as.factor(ISP$q0065_0003)
ISP$h3.3 <- car::recode(ISP$h3.3, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))

table(ISP$h3.3)

## I. Tareas teletrabajbles (ámbos modulos)
# i.1 (q0066) - Tareas que cree se podrían teletrabajar ---------
# i1.1 (q0066_0001) - reuniones ------- 
table(ISP$q0066_0001)
ISP$i1.1 <- as.factor(ISP$q0066_0001)
ISP$i1.1 <- car::recode(ISP$i1.1, recodes= c("1='Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))
levels(ISP$i1.1)
table(ISP$i1.1)

# i1.2 (q0066_0002) - atencion  ------- 
ISP$i1.2 <- as.factor(ISP$q0066_0002)
ISP$i1.2 <- car::recode(ISP$i1.2, recodes= c("1='Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))

# i1.3 (q0066_0003) - informacion  ------- 
ISP$i1.3 <- as.factor(ISP$q0066_0003)
ISP$i1.3 <- car::recode(ISP$i1.3, recodes= c("1='Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))

# i1.4 (q0066_0004) - tareas simples  sin concentracion ------- 
ISP$i1.4 <- as.factor(ISP$q0066_0004)
ISP$i1.4 <- car::recode(ISP$i1.4, recodes= c("1='Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))


# i1.5 (q0066_0005) - tareas simples concentracion  ------- 
ISP$i1.5 <- as.factor(ISP$q0066_0005)
ISP$i1.5 <- car::recode(ISP$i1.5, recodes= c("1='Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))

# i1.6 (q0066_0006) - tareas complejas concentracion  ------- 
table(ISP$q0066_0006)
ISP$i1.6 <- as.factor(ISP$q0066_0006)
ISP$i1.6 <- car::recode(ISP$i1.6, recodes= c("1='Si';NA='No'"), as.factor = T,
                        levels= c("Si", "No"))
table(ISP$i1.6)

## i3 -------------

#i3.1 ----
table(ISP$q0067_0001)
ISP$i3.1 <- as.factor(ISP$q0067_0001)
ISP$i3.1 <- car::recode(ISP$i3.1, recodes = c("1='Muy en desacuerdo';2='En desacuerdo'; 3='Ni acuerdo ni desacuerdo'; 4= 'De acuerdo'; 5='Muy de acuerdo'"), as.factor = T,
                        levels =  c("Muy en desacuerdo", "En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo", "Muy de acuerdo"))

#i3.2 ----
table(ISP$q0067_0002)
ISP$i3.2 <- as.factor(ISP$q0067_0002)
ISP$i3.2 <- car::recode(ISP$i3.2, recodes = c("1='Muy en desacuerdo';2='En desacuerdo'; 3='Ni acuerdo ni desacuerdo'; 4= 'De acuerdo'; 5='Muy de acuerdo'"), as.factor = T,
                        levels =  c("Muy en desacuerdo", "En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo", "Muy de acuerdo"))

#i3.3 ----
table(ISP$q0067_0003)
ISP$i3.3 <- as.factor(ISP$q0067_0003)
ISP$i3.3 <- car::recode(ISP$i3.3, recodes = c("1='Muy en desacuerdo';2='En desacuerdo'; 3='Ni acuerdo ni desacuerdo'; 4= 'De acuerdo'; 5='Muy de acuerdo'"), as.factor = T,
                        levels =  c("Muy en desacuerdo", "En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo", "Muy de acuerdo"))

#i3.4 ----
table(ISP$q0067_0004)
ISP$i3.4 <- as.factor(ISP$q0067_0004)
ISP$i3.4 <- car::recode(ISP$i3.4, recodes = c("1='Muy en desacuerdo';2='En desacuerdo'; 3='Ni acuerdo ni desacuerdo'; 4= 'De acuerdo'; 5='Muy de acuerdo'"), as.factor = T,
                        levels =  c("Muy en desacuerdo", "En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo", "Muy de acuerdo"))

#i3.5 ----
table(ISP$q0067_0005)
ISP$i3.5 <- as.factor(ISP$q0067_0005)
ISP$i3.5 <- car::recode(ISP$i3.5, recodes = c("1='Muy en desacuerdo';2='En desacuerdo'; 3='Ni acuerdo ni desacuerdo'; 4= 'De acuerdo'; 5='Muy de acuerdo'"), as.factor = T,
                        levels =  c("Muy en desacuerdo", "En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo", "Muy de acuerdo"))

#i3.6 ----
table(ISP$q0067_0006)
ISP$i3.6 <- as.factor(ISP$q0067_0006)
ISP$i3.6 <- car::recode(ISP$i3.6, recodes = c("1='Muy en desacuerdo';2='En desacuerdo'; 3='Ni acuerdo ni desacuerdo'; 4= 'De acuerdo'; 5='Muy de acuerdo'"), as.factor = T,
                        levels =  c("Muy en desacuerdo", "En desacuerdo", "Ni acuerdo ni desacuerdo", "De acuerdo", "Muy de acuerdo"))

table(ISP$i3.6)

# i2 (q0084) - Nuevas modalidades preferidas  ---------
table(ISP$q0068)
ISP$i2 <- as.factor(ISP$q0068)
ISP$i2 <- car::recode(ISP$i2, recodes= c("1='Teletrabajo total';2='Teletrabajo parcial';3='Trabajo normal';NA=NA"), as.factor = T,
                      levels= c("Teletrabajo total", "Teletrabajo parcial", "Trabajo normal"))

# Z. Correo de contacto
ISP$mail <- ISP$q0069
write.csv(ISP$mail, file = "informe_resultados/data/isp-email.csv")

## 4. Seleccionar variables
ISP_proc <- ISP %>% select(- starts_with(c("q","p")))
save(ISP_proc, file = "informe_resultados/data/ISP_proc.RData")
write.csv(ISP_proc, file = "informe_resultados/data/ISP_proc.csv")
