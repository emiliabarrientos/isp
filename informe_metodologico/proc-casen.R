# 0. Procesamiento de datos : CASEN
# Valentina Andrade
# 4 de junio

# 1. Cargar librerías
pacman::p_load(haven, dplyr, survey, srvyr)

# 2. Cargar datos

casen <- read_spss("data/Casen 2017.sav")

#3. Seleccion
names(casen)
casen1 <- casen %>%  select(starts_with("o"), edad, sexo, s12,
                           oficio4, rama1, rama4, expc, comuna, region, expr)
# 4. Filtros relevantes
# Mayor a 18 años
# Trabajadores publicos  (o15)
casen1 <- casen1 %>%  filter(edad > 18 & o15 == 3)

# 5. Edad ----
# Crear objeto suvrye
casen1<-casen1 %>% mutate(edad_cat= case_when(edad<40 ~ 1,
                                        edad>49 & edad<60 ~ 2,
                                        edad>59 & edad<80 ~ 3,                                            TRUE ~ NA_real_))
casen1$edad_cat<-as.factor(casen1$edad_cat)

# 6. Sexo
casen1$sexo<-as.factor(casen1$sexo)

# 7. Region
casen1$region<-as.factor(casen1$region)

#8. Comuna
casen1$comuna<-as.factor(casen1$comuna)

#9.  Estamento ----------
casen1$oficio1<-as.factor(casen1$oficio1)
casen1$oficio1 <- car::recode(casen1$oficio1, recodes = c('999 = NA'))

#10. Rama-------------
table(casen1$rama1)
levels(isp$b1)
# "Administracion Central" 
# "Educacion Municipal"    
# "Educacion Universitaria"
# "FFAA y Orden"           
# "Judicial"               
# "Municipal"              
#  "Obras Sanitarias"       
#  "Otros"                  
#  "Salud Hospitalaria"     
#  "Salud Municipal"        
#  "SII"
casen1$rama1<-as.factor(casen1$rama1)
casen1 <- casen1 %>% mutate(rama = case_when(rama1 == 13 ~ "Educacion (Municipal y Universitaria)",
                                             rama1 == 12 ~  "Administracion publica y defensa",
                                             rama1 ==  14 ~ "Salud (Municipal y Hospitalaria)",
                                             rama1 ==  5 ~ "Obras sanitarias",
                                             rama1 == 7 ~ NA_character_,
                                             rama1 == 8 ~ NA_character_,
                                             rama1 == 10 ~ NA_character_,
                                             rama1 == 11 ~ NA_character_,
                                             rama1 == 16 ~ NA_character_,
                                             TRUE  ~ "Otro"))

table(casen1$rama)
# 11.  Jornada ---------------------
table(isp$b7)
table(casen1$o18)
casen1$o18 <- as.factor(casen1$o18)
casen1$jornada <- car::recode(casen1$o18, recodes = c("1='Completa';2='Parcial';3:9=NA"), as.factor = T,
                      levels = c('Parcial', 'Completa'))

#12. Tipo de contrato----------------
table(isp$b6)
table(casen1$o14) # 1 es honorario y 3 No, 2 Y 9 NA
table(casen1$o16) # 1 indefinido y 2 es fijo 9NA
table(casen1$o20) # 2 y 3 subcontratado
table(casen1$o16, casen1$o14)
casen1$o16 <- as.factor(casen1$o16)
casen1$contrato <- car::recode(casen1$o16, recodes = c("1='Planta';2='Contrata';9=NA"), as.factor = T,
                               levels = c('Planta', 'Contrata'))

#13. Sindicato-------------
casen1 <-  casen1 %>%  mutate(sindicato = case_when(o24a == 1 ~ 1 , o24b == 1 ~  1, o24b == 2 ~  2, TRUE~NA_real_ ))
casen1$sindicato <- as.factor(casen1$sindicato)
#14.  Sistema previsional--------------------
casen1$s12 <- as.factor(casen1$s12)
casen1$salud <- car::recode(casen1$s12, recodes = c("1:5='FONASA';6='CAPREDENA/DIPRECA';7='ISAPRE';8:hi=NA "), as.factor = T,
                               levels = c('FONASA', 'ISAPRE', 'CAPREDENA/DIPRECA'))

# 14.  Expandir ########
casen_regional <- casen1 %>% as_survey_design(ids = 1, weights = expr)
casen_comunal <- casen1 %>% as_survey_design(ids = 1, weights = expc)
options(survey.lonely.psu = "certainty" )


## Comuna ---------
comuna <-casen_comunal %>% group_by(comuna) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                     total = survey_total(vartype = "ci",na.rm = TRUE)) %>% arrange(desc(total))
## Región --------
region<-casen_regional %>% group_by(region) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                      total = survey_total(vartype = "ci",na.rm = TRUE))
## Sexo ---------
sexo <-casen_regional %>% group_by(sexo) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                    total = survey_total(vartype = "ci",na.rm = TRUE))
## Edad -----------
edad <-casen_regional %>% group_by(edad_cat) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                        total = survey_total(vartype = "ci",na.rm = TRUE))
## Rama --------
rama <-casen_regional %>% group_by(rama) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                          total = survey_total(vartype = "ci",na.rm = TRUE))
## Ocupación -----------
ocup <-casen_regional %>% group_by(oficio1) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                           total = survey_total(vartype = "ci",na.rm = TRUE))
## Jornada -----------
jor <-casen_regional %>% group_by(jornada) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                          total = survey_total(vartype = "ci",na.rm = TRUE))
## Tipo de contrato
contrato <-casen_regional %>% group_by(contrato) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                          total = survey_total(vartype = "ci",na.rm = TRUE))
## Sindicato
sindicato <-casen_regional %>% group_by(sindicato) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                          total = survey_total(vartype = "ci",na.rm = TRUE))
## Salud
salud <-casen_regional %>% group_by(salud) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                          total = survey_total(vartype = "ci",na.rm = TRUE))


# X. Guardar----------
save(region,comuna,sexo,edad, ocup, rama, jor, contrato, sindicato, salud, file = "data/casen-expandida.RData")


