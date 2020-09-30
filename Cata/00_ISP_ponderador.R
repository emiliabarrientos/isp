
#######################
#Catalina Miranda
##Informe descriptivo ISP
## 11 de septiembre 2020

#Libreria
library(dplyr)
library(car)
library(summarytools)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(writexl)
library(tibble)
library(readr)
library(survey)
library(srvyr)

#Base
load("C:/Users/catac/Dropbox/Cata/Github/isp/informe_resultados/data/ISP_privisoria.RData")

ISP_privisoria %>% glimpse() 
ISP_privisoria %>% count(a1_sexo)
# a1_sexo       n
#   <fct>     <int>
#1 Femenino   1883
#2 Masculino  1019
#3 Otro         10

ISP_privisoria %>% count(a0_pais)
         #1 Argentina    349
         #2 Brasil       561
         #3 Colombia     319
         #4 Costa Rica   426
         #5 Ecuador      338
         #6 México       653
         #7 Perú         266
ISP_privisoria %>% count(a6.2)
ISP_privisoria %>% count(b1)


ISP_privisoria = ISP_privisoria %>%
  mutate(pond1=case_when(
    a0_pais== "Argentina"  & a1_sexo=="Masculino" ~ 1.50101, 
    a0_pais== "Argentina"  & a1_sexo=="Femenino"  ~ 0.85737,
    a0_pais== "Argentina"  & a1_sexo=="Otro"      ~ 1,
    a0_pais== "Brasil"     & a1_sexo=="Masculino" ~ 1.11194, 
    a0_pais== "Brasil"     & a1_sexo=="Femenino"  ~ 0.96000,
    a0_pais== "Brasil"     & a1_sexo=="Otro"      ~ 1,
    a0_pais== "Colombia"   & a1_sexo=="Masculino" ~ 0.80994,
    a0_pais== "Colombia"   & a1_sexo=="Femenino"  ~ 1.15833,
    a0_pais== "Colombia"     & a1_sexo=="Otro"    ~ 1,
    a0_pais== "Costa Rica" & a1_sexo=="Masculino" ~ 0.82075,
    a0_pais== "Costa Rica" & a1_sexo=="Femenino"  ~ 1.16647,
    a0_pais== "Costa Rica" & a1_sexo=="Otro"      ~ 1,
    a0_pais== "Ecuador"    & a1_sexo=="Masculino" ~ 1.03989,
    a0_pais== "Ecuador"    & a1_sexo=="Femenino"  ~ 0.97538,
    a0_pais== "Ecuador"    & a1_sexo=="Otro"      ~ 1,
    a0_pais== "México"     & a1_sexo=="Masculino" ~ 1.15778,
    a0_pais== "México"     & a1_sexo=="Femenino"  ~ 0.92031,
    a0_pais== "México"     & a1_sexo=="Otro"      ~ 1,
    a0_pais== "Perú"       & a1_sexo=="Masculino" ~ 1.06311,
    a0_pais== "Perú"       & a1_sexo=="Femenino"  ~ 0.96012,
    a0_pais== "Perú"       & a1_sexo=="Otro"      ~ 1,
    TRUE ~ -6
  ))
ISP_privisoria %>% count(pond1)

pond1_1 <- ISP_privisoria %>% pull(pond1)


#Creacion de ponderadores ft OIT
ISP_privisoria = ISP_privisoria %>%
  mutate(pond2=case_when(
    a0_pais== "Argentina"  & a1_sexo=="Masculino" ~ 1.54487526, 
    a0_pais== "Argentina"  & a1_sexo=="Femenino"  ~ 0.84423207,
    a0_pais== "Argentina"  & a1_sexo=="Otro"      ~ 1,
    a0_pais== "Brasil"     & a1_sexo=="Masculino" ~ 1.19319122, 
    a0_pais== "Brasil"     & a1_sexo=="Femenino"  ~ 0.92918362,
    a0_pais== "Brasil"     & a1_sexo=="Otro"      ~ 1,
    a0_pais== "Colombia"   & a1_sexo=="Masculino" ~ 0.88859946,
    a0_pais== "Colombia"   & a1_sexo=="Femenino"  ~ 1.09966339,
    a0_pais== "Colombia"     & a1_sexo=="Otro"    ~ 1,
    a0_pais== "Costa Rica" & a1_sexo=="Masculino" ~ 0.87330556,
    a0_pais== "Costa Rica" & a1_sexo=="Femenino"  ~ 1.12151527,
    a0_pais== "Costa Rica" & a1_sexo=="Otro"      ~ 1,
    a0_pais== "Ecuador"    & a1_sexo=="Masculino" ~ 1.10548431,
    a0_pais== "Ecuador"    & a1_sexo=="Femenino"  ~ 0.93489246,
    a0_pais== "Ecuador"    & a1_sexo=="Otro"      ~ 1,
    a0_pais== "México"     & a1_sexo=="Masculino" ~ 1.21575347,
    a0_pais== "México"     & a1_sexo=="Femenino"  ~ 0.89017946,
    a0_pais== "México"     & a1_sexo=="Otro"      ~ 1,
    a0_pais== "Perú"       & a1_sexo=="Masculino" ~ 1.13761845,
    a0_pais== "Perú"       & a1_sexo=="Femenino"  ~ 0.91303859,
    a0_pais== "Perú"       & a1_sexo=="Otro"      ~ 1,
    TRUE ~ -6
  ))
ISP_privisoria %>% count(pond2)

pond2_1 <- ISP_privisoria %>% pull(pond2)

ISP_privisoria = ISP_privisoria %>%
  mutate(modalidad=case_when(
    d5_mod == "Teletrabajo total" ~ "Teletrabajo",   #Factor
    d5_mod == "Teletrabajo parcial" ~ "Mixta", 
    d5_mod == "Trabajo normal" ~ "Presencial",
    TRUE ~ as.character(d5_mod) 
  )) %>% 
  filter(!is.na(d5_mod))



saveRDS(ISP_privisoria,"C:/Users/catac/Dropbox/Cata/Github/isp/informe_resultados/data/data_isp.rds")

data_isp %>% glimpse()
ISP_privisoria %>% glimpse()
