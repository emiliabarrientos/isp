
#######################
#Catalina Miranda
##Informe descriptivo ISP
## 29 de septiembre 2020


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
data_isp <- readRDS("C:/Users/catac/Dropbox/Cata/Github/isp/informe_resultados/data/data_isp.rds")

##### RECODIFICACIONES#######

data_isp %>% count(d5_mod) 
data_isp %>% count(modalidad) 

#MODALIDAD
data_isp = data_isp %>%
  mutate(modalidad=case_when(
    d5_mod == "Teletrabajo total" ~ "Teletrabajo",   #Factor
    d5_mod == "Teletrabajo parcial" ~ "Mixta", 
    d5_mod == "Trabajo normal" ~ "Presencial",
    TRUE ~ as.character(d5_mod) 
  )) %>% 
  filter(!is.na(d5_mod))

##Edad
data_isp %>% count(edad_cat)
#edad_cat     n
#<chr>    <int>
#1 Adultos   2342
#2 Jóvenes    498
#3 NA          72

data_isp_1 = data_isp_1 %>%
  mutate(edad_cat_recode=case_when(
    edad_cat == "Adultos" ~ "Adultos",   #Factor
    edad_cat == "Jóvenes" ~ "Jóvenes", 
    TRUE ~ as.character(edad_cat) 
  )) %>% 
  filter(!is.na(edad_cat_recode))

data_isp %>% count(edad_cat, edad_cat_recode)

#Numero de niños y adultos en el hogar
data_isp %>% count(a6.3)
data_isp %>% glimpse()

data_isp = data_isp %>%
  mutate(a6_riesgo_recode=case_when(
    a6.3 == 0  ~ "No",   #Factor
    a6.3 == 1  ~ "Sí", 
    a6.3 == 2  ~ "Sí", 
    a6.3 == 3  ~ "Sí", 
    a6.3 == 4  ~ "Sí", 
    a6.3 == 5  ~ "Sí", 
    a6.3 == 6  ~ "Sí", 
    a6.3 == 7  ~ "Sí", 
    a6.3 == 10 ~ "Sí", 
    a6.3 == 11 ~ "Sí", 
    a6.3 == 15 ~ "Sí",
  ))

data_isp %>% count(a6.4, a6_preesc_recode)
# a6.4     n
#<dbl>   <int>
#1   0  2009
#2   1   340
#3   2    51
#4   3     7
#5   4     3
#6   5     2
#7   6     2
#8   8     1
#9  14     1

data_isp = data_isp %>%
  mutate(a6_preesc_recode=case_when(
    a6.4 == 0  ~ "No",   #Factor
    a6.4 == 1  ~ "Sí", 
    a6.4 == 2  ~ "Sí", 
    a6.4 == 3  ~ "Sí", 
    a6.4 == 4  ~ "Sí", 
    a6.4 == 5  ~ "Sí", 
    a6.4 == 6  ~ "Sí", 
    a6.4 == 8  ~ "Sí", 
    a6.4 == 14 ~ "Sí",
  ))


data_isp = data_isp %>%
  mutate(a6_basica_recode=case_when(
    a6.5 == 0  ~ "No",   #Factor
    a6.5 == 1  ~ "Sí", 
    a6.5 == 2  ~ "Sí", 
    a6.5 == 3  ~ "Sí", 
    a6.5 == 4  ~ "Sí", 
    a6.5 == 5  ~ "Sí", 
    a6.5 == 6  ~ "Sí", 
    a6.5 == 9  ~ "Sí", 
    a6.5 == 10 ~ "Sí", 
    a6.5 == 11 ~ "Sí", 
    a6.5 == 14 ~ "Sí",
  ))

