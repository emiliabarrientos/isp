#######################
#Catalina Miranda
##Informe descriptivo ISP
## 21 de septiembre 2020
#######################

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
library(readxl)

#Caracterización de los y las encuestadas

data_isp_rc <- readRDS("C:/Users/catac/Dropbox/Cata/Github/isp/informe_resultados/data/data_isp.rds")

#genero en comparacion a OIT
data_isp_rc %>% count (a1_sexo)

OIT_pod <- read_excel("C:/Users/catac/Dropbox/Cata/Pegas/Nodo XXI/OIT_pod.xlsx")

mergesvy <- data_isp_rc %>%
  as_survey_design(
    weights = pond1)

table_freq_01 <- mergesvy %>% 
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df=data.frame(table_freq_01)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(género=a1_sexo) %>% 
  mutate(Encuesta="ISP") %>% 
  select(género, proportion, Encuesta, p)

df_1=data.frame(OIT_pod)
df_compare <- rbind(df, df_1)

df_compare %>% 
  ggplot(aes(x=género, y=proportion, fill=Encuesta))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 5, angle =90)+
  facet_grid(~Encuesta, scales = "free_x")+
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent)+
  scale_fill_manual(values = c("#d40c04","#d3d3d3"))+
  labs(x = "Género", y = "%")

#Pais
mergesvy <- data_isp_rc %>%
  as_survey_design(
    weights = pond1)

table_freq_01 <- mergesvy %>% 
  dplyr::group_by(a0_pais) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df=data.frame(table_freq_01)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(país=a0_pais) 

df %>% 
  ggplot(aes(x=país, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 5, angle =90)+
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent)+
  labs(x = "País", y = "%")

T1.1<-freq(data_isp_rc$a0_pais, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 0)

#Educacion
data_isp_rc %>% count(a3_educ)

mergesvy <- data_isp_rc %>%
  as_survey_design(
    weights = pond1)

table_freq_01 <- mergesvy %>% 
  dplyr::group_by(a3_educ) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df=data.frame(table_freq_01)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) 

  df %>% 
  ggplot(aes(x=a3_educ, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 5, angle =90)+
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent)+
  labs(x = "Nivel educativo", y = "%")

#Edad
library(dplyr)
data_isp_rc %>% count(edad_cat)

data_isp_rc = data_isp_rc %>%
  mutate(edad_cat_recode=case_when(
    edad_cat == "Adultos" ~ "Adultos",   #Factor
    edad_cat == "Jóvenes" ~ "Jóvenes", 
    TRUE ~ as.character(edad_cat) 
  )) %>% 
  filter(!is.na(edad_cat_recode))


mergesvy <- data_isp_rc %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>% 
  dplyr::group_by(edad_cat_recode) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df_1=data.frame(table_freq_02)
#summary(factor(df$proportion))

df_1= df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) 

df_1 %>% 
  ggplot(aes(x=edad_cat_recode, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 5, angle =90)+
  scale_y_continuous(limits = c(0, 0.9), labels = scales::percent)+
  labs(x = "Edad", y = "%")

#N de personas en el hogar
data_isp_c %>% count (a6.1)
data_isp_c %>% count (a6.1, a6.1_rec)
data_isp_c %>% count (a6.1_rec)

#Tabla con promedio

#Pais
T4.6w3<-data_isp_rc %>% group_by(modalidad) %>% summarize(C=weighted.mean(a6.1,pond1, na.rm = TRUE))

T4.6q_3<-data_isp_rc %>% group_by(a5_etnia) %>% summarize(C=weighted.mean(a6.1,pond1, na.rm = TRUE))

#Edad
T4.63<-data_isp_rc %>% group_by(edad_cat) %>% summarize(C=weighted.mean(a6.1,pond1, na.rm = TRUE))

#Modalidad
T4.6aw3<-data_isp_rc %>% group_by(a1_sexo) %>% summarize(C=weighted.mean(a6.1,pond1, na.rm = TRUE))

#Pais
T4.s6aw3<-data_isp_rc %>% group_by(a0_pais) %>% summarize(C=weighted.mean(a6.1,pond1, na.rm = TRUE))


T1.e6.8<-freq(dta$e6.3_desc, weights = dta$pond1, useNA = c("no"),  round.digits = 2)

data_isp_c = data_isp_c %>%
  mutate(a6.1_rec=case_when(
    a6.1 == 1  ~ "1",   #Factor
    a6.1 == 2  ~ "2", 
    a6.1 == 3  ~ "3", 
    a6.1 == 4  ~ "4", 
    a6.1 == 5  ~ "5", 
    a6.1 == 6  ~ "6", 
    a6.1 == 7  ~ "7 o más", 
    a6.1 == 8  ~ "7 o más", 
    a6.1 == 9  ~ "7 o más", 
    a6.1 == 10 ~ "7 o más", 
    a6.1 == 11 ~ "7 o más",
    a6.1 == 12 ~ "7 o más",
    a6.1 == 16 ~ "7 o más",
  ))

data_isp_rc = data_isp_rc %>%
  mutate(a6.1=as.factor(a6.1))

data_isp_rc %>% count(a6.1)

mergesvy <- data_isp_rc %>%
  as_survey_design(
    weights = pond1)

table_freq_03 <- mergesvy %>% 
  dplyr::group_by(a6.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df_1=data.frame(table_freq_03)
#summary(factor(df$proportion))

df_1= df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) 

df_1 %>% 
  ggplot(aes(x=a6.1, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 5, angle =90)+
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent)+
  labs(x = "Número de personas en el hogar", y = "%")

#recursos economicos

data_isp_rc = data_isp_rc %>% 
  filter(etnia == "Sí, pertenezco") 
data_isp_rc %>% count(etnia)
pond1 <- data_isp_rc %>% pull(pond1)

T1.8<-ctable(data_isp_rc$edad_cat, data_isp_rc$a6.2, weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T1.9<-ctable(data_isp_rc$a0_pais, data_isp_rc$a6.2, weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)


#COn quién comparte

T1.8<-ctable(data_isp_rc$edad_cat, data_isp_rc$a6.2.filtro, weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T1.9<-ctable(data_isp_rc$a0_pais, data_isp_rc$a6.2.filtro, weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T1.10<-ctable(data_isp_rc$a5_etnia, data_isp_rc$a6.2.filtro, weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T1.11<-ctable(data_isp_rc$modalidad, data_isp_rc$a6.2.filtro, weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)


data_isp_rc = data_isp_rc %>%
 filter(a1_sexo!="Otro")

data_isp_rc %>% count(a1_sexo)

T1a.7<-ctable(data_isp_rc$a1_sexo, data_isp_rc$a6.2.filtro, weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)


mergesvy <- data_isp_rc %>%
  as_survey_design(
    weights = pond1)

table_freq <- mergesvy %>%
  dplyr::group_by(edad_cat_recode,a6.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df=data.frame(table_freq, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))

df %>% 
  ggplot(aes(x=a6.2, y=proportion))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 5, angle =90)+
  scale_y_continuous(limits = c(0, 0.6), labels = scales::percent)+
  labs(x = "Número de personas en el hogar", y = "%")

#TABLAS
T2.1<-freq(data_isp_rc$a6.2, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 2)
T2.5<-freq(data_isp_rc$a6.2.filtro, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 2)
T3.1<-freq(data_isp_rc$a6.1, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 2)
T2.2<-freq(data_isp_rc$edad_cat_recode, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 2)
T2.3<-freq(data_isp_rc$a5_etnia, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 2)
T2.4<-freq(data_isp_rc$a3_educ, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 2)

data_isp_rc %>% glimpse ()
data_isp_rc %>% count(edad_cat_recode) %>% View


#modalidad

mergesvy <- data_isp_rc %>%
  as_survey_design(
    weights = pond1)

table_freq <- mergesvy %>%
  dplyr::group_by(a0_pais, modalidad) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df=data.frame(table_freq, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))

df %>% 
  ggplot(aes(x=modalidad, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  facet_wrap(~a0_pais)+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 5, angle =90)+
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent)+
  labs(x = "Modalidad de trabajo", y = "%")
  #theme(panel.background = element_rect(fill = "red"))

##///##
mergesvy <- data_isp_rc %>%
  as_survey_design(
    weights = pond1)

table_freq <- mergesvy %>%
  dplyr::group_by(a5_etnia, modalidad) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df=data.frame(table_freq, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))

df %>% 
  ggplot(aes(x=modalidad, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  facet_wrap(~a5_etnia)+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 5, angle =90)+
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent)+
  labs(x = "Modalidad de trabajo", y = "%")
#theme(panel.background = element_rect(fill = "red"))

## genero y modalidad
table_freq <- mergesvy %>%
  filter (a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo, modalidad) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df=data.frame(table_freq, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))

df %>% 
  ggplot(aes(x=modalidad, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  facet_wrap(~a1_sexo)+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 5, angle =90)+
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent)+
  labs(x = "Modalidad de trabajo", y = "%")

T2.5<-freq(data_isp_rc$modalidad, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 2)

# POBLACION DE RIESGO

#Genero
T4e3<-data_isp_rc %>% group_by(a1_sexo) %>% summarize(C=weighted.mean(a6.3,pond1, na.rm = TRUE))

#Etnia
T4.6q_3<-data_isp_rc %>% group_by(a5_etnia) %>% summarize(C=weighted.mean(a6.3,pond1, na.rm = TRUE))

#Edad
T4.63<-data_isp_rc %>% group_by(edad_cat) %>% summarize(C=weighted.mean(a6.3,pond1, na.rm = TRUE))

#Pais
T4.6waa3<-data_isp_rc %>% group_by(a0_pais) %>% summarize(C=weighted.mean(a6.3,pond1, na.rm = TRUE))

#Modalidad
T4.6w3<-data_isp_rc %>% group_by(modalidad) %>% summarize(C=weighted.mean(a6.3,pond1, na.rm = TRUE))

#PREESCOLAR
#Genero
T5e3<-data_isp_rc %>% group_by(a1_sexo) %>% summarize(C=weighted.mean(a6.4,pond1, na.rm = TRUE))

#Etnia
T5.6q_3<-data_isp_rc %>% group_by(a5_etnia) %>% summarize(C=weighted.mean(a6.4,pond1, na.rm = TRUE))

#Edad
T5.63<-data_isp_rc %>% group_by(edad_cat) %>% summarize(C=weighted.mean(a6.4,pond1, na.rm = TRUE))

#Pais
T5.6waa3<-data_isp_rc %>% group_by(a0_pais) %>% summarize(C=weighted.mean(a6.4,pond1, na.rm = TRUE))

#Modalidad
T5.6w3<-data_isp_rc %>% group_by(modalidad) %>% summarize(C=weighted.mean(a6.4,pond1, na.rm = TRUE))


#Basica
#Genero
aT5e3<-data_isp_rc %>% group_by(a1_sexo) %>% summarize(C=weighted.mean(a6.5,pond1, na.rm = TRUE))

#Etnia
aT5.6q_3<-data_isp_rc %>% group_by(a5_etnia) %>% summarize(C=weighted.mean(a6.5,pond1, na.rm = TRUE))

#Edad
aT5.63<-data_isp_rc %>% group_by(edad_cat) %>% summarize(C=weighted.mean(a6.5,pond1, na.rm = TRUE))

#Pais
aT5.6waa3<-data_isp_rc %>% group_by(a0_pais) %>% summarize(C=weighted.mean(a6.5,pond1, na.rm = TRUE))

#Modalidad
aT5.6w3<-data_isp_rc %>% group_by(modalidad) %>% summarize(C=weighted.mean(a6.5,pond1, na.rm = TRUE))


data_isp_rc = data_isp_rc %>%
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
  )) %>% 
  filter(!is.na(a6_riesgo_recode))

data_isp_rc %>% count(a6.3, a6_riesgo_recode)
data_isp %>% count(a6_riesgo_recode)

data_isp_rc = data_isp_rc %>%
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
  )) %>% 
  filter(!is.na(a6_preesc_recode))

data_isp_rc %>% count(a6.4, a6_preesc_recode)
data_isp_rc %>% count(a6_preesc_recode)

data_isp_rc = data_isp_rc %>%
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
  ))  %>% 
  filter(!is.na(a6_basica_recode))

data_isp_rc %>% count(a6.5, a6_basica_recode)
data_isp_rc %>% count(a6_basica_recode)

data_isp_rc %>% glimpse ()

T1.2<-ctable(isp$a1_sexo, isp$d5_mod, weights = isp$pond2, useNA = c("no"), round.digits = 1)

T1.1<-freq(data_isp_rc$a6_basica_recode, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 2)
T1.2<-freq(data_isp_rc$a6_preesc_recode, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 2)
T1.3<-freq(data_isp_rc$a6_riesgo_recode, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 2)



T1.3<-freq(data_isp_rc$etnia, weights = data_isp_rc$pond1, useNA = c("no"),  round.digits = 2)


#Grafico N niños, adolescentes y adultos
data_isp_c %>% count(a6.3)
data_isp_c %>% count(a6.4)
data_isp_rc %>% count(a6.5)

#mayores
#data_isp_rc = data_isp_rc %>% 
#  mutate(a6.3=as.factor(a6.3)) %>% 
#  mutate(a6.4=as.factor(a6.4)) %>% 
#  mutate(a6.5=as.factor(a6.5)) 

mergesvy <- data_isp_rc %>%
  as_survey_design(
    weights = pond1)

table_freq_032 <- mergesvy %>%
  dplyr::group_by(a6_riesgo_recode) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1=data.frame(table_freq_032, digits=2)
#summary(factor(df$proportion))

df_1 = df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=a6_riesgo_recode) %>% 
  mutate(variable="a6_riesgo_recode") 

##preescolar 
table_freq_3 <- mergesvy %>%
  dplyr::group_by(a6_preesc_recode) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2 = df_2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=a6_preesc_recode) %>% 
  mutate(variable="a6_preesc_recode")

#Basica
table_freq_03 <- mergesvy %>%
  dplyr::group_by(a6_basica_recode) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4=data.frame(table_freq_03, digits=2)
#summary(factor(df$proportion))

df_4 = df_4 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=a6_basica_recode) %>% 
  mutate(variable="a6_basica_recode")

df_final_post <- rbind(df_1, df_2, df_4)

df_final_post$variable = factor(df_final_post$variable, labels=c("niños/niñas en educación básica", "niños/niñas en educación preescolar", "personas en población de riesgo"))

order_arg = ggplot(df_final_post, aes(x=g,y=proportion)) +
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.3), vjust=-0.8, size = 5)+
  facet_wrap(~variable, scales = "free_x")+
  labs(x = "Respuesta", y = "%") +
  scale_y_continuous(limits = c(0, 0.9), labels = scales::percent)

##etnia

data_isp_rc = data_isp_rc %>%
  mutate(etnia=case_when(
    a5_etnia == "Si"  ~ "Sí, pertenezco",   #Factor
    a5_etnia == "No"  ~ "No pertenezco"
  ))


mergesvy <- data_isp_rc %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>% 
  dplyr::group_by(etnia) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df_1=data.frame(table_freq_02)
#summary(factor(df$proportion))

df_1= df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) 

df_1 %>% 
  ggplot(aes(x=etnia, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 5, angle =90)+
  scale_y_continuous(limits = c(0, 0.9), labels = scales::percent)+
  labs(x = "Etnia", y = "%")


