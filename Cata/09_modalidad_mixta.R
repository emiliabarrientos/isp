#######################
#Catalina Miranda
##Informe descriptivo ISP
## 02 de octubre 2020

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
data_isp_1 <- readRDS("C:/Users/catac/Dropbox/Cata/Github/isp/informe_resultados/data/data_isp.rds")%>% 
  filter(!is.na(d4.1)) %>% 
  filter(!is.na(d4.2)) %>% 
  filter(!is.na(d4.3)) %>% 
  filter(!is.na(d4.4)) %>% 
  filter(!is.na(d4.5)) %>% 
  filter(!is.na(d4.6)) %>% 
  filter(!is.na(d4.7)) %>% 
  filter(!is.na(edad_cat))


mergesvy <- data_isp_1 %>%
  as_survey_design(
    weights = pond1)
#D4.1
table_freq_02_n <- mergesvy %>%
  dplyr::group_by(d4.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_n=data.frame(table_freq_02_n, digits=2)
#summary(factor(df$proportion))

df_n= df_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.1, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.1) %>% 
  mutate(variable="d4.1")

#D4.2
table_freq_03_n <- mergesvy %>%
  dplyr::group_by(d4.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1_n=data.frame(table_freq_03_n, digits=2)
#summary(factor(df$proportion))

df_1_n = df_1_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.2, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>%  
  rename(g=d4.2) %>% 
  mutate(variable="d4.2")

#D4.3
table_freq_04_n <- mergesvy %>%
  dplyr::group_by(d4.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2_n=data.frame(table_freq_04_n, digits=2)
#summary(factor(df$proportion))

df_2_n = df_2_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.3, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.3) %>% 
  mutate(variable="d4.3")

#D4.4
table_freq_05_n <- mergesvy %>%
  dplyr::group_by(d4.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_3_n=data.frame(table_freq_05_n, digits=2)
#summary(factor(df$proportion))

df_3_n = df_3_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.4, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.4) %>% 
  mutate(variable="d4.4")

#D4.5
table_freq_06_n <- mergesvy %>%
  dplyr::group_by(d4.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_n=data.frame(table_freq_06_n, digits=2)
#summary(factor(df$proportion))

df_4_n = df_4_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.5, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.5) %>% 
  mutate(variable="d4.5")

#D4.6
table_freq_07_n <- mergesvy %>%
  dplyr::group_by(d4.6) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5_n=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df_5_n = df_5_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.6, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.6) %>% 
  mutate(variable="d4.6")
#D4.7
table_freq_00_n <- mergesvy %>%
  dplyr::group_by(d4.7) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7_n = df_7_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.7, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.7) %>% 
  mutate(variable="d4.7")

df_final <- rbind(df_n, df_1_n, df_2_n, df_3_n, df_4_n, df_5_n, df_7_n)

df_final$variable = factor(df_final$variable, labels=c("Velocidad al trabajar", "Trabajo asignado", "Descanso", "Trabajo fuera horario", "Trabajo finde","Salario mensual", "Derechos laborales"))


order_arg=ggplot(df_final, aes(label, proportion)) +
  geom_text(aes(label = p), position = position_dodge(0.3), vjust=-0.1, size = 5)+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  facet_wrap(~variable)+
  theme(axis.text.x = element_text(angle =45, hjust = 1, size=14)) +
  labs(x = "Items intensidad laboral", y = "%") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

#etnia

data_isp_1 = data_isp_1 %>%
  mutate(etnia=case_when(
    a5_etnia == "Si"  ~ "Sí, pertenezco",   #Factor
    a5_etnia == "No"  ~ "No pertenezco"
  ))

mergesvy <- data_isp_1 %>%
  as_survey_design(
    weights = pond1)
#D4.1
table_freq_02_n <- mergesvy %>%
  dplyr::group_by(etnia, d4.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_n=data.frame(table_freq_02_n, digits=2)
#summary(factor(df$proportion))

df_n= df_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.1, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.1) %>% 
  mutate(variable="d4.1") %>% 
  rename (Etnia =etnia)

#D4.2
table_freq_03_n <- mergesvy %>%
  dplyr::group_by(etnia, d4.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1_n=data.frame(table_freq_03_n, digits=2)
#summary(factor(df$proportion))

df_1_n = df_1_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.2, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>%  
  rename(g=d4.2) %>% 
  mutate(variable="d4.2") %>% 
  rename(Etnia=etnia)

#D4.3
table_freq_04_n <- mergesvy %>%
  dplyr::group_by(etnia, d4.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2_n=data.frame(table_freq_04_n, digits=2)
#summary(factor(df$proportion))

df_2_n = df_2_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.3, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.3) %>% 
  mutate(variable="d4.3") %>% 
  rename(Etnia=etnia)

#D4.4
table_freq_05_n <- mergesvy %>%
  dplyr::group_by(etnia, d4.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_3_n=data.frame(table_freq_05_n, digits=2)
#summary(factor(df$proportion))

df_3_n = df_3_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.4, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.4) %>% 
  mutate(variable="d4.4") %>% 
  rename(Etnia=etnia)

#D4.5
table_freq_06_n <- mergesvy %>%
  dplyr::group_by(etnia, d4.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_n=data.frame(table_freq_06_n, digits=2)
#summary(factor(df$proportion))

df_4_n = df_4_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.5, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.5) %>% 
  mutate(variable="d4.5") %>% 
  rename (Etnia=etnia)

#D4.6
table_freq_07_n <- mergesvy %>%
  dplyr::group_by(etnia,d4.6) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5_n=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df_5_n = df_5_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.6, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.6) %>% 
  mutate(variable="d4.6") %>% 
  rename(Etnia=etnia)
#D4.7
table_freq_00_n <- mergesvy %>%
  dplyr::group_by(etnia,d4.7) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7_n = df_7_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.7, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.7) %>% 
  mutate(variable="d4.7")%>% 
  rename(Etnia=etnia)

df_final <- rbind(df_n, df_1_n, df_2_n, df_3_n, df_4_n, df_5_n, df_7_n)

df_final$variable = factor(df_final$variable, labels=c("Velocidad al trabajar","Trabajo asignado", "Descanso", "Trabajo fuera horario", "Trabajo finde","Salario mensual", "Derechos laborales"))


df_final %>% 
  ggplot(aes(x=label, y=proportion, fill=Etnia))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11.5)) +
  facet_wrap(~variable)+
  labs(x = "Intensidad laboral", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

T1.7<-ctable(data_isp_1$a1_sexo, data_isp_1$d4.1, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T2.7<-ctable(data_isp_1$a1_sexo, data_isp_1$d4.2, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T3.7<-ctable(data_isp_1$a1_sexo, data_isp_1$d4.3, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T4.7<-ctable(data_isp_1$a1_sexo, data_isp_1$d4.4, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T5.7<-ctable(data_isp_1$a1_sexo, data_isp_1$d4.5, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T6.7<-ctable(data_isp_1$a1_sexo, data_isp_1$d4.6, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T7.7<-ctable(data_isp_1$a1_sexo, data_isp_1$d4.7, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)

##Modalidad 
T1.8<-ctable(data_isp_1$modalidad, data_isp_1$d4.1, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T2.8<-ctable(data_isp_1$modalidad, data_isp_1$d4.2, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T3.8<-ctable(data_isp_1$modalidad, data_isp_1$d4.3, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T4.8<-ctable(data_isp_1$modalidad, data_isp_1$d4.4, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T5.8<-ctable(data_isp_1$modalidad, data_isp_1$d4.5, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T6.8<-ctable(data_isp_1$modalidad, data_isp_1$d4.6, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
T7.8<-ctable(data_isp_1$modalidad, data_isp_1$d4.7, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)

#Edad

data_isp_1 = data_isp_1 %>%
  mutate(edad_cat_recode=case_when(
    edad_cat == "Adultos" ~ "Adultos",   #Factor
    edad_cat == "Jóvenes" ~ "Jóvenes", 
    TRUE ~ as.character(edad_cat) 
  )) %>% 
  filter(!is.na(edad_cat_recode))

mergesvy <- data_isp_1 %>%
  as_survey_design(
    weights = pond1)
#D4.1
table_freq_02_n <- mergesvy %>%
  dplyr::group_by(edad_cat_recode, d4.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_n=data.frame(table_freq_02_n, digits=2)
#summary(factor(df$proportion))

df_n= df_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.1, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.1) %>% 
  mutate(variable="d4.1") %>% 
  rename (Edad =edad_cat_recode)

#D4.2
table_freq_03_n <- mergesvy %>%
  dplyr::group_by(edad_cat_recode, d4.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1_n=data.frame(table_freq_03_n, digits=2)
#summary(factor(df$proportion))

df_1_n = df_1_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.2, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>%  
  rename(g=d4.2) %>% 
  mutate(variable="d4.2") %>% 
  rename(Edad=edad_cat_recode)

#D4.3
table_freq_04_n <- mergesvy %>%
  dplyr::group_by(edad_cat_recode, d4.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2_n=data.frame(table_freq_04_n, digits=2)
#summary(factor(df$proportion))

df_2_n = df_2_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.3, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.3) %>% 
  mutate(variable="d4.3") %>% 
  rename(Edad=edad_cat_recode)

#D4.4
table_freq_05_n <- mergesvy %>%
  dplyr::group_by(edad_cat_recode, d4.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_3_n=data.frame(table_freq_05_n, digits=2)
#summary(factor(df$proportion))

df_3_n = df_3_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.4, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.4) %>% 
  mutate(variable="d4.4") %>% 
  rename(Edad=edad_cat_recode)

#D4.5
table_freq_06_n <- mergesvy %>%
  dplyr::group_by(edad_cat_recode, d4.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_n=data.frame(table_freq_06_n, digits=2)
#summary(factor(df$proportion))

df_4_n = df_4_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.5, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.5) %>% 
  mutate(variable="d4.5") %>% 
  rename (Edad=edad_cat_recode)

#D4.6
table_freq_07_n <- mergesvy %>%
  dplyr::group_by(edad_cat_recode,d4.6) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5_n=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df_5_n = df_5_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.6, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.6) %>% 
  mutate(variable="d4.6") %>% 
  rename(Edad=edad_cat_recode)
#D4.7
table_freq_00_n <- mergesvy %>%
  dplyr::group_by(edad_cat_recode,d4.7) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7_n = df_7_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.7, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.7) %>% 
  mutate(variable="d4.7")%>% 
  rename(Edad=edad_cat_recode)

df_final <- rbind(df_n, df_1_n, df_2_n, df_3_n, df_4_n, df_5_n, df_7_n)

df_final$variable = factor(df_final$variable, labels=c("Velocidad al trabajar", "Trabajo asignado", "Descanso", "Trabajo fuera horario", "Trabajo finde","Salario mensual", "Derechos laborales"))

df_final %>% 
  ggplot(aes(x=label, y=proportion, fill=Edad))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 12)) +
  facet_wrap(~variable)+
  labs(x = "Intensidad laboral", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

#Genero
mergesvy <- data_isp_1 %>%
  as_survey_design(
    weights = pond1)
#D4.1
table_freq_02_n <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo, d4.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_n=data.frame(table_freq_02_n, digits=2)
#summary(factor(df$proportion))

df_n= df_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.1, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.1) %>% 
  mutate(variable="d4.1") %>% 
  rename (Género=a1_sexo)

#D4.2
table_freq_03_n <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo, d4.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1_n=data.frame(table_freq_03_n, digits=2)
#summary(factor(df$proportion))

df_1_n = df_1_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.2, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>%  
  rename(g=d4.2) %>% 
  mutate(variable="d4.2") %>% 
  rename(Género=a1_sexo)

#D4.3
table_freq_04_n <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo, d4.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2_n=data.frame(table_freq_04_n, digits=2)
#summary(factor(df$proportion))

df_2_n = df_2_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.3, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.3) %>% 
  mutate(variable="d4.3") %>% 
  rename(Género=a1_sexo)

#D4.4
table_freq_05_n <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo, d4.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_3_n=data.frame(table_freq_05_n, digits=2)
#summary(factor(df$proportion))

df_3_n = df_3_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.4, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.4) %>% 
  mutate(variable="d4.4") %>% 
  rename(Género=a1_sexo)

#D4.5
table_freq_06_n <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo, d4.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_n=data.frame(table_freq_06_n, digits=2)
#summary(factor(df$proportion))

df_4_n = df_4_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.5, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.5) %>% 
  mutate(variable="d4.5") %>% 
  rename (Género=a1_sexo)

#D4.6
table_freq_07_n <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo,d4.6) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5_n=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df_5_n = df_5_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.6, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.6) %>% 
  mutate(variable="d4.6") %>% 
  rename(Género=a1_sexo)
#D4.7
table_freq_00_n <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo,d4.7) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7_n = df_7_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(d4.7, labels=c("Disminuyó", "Sigue Igual", "Aumentó"))) %>% 
  rename(g=d4.7) %>% 
  mutate(variable="d4.7")%>% 
  rename(Género=a1_sexo)

df_final <- rbind(df_n, df_1_n, df_2_n, df_3_n, df_4_n, df_5_n, df_7_n)

df_final$variable = factor(df_final$variable, labels=c("Velocidad al trabajar","Trabajo asignado", "Descanso", "Trabajo fuera horario", "Trabajo finde","Salario mensual", "Derechos laborales"))


df_final %>% 
  ggplot(aes(x=label, y=proportion, fill=Género))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11)) +
  facet_wrap(~variable)+
  labs(x = "Intensidad laboral", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)


#Horas de trabajo
#Horas dedicada a cuidados en promedio
Tn<-dta %>% summarize(PH=weighted.mean(e6.4,pond1, na.rm = TRUE))

#Genero
T4e3<-dta %>% group_by(a1_sexo) %>% summarize(C=weighted.mean(e6.4,pond1, na.rm = TRUE))

#Etnia
T4.6q_3<-dta %>% group_by(a5_etnia) %>% summarize(C=weighted.mean(e6.4,pond1, na.rm = TRUE))

#Edad
T4.63<-dta %>% group_by(edad_cat) %>% summarize(C=weighted.mean(e6.4,pond1, na.rm = TRUE))


#Modalidad
T4.6w3<-dta %>% group_by(modalidad) %>% summarize(C=weighted.mean(e6.4,pond1, na.rm = TRUE))

T1.e6.8<-freq(dta$e6.3_desc, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
