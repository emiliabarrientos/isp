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
library(radiant.data)
install.packages("radiant.data")

#Base
data_isp <- readRDS("C:/Users/catac/Dropbox/Cata/Github/isp/informe_resultados/data/data_isp.rds") 

dta = data_isp %>% 
  filter(modalidad!="Presencial")


#Propiedad de las herramientas
data_isp %>% count(e3.1)
data_isp %>% count(e3.2)
data_isp %>% count(e3.3)


pond_1 <- data_isp %>% pull(pond1)
#Propiedad de las herramientas
T1.e1.1<-freq(dta$e3.1, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e2.1<-freq(dta$e3.2, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e3.1<-freq(dta$e3.3, weights = dta$pond1, useNA = c("no"),  round.digits = 2)

dta %>% count(modalidad)

##Calidad de las herramientas
data_isp %>% count(e4.1)
data_isp <- data_isp %>%
  mutate_at( c("e4.1", "e4.2", "e4.3", "e4.4", "e4.5"),
             ~ case_when(
               . == "Muy mala"       ~ "Mala",   #Factor
               . == "Mala"           ~ "Mala",
               . == "Regular"        ~ "Regular",
               . == "Buena"          ~ "Buena", 
               . == "Muy buena"      ~ "Buena", 
               . == "No la utilizo"  ~ "No la utilizo", 
               TRUE ~ as.character(.)))

T1.e1.1<-freq(dta$e4.1, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e2.1<-freq(dta$e4.2, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e3.1<-freq(dta$e4.3, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e4.1<-freq(dta$e4.4, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e5.1<-freq(dta$e4.5, weights = dta$pond1, useNA = c("no"),  round.digits = 2)

T1.17<-ctable(dta$edad_cat, dta$e4.1, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T2.7<-ctable(dta$edad_cat, dta$e4.2, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T3.7<-ctable(dta$edad_cat, dta$e4.3, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T4.7<-ctable(dta$edad_cat, dta$e4.4, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T4.7<-ctable(dta$edad_cat, dta$e4.5, weights = dta$pond1, useNA = c("no"), round.digits = 2)


#Apoyo empleador
data_isp %>% count(modalidad)
T1.e5.1<-freq(dta$e5.1, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e5.2<-freq(dta$e5.2, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e5.3<-freq(dta$e5.3, weights = dta$pond1, useNA = c("no"),  round.digits = 2)

T5.7<-ctable(data_isp$a0_pais, data_isp$g1_recode, weights = data_isp$pond1, useNA = c("no"), round.digits = 2)
T5.7<-ctable(data_isp$a0_pais, data_isp$g1_recode, weights = data_isp$pond1, useNA = c("no"), round.digits = 2)
T5.7<-ctable(data_isp$a0_pais, data_isp$g1_recode, weights = data_isp$pond1, useNA = c("no"), round.digits = 2)

data_isp %>% count(modalidad)
dta %>% count(modalidad)

#Uso de plataformas
data_isp %>% count(e5.1)
T1.e6.1<-freq(dta$e6.1_teams, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e6.2<-freq(dta$e6.1_zoom, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e6.3<-freq(dta$e6.1_meet, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e6.4<-freq(dta$e6.1_intranet, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e6.5<-freq(dta$e6.1_whatsapp, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e6.6<-freq(dta$e6.1_yammer, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e6.7<-freq(dta$e6.1_duo, weights = dta$pond1, useNA = c("no"),  round.digits = 2)
T1.e6.8<-freq(dta$e6.1_otro, weights = dta$pond1, useNA = c("no"),  round.digits = 2)

#Desconexion
T1.e6.9<-freq(data_isp$e6.3_desc, weights = data_isp$pond1, useNA = c("no"),  round.digits = 2)

#Conexion
T1.e6.10<-freq(data_isp$e6.2, weights = data_isp$pond1, useNA = c("no"),  round.digits = 2)

data_isp %>% glimpse(
  
)

#Propiedad de las herramientas

dta %>% count(modalidad)

T44.7<-ctable(dta$a1_sexo, dta$e3.1, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T55.7<-ctable(dta$a1_sexo, dta$e3.2, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T66.7<-ctable(dta$a1_sexo, dta$e3.3, weights = dta$pond1, useNA = c("no"), round.digits = 2)



mergesvy <- data_isp_1 %>%
  as_survey_design(
    weights = pond1)
#e3.1
table_freq_06_n <- mergesvy %>%
  filter(!is.na(e3.1)) %>%
  filter(modalidad!="Presencial") %>% 
  dplyr::group_by(modalidad, e3.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_n=data.frame(table_freq_06_n, digits=2)
#summary(factor(df$proportion))

df_4_n = df_4_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  rename(g=e3.1) %>% 
  mutate(variable="e3.1") 

#e3.2
table_freq_07_n <- mergesvy %>%
  filter(!is.na(e3.2)) %>%
  dplyr::group_by(modalidad,e3.2) %>%
  filter(modalidad!="Presencial") %>% 
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5_n=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df_5_n = df_5_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  rename(g=e3.2) %>% 
  mutate(variable="e3.2") 

#e3.3
table_freq_00_n <- mergesvy %>%
  filter(!is.na(e3.3)) %>%
  filter(modalidad!="Presencial") %>% 
  dplyr::group_by(modalidad,e3.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7_n = df_7_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=e3.3) %>% 
  mutate(variable="e3.3")

df_final <- rbind(df_4_n, df_5_n, df_7_n)

df_final$variable = factor(df_final$variable, labels=c("Computador o notebook","Celular", "Internet"))


df_final %>% 
  ggplot(aes(x=g, y=proportion, fill=modalidad))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 10)) +
  facet_wrap(~variable)+
  labs(x = "Propiedad de las herramientas", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9", "#d3d3d3"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)


#Apoyo empleador
mergesvy <- data_isp_1 %>%
  as_survey_design(
    weights = pond1)
#e5.1
table_freq_07_n <- mergesvy %>%
  filter(!is.na(e5.1)) %>%
  filter(modalidad!="Presencial") %>% 
  dplyr::group_by(modalidad, e5.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df1=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df1 = df1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  rename(g=e5.1) %>% 
  mutate(variable="e5.1") 

#e3.2
table_freq_n <- mergesvy %>%
  filter(!is.na(e5.2)) %>%
  dplyr::group_by(modalidad,e5.2) %>%
  filter(modalidad!="Presencial") %>% 
  summarize(proportion = survey_mean(,na.rm=TRUE))
df2=data.frame(table_freq_n, digits=2)
#summary(factor(df$proportion))

df2= df2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  rename(g=e5.2) %>% 
  mutate(variable="e5.2") 

#e3.3
table_freq_02_n <- mergesvy %>%
  filter(!is.na(e5.3)) %>%
  filter(modalidad!="Presencial") %>% 
  dplyr::group_by(modalidad,e5.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df3=data.frame(table_freq_02_n, digits=2)
#summary(factor(df$proportion))

df3 = df3 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=e5.3) %>% 
  mutate(variable="e5.3")

df_final <- rbind(df1, df2, df3)

df_final$variable = factor(df_final$variable, labels=c("Hacer teletrabajo","Normas de seguridad", "Accidente laboral"))


df_final %>% 
  ggplot(aes(x=g, y=proportion, fill=modalidad))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 10)) +
  facet_wrap(~variable)+
  labs(x = "Propiedad de las herramientas", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9", "#d3d3d3"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

#Horas de trabajo
T1.e6.8<-freq(dta$e6.4, weights = dta$pond1, useNA = c("no"),  round.digits = 2)


T1.e6.88<-freq(dta$e6.3_desc, weights = dta$pond1, useNA = c("no"),  round.digits = 2) #Desconexión

TA<-freq(data_isp$e6.3_desc, weights = data_isp$pond1, useNA = c("no"),  round.digits = 2) #Desconexión

#Desconexión

data_isp <- readRDS("C:/Users/catac/Dropbox/Cata/Github/isp/informe_resultados/data/data_isp.rds") 

dta = data_isp %>% 
  filter(modalidad!="Presencial")

dta %>% count(modalidad)

mergesvy <- dta %>%
  as_survey_design(
    weights = pond1)

table_freq_1 <- mergesvy %>%
  filter(!is.na(e6.3_desc)) %>%
  filter(a1_sexo!="Otro") %>%
  dplyr::group_by(a1_sexo,e6.3_desc) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df3=data.frame(table_freq_1, digits=2)
#summary(factor(df$proportion))

df3 = df3 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(Género=a1_sexo)

df3 %>% 
  ggplot(aes(x=e6.3_desc, y=proportion, fill=Género))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11)) +
  facet_wrap(~Género)+
  labs(x = "Desconexión durante 12 horas seguidas", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9", "#d3d3d3"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

#ETNIA

dta = dta %>%
  mutate(etnia=case_when(
    a5_etnia == "Si"  ~ "Sí, pertenezco",   #Factor
    a5_etnia == "No"  ~ "No pertenezco"
  ))

dta %>% count(modalidad)
data_isp %>% count(modalidad)

dta %>% glimpse ()
data_isp %>% glimpse ()
dta %>% count (modalidad)

mergesvy <- dta %>%
  as_survey_design(
    weights = pond1)

table_freq_q1 <- mergesvy %>%
  filter(!is.na(e6.3_desc)) %>%
  dplyr::group_by(etnia,e6.3_desc) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df13=data.frame(table_freq_q1, digits=2)
#summary(factor(df$proportion))

df13 = df13 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(Etnia=etnia)

df13 %>% 
  ggplot(aes(x=e6.3_desc, y=proportion, fill=Etnia))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11)) +
  facet_wrap(~Etnia)+
  labs(x = "Desconexión durante 12 horas seguidas", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9", "#d3d3d3"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

T1.2<-ctable(dta$a1_sexo, dta$e6.3_desc, weights = dta$pond1, useNA = c("no"), round.digits = 1)
T1.2<-ctable(dta$etnia, dta$e6.3_desc, weights = dta$pond1, useNA = c("no"), round.digits = 1)

#Edad

dta = dta %>%
  mutate(edad_cat_recode=case_when(
    edad_cat == "Adultos" ~ "Adultos",   #Factor
    edad_cat == "Jóvenes" ~ "Jóvenes", 
    TRUE ~ as.character(edad_cat) 
  )) %>% 
  filter(!is.na(edad_cat_recode))

mergesvy <- dta %>%
  as_survey_design(
    weights = pond1)

table_freq_11 <- mergesvy %>%
  filter(!is.na(e6.3_desc)) %>%
  dplyr::group_by(edad_cat_recode,e6.3_desc) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df1a3=data.frame(table_freq_11, digits=2)
#summary(factor(df$proportion))

df1a3 = df1a3 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(Edad=edad_cat_recode)

df1a3 %>% 
  ggplot(aes(x=e6.3_desc, y=proportion, fill=Edad))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11)) +
  facet_wrap(~Edad)+
  labs(x = "Desconexión durante 12 horas seguidas", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9", "#d3d3d3"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

T1.2<-ctable(dta$a1_sexo, dta$e6.3_desc, weights = dta$pond1, useNA = c("no"), round.digits = 1)
T1.2<-ctable(dta$etnia, dta$e6.3_desc, weights = dta$pond1, useNA = c("no"), round.digits = 1)
T1.2<-ctable(dta$edad_cat_recode, dta$e6.3_desc, weights = dta$pond1, useNA = c("no"), round.digits = 1)

#Medidas de trabajo
dt_isp = data_isp %>% 
  filter(modalidad!="Teletrabajo")
dt_isp %>% count(modalidad)
T1.f6.1<-freq(dt_isp$f1.1, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.2<-freq(dt_isp$f1.2,weights = dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.3<-freq(dt_isp$f1.3,weights = dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.4<-freq(dt_isp$f1.4, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.5<-freq(dt_isp$f1.5, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.6<-freq(dt_isp$f1.6, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.7<-freq(dt_isp$f1.7, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.8<-freq(dt_isp$f1.8, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.9<-freq(dt_isp$f1.9, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.10<-freq(dt_isp$f1.10, weights = dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.11<-freq(dt_isp$f1.11, weights = dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.12<-freq(dt_isp$f1.12, weights = dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.13<-freq(dt_isp$f1.13, weights = dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.14<-freq(dt_isp$f1.14, weights = dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.f6.15<-freq(dt_isp$f1.15, weights = dt_isp$pond1, useNA = c("no"),  round.digits = 2)

#Problemas de uso 
T1.fd.6<-freq(dt_isp$f3.1, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.fd.7<-freq(dt_isp$f3.2, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.fd.8<-freq(dt_isp$f3.3, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.fd.9<-freq(dt_isp$f3.4, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)

#Problemas de cumplimiento de labores
T1.ff.6<-freq(dt_isp$f2.1, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.ff.7<-freq(dt_isp$f2.2, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.ff.8<-freq(dt_isp$f2.3, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.ff.9<-freq(dt_isp$f2.4, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.fsf.9<-freq(dt_isp$f2.5, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.ffs.9<-freq(dt_isp$f2.6, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)
T1.aff.9<-freq(dt_isp$f2.7, weights =dt_isp$pond1, useNA = c("no"),  round.digits = 2)

dt_isp %>% glimpse()
dt_isp %>% count(modalidad)

#Intensidad laboral etnia
data_isp_1 = data_isp %>%
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
  ggplot(aes(x=g, y=proportion, fill=Etnia))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11)) +
  facet_wrap(~variable)+
  labs(x = "Intensidad laboral", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

#Apoyo al empleador segun genero
T1.7<-ctable(dta$a1_sexo, dta$e5.1, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T2.7<-ctable(dta$a1_sexo, dta$e5.2, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T3.7<-ctable(dta$a1_sexo, dta$e5.3, weights = dta$pond1, useNA = c("no"), round.digits = 2)

#etnia
T1.8<-ctable(dta$a5_etnia, dta$e5.1, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T2.8<-ctable(dta$a5_etnia, dta$e5.2, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T3.8<-ctable(dta$a5_etnia, dta$e5.3, weights = dta$pond1, useNA = c("no"), round.digits = 2)

#edad
T1.66<-ctable(dta$edad_cat, dta$e5.1, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T2.66<-ctable(dta$edad_cat, dta$e5.2, weights = dta$pond1, useNA = c("no"), round.digits = 2)
T3.66<-ctable(dta$edad_cat, dta$e5.3, weights = dta$pond1, useNA = c("no"), round.digits = 2)
