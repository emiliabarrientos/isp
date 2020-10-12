
#######################
#Catalina Miranda
##Informe descriptivo ISP
## 06 de octubre 2020

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
library(labelled)
library(haven)

#Base ft salud mental
data_isp_sm <- readRDS("C:/Users/catac/Dropbox/Cata/Github/isp/informe_resultados/data/data_isp_sm.rds")

#Quitar las etiquetas // pasar a caracter

data_isp_sm %>% mutate(n_rps2 = as.character(n_rps)) %>% count(n_rps, n_rps2)
 
data_isp_sm = data_isp_sm %>%
  mutate(n_rps2=case_when(
    n_rps == "1" ~ "1",   #Factor
    n_rps == "2" ~ "2", 
    n_rps == "3" ~ "3", 
    TRUE ~ as.character(n_rps) 
  )) 

data_isp_sm %>% count(n_rps, n_rps2)
data_isp_sm %>% count(n_rps)

data_isp_sm %>% count(h3.1)
data_isp_sm %>% count(h3.3)


#
mergesvy <- data_isp_sm %>%
  as_survey_design(
    weights = pond1)

#Riesgo psicosocial
table_freq_06_n <- mergesvy %>%
  dplyr::group_by(n_rps2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_n=data.frame(table_freq_06_n, digits=2)
#summary(factor(df$proportion))

df_4_n = df_4_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(n_rps2, labels=c("Bajo", "Medio", "Alto"))) %>% 
  rename(g=n_rps2) %>% 
  mutate(variable="n_rps2") 


df_4_n %>% 
  ggplot(aes(x=label, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11)) +
  labs(x = "Riesgo psicosocial", y = "%") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

#Riesgo psicosocial segun genero

table_freq_06_n <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo, n_rps2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_n=data.frame(table_freq_06_n, digits=2)
#summary(factor(df$proportion))

df_4_n = df_4_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(n_rps2, labels=c("Bajo", "Medio", "Alto"))) %>% 
  rename(g=n_rps2) %>% 
  mutate(variable="n_rps2") 


df_4_n %>% 
  ggplot(aes(x=label, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11)) +
  labs(x = "Riesgo psicosocial", y = "%") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)


###Salud mental

data_isp_sm <- data_isp_sm %>%
  mutate_at( c("h2.1", "h2.2", "h2.3", "h2.4", "h2.5"),
             ~ case_when(
               . == "5" ~ "5",   #Factor
               . == "4" ~ "4", 
               . == "3" ~ "3" , 
               . == "2" ~ "2", 
               . == "1" ~ "1", 
               TRUE ~ as.character(.)))%>%
  filter(!is.na(h2.1)) %>% 
  filter(!is.na(h2.2)) %>% 
  filter(!is.na(h2.3)) %>% 
  filter(!is.na(h2.4)) 

data_isp_sm %>% count(h2.1)
data_isp_sm %>% count(h2.2)
data_isp_sm %>% count(h2.3)
data_isp_sm %>% count(h2.4)
data_isp_sm %>% count(h2.5)

mergesvy <- data_isp_sm %>%
  as_survey_design(
    weights = pond1)

data_isp_sm %>% count(h2.1)

#h2.1
table_freq_07_n <- mergesvy %>%
  filter(!is.na(h2.1)) %>%
  dplyr::group_by(h2.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5_n=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df_5_n = df_5_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.1, labels=c("Nunca","Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.1) %>% 
  mutate(variable="h2.1") 

#h2.2
table_freq_00_n <- mergesvy %>%
  filter(!is.na(h2.2)) %>%
  dplyr::group_by(h2.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7_n = df_7_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.2, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.2) %>% 
  mutate(variable="h2.2") 

#h2.3
table_freq_200_n <- mergesvy %>%
  filter(!is.na(h2.3)) %>%
  dplyr::group_by(h2.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n2=data.frame(table_freq_200_n, digits=2)
#summary(factor(df$proportion))

df_7_n2 = df_7_n2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))%>% 
  mutate(label=factor(h2.3, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.3) %>% 
  mutate(variable="h2.3") 

#h2.4
table_freq_020_n <- mergesvy %>%
  filter(!is.na(h2.4)) %>%
  dplyr::group_by(h2.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_2n=data.frame(table_freq_020_n, digits=2)
#summary(factor(df$proportion))

df_7_2n = df_7_2n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))%>% 
  mutate(label=factor(h2.4, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.4) %>% 
  mutate(variable="h2.4") 

#h2.5
table_freq_00_n <- mergesvy %>%
  filter(!is.na(h2.5)) %>%
  dplyr::group_by(h2.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n1=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7_n1 = df_7_n1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.5, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.5) %>% 
  mutate(variable="h2.5") 

df_final <- rbind(df_5_n, df_7_n, df_7_n2, df_7_2n, df_7_n1)

df_final$variable = factor(df_final$variable, labels=c("Nerviosa/o", "Decaída/o","Tranquila/o y Calmada/o", "Desanimada/o y Triste", "Feliz"))


df_final %>% 
  ggplot(aes(x=label, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4.5, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11)) +
  facet_wrap(~variable)+
  labs(x = "Salud mental", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9", "#d3d3d3"))+
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent)

#Genero

mergesvy <- data_isp_sm %>%
  as_survey_design(
    weights = pond1)

data_isp_sm %>% count(h2.1)

#h2.1
table_freq_07_n <- mergesvy %>%
  filter(!is.na(h2.1)) %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo, h2.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df = df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.1, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.1) %>% 
  mutate(variable="h2.1") %>% 
  rename(Género=a1_sexo)

#h2.2
table_freq_00_n <- mergesvy %>%
  filter(!is.na(h2.2)) %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo,h2.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_1 = df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.2, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces","La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.2) %>% 
  mutate(variable="h2.2") %>% 
  rename(Género=a1_sexo)

#h2.3
table_freq_200_n <- mergesvy %>%
  filter(!is.na(h2.3)) %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo,h2.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2=data.frame(table_freq_200_n, digits=2)
#summary(factor(df$proportion))

df_2 = df_2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))%>% 
  mutate(label=factor(h2.3, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.3) %>% 
  mutate(variable="h2.3") %>% 
  rename(Género=a1_sexo)

#h2.4
table_freq_020_n <- mergesvy %>%
  filter(!is.na(h2.4)) %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo,h2.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_3=data.frame(table_freq_020_n, digits=2)
#summary(factor(df$proportion))

df_3 = df_3 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))%>% 
  mutate(label=factor(h2.4, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.4) %>% 
  mutate(variable="h2.4") %>% 
  rename(Género=a1_sexo)

#h2.5
table_freq_00_n <- mergesvy %>%
  filter(!is.na(h2.5)) %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(a1_sexo,h2.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_4 = df_4 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.5, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.5) %>% 
  mutate(variable="h2.5") %>% 
  rename(Género=a1_sexo)
  

df_final<- rbind(df, df_1, df_2, df_3, df_4)

df_final$variable = factor(df_final$variable, labels=c("Nerviosa/o", "Decaída/o","Tranquila/o y Calmada/o", "Desanimada/o y Triste", "Feliz"))

df_final %>% 
  ggplot(aes(x=label, y=proportion, fill=Género))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11.5)) +
  facet_wrap(~variable)+
  labs(x = "Salud mental", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)


pond_1 <- data_isp_sm %>% pull(pond1)
T1.7<-ctable(data_isp_sm$modalidad, data_isp_sm$h2.1, weights = pond_1, round.digits = 2)
T1.f7<-ctable(data_isp_sm$modalidad, data_isp_sm$h2.2, weights = pond_1, round.digits = 2)
T1.ff7<-ctable(data_isp_sm$modalidad, data_isp_sm$h2.3, weights = pond_1, round.digits = 2)
T1.fff7<-ctable(data_isp_sm$modalidad, data_isp_sm$h2.4, weights = pond_1, round.digits = 2)
T1.faff7<-ctable(data_isp_sm$modalidad, data_isp_sm$h2.5, weights = pond_1, round.digits = 2)

#Etnia

data_isp_sm= data_isp_sm %>%
  mutate(etnia=case_when(
    a5_etnia == "Si"  ~ "Sí, pertenezco",   #Factor
    a5_etnia == "No"  ~ "No pertenezco"
  ))


mergesvy <- data_isp_sm %>%
  as_survey_design(
    weights = pond1)

data_isp_sm %>% count(h2.1)

#h2.1
table_freq_07_n <- mergesvy %>%
  filter(!is.na(h2.1)) %>%
  dplyr::group_by(etnia, h2.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df_5 = df_5 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.1, labels=c("Nunca","Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.1) %>% 
  mutate(variable="h2.1") %>% 
  rename(Etnia=etnia)

#h2.2
table_freq_00_n <- mergesvy %>%
  filter(!is.na(h2.2)) %>%
  dplyr::group_by(etnia,h2.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7 = df_7 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.2, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.2) %>% 
  mutate(variable="h2.2") %>% 
  rename(Etnia=etnia)

#h2.3
table_freq_200_n <- mergesvy %>%
  filter(!is.na(h2.3)) %>%
  dplyr::group_by(etnia,h2.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7n2=data.frame(table_freq_200_n, digits=2)
#summary(factor(df$proportion))

df_7n2 = df_7n2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))%>% 
  mutate(label=factor(h2.3, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.3) %>% 
  mutate(variable="h2.3") %>% 
  rename(Etnia=etnia)

#h2.4
table_freq_020_n <- mergesvy %>%
  filter(!is.na(h2.4)) %>%
  dplyr::group_by(etnia,h2.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_72n=data.frame(table_freq_020_n, digits=2)
#summary(factor(df$proportion))

df_72n = df_72n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))%>% 
  mutate(label=factor(h2.4, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces","La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.4) %>% 
  mutate(variable="h2.4") %>% 
  rename(Etnia=etnia)

#h2.5
table_freq_00_n <- mergesvy %>%
  filter(!is.na(h2.5)) %>%
  dplyr::group_by(etnia,h2.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n1=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7_n1 = df_7_n1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.5, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.5) %>% 
  mutate(variable="h2.5") %>% 
  rename(Etnia=etnia)


df_final <- rbind(df_5, df_7, df_7n2, df_72n, df_7_n1)

df_final$variable = factor(df_final$variable, labels=c("Nerviosa/o", "Decaída/o","Tranquila/o y Calmada/o", "Desanimada/o y Triste", "Feliz"))

df_final %>% 
  ggplot(aes(x=label, y=proportion, fill=Etnia))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11.5)) +
  facet_wrap(~variable)+
  labs(x = "Salud mental", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

#Salud emocional segun edad

data_isp_sm = data_isp_sm %>%
  mutate(edad_cat_recode=case_when(
    edad_cat == "Adultos" ~ "Adultos",   #Factor
    edad_cat == "Jóvenes" ~ "Jóvenes", 
    TRUE ~ as.character(edad_cat) 
  )) %>% 
  filter(!is.na(edad_cat_recode))

data_isp_sm %>% count(edad_cat, edad_cat_recode)

mergesvy <- data_isp_sm %>%
  as_survey_design(
    weights = pond1)

data_isp_sm %>% count(h2.1)

#h2.1

table_freq_07_n <- mergesvy %>%
  filter(!is.na(h2.1)) %>%
  dplyr::group_by(edad_cat_recode, h2.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df_5 = df_5 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.1, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.1) %>% 
  mutate(variable="h2.1") %>% 
  rename(Edad=edad_cat_recode)

#h2.2
table_freq_00_n <- mergesvy %>%
  filter(!is.na(h2.2)) %>%
  dplyr::group_by(edad_cat_recode,h2.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7 = df_7 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.2, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.2) %>% 
  mutate(variable="h2.2") %>% 
  rename(Edad=edad_cat_recode)

#h2.3
table_freq_200_n <- mergesvy %>%
  filter(!is.na(h2.3)) %>%
  dplyr::group_by(edad_cat_recode,h2.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7n2=data.frame(table_freq_200_n, digits=2)
#summary(factor(df$proportion))

df_7n2 = df_7n2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))%>% 
  mutate(label=factor(h2.3, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.3) %>% 
  mutate(variable="h2.3") %>% 
  rename(Edad=edad_cat_recode)

#h2.4
table_freq_020_n <- mergesvy %>%
  filter(!is.na(h2.4)) %>%
  dplyr::group_by(edad_cat_recode,h2.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_72n=data.frame(table_freq_020_n, digits=2)
#summary(factor(df$proportion))

df_72n = df_72n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))%>% 
  mutate(label=factor(h2.4, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.4) %>% 
  mutate(variable="h2.4") %>% 
  rename(Edad=edad_cat_recode)

table_freq_00_n <- mergesvy %>%
  filter(!is.na(h2.5)) %>%
  dplyr::group_by(edad_cat_recode,h2.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n1=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7_n1 = df_7_n1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  mutate(label=factor(h2.5, labels=c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=h2.5) %>% 
  mutate(variable="h2.5") %>% 
  rename(Edad=edad_cat_recode)

df_final <- rbind(df_5, df_7, df_7n2, df_72n, df_7_n1)


df_final$variable = factor(df_final$variable, labels=c("Nerviosa/o", "Decaída/o","Tranquila/o y Calmada/o", "Desanimada/o y Triste", "Feliz"))

df_final %>% 
  ggplot(aes(x=label, y=proportion, fill=Edad))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11.5)) +
  facet_wrap(~variable)+
  labs(x = "Salud mental", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

pond_1 <- data_isp_sm %>% pull(pond1)
T1.7<-ctable(data_isp_sm$a0_pais, data_isp_sm$h2.1, weights = pond_1, round.digits = 2)
T2.7<-ctable(data_isp_sm$a0_pais, data_isp_sm$h2.2, weights = pond_1, round.digits = 2)
T3.7<-ctable(data_isp_sm$a0_pais, data_isp_sm$h2.3, weights = pond_1, round.digits = 2)
T4.7<-ctable(data_isp_sm$a0_pais, data_isp_sm$h2.4, weights = pond_1, round.digits = 2)


#Molestias 1 vs molestia 2

mergesvy <- data_isp_sm %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  filter(!is.na(h3.1)) %>%
  dplyr::group_by(h3.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1 = df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=h3.1) %>% 
  mutate(variable="h3.1") 

#Molestia 2
table_freq_3 <- mergesvy %>%
  filter(!is.na(h3.3)) %>%
  dplyr::group_by(h3.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2 = df_2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  rename(g=h3.3) %>% 
  mutate(variable="h3.3")

df_final_post <- rbind(df_1, df_2)

df_final_post$variable = factor(df_final_post$variable, labels=c("Molestia 1", "Molestia 2"))

df_final_post %>% 
  ggplot(aes(x=g, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4.5, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11)) +
  facet_wrap(~variable)+
  labs(x = "Tipo de Molestia", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9", "#d3d3d3"))+
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent)

#Genero
mergesvy <- data_isp_sm %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  filter(!is.na(h3.1)) %>%
  filter(a1_sexo!="Otro") %>%
  dplyr::group_by(a1_sexo, h3.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1 = df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=h3.1) %>% 
  mutate(variable="h3.1") %>% 
  rename(Género=a1_sexo)

#Molestia 2
table_freq_3 <- mergesvy %>%
  filter(!is.na(h3.3)) %>%
  filter(a1_sexo!="Otro") %>%
  dplyr::group_by(a1_sexo,h3.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2 = df_2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  rename(g=h3.3) %>% 
  mutate(variable="h3.3")%>% 
  rename(Género=a1_sexo)

df_final_post <- rbind(df_1, df_2)

df_final_post$variable = factor(df_final_post$variable, labels=c("Molestia 1", "Molestia 2"))

df_final_post %>% 
  ggplot(aes(x=g, y=proportion, fill=Género))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11.5)) +
  facet_wrap(~variable)+
  labs(x = "Tipo de Molestia", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9"))+
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent)


#Edad

mergesvy <- data_isp_sm %>%
  as_survey_design(
    weights = pond1)

data_isp_sm = data_isp_sm %>%
  mutate(edad_cat_recode=case_when(
    edad_cat == "Adultos" ~ "Adultos",   #Factor
    edad_cat == "Jóvenes" ~ "Jóvenes", 
    TRUE ~ as.character(edad_cat) 
  )) %>% 
  filter(!is.na(edad_cat_recode))

table_freq_02 <- mergesvy %>%
  filter(!is.na(h3.1)) %>%
  dplyr::group_by(edad_cat_recode, h3.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1a=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1a = df_1a %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=h3.1) %>% 
  mutate(variable="h3.1") %>% 
  rename(Edad=edad_cat_recode)


table_freq_3 <- mergesvy %>%
  filter(!is.na(h3.3)) %>%
  dplyr::group_by(edad_cat_recode, h3.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2a=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2a = df_2a %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  rename(g=h3.3) %>% 
  mutate(variable="h3.3")%>% 
  rename(Edad=edad_cat_recode)


df_final_post <- rbind(df_1a, df_2a)

df_final_post$variable = factor(df_final_post$variable, labels=c("Molestia 1", "Molestia 2"))

df_final_post %>% 
  ggplot(aes(x=g, y=proportion, fill=Edad))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11.5)) +
  facet_wrap(~variable)+
  labs(x = "Tipo de Molestia", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9"))+
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent)

#Etnia

data_isp_sm= data_isp_sm %>%
  mutate(etnia=case_when(
    a5_etnia == "Si"  ~ "Sí, pertenezco",   #Factor
    a5_etnia == "No"  ~ "No pertenezco"
  ))

mergesvy <- data_isp_sm %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  filter(!is.na(h3.1)) %>%
  dplyr::group_by(etnia, h3.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1a=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1a = df_1a %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=h3.1) %>% 
  mutate(variable="h3.1") %>% 
  rename(Etnia=etnia)


table_freq_3 <- mergesvy %>%
  filter(!is.na(h3.3)) %>%
  dplyr::group_by(etnia, h3.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2a=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2a = df_2a %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  rename(g=h3.3) %>% 
  mutate(variable="h3.3")%>% 
  rename(Etnia=etnia)


df_final_post <- rbind(df_1a, df_2a)

df_final_post$variable = factor(df_final_post$variable, labels=c("Molestia 1", "Molestia 2"))

df_final_post %>% 
  ggplot(aes(x=g, y=proportion, fill=Etnia))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11.5)) +
  facet_wrap(~variable)+
  labs(x = "Tipo de Molestia", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9"))+
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent)


#Modalidad
mergesvy <- data_isp_sm %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  filter(!is.na(h3.1)) %>%
  dplyr::group_by(modalidad, h3.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1a=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1a = df_1a %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=h3.1) %>% 
  mutate(variable="h3.1") %>% 
  rename(Modalidad=modalidad)


table_freq_3 <- mergesvy %>%
  filter(!is.na(h3.3)) %>%
  dplyr::group_by(modalidad, h3.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2a=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2a = df_2a %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  rename(g=h3.3) %>% 
  mutate(variable="h3.3")%>% 
  rename(Modalidad=modalidad)


df_final_post <- rbind(df_1a, df_2a)

df_final_post$variable = factor(df_final_post$variable, labels=c("Molestia 1", "Molestia 2"))

df_final_post %>% 
  ggplot(aes(x=g, y=proportion, fill=Modalidad))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11.5)) +
  facet_wrap(variable~Modalidad)+
  labs(x = "Tipo de Molestia", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#d3d3d3", "#e05e5a"))+
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent)


