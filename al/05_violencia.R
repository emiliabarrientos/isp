#######################
#Catalina Miranda
##Informe descriptivo ISP
## 19 de septiembre 2020

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
data_isp_v <- readRDS("C:/Users/catac/Dropbox/Cata/Github/isp/informe_resultados/data/data_isp.rds")

data_isp_v %>% count(g7.5)

data_isp_v <- data_isp_v %>%
  mutate_at( c("g8.1", "g8.2", "g8.3", "g7.5", "g8.7","g8.8", "g7.4", "g8.4", "g8.5", "g8.6"),
             ~ case_when(
               . == "Siempre"                 ~ "5",   #Factor
               . == "La mayoría de las veces" ~ "4", 
               . == "Algunas veces"           ~ "3", 
               . == "Nunca"                   ~ "2", 
               . == "No aplica"               ~ "1", 
               TRUE ~ "0"
             ))

data_isp_v %>% glimpse()

#Transformacion de variables  ####
data_isp_v = data_isp_v %>%
  mutate(g8.1=as.numeric(g8.1)) %>% 
  mutate(g8.2=as.numeric(g8.2)) %>% 
  mutate(g8.3=as.numeric(g8.3))

data_isp_v = data_isp_v %>%
  mutate(violencia_economica = g8.1 + g8.2 + g8.3)%>% 
  mutate(violencia_economica=as.factor(violencia_economica)) 

data_isp_v %>% count(violencia_psicologica) %>% View
data_isp_v %>% count(violencia_sexual)

# violencia fisica
data_isp_v = data_isp_v %>%
  mutate(g7.5=as.numeric(g7.5)) %>% 
  mutate(g8.7=as.numeric(g8.7)) 

data_isp_v = data_isp_v %>%
  mutate(violencia_fisica = g7.5 + g8.7) %>% 
  mutate(violencia_fisica=as.factor(violencia_fisica)) 

#violencia sexual

data_isp_v = data_isp_v %>% 
  rename(violencia_sexual="g8.8")

#Violenica psicologica
data_isp_v = data_isp_v %>%
  mutate(g7.4=as.numeric(g7.4)) %>% 
  mutate(g8.4=as.numeric(g8.4)) %>% 
  mutate(g8.5=as.numeric(g8.5))%>% 
  mutate(g8.6=as.numeric(g8.6))

data_isp_v = data_isp_v %>%
  mutate(violencia_psicologica = g7.4+g8.4 + g8.5 + g8.6)%>% 
  mutate(violencia_psicologica=as.factor(violencia_psicologica)) 

###Graficos indice violencia
mergesvy <- data_isp_v %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  dplyr::group_by(violencia_psicologica) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1 = df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_psicologica) %>% 
  mutate(variable="violencia_psicologica")

##violencia psicologica
table_freq_3 <- mergesvy %>%
  dplyr::group_by(violencia_fisica) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2 = df_2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_fisica) %>% 
  mutate(variable="violencia_fisica")

##violencia sexual
table_freq_4 <- mergesvy %>%
  dplyr::group_by(violencia_sexual) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4=data.frame(table_freq_4, digits=2)
#summary(factor(df$proportion))

df_4 = df_4 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_sexual) %>% 
  mutate(variable="violencia_sexual")

##violencia economica
table_freq_5 <- mergesvy %>%
  dplyr::group_by(violencia_economica) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5=data.frame(table_freq_5, digits=2)
#summary(factor(df$proportion))

df_5 = df_5 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_economica) %>% 
  mutate(variable="violencia_economica")

df_final_v <- rbind(df_1, df_2, df_4, df_5)

df_final_v$variable = factor(df_final_v$variable, labels=c("violencia económica","violencia fisica", "violencia psicológica","violencia sexual"))

order_yy = ggplot(df_final_v, aes(x=g,y=proportion)) +
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 4, angle =90)+
  facet_wrap(~variable, scales = "free_x")+
  theme(axis.text.x = element_text(angle =45, hjust = 1)) +
  labs(x = "Response", y = "%") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)+
  geom_col()

data_isp_v %>% count(g8.7)

##Indices ft edad

data_isp_v = data_isp_v %>%
  mutate(edad_cat_recode=case_when(
    edad_cat == "Adultos" ~ "Adultos",   #Factor
    edad_cat == "Jóvenes" ~ "Jóvenes", 
    TRUE ~ as.character(edad_cat) 
  )) %>% 
  filter(!is.na(edad_cat_recode))

data_isp_v %>% count(edad_cat, edad_cat_recode)

mergesvy <- data_isp_v %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  dplyr::group_by( edad_cat_recode,violencia_psicologica) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1_1=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1_1 = df_1_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_psicologica) %>% 
  mutate(variable="violencia_psicologica") %>% 
  rename(edad=edad_cat_recode)

##violencia psicologica
table_freq_3 <- mergesvy %>%
  dplyr::group_by(edad_cat_recode, violencia_fisica) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2_1=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2_1 = df_2_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_fisica) %>% 
  mutate(variable="violencia_fisica") %>% 
  rename(edad=edad_cat_recode)

##violencia sexual
pond_1 <- data_isp_v %>% pull(pond1)
T1.7<-ctable(data_isp_v$violencia_sexual, data_isp_v$a1_sexo, weights = pond_1, round.digits = 1)


data_isp_v %>% count(edad_cat_recode, violencia_sexual)

table_freq_4 <- mergesvy %>%
  dplyr::group_by(edad_cat_recode, violencia_sexual) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_1=data.frame(table_freq_4, digits=2)
#summary(factor(df$proportion))

df_4_1 = df_4_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_sexual) %>% 
  mutate(variable="violencia_sexual") %>% 
  rename(edad=edad_cat_recode)

##violencia economica
table_freq_5 <- mergesvy %>%
  dplyr::group_by(edad_cat_recode,violencia_economica) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5_1=data.frame(table_freq_5, digits=2)
#summary(factor(df$proportion))

df_5_1 = df_5_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_economica) %>% 
  mutate(variable="violencia_economica") %>% 
  rename(edad=edad_cat_recode)

df_final_cr <- rbind(df_1_1, df_2_1, df_4_1, df_5_1)

order_col = ggplot(df_final_cr, aes(x=g,y=proportion,  fill=edad)) +
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.3), vjust=-0.8, size = 3)+
  facet_wrap(~variable, scales = "free_x")+
  theme(axis.text.x = element_text(angle =45, hjust = 1)) +
  labs(x = "Response", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e05e5a"))+
  scale_y_continuous(limits = c(0, 0.9), labels = scales::percent)

##Indices ft género

mergesvy <- data_isp_v %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(violencia_psicologica, a1_sexo) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1_1=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1_1 = df_1_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_psicologica) %>% 
  mutate(variable="violencia_psicologica") %>% 
  rename(género=a1_sexo)

##violencia fisica
table_freq_3 <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(violencia_fisica, a1_sexo) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2_1=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2_1 = df_2_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_fisica) %>% 
  mutate(variable="violencia_fisica") %>% 
  rename(género=a1_sexo)

pond_1 <- data_isp_v %>% pull(pond1)
T1.7<-ctable(data_isp_v$violencia_fisica, data_isp_v$a1_sexo, weights = pond_1, round.digits = 1)

##violencia sexual

table_freq_4 <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(violencia_sexual, a1_sexo) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_1=data.frame(table_freq_4, digits=2)
#summary(factor(df$proportion))

df_4_1 = df_4_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_sexual) %>% 
  mutate(variable="violencia_sexual") %>% 
  rename(género=a1_sexo)

##violencia economica
table_freq_5 <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(violencia_economica, a1_sexo) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5_1=data.frame(table_freq_5, digits=2)
#summary(factor(df$proportion))

df_5_1 = df_5_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=violencia_economica) %>% 
  mutate(variable="violencia_economica") %>% 
  rename(género=a1_sexo)

df_final_cr <- rbind(df_1_1, df_2_1, df_4_1, df_5_1)

df_final_cr$variable = factor(df_final_cr$variable, labels=c("violencia económica","violencia fisica", "violencia psicológica","violencia sexual"))


order_col = ggplot(df_final_cr, aes(x=g,y=proportion,  fill=género)) +
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.2, hjust = -0.1, size = 4, angle =90)+
  facet_wrap(~variable, scales = "free_x")+
  theme(axis.text.x = element_text(angle =45, hjust = 1)) +
  labs(x = "Violencia", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e05e5a"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

#Violencia y recursos económicos
mergesvy <- data_isp_rc %>%
  as_survey_design(
    weights = pond1)

table_freq <- mergesvy %>%
  dplyr::group_by(a6.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df=data.frame(table_freq, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))

df %>% 
  ggplot(aes(x=a6.2, y=proportion))+
  geom_text(aes(label = p), position = position_dodge(0.3), vjust=-0.1, size = 2.5)+
  labs(x = "Recursos económicos en el hogar", y = "%") +
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent)+
  geom_col()
