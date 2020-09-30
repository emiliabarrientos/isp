#######################
#Catalina Miranda
##Informe descriptivo ISP
## 15 de septiembre 2020

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

#############################
#Indices sumativos 
#############################

data_isp %>% count(g4.6)

#Para el índice
data_isp_2 <- data_isp %>%
  mutate_at( c("g4.1", "g4.2", "g4.3", "g4.4", "g4.5","g4.6"),
             ~ case_when(
               . == "Siempre"                 ~ "5",   #Factor
               . == "La mayoría de las veces" ~ "4", 
               . == "Algunas veces"           ~ "3", 
               . == "Nunca"                   ~ "2", 
               . == "No aplica"               ~ "1", 
               TRUE ~ "0"
             ))

data_isp_2 %>% glimpse()

#Transformacion de variables  ####
data_isp_2 = data_isp_2 %>%
  mutate(g4.1=as.numeric(g4.1)) %>% 
  mutate(g4.2=as.numeric(g4.2)) %>% 
  mutate(g4.3=as.numeric(g4.3))

data_isp_2 = data_isp_2 %>%
  mutate(cuidado_directo = g4.1 + g4.2 + g4.3)%>% 
  mutate(cuidado_directo=as.factor(cuidado_directo)) 

data_isp_2 %>% count(cuidado_directo)

# Cuidados indirecto
data_isp_2 = data_isp_2 %>%
  mutate(g4.4=as.numeric(g4.4)) %>% 
  mutate(g4.5=as.numeric(g4.5)) 

data_isp_2 = data_isp_2 %>%
  mutate(cuidado_indirecto = g4.4 + g4.5) %>% 
  mutate(cuidado_indirecto=as.factor(cuidado_indirecto)) 
class(data_isp_2$cuidado_indirecto)

data_isp_2 %>% count(cuidado_indirecto)

#Gestion de los cuidados

data_isp_2 = data_isp_2 %>% 
  rename(gestión_cuidados="g4.6")

data_isp_2 %>% count(gestión_cuidados)
data_isp_2 %>% glimpse()


#########

mergesvy <- data_isp_2 %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  dplyr::group_by(cuidado_directo) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1 = df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=cuidado_directo) %>% 
  mutate(variable="cuidado_directo")

##indirecto
table_freq_3 <- mergesvy %>%
  dplyr::group_by(cuidado_indirecto) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2 = df_2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=cuidado_indirecto) %>% 
  mutate(variable="cuidado_indirecto")

##gestion
table_freq_4 <- mergesvy %>%
  dplyr::group_by(gestión_cuidados) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4=data.frame(table_freq_4, digits=2)
#summary(factor(df$proportion))

df_4 = df_4 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=gestión_cuidados) %>% 
  mutate(variable="gestión_cuidados")


df_final_c <- rbind(df_1, df_2, df_4)

class(df_final_c)

order_arg = ggplot(df_final_c, aes(x=g,y=proportion)) +
  geom_text(aes(label = p), position = position_dodge(0.3), vjust=-0.8, size = 3)+
  facet_wrap(~variable, scales = "free_x")+
  theme(axis.text.x = element_text(angle =45, hjust = 1)) +
  labs(x = "Response", y = "%") +
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent)+
  geom_col()

##Indices ft edad

mergesvy <- data_isp_2 %>%
  as_survey_design(
    weights = pond1)

data_isp_2 %>% glimpse()

table_freq_02 <- mergesvy %>%
  dplyr::group_by(cuidado_directo, edad_cat_recode) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1 = df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=cuidado_directo) %>% 
  mutate(variable="cuidado_directo") %>% 
  mutate(edad=edad_cat_recode)  
  
##indirecto
table_freq_3 <- mergesvy %>%
  dplyr::group_by(cuidado_indirecto, edad_cat_recode) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2 = df_2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=cuidado_indirecto) %>% 
  mutate(variable="cuidado_indirecto") %>% 
  mutate(edad=edad_cat_recode)  

##gestion
table_freq_4 <- mergesvy %>%
  dplyr::group_by(gestión_cuidados, edad_cat_recode) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4=data.frame(table_freq_4, digits=2)
#summary(factor(df$proportion))

df_4 = df_4 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=gestión_cuidados) %>% 
  mutate(variable="gestión_cuidados")%>% 
  mutate(edad=edad_cat_recode)  

df_final_c <- rbind(df_1, df_2, df_4)

order_col = ggplot(df_final_c, aes(x=g,y=proportion,  fill=edad)) +
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.3), vjust=-0.8, size = 3)+
  facet_wrap(~variable, scales = "free_x")+
  theme(axis.text.x = element_text(angle =45, hjust = 1)) +
  labs(x = "Response", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e05e5a"))+
  scale_y_continuous(limits = c(0, 0.9), labels = scales::percent)


##Indices ft género
mergesvy <- data_isp_2 %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(cuidado_directo, a1_sexo) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1 = df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=cuidado_directo) %>% 
  mutate(variable="cuidado_directo") %>% 
  rename(género=a1_sexo)

##indirecto
table_freq_3 <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(cuidado_indirecto, a1_sexo) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2 = df_2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=cuidado_indirecto) %>% 
  mutate(variable="cuidado_indirecto")%>% 
  rename(género=a1_sexo)

##gestion
table_freq_4 <- mergesvy %>%
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(gestión_cuidados, a1_sexo) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4=data.frame(table_freq_4, digits=2)
#summary(factor(df$proportion))

df_4 = df_4 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=gestión_cuidados) %>% 
  mutate(variable="gestión_cuidados")%>% 
  rename(género=a1_sexo)


df_final_c <- rbind(df_1, df_2, df_4)

order_col = ggplot(df_final_c, aes(x=g,y=proportion,  fill=género)) +
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.3), vjust=-0.8, size = 3)+
  facet_wrap(~variable, scales = "free_x")+
  theme(axis.text.x = element_text(angle =45, hjust = 1)) +
  labs(x = "Response", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e05e5a"))+
  scale_y_continuous(limits = c(0, 0.9), labels = scales::percent)


##Indices ft etnia

data_isp_2 %>% count(a5_etnia)

mergesvy <- data_isp_2 %>%
  as_survey_design(
    weights = pond1)

data_isp_2 %>% glimpse()

table_freq_02 <- mergesvy %>%
  dplyr::group_by(cuidado_directo, a5_etnia) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1_e=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1_e = df_1_e %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=cuidado_directo) %>% 
  mutate(variable="cuidado_directo")%>% 
  mutate(etnia=a5_etnia)

##indirecto
table_freq_3 <- mergesvy %>%
  dplyr::group_by(cuidado_indirecto, a5_etnia) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2_e=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2_e = df_2_e %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=cuidado_indirecto) %>% 
  mutate(variable="cuidado_indirecto") %>% 
  mutate(etnia=a5_etnia)

##gestion
table_freq_4 <- mergesvy %>%
  dplyr::group_by(gestión_cuidados, a5_etnia) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_e=data.frame(table_freq_4, digits=2)
#summary(factor(df$proportion))

df_4_e = df_4_e %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=gestión_cuidados) %>% 
  mutate(variable="gestión_cuidados") %>% 
  mutate(etnia=a5_etnia)

df_final_c <- rbind(df_1_e, df_2_e, df_4_e)

order_col = ggplot(df_final_c, aes(x=g,y=proportion,  fill=etnia)) +
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.3), vjust=-0.8, size = 3)+
  facet_wrap(~variable, scales = "free_x")+
  theme(axis.text.x = element_text(angle =45, hjust = 1)) +
  labs(x = "Response", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e05e5a"))+
  scale_y_continuous(limits = c(0, 0.9), labels = scales::percent)


##Indices ft modalidad

data_isp_2 %>% count(d5_mod)

mergesvy <- data_isp_2 %>%
  as_survey_design(
    weights = pond1)

data_isp_2 %>% glimpse()

table_freq_02 <- mergesvy %>%
  dplyr::group_by(cuidado_directo, d5_mod) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1_d=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1_d = df_1_d %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=cuidado_directo) %>% 
  mutate(variable="cuidado_directo")%>% 
  mutate(modalidad=d5_mod)

##indirecto
table_freq_3 <- mergesvy %>%
  dplyr::group_by(cuidado_indirecto, d5_mod) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2_d=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2_d = df_2_d %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=cuidado_indirecto) %>% 
  mutate(variable="cuidado_indirecto") %>% 
  mutate(modalidad=d5_mod)

##gestion
table_freq_4 <- mergesvy %>%
  dplyr::group_by(gestión_cuidados, d5_mod) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_d=data.frame(table_freq_4, digits=2)
#summary(factor(df$proportion))

df_4_d = df_4_d %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=gestión_cuidados) %>% 
  mutate(variable="gestión_cuidados") %>% 
  mutate(modalidad=d5_mod)

df_final_c <- rbind(df_1_d, df_2_d, df_4_d)


order_coel = ggplot(df_final_c, aes(x=g,y=proportion,  fill=modalidad)) +
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 3, angle =90)+
  facet_grid(variable ~ modalidad, scales = "free")+
  theme(axis.text.x = element_text(angle =90, hjust = 0)) +
  labs(x = "Response", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e05e5a","#d3d3d3"))+
  scale_y_continuous(limits = c(0, 0.9), labels = scales::percent)



