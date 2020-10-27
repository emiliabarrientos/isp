
#######################
#Catalina Miranda
##Informe descriptivo ISP
## 4 de octubre 2020


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


mergesvy <- data_isp %>%
  as_survey_design(
    weights = pond1)
#i1.1
table_freq_06_n <- mergesvy %>%
  filter(!is.na(i3.1)) %>%
  dplyr::group_by(i3.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_n=data.frame(table_freq_06_n, digits=2)
#summary(factor(df$proportion))

df_4_n = df_4_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=i3.1) %>% 
  mutate(variable="i3.1") 

#i2.2
table_freq_07_n <- mergesvy %>%
  filter(!is.na(i3.2)) %>%
  dplyr::group_by(i3.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5_n=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df_5_n = df_5_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=i3.2) %>% 
  mutate(variable="i3.2") 

#i2.3
table_freq_00_n <- mergesvy %>%
  filter(!is.na(i3.3)) %>%
  dplyr::group_by(i3.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n=data.frame(table_freq_00_n, digits=2)
#summary(factor(df$proportion))

df_7_n = df_7_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=i3.3) %>% 
  mutate(variable="i3.3") 
  
  #i1.4
  table_freq_200_n <- mergesvy %>%
  filter(!is.na(i3.4)) %>%
  dplyr::group_by(i3.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_n2=data.frame(table_freq_200_n, digits=2)
#summary(factor(df$proportion))

df_7_n2 = df_7_n2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))%>% 
  rename(g=i3.4) %>% 
  mutate(variable="i3.4") 


#i1.5
table_freq_020_n <- mergesvy %>%
  filter(!is.na(i3.5)) %>%
  dplyr::group_by(i3.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_2n=data.frame(table_freq_020_n, digits=2)
#summary(factor(df$proportion))

df_7_2n = df_7_2n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))%>% 
  rename(g=i3.5) %>% 
  mutate(variable="i3.5") 

#i1.6
table_freq_005_n <- mergesvy %>%
  filter(!is.na(i3.6)) %>%
  dplyr::group_by(i3.6) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7_1n=data.frame(table_freq_005_n, digits=2)
#summary(factor(df$proportion))

df_7_1n = df_7_1n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2))%>% 
  rename(g=i3.6) %>% 
  mutate(variable="i3.6") 

df_final <- rbind(df_4_n, df_5_n, df_7_n, df_7_2n, df_7_n2, df_7_1n)

df_final$variable = factor(df_final$variable, labels=c("Reuniones", "Atención usuarios","Resolución problemas", "Llamados u organizar archivos", "Digitar o llenar fichas", "Tareas complejas"))


df_final %>% 
  ggplot(aes(x=g, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0.5, hjust = 0, size = 4.5, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0, size = 11)) +
  facet_wrap(~variable)+
  labs(x = "Habilidades que se pueden teletrabajar", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e3dac9", "#d3d3d3"))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)


T1.aff.9<-freq(data_isp$modalidad, weights =data_isp$pond1, useNA = c("no"),  round.digits = 2)
T2.aff.9<-freq(data_isp$i2, weights =data_isp$pond1, useNA = c("no"),  round.digits = 2)


T2.eeaff.9<-freq(data_isp$d4., weights =data_isp$pond1, useNA = c("no"),  round.digits = 2)


T1.7<-ctable(data_isp_1$a1_sexo, data_isp_1$d4.1, weights = data_isp_1$pond1, useNA = c("no"), round.digits = 2)
