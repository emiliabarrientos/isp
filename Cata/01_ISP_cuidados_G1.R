
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

#####################
########### G1
#####################

data_isp %>% count(g1)
#g1        n
#<fct> <int>
#1 Si   541
#2 No   1937
#3 NA   434

data_isp = data_isp %>%
  mutate(g1_recode=case_when(
    g1 == "Si" ~ "Sí",   #Factor
    g1 == "No" ~ "No", 
    TRUE ~ as.character(g1) 
  )) %>% 
  filter(!is.na(g1_recode))

data_isp %>% count(g1, g1_recode)

####################################
#Descriptivos cuidados
###################################

#G1 por pais
mergesvy <- data_isp %>%
  as_survey_design(
    weights = pond1)

# frequency table via taylor series linearization
table_freq_01 <- mergesvy %>%
  dplyr::group_by(a0_pais, g1_recode) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df=data.frame(table_freq_01, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g1_recode, labels=c("No", "Sí")))

g1_graph=ggplot(df, aes(label, proportion)) +
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  facet_wrap(~ a0_pais) +
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 4, angle =90)+
  labs(x = "Respuesta", y = "%") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) 


#Tablas
T2.1<-freq(data_isp$g1_recode, weights = data_isp$pond1, useNA = c("no"),  round.digits = 2) 
T1.7<-ctable(data_isp$a0_pais, data_isp$g1_recode, weights = data_isp$pond1, useNA = c("no"), round.digits = 2)

T1.6<-ctable(data_isp$a1_sexo,data_isp$g1_recode, weights = data_isp$pond1, useNA = c("no"), round.digits = 2)
T1.99<-ctable(data_isp$a5_etnia, data_isp$g1_recode, weights = data_isp$pond1, useNA = c("no"), round.digits = 2)
T1.9<-ctable(data_isp$edad_cat, data_isp$g1_recode, weights = data_isp$pond1, useNA = c("no"), round.digits = 2)

# G1 by country / sex
mergesvy <- data_isp %>%
  as_survey_design(
    weights = pond1)

# frequency table via taylor series linearization
table_freq_01 <- mergesvy %>%
  dplyr::group_by(a0_pais, g1_recode, a1_sexo) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df=data.frame(table_freq_01, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g1_recode, labels=c("No", "Sí"))) %>%
  rename(género=a1_sexo) %>%
  rename(reducción=g1_recode)

df %>% 
  ggplot(aes(x=género, y=proportion, fill=reducción))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(1), vjust=-0.3)+
  facet_wrap(~a0_pais)+
  scale_y_continuous(limits = c(0, 0.9), labels = scales::percent)+
  scale_fill_manual(values = c("#d40c04", "#e05e5a", "#d3d3d3"))


# G1 by country / age

data_isp %>% glimpse()
data_isp %>% count(edad_cat)

mergesvy <- data_isp %>%
  as_survey_design(
    weights = pond1)

# frequency table via taylor series linearization
table_freq_01 <- mergesvy %>%
  dplyr::group_by(modalidad, g1_recode) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df=data.frame(table_freq_01, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  rename(reducción=g1_recode)

df %>% 
  ggplot(aes(x=reducción, y=proportion))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(1), vjust=-0.3)+
  facet_wrap(~modalidad)+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)+
  scale_fill_manual(values = c("#d40c04", "#e05e5a", "#d3d3d3"))

# G1 por pais y modalidad de trabajo

data_isp %>% glimpse()
data_isp %>% group_by(a0_pais, g1_recode, d5_mod) %>% count()
data_isp %>% count(g1_recode)

mergesvy <- data_isp %>%
  as_survey_design(
    weights = pond1)

# frequency table via taylor series linearization
table_freq_01 <- mergesvy %>% 
  #filter(a0_pais != "Argentina") %>% 
  dplyr::group_by(modalidad, g1_recode) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df_3=data.frame(table_freq_01, digits=2)
#summary(factor(df$proportion))

df_3= df_3 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  rename(reducción=g1_recode)

df_3 %>% 
  ggplot(aes(x=reducción, y=proportion))+
  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 4, angle =90)+
  facet_wrap(~modalidad)+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)+
  labs(x = "Reducción de trabajo", y = "%") 

T1.9<-ctable(data_isp$g1_recode, data_isp$d5_mod, weights = data_isp$pond1, useNA = c("no"), round.digits = 2)
  
data_isp %>% group_by(a1_sexo, g1_recode, a6_preesc_recode) %>% tally(wt=pond1) %>% mutate(porcentaje=n/sum(n)*100, round.digits = 2)



df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%


# G1 por pais /etnia

data_isp %>% glimpse()
data_isp %>% group_by(a0_pais, g1_recode, a6_preesc_recode) %>% count() %>% View
data_isp %>% count(a5_etnia)

mergesvy <- data_isp %>%
  as_survey_design(
    weights = pond1)

# frequency table via taylor series linearization
table_freq_01 <- mergesvy %>% 
  filter(a0_pais != "Argentina") %>% 
  dplyr::group_by(a0_pais, g1_recode, a6_riesgo_recode) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df=data.frame(table_freq_01, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g1_recode, labels=c("No", "Sí"))) %>% 
  rename(edad = edad_cat_recode)

df %>% 
  ggplot(aes(x=g1_recode, y=proportion, fill=a6_riesgo_recode))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(1), vjust=-0.3)+
  facet_wrap(~a0_pais)+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)+
  scale_fill_manual(values = c("#d40c04", "#fee4e2", "#d3d3d3"))

#Chequear resultados

data_isp = data_isp %>% filter(a0_pais=="Argentina")
pond_1 <- data_isp %>% pull(pond1)
T1.7<-ctable(data_isp$g1_recode, data_isp$a1_sexo, data_isp$a6_basica_recode, weights = pond_1, round.digits = 1)
T1.7<-ctable(data_isp$g1.4, data_isp$a1_sexo, weights = pond_1, round.digits = 1)


data_isp_1 %>% group_by(g4.1) %>% tally(wt=pond1) %>% mutate(porcentaje=n/sum(n)*100)
