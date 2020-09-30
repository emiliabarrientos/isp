#ÍTEMS DE CUIDADO

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

data_isp_1 <- data_isp %>%
  mutate_at( c("g4.1", "g4.2", "g4.3", "g4.4", "g4.5","g4.6"),
             ~ case_when(
               . == "Siempre"                 ~ "5",   #Factor
               . == "La mayoría de las veces" ~ "4", 
               . == "Algunas veces"           ~ "3", 
               . == "Nunca"                   ~ "2", 
               . == "No aplica"               ~ "1", 
               TRUE ~ as.character(.)))%>%
  filter(!is.na(g4.1)) %>% 
  filter(!is.na(g4.2)) %>% 
  filter(!is.na(g4.3)) %>% 
  filter(!is.na(g4.4)) %>% 
  filter(!is.na(g4.5)) %>% 
  filter(!is.na(g4.6)) 

data_isp %>% glimpse()
data_isp_1 %>% group_by (a0_pais) %>% count(g4.4) %>% View
data_isp_1 %>% count(g4.1)
data_isp_1 %>% count(sexo_recode)

# g4.1  ####
mergesvy <- data_isp_1 %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  dplyr::group_by(a0_pais, g4.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.1, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.1) %>% 
  mutate(variable="g4.1")

#G4.2
table_freq_03 <- mergesvy %>%
  dplyr::group_by(a0_pais, g4.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1=data.frame(table_freq_03, digits=2)
#summary(factor(df$proportion))

df_1 = df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.2, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.2) %>% 
  mutate(variable="g4.2")

#G4.3
table_freq_04 <- mergesvy %>%
  dplyr::group_by(a0_pais, g4.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2=data.frame(table_freq_04, digits=2)
#summary(factor(df$proportion))

df_2 = df_2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.3, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.3) %>% 
  mutate(variable="g4.3")

#G4.4
table_freq_05 <- mergesvy %>%
  filter(a0_pais != "Argentina" & a0_pais != "Colombia" & a0_pais != "Perú") %>% 
  dplyr::group_by(a0_pais, g4.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_3=data.frame(table_freq_05, digits=2)
#summary(factor(df$proportion))

df_3 = df_3 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.4, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.4) %>% 
  mutate(variable="g4.4")

table_freq_10 <- mergesvy %>%
  filter(a0_pais != "Ecuador" & a0_pais != "Costa Rica" & a0_pais != "Brasil" & a0_pais != "México") %>% 
  dplyr::group_by(a0_pais, g4.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_7=data.frame(table_freq_10, digits=2)
#summary(factor(df$proportion))

df_7 = df_7 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.4, labels=c("Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.4) %>% 
  mutate(variable="g4.4")

#G4.5
table_freq_06 <- mergesvy %>%
  filter(a0_pais != "Argentina" & a0_pais != "Perú") %>% 
  dplyr::group_by(a0_pais, g4.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4=data.frame(table_freq_06, digits=2)
#summary(factor(df$proportion))

df_4 = df_4 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.5, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.5) %>% 
  mutate(variable="g4.5")

table_freq_08 <- mergesvy %>%
  filter(a0_pais=="Argentina") %>% 
dplyr::group_by(a0_pais, g4.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_8=data.frame(table_freq_08, digits=2)
#summary(factor(df$proportion))

df_8 = df_8 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.5, labels=c("Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.5) %>% 
  mutate(variable="g4.5")


table_freq_09 <- mergesvy %>%
  filter(a0_pais=="Perú") %>% 
  dplyr::group_by(a0_pais, g4.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_9=data.frame(table_freq_09, digits=2)
#summary(factor(df$proportion))

df_9 = df_9 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.5, labels=c("Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.5) %>% 
  mutate(variable="g4.5")

data_isp %>% group_by(a0_pais) %>% count(g4.5) %>% View

#G4.6
table_freq_07 <- mergesvy %>%
  dplyr::group_by(a0_pais, g4.6) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5=data.frame(table_freq_07, digits=2)
#summary(factor(df$proportion))

df_5 = df_5 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.6, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.6) %>% 
  mutate(variable="g4.6")

df_final <- rbind(df, df_1, df_2, df_3, df_4, df_5, df_7, df_8, df_9)

order_arg=ggplot(df_final, aes(label, proportion)) +
  geom_text(aes(label = p), position = position_dodge(0.3), vjust=-0.1, size = 2.5)+
  facet_grid(variable~a0_pais)+
  theme(axis.text.x = element_text(angle =45, hjust = 1)) +
  labs(x = "Response", y = "%") +
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent)+
  geom_col()

#################################
# g4.1  ####
mergesvy <- data_isp_1 %>%
  as_survey_design(
    weights = pond1)

table_freq_02_n <- mergesvy %>%
  dplyr::group_by(g4.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_n=data.frame(table_freq_02_n, digits=2)
#summary(factor(df$proportion))

df_n= df_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.1, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.1) %>% 
  mutate(variable="g4.1")

#G4.2
table_freq_03_n <- mergesvy %>%
  dplyr::group_by(g4.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1_n=data.frame(table_freq_03_n, digits=2)
#summary(factor(df$proportion))

df_1_n = df_1_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.2, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.2) %>% 
  mutate(variable="g4.2")

#G4.3
table_freq_04_n <- mergesvy %>%
  dplyr::group_by(g4.3) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2_n=data.frame(table_freq_04_n, digits=2)
#summary(factor(df$proportion))

df_2_n = df_2_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.3, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.3) %>% 
  mutate(variable="g4.3")

#G4.4
table_freq_05_n <- mergesvy %>%
  dplyr::group_by(g4.4) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_3_n=data.frame(table_freq_05_n, digits=2)
#summary(factor(df$proportion))

df_3_n = df_3_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.4, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.4) %>% 
  mutate(variable="g4.4")

#G4.5
table_freq_06_n <- mergesvy %>%
  dplyr::group_by(g4.5) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_4_n=data.frame(table_freq_06_n, digits=2)
#summary(factor(df$proportion))

df_4_n = df_4_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.5, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.5) %>% 
  mutate(variable="g4.5")

#G4.6
table_freq_07_n <- mergesvy %>%
  dplyr::group_by(g4.6) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_5_n=data.frame(table_freq_07_n, digits=2)
#summary(factor(df$proportion))

df_5_n = df_5_n %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>%
  mutate(label=factor(g4.6, labels=c("No aplica", "Nunca", "Algunas veces", "La mayoría de las veces", "Siempre"))) %>% 
  rename(g=g4.6) %>% 
  mutate(variable="g4.6")

df_final <- rbind(df_n, df_1_n, df_2_n, df_3_n, df_4_n, df_5_n)

order_arg=ggplot(df_final, aes(label, proportion)) +
  geom_text(aes(label = p), position = position_dodge(0.3), vjust=-0.1, size = 5)+
  facet_wrap(~variable)+
  theme(axis.text.x = element_text(angle =45, hjust = 1)) +
  labs(x = "Response", y = "%") +
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent)+
  geom_col()
