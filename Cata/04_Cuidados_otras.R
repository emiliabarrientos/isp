#######################
#Catalina Miranda
##Informe descriptivo ISP
## 18 de septiembre 2020

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
data_isp_c <- readRDS("C:/Users/catac/Dropbox/Cata/Github/isp/informe_resultados/data/data_isp.rds")
data_isp %>% glimpse()

data_isp %>% count(g2)

#Trabajo de cuidados en el hogar

data_isp_c = data_isp_c %>% 
  filter(!is.na(g2)) 
T1.1<-freq(data_isp_c$g2, weights = data_isp_c$pond1, useNA = c("no"),  round.digits = 2)

pond_1 <- data_isp_c %>% pull(pond1)
T1.7<-ctable(data_isp_c$g2, data_isp_c$a1_sexo, weights = pond_1, round.digits = 2)

pond_1 <- data_isp_c %>% pull(pond1)
T1.8<-ctable(data_isp_c$g2, data_isp_c$a5_etnia , weights = pond_1, round.digits = 2)

data_isp_c %>%glimpse ()

pond_1 <- data_isp_c %>% pull(pond1)
T1.9<-ctable(data_isp_c$g2, data_isp_c$modalidad , weights = pond_1, round.digits = 2)

#### Género


mergesvy <- data_isp_c %>%
  as_survey_design(
    weights = pond1)

table_freq_01 <- mergesvy %>% 
  filter(!is.na(g2)) %>% 
  filter(a1_sexo!="Otro") %>% 
  dplyr::group_by(g2,a0_pais, a1_sexo) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df=data.frame(table_freq_01, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(género=a1_sexo) %>% 
  mutate(`Trabajo de cuidado`=g2)

df %>% 
  ggplot(aes(x=género, y=proportion, fill=`Trabajo de cuidado`))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 3, angle =90)+
  facet_wrap(~a0_pais)+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)+
  scale_fill_manual(values = c("#d40c04", "#e05e5a", "#d3d3d3", "#fee4e2","#e3dac9"))
 
#Trabajo y modalidad 
mergesvy <- data_isp_c %>%
  as_survey_design(
    weights = pond1)

table_freq_01 <- mergesvy %>% 
  filter(!is.na(g2)) %>% 
  dplyr::group_by(g2, d5_mod) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
#help(srvyr)

df=data.frame(table_freq_01, digits=2)
#summary(factor(df$proportion))

df= df %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(modalidad=d5_mod) %>% 
  mutate(`Trabajo de cuidado`=g2)

  
df %>% 
  ggplot(aes(x=modalidad, y=proportion, fill=`Trabajo de cuidado`))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 4, angle =90)+
  theme(axis.text.x = element_text(angle =90, hjust = 0)) +
  labs(x = "Modalidad", y = "%") +
  scale_fill_manual(values = c("#d40c04", "#e05e5a", "#d3d3d3", "#fee4e2","#e3dac9"))+
  scale_y_continuous(limits = c(0, 0.9), labels = scales::percent)


#Red de apoyo
data_isp_c <- data_isp_c %>%
  mutate_at( c("g6.1_abuelo", "g6.1_otro", "g6.1_guarderia", "g6.1_escuela", "g6.1_tcp","g6.1_ninguno","g6.2_abuelo", "g6.2_otro", "g6.2_guarderia", "g6.2_escuela", "g6.2_tcp","g6.2_ninguno"),
             ~ case_when(
               . == "Si"  ~ "1",   #Factor
               . == "No"  ~ "0"
             ))

data_isp_c = data_isp_c %>%
  mutate(g6.1_abuelo=as.numeric(g6.1_abuelo)) %>% 
  mutate(g6.2_abuelo=as.numeric(g6.2_abuelo)) %>% 
  mutate(g6.1_otro=as.numeric(g6.1_otro)) %>% 
  mutate(g6.2_otro=as.numeric(g6.2_otro)) %>% 
  mutate(g6.1_guarderia=as.numeric(g6.1_guarderia)) %>% 
  mutate(g6.2_guarderia=as.numeric(g6.2_guarderia)) %>% 
  mutate(g6.1_escuela=as.numeric(g6.1_escuela)) %>% 
  mutate(g6.2_escuela=as.numeric(g6.2_escuela)) %>% 
  mutate(g6.2_guarderia=as.numeric(g6.2_guarderia)) %>% 
  mutate(g6.1_tcp=as.numeric(g6.1_tcp)) %>% 
  mutate(g6.2_tcp=as.numeric(g6.2_tcp)) %>% 
  mutate(g6.1_ninguno=as.numeric(g6.1_ninguno)) %>% 
  mutate(g6.2_ninguno=as.numeric(g6.2_ninguno))

data_isp_c = data_isp_c %>%
  mutate(red_apoyo_pre = g6.1_abuelo + g6.1_otro+g6.1_guarderia+g6.1_escuela+g6.1_tcp+g6.1_ninguno)%>% 
  mutate(red_apoyo_pre=as.factor(red_apoyo_pre)) 

data_isp_c = data_isp_c %>%
  mutate(red_apoyo_post = g6.2_abuelo + g6.2_otro+g6.2_guarderia+g6.2_escuela+g6.2_tcp+g6.2_ninguno)%>% 
  mutate(red_apoyo_post=as.factor(red_apoyo_post))


data_isp_c %>% count(red_apoyo_pre)
data_isp_c %>% count(red_apoyo_post)

mergesvy <- data_isp_c %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  dplyr::group_by(red_apoyo_pre) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1 = df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=red_apoyo_pre) %>% 
  mutate(variable="red_apoyo_pre") 

##red de apoyo post 
table_freq_3 <- mergesvy %>%
  dplyr::group_by(red_apoyo_post) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2 = df_2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=red_apoyo_post) %>% 
  mutate(variable="red_apoyo_post")

df_final_post <- rbind(df_1, df_2)

df_final_post %>% count(variable)

df_final_post$variable = factor(df_final_post$variable, labels=c("red de apoyo durante la crisis sanitaria", "red de apoyo previa la crisis sanitaria"))
  
order_arg = ggplot(df_final_post, aes(x=g,y=proportion)) +
  geom_text(aes(label = p), position = position_dodge(0.3), vjust=-0.8, size = 5)+
  facet_wrap(~variable, scales = "free_x")+
  labs(x = "Respuesta", y = "%") +
  scale_y_continuous(limits = c(0, 0.7), labels = scales::percent)+
  geom_col()

#Horas de trabajo de cuidados en promedio

data_isp_c = data_isp_c %>%
  mutate(g3.1=case_when(
    g3.1 == 0  ~ "0",   #Factor
    g3.1 == 1  ~ "1", 
    g3.1 == 2  ~ "2", 
    g3.1 == 3  ~ "3", 
    g3.1 == 4  ~ "4", 
    g3.1 == 5  ~ "5", 
    g3.1 == 6  ~ "6", 
    g3.1 == 7  ~ "7", 
    g3.1 == 8 ~ "8", 
    g3.1 == 9 ~ "9", 
    g3.1 == 10 ~ "10",
    g3.1 %in% 11:22 ~ "11 o más"
  ))


data_isp_c = data_isp_c %>%
  mutate(g3.2=case_when(
    g3.2 == 0  ~ "0",   #Factor
    g3.2 == 1  ~ "1", 
    g3.2 == 2  ~ "2", 
    g3.2 == 3  ~ "3", 
    g3.2 == 4  ~ "4", 
    g3.2 == 5  ~ "5", 
    g3.2 == 6  ~ "6", 
    g3.2 == 7  ~ "7", 
    g3.2 == 8 ~ "8", 
    g3.2 == 9 ~ "9", 
    g3.2 == 10 ~ "10",
    g3.2 %in% 11:22 ~ "11 o más"
  ))
data_isp_c %>% count(g3.2) %>% View

data_isp_c = data_isp_c %>%
mutate(g3.1=as.factor(g3.1)) %>% 
mutate(g3.2=as.factor(g3.2)) 
  
data_isp_c %>% count(g3.1) %>% View
data_isp_c %>% count(g3.2)
       
mergesvy <- data_isp_c %>%
  as_survey_design(
    weights = pond1)

table_freq_02 <- mergesvy %>%
  filter(!is.na(g3.1)) %>%
  dplyr::group_by(g3.1) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_1=data.frame(table_freq_02, digits=2)
#summary(factor(df$proportion))

df_1 = df_1 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=g3.1) %>% 
  mutate(variable="g3.1") 

##red de apoyo post 
table_freq_3 <- mergesvy %>%
  filter(!is.na(g3.2)) %>%
  dplyr::group_by(g3.2) %>%
  summarize(proportion = survey_mean(,na.rm=TRUE))
df_2=data.frame(table_freq_3, digits=2)
#summary(factor(df$proportion))

df_2 = df_2 %>%
  mutate(perc = proportion * 100) %>%
  mutate(p=round(perc, 2)) %>% 
  rename(g=g3.2) %>% 
  mutate(variable="g3.2")

df_final_post <- rbind(df_1, df_2)

df_final_post$variable = factor(df_final_post$variable, labels=c("Horas de trabajo de cuidados previa la crisis sanitaria", "Horas de trabajo de cuidados durante la crisis sanitaria"))

order_carg = ggplot(df_final_post, aes(x= factor(g,level = c("0","1","2","3","4","5","6","7","8","9","10", "11 o más")),y=proportion)) +
  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 4, angle =90)+
  facet_wrap(~variable, scales = "free_x")+
  labs(x = "Horas en promedio al día", y = "%") +
  theme(axis.text.x = element_text(angle =90, hjust = 1)) +
  scale_y_continuous(limits = c(0, 0.3), labels = scales::percent)+
  geom_col()
