#----Trabajo y Pandemia Servicios Públicos Chile----

# 0. Cargar base de datos
load(file = "data/isp.RData")

#----1. Paquetes y base de datos----

library(dplyr)
library(car)
library(summarytools)
library(ggplot2)
library(magrittr)
library(labelVector)

freq(CASEN2017P$CASEN2017_PON.estamento, weights = CASEN2017P$CASEN2017_PON.expr)
freq(isp$b3, weights = isp$pond)
freq(isp$b3, weights = isp$pond)
ctable(isp$b3, isp$a1_sexo)
#----2. Ponderadores y ajuste variables----

# Ponderadores en base a Casen con estamento y sexo

pond2<-rep(0, 7001) # variable ponderadores
isp<-cbind(isp, pond2) 

isp["pond2"][isp["a1_sexo"]=="Masculino" & isp["b3"]=="Directivo"]     <-1.734494949103
isp["pond2"][isp["a1_sexo"]=="Masculino" & isp["b3"]=="Profesional"]   <-0.860820537450
isp["pond2"][isp["a1_sexo"]=="Masculino" & isp["b3"]=="Tecnico"]       <-1.842755667712
isp["pond2"][isp["a1_sexo"]=="Masculino" & isp["b3"]=="Administrativo"]<-0.549628872918
isp["pond2"][isp["a1_sexo"]=="Masculino" & isp["b3"]=="Auxiliar"]      <-9.035993567767

isp["pond2"][isp["a1_sexo"]=="Femenino" & isp["b3"]=="Directivo"]     <-0.696075247458
isp["pond2"][isp["a1_sexo"]=="Femenino" & isp["b3"]=="Profesional"]   <-0.578729289789
isp["pond2"][isp["a1_sexo"]=="Femenino" & isp["b3"]=="Tecnico"]       <-0.931454420581
isp["pond2"][isp["a1_sexo"]=="Femenino" & isp["b3"]=="Administrativo"]<-0.519751378454
isp["pond2"][isp["a1_sexo"]=="Femenino" & isp["b3"]=="Auxiliar"]      <-7.180799887698

isp["pond2"][isp["a1_sexo"]=="Otro" & isp["b3"]=="Directivo"]     <-1
isp["pond2"][isp["a1_sexo"]=="Otro" & isp["b3"]=="Profesional"]   <-0.865985172804
isp["pond2"][isp["a1_sexo"]=="Otro" & isp["b3"]=="Tecnico"]       <-1.335037067990
isp["pond2"][isp["a1_sexo"]=="Otro" & isp["b3"]=="Administrativo"]<-1
isp["pond2"][isp["a1_sexo"]=="Otro" & isp["b3"]=="Auxiliar"]      <-

ctable(isp$b3, isp$a1_sexo, useNA = "no")
ctable(isp$b3, isp$a1_sexo, useNA = "no", weights = isp$pond2)

isp <- 
  set_label(isp,
            pond2 = "Ponderador sexo-estamento")

isp %>% select(a1_sexo, b3, pond2)
table(isp$pond2, useNA =  "ifany")
# Guardar -----------
save(isp, file = "data/isp.RData")
