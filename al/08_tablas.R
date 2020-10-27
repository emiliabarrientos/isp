#####TABLAS

data_isp_rc <- readRDS("C:/Users/catac/Dropbox/Cata/Github/isp/informe_resultados/data/data_isp.rds")

#A6.2
data_isp_rc %>% count(a6.2)

pond_1 <- data_isp_rc %>% pull(pond1)

T1.1<-freq(data_isp_c$a6.2, weights = data_isp_c$pond1, useNA = c("no"),  round.digits = 2)
T1.2<-freq(data_isp_c$a0_pais, data_isp_c$a6.2, weights = data_isp_c$pond_1, useNA = c("no"),  round.digits = 2)
T1.3<-ctable(data_isp_rc$a1_sexo, data_isp_rc$a6.2, weights = pond_1, useNA = c("no"), round.digits = 2)
T1.4<-ctable(data_isp_rc$a5_etnia , data_isp_rc$a6.2, weights = pond_1, useNA = c("no"), round.digits = 2)
T1.00<-ctable(data_isp_rc$edad_cat , data_isp_rc$a6.2, weights = pond_1, useNA = c("no"), round.digits = 2)
T1.5<-ctable(data_isp_rc$modalidad , data_isp_rc$a6.2, weights = pond_1, useNA = c("no"), round.digits = 2)

T1.7<-ctable(data_isp_rc$a1_sexo , data_isp_rc$a6.2.filtro, weights = pond_1, useNA = c("no"), round.digits = 2)

#Modalidad de trabajo
T1.6<-freq(data_isp_rc$modalidad, weights = data_isp_c$pond1, useNA = c("no"),  round.digits = 2)
T1.8<-freq(data_isp_rc$modalidad, data_isp_c$a0_pais, weights = data_isp_c$pond_1, useNA = c("no"), round.digits = 2)
T1.9<-ctable(data_isp_rc$modalidad, data_isp_c$a1_sexo, weights = pond_1, useNA = c("no"), round.digits = 2)
T1.10<-ctable(data_isp_rc$modalidad, data_isp_c$a5_etnia , weights = pond_1,useNA = c("no"), round.digits = 2)
T1.11<-ctable(data_isp_rc$modalidad, data_isp_c$modalidad , weights = pond_1, useNA = c("no"), round.digits = 2)


pond_1 <- data_isp_c %>% pull(pond1)
T1.5<-ctable(data_isp_c$g3.1, data_isp_c$modalidad , weights = pond_1, useNA = c("no"), round.digits = 2)

#Trabajo de cuidados (quién lo hace)
T1.5r<-ctable(data_isp_rc$a1_sexo, data_isp_rc$g2, weights = pond_1, useNA = c("no"), round.digits = 2)
T1.5sr<-ctable(data_isp_rc$a5_etnia, data_isp_rc$g2, weights = pond_1, useNA = c("no"), round.digits = 2)
T1.5ssr<-ctable(data_isp_rc$modalidad, data_isp_rc$g2, weights = pond_1, useNA = c("no"), round.digits = 2)

#Violencia

T2.1<-freq(data$g8.1 , weights = data$pond1, useNA = c("no"), round.digits = 2) #Controlar los gastos del hogar
T2.2<-freq(data$g8.2, weights = data$pond1, useNA = c("no"),  round.digits = 2) #Controlar sus gastos personales
T2.3<-freq(data$g8.3, weights = data$pond1, useNA = c("no"),  round.digits = 2) #Reducir el presupuesto mensual
T2.4<-freq(data$g7.5, weights = data$pond1, useNA = c("no"),  round.digits = 2) #Ha experimentado situaciones de violencia física
T2.5<-freq(data$g8.7, weights = data$pond1, useNA = c("no"),  round.digits = 2) #Golpes
T2.6<-freq(data$g8.8, weights = data$pond1, useNA = c("no"),  round.digits = 2) #Relaciones sexuales no consentidas
T2.7<-freq(data$g7.4, weights = data$pond1, useNA = c("no"),  round.digits = 2) #Ha experimentado situaciones de violencia psicológica
T2.8<-freq(data$g8.4, weights = data$pond1, useNA = c("no"),  round.digits = 2) #Gritos
T2.9<-freq(data$g8.5, weights = data$pond1, useNA = c("no"),  round.digits = 2) #Insultos
T2.10<-freq(data$g8.6, weights = data$pond1, useNA = c("no"),  round.digits = 2) #Amenazas

#Violencia económica / Controlar los gastos del hogar
data = data_isp_rc %>% 
filter (a1_sexo =="Femenino")

pond_2 <- data %>% pull(pond1)
T3.14<-ctable(data$a1_sexo, data$g8.1, weights = pond_2, useNA = c("no"), round.digits = 2) #Controlar los gastos del hogar
T3.15<-ctable(data$a5_etnia , data$g8.1, weights = pond_2, useNA = c("no"), round.digits = 2)#Controlar los gastos del hogar
T3.16<-ctable(data$edad_cat , data$g8.1, weights = pond_2, useNA = c("no"), round.digits = 2)#Controlar los gastos del hogar
T3.17<-ctable(data$modalidad , data$g8.1, weights = pond_2, useNA = c("no"), round.digits = 2)#Controlar los gastos del hogar


#Violencia económica / Controlar sus gastos personales

pond_2 <- data %>% pull(pond1)
T5.14<-ctable(data$a1_sexo, data$g8.2, weights = pond_2, useNA = c("no"), round.digits = 2) #Controlar sus gastos personales
T5.15<-ctable(data$a5_etnia , data$g8.2, weights = pond_2, useNA = c("no"), round.digits = 2)#Controlar sus gastos personales
T5.16<-ctable(data$edad_cat , data$g8.2, weights = pond_2, useNA = c("no"), round.digits = 2)#Controlar sus gastos personales
T5.17<-ctable(data$modalidad , data$g8.2, weights = pond_2, useNA = c("no"), round.digits = 2)#Controlar sus gastos personales

#Violencia económica / #Reducir el presupuesto mensual
pond_2 <- data %>% pull(pond1)
T4.14<-ctable(data$a1_sexo, data$g8.3, weights = pond_2, useNA = c("no"), round.digits = 2) #Reducir el presupuesto mensual
T4.15<-ctable(data$a5_etnia , data$g8.3, weights = pond_2, useNA = c("no"), round.digits = 2)#Reducir el presupuesto mensual
T4.16<-ctable(data$edad_cat , data$g8.3, weights = pond_2, useNA = c("no"), round.digits = 2)#Reducir el presupuesto mensual
T4.17<-ctable(data$modalidad , data$g8.3, weights = pond_2, useNA = c("no"), round.digits = 2)#Reducir el presupuesto mensual

#Redes de apoyo
T7.1<-freq(data_isp_rc$g6.1_abuelo , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T7.3<-freq(data_isp_rc$g6.1_otro , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T7.3<-freq(data_isp_rc$g6.1_guarderia , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T7.4<-freq(data_isp_rc$g6.1_escuela , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T7.5<-freq(data_isp_rc$g6.1_tcp , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T7.6<-freq(data_isp_rc$g6.1_ninguno , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T7.7<-freq(data_isp_rc$g6.2_abuelo , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T7.8<-freq(data_isp_rc$g6.2_otro , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T7.9<-freq(data_isp_rc$g6.2_guarderia , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T7.99<-freq(data_isp_rc$g6.2_escuela , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T7.88<-freq(data_isp_rc$g6.2_tcp , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
T7.89<-freq(data_isp_rc$g6.2_ninguno , weights = data_isp_rc$pond1, useNA = c("no"), round.digits = 2)
