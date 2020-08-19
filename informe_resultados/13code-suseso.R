#0. Recodificar SUSESO

# 1. Cargar librerias 
pacman::p_load(dplyr, tidyverse, forcats)

# 2. Cargar base de datos
load("data/isp3.RData")

# 3. Crear Riesgo psicosocial
# Niveles: Riesgo bajo, riesgo medio, riesgo alto
## Se rescalan sumando 7 pues Likert parte en 1 y no en 0

# 3.1 Calculo :sum(puntaje obtenido)/ puntaje maximo * 100
# Puntaje máximo posible: 28 + 7= 35
# Sumar likert donde 0= Nunca y 5 = Siempre
# Sumar variables h1.1 a h1.7 para cada individuo

isp <- isp %>% 
  mutate(sum_rps = rowSums(select(., contains("h1.")))) %>% 
  mutate(rps = (sum_rps/35)*100) %>% mutate(n_rps = case_when(rps < 35.56 ~ 1,
                                                              rps > 35.57 & rps <49.85 ~ 2,
                                                              rps > 49.86 ~ 3, 
                                                              TRUE ~ NA_real_))
summary(isp$rps)

isp$n_rps<-labelled_spss(isp$n_rps, c("Bajo" = 1, "Medio" = 2, "Alto" = 3),
                        label = "Exigencias cuantitativas")


# Una vez calculado el nivel de riesgo por cada trabajador se calcula la prevalencia de riesgo
#Se muestra con un grafico de barras apiladas calculando %

# 3.2 Salud Mental (SM, pag 20)
# Sumatoria Item/20*100 --> Es 20 pues Likert va de 0 a 4
# En consecuencia sera sum(item)/25*100

#3.2.1 Reverse factor (h2.1, h2.2, h.2.4)
isp$h2.1 <- as_factor(isp$h2.1)
isp$h2.1 <- fct_rev(isp$h2.1)
isp$h2.1 <- as.numeric(isp$h2.1)

isp$h2.2 <- as_factor(isp$h2.2)
isp$h2.2 <- fct_rev(isp$h2.2)
isp$h2.2 <- as.numeric(isp$h2.2)

isp$h2.4 <- as_factor(isp$h2.4)
isp$h2.4 <- fct_rev(isp$h2.4)
isp$h2.4 <- as.numeric(isp$h2.4)


# 3.2.2 Crear variable
isp <- isp %>% 
  mutate(sum_sm = rowSums(select(., contains("h2.")))) %>% 
  mutate(sm = (sum_sm/25)*100) 

# Un mayor puntaje significa mayor bienestar

#Guardar ----
save(isp,file= "data/isp3.RData")
