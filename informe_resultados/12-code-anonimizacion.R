##### Anonimizacion para ISP ##########

#1. Cargar paquetes 
pacman::p_load(dplyr,
               tidyverse, 
               labelVector,
               haven)

#2. Cargar base de datos
load("data/isp.RData")

#3 Publico
# Para trabajo de nosotros la unica variable que será eliminada será email, hora y dia
ISP_anonima<- isp %>% select(starts_with(c("a","b", "c", "d", "e", "f", "g", "h", "i", "p")))
# La base de datos que será entregada a ISP debería contener mas variables anonimizadas
## Email, ocupación y el otro de sindicatos
isp_cl2020 <-ISP_anonima %>%  select(-c(b4_isco_cod, b4_isco))


# 4. Guardar
#RData
save(isp_cl2020, file ="data/ISP_CL2020.RData")
write.csv(isp_cl2020, file ="data/ISP_CL2020.csv")
writexl::write_xlsx(isp_cl2020, "data/ISP_CL2020.xlsx")
write_sav(isp_cl2020, "data/ISP_CL2020.sav")
