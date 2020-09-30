##### Missing y correccion de flujo

load("informe_resultados/data/ISP_val.RData")

#1. Combinacion de variables
# La primera correcio de flujo ocurrio en el 03-code-procdata.R 
#Pasa que personas se devolvieron en el cuestionario y pudo contestar dos modulos. Se seleccionó la ultima forma de modalidad que señalo

# 2. Seleccion multiple

# Plataforma
table(ISP_val$e6.1_teams)
table(ISP_val$e6.1_zoom)
table(ISP_val$e6.1_meet)
table(ISP_val$e6.1_yammer)
table(ISP_val$e6.1_duo)
table(ISP_val$e6.1_intranet)
table(ISP_val$e6.1_otro)

# 3. Missing por modulo
sum(is.na(ISP_val$a6.3)) #0
sum(is.na(ISP_val$d5_mod)) #0
sum(is.na(ISP_val$e7)) #910
# Modulo f
modulof <-  ISP_val %>% filter(!is.na(e7))
sum(is.na(modulof$f2.6)) #1215
# Modulo g
modulog <-  modulof %>% filter(!is.na(f2.6))
sum(is.na(modulog$g3.1)) #36

#Modulo h 
moduloh <-  modulog %>% filter(!is.na(g3.1))
sum(is.na(modulog$h3.3)) #74
#Modulo i
moduloi <-  moduloh %>% filter(!is.na(h3.3))
sum(is.na(moduloi$i2)) #42
sum(complete.cases(ISP_val))

# 4. Duplicados 
table(duplicated(ISP_val$respondent_id)) # 0
table(duplicated(ISP_val$ip_address))    
ISP_d <- ISP_val  %>% distinct(ip_address, .keep_all =  T)
ISP_val$ip_address[duplicated(ISP_val$ip_address)]
