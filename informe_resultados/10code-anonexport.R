##### Anonimizacion y etiquetado ##########

#1. Cargar paquetes 
pacman::p_load(dplyr,
               tidyverse, 
               labelVector)

#2. Cargar base de datos
load("informe_resultados/data/ISP_val.RData")
table(ISP_val$d5_mod)
#3. Explorar base de datos
as_tibble(ISP_val)

#4. Anonimización ------

#4.1 Publico
# Para trabajo de nosotros la unica variable que será eliminada será email, hora y dia
names(ISP_val)
ISP_anonima <- ISP_val %>% select(-c(respondent_id,date,hour,mail, ip_address))
ISP_anonima<- ISP_anonima %>% select(starts_with(c("a","b", "c", "d", "e", "f", "g", "h", "i")))
# La base de datos que será entregada a ISP debería contener mas variables anonimizadas
## Email, ocupación y el otro de sindicatos
names(ISP_anonima)
# 5. Etiquetar
get_label(ISP)
isp <- 
  set_label(ISP_anonima,
            a0_país = "País",
             a1_sexo = "Genero",
             a2_edad = "Edad",
            edad_cat = "Tramos edad",
             a3_educ = "Nivel educacional",
            a5_etnia = "Etnia",
             a6.1 = "Nº de personas viven en hogar",      
             a6.2 = "Nº personas poblacion de riesgo",
             a6.3 =   "Nº personas estudiantes",       
             b1 = "Sector", 
             b2_reg = "Region de trabajo",    
             b2_comuna = "Comuna de trabajo",
             b3 = "Estamento, profesion, cargo",
             b4_isco = "Ocupacion - codISCO08",       
             b4_isco_cod = "Ocupacion - ISCO08",
             b5 = "Asociacion-Sindicato",         
             b6 = "Modalidad contractual",           
             b7 = "Jornada laboral",           
             b8 = "Nº horas trabajo antes de crisis",          
             b9 = "Nº horas trabajo - post crisis",         
             b10 = "Salario",          
             c1 = "Sistema afiliacion salud",          
             d1.1 = "Velocidad trabajo",         
             d1.2 = "Cantidad trabajo",          
             d1.3 = "Organizacion trabajo",         
             d1.4 = "Tiempo de descanso",          
             d2.1 = "Trabajo fuera de jornada",         
             d2.2 = "Trabajo fin de semana",          
             d3   = "Flexibilidad salarial",          
             d4.1 = "Velocidad trabajo - post crisis",           
             d4.2 = "Cantidad trabajo - post crisis",          
             d4.3 = "Organizacion trabajo - post crisis",           
             d4.4 = "Tiempo de descanso - post crisis",          
             d4.5 = "Trabajo fuera de jornada - post crisis",           
             d4.6 = "Trabajo fin de semana - post crisis",          
             d4.7 = " Flexibilidad salarila-post crisis",         
             d5_mod = "Modalidad de trabajo",    
             e1    = "N° personas en hogar durante teletrabajo",            
             e2.1 = "Uso Computador de escritorio o notebook",         
             e2.2 = "Uso Celular",          
             e2.3 = "Uso Conexión a Internet",         
             e2.4 = "Uso Escritorio o mesa",          
             e3.1 = "Propiedad Computador de escritorio o notebook",         
             e3.2 = "Propiedad Celular",          
             e3.3 = "Propiedad Conexión a Internet",         
             e4.1 = "Calidad Computador de escritorio o notebook",          
             e4.2 = "Calidad Celular",         
             e4.3 = "Calidad Internet",          
             e4.4 = "Calidad Silla",         
             e4.5 = "Calidad Escritorio o mesa",          
             e5.1 = "Apoyo empleador - organizar su trabajo y jornada en el hogar",         
             e5.2 = "Apoyo empleador - seguridad e higiene en el hogar",          
             e5.3 = "Apoyo empleador - accidente profesional",         
             e6.1_teams    = "Plataforma - Teams ", 
             e6.1_zoom     = "Plataforma - Zoom",
             e6.1_meet     = "Plataforma - Meet",
             e6.1_intranet = "Plataforma - Intranet",
             e6.1_whatsapp = "Plataforma - Whats app",
             e6.1_yammer   = "Plataforma - Yammer",
             e6.1_duo      = "Plataforma - Duo",
             e6.1_otro     = "Plataforma - Otro",
             e6.2          = "Plataforma - N° horas",
             e6.3_vpn      = "Problemas - acceso VPN",
             e6.3_desc     = " Problemas - Desconexion",
             e7   = "Lugar de teletrabajo",        
             f1.1   = "Medidas de seguridad - Prevencion de contagios",          
             f1.2   = "Medidas de seguridad - Liberar jornada ser pob. riesgo",         
             f1.3   = "Medidas de seguridad - Liberar jornada fam. pob. riesgo",          
             f1.4   = "Medidas de seguridad - Horario ingreso/salida",         
             f1.5   = "Medidas de seguridad - Horario reducido at.publico",          
             f1.6   = "Medidas de seguridad - Separacion puestos trabajo",         
             f1.7   = "Medidas de seguridad - Sanitizacion instalaciones",          
             f1.8   = "Medidas de seguridad - Sanitizacion calzado",         
             f1.9   = "Medidas de seguridad - Control de temperatura",          
             f1.10  = "Medidas de seguridad - Uso mascarilla",       
             f1.11  = "Medidas de seguridad - Uso guantes",       
             f1.12  = "Medidas de seguridad - Uso protectores",      
             f1.13  = "Medidas de seguridad - Lavado manos",     
             f1.14  = "Medidas de seguridad - Vidrios at.publico",    
             f1.15  = "Medidas de seguridad - Distancia at.publico",     
             f2.1   = "Problemas - atencion publico",    
             f2.2   = "Problemas - mas tiempo trabajo",   
             f2.3   = "Problemas - manipular herramientas",   
             f2.4   = "Problemas - irritacion piel",    
             f3.1   = "Riesgo - incumplimiento medidas funcionarios ",   
             f3.2   = "Riesgo - incumplimiento medidad usuarios",   
             f3.3   = "Riesgo - violencia jefatura",     
             f3.4   = "Riesgo - violencia usuarios",     
             f3.5   = "Riesgo - violencia fisica usuarios",      
             f3.6   = "Riesgo - riesgo de contagio COVID",       
             g1     = "Labor de trabajo domestico",    
             g2.1   = "Durante el trabajo, ¿piensa en las exigencias domésticas y familiares?",      
             g2.2   = "Las tareas del hogar interfieren en trabajo",      
             g2.3   = "El cuidado de algún miembro del hogar interfiere en trabajo",        
             g2.4   = "Las actividades otros miembros hogar interfieren en la realización trabajo",      
             g3.1   = "Conflicto - vecinos",        
             g3.2   = "Conflicto - miembro hogar",      
             g3.3   = "Conflicto - violencia miembro hogar",        
             g3.4   = "Conflicto - violencia psicologica",      
             g3.5   = "Conflicto - violencia fisica",        
             g4     = "Cantidad de trabajo domestico - post crisis",    
             h1.1   = "Intensidad - velocidad",      
             h1.2   = "Intensidad - organizacion trabajo",  
             h1.3   = "Intensidad - tiempo finalizar trabajo",      
             h1.4   = "Intensidad - tiempo entrega trabajo", 
             h1.5   = "Intensidad - tranquilidad entrega trabajo",      
             h1.6   = "Intensidad - tiempo suficiente trabajo", 
             h1.7   = "Intensidad - quedarse salida trabajo",      
             h2.1   = "Salud mental - nervios", 
             h2.2   = "Salud mental - animo",      
             h2.3   = "Salud mental - tranquilo", 
             h2.4   = "Salud mental - triste",      
             h2.5   = "Salud mental - feliz",  
             h3.1   = "Salud fisica - tension muscular",      
             h3.2   = "Salud fisica - percepcion molestias",  
             h3.3   = "Salud fisica - palpitaciones",      
             i1.1   = "Tareas teletrabajables - reuniones",  
             i1.2   = "Tareas teletrabajables - atencion usuarios",      
             i1.3   = "Tareas teletrabajables - informacion proveedores",  
             i1.4   = "Tareas teletrabajables - tareas simples sin concentracion",      
             i1.5   = "Tareas teletrabajables - tareas simples con concentracion",    
             i1.6   = "Tareas teletrabajables - tareas complejas",        
             i2 = "Modalidad de trabajo preferida")

isp <- ISP_anonima
# 6. Exportacion
#RData
save(isp, file ="informe_resultados/data/isp.RData")
write.csv(isp, file ="informe_resultados/data/isp.csv")
writexl::write_xlsx(isp, "informe_resultados/data/isp.xlsx")
