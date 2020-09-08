##### Procesamiento de variables #####

# 1.Cargar librerias ----
pacman::p_load(haven,dplyr, tidyverse,lubridate)

#2. Cargar base de datos -----
ISP <- read.csv(file= "informe_resultados/data/Condiciones Laborales en Pandemia - Región Interamericana - ISP.csv")

#ISP <- read_sav(file= "informe_metodologico/data/ISP.sav")

#3. Explorar base de datos ----
dim(ISP)
names(ISP) #228 variables

## 0. Identificadores ----
# "CollectorNm"
table(ISP$CollectorNm, useNA = "ifany")
# Se eliminara

# "respondent_id"
table(ISP$respondent_id, useNA = "ifany")
str(ISP$respondent_id)

# "collector_id"
table(ISP$collector_id, useNA = "ifany")
#Se eliminara

# "date_created" 
table(ISP$date_created, useNA = "ifany")

# "date_modified"
table(ISP$date_modified, useNA = "ifany")
#Se elimina

# "ip_address"
table(ISP$ip_address, useNA = "ifany")

# "email_address"
table(ISP$email_address, useNA = "ifany")
#se elimina

# "first_name"   
table(ISP$first_name, useNA = "ifany")
#se elimina

# "last_name"
table(ISP$last_name, useNA = "ifany")
# se limina

# "custom_1"     
table(ISP$custom_1, useNA = "ifany")
#se elimina

# Eliminar variables
ISP <- ISP %>% select(- c(custom_1, last_name, first_name, email_address, collector_id, CollectorNm))
# Transformar vectores
sapply(ISP, class) # Ver tipo
ISP[1] <- sapply(ISP[1],as.numeric) # id a numerica
#ISP$date_created <- as.Date(ISP$date_created) - revisar

## A. Modulo Sociodemograficas ----
# a0_pais ("") - Pais----
ISP$a0_pais <- as.character(ISP$x)
ISP$a0_pais <- car::recode(ISP$a0_pais, recodes = c(""))

# a1_sexo ("q0001") - Sexo --------------------
ISP$q0001 <- as.factor(ISP$q0001)
ISP$a1_sexo <- car::recode(ISP$q0001, recodes = c("1='Masculino';2='Femenino';3='Otro'"), as.factor = T)
# a2_edad ("q0002") Edad   -------------        
ISP$a2_edad <- as.numeric(ISP$q0002)
ISP <- ISP %>% mutate(edad_cat = case_when(a2_edad < 36 ~ "Jóvenes",
                                           a2_edad > 36 ~ "Adultos",
                                           TRUE ~ NA_character_))
table(ISP$edad_cat)
ISP %>% select(a2_edad, edad_cat)
# a3_educ ("q0003") Educacion ----------------------
ISP$q0003 <- as.factor(ISP$q0003)
ISP$a3_educ <- car::recode(ISP$q0003, recodes = c("1='Sin estudios';2='Basica';3='Media';4='Tecnico Profesional';5='Universitaria';6='Posgrado'"), as.factor = T,
                           levels = c("Sin estudios","Basica","Media","Tecnico Profesional","Universitaria", "Posgrado"))
levels(ISP$a3_educ) #verificar
# a6.1, a6.2 y a6.3 (q0021, q0022,, q0023) - Familia ------------------ 
ISP$a6.1 <- as.numeric(ISP$q0021)
ISP$a6.2 <- as.numeric(ISP$q0022)
ISP$a6.3 <- as.numeric(ISP$q0023)

# Filtro
ISP$a6.2.filtro <- as.factor(ISP$x)
ISP$a6.2.filtro <- car::recode(ISP$a6.2.filtro, recodes = c("='Mi esposo (a) o pareja con la que vivo';='Mi esposo (a) o pareja con la que no vivo';='Ex esposo (a) o ex pareja';='Otra persona (padre, madre, hijos (as), otro familiar, otra persona)'"), as.factor = T,
                               levels = c('Mi esposo (a) o pareja con la que vivo','Mi esposo (a) o pareja con la que no vivo','Ex esposo (a) o ex pareja','Otra persona (padre, madre, hijos (as), otro familiar, otra persona)'))


# B. Modulo Empleo ------------------------------------------------------
# b1 - (q0024- q0024_other) Servicio que trabaja --------------
ISP$b <- as.character(ISP$q0024)
table(ISP$b)
#Other
ISP$q0024_other[ISP$q0024_other == ""] <- NA
table(ISP$q0024_other)# 3507 personas rellenaron con otros

#Crear funciones
clean_strings <- function( s ) {
  clz <- class( s )
  s %<>%
    gsub( "^\\s+", "" , . ) %>% # leading whitepace
    gsub( "\\s+$", "" , . ) %>% # trailing whitespace
    gsub( "\"+", "\"", . )  %>% # embedded "'s
    as( clz ) # u don't mess with the type
  return( s )
}
simplify_strings <- function( s ) {
  s %<>%
    clean_strings() %>%
    gsub( "á", "a", . ) %>%
    gsub( "é", "e", . ) %>%
    gsub( "í", "i", . ) %>%
    gsub( "ó", "o", . ) %>%
    gsub( "ú", "u", . ) %>%
    tolower()
  return( s )
}

ISP$b1.2 <- ISP$q0024_other
ISP$b1.2 <- ISP$b1.2 %>% simplify_strings()
write.csv(ISP$b1.2, file = "data/isp-sector.csv")
## 99 No responde; 88 No sabe; 85 No aplica; 77 Otros; 66 Falta Info
isp_sector <- read.csv(file = "data/isp-sector2.csv", sep = ";")
ISP <- cbind(ISP, isp_sector) 
ISP$b1_otro_cod <- as.character(ISP$b1_otro_cod)
#Imputar
ISP <- ISP %>% mutate(b1 = ifelse(is.na(b1_otro_cod), b, b1_otro_cod))
#Recodificar
ISP$b1 <- as.factor(ISP$b1)
ISP$b1 <- car::recode(ISP$b1, recodes = c("1='Salud Municipal';2='Salud Hospitalaria';3='Educacion Municipal';4='Educacion Universitaria';5='Municipal';6='Judicial';7='Obras Sanitarias';0='Otros'; 8= 'Administracion Central'; 9='SII'; 10='Gendarmería'; 99=99; 85=85;66=66"), as.factor = T)
ISP <- ISP %>% select(-c(b,b1_otro,b1_otro_cod))

# b2_ reg (q0025) - Region de trabajo -----------
ISP$q0025 <- as.factor(ISP$q0025)
ISP$b2_reg <- car::recode(ISP$q0025, recodes = c("1='Arica-Parinacota';2='Tarapaca';3='Antofagasta';4='Atacama';5='Coquimbo';6='Valparaiso'; 7='Metropolitana';8='OHiggins';9='Maule';10='Nuble';11='Biobio';12='Araucania';13='Los-Rios';14='Los-Lagos'; 15='Aysen'; 16='Magallanes'; NA=NA"), as.factor = T,
                          levels = c('Tarapaca','Antofagasta','Atacama','Coquimbo','Valparaiso','OHiggins','Maule','Biobio','Araucania','Los-Lagos','Aysen','Magallanes','Metropolitana','Los-Rios','Arica-Parinacota','Nuble'))
levels(ISP$b2_reg)

# b2_ comuna (q0026-q00020) - Comuna de residencia ---------------
table(ISP$q0020)
ISP <- ISP %>% mutate(b2_comuna = case_when(q0026 == 1 ~"Arica",
                                            q0026 == 2 ~"Camarones",
                                            q0026 == 4 ~"General Lagos",
                                            q0027 == 1 ~"Iquique",
                                            q0027 == 4 ~"Huara",
                                            q0027 == 5 ~"Pica",
                                            q0027 == 6 ~"Pozo Almonte",
                                            q0027 == 7 ~"Alto Hospicio",
                                            q0028 == 1 ~"Antofagasta",
                                            q0028 == 2 ~"Mejillones",
                                            q0028 == 3 ~"Sierra Gorda",
                                            q0028 == 4 ~"Taltal",
                                            q0028 == 5 ~"Calama",
                                            q0028 == 7 ~"San Pedro de Atacama",
                                            q0028 == 8 ~"Tocopilla",
                                            q0029 == 1 ~"Copiapo",
                                            q0029 == 2 ~"Caldera",
                                            q0029 == 3 ~"Tierra Amarilla",
                                            q0029 == 4 ~"Chanaral",
                                            q0029 == 5 ~"Diego de Almagro",
                                            q0029 == 6 ~"Vallenar",
                                            q0029 == 8 ~"Freirina",
                                            q0029 == 9 ~"Huasco",
                                            q0030 == 1 ~"La Serena",
                                            q0030 == 2 ~"Coquimbo",
                                            q0030 == 3 ~"Andacollo",
                                            q0030 == 4 ~"La Higuera",
                                            q0030 == 5 ~"Paiguano",
                                            q0030 == 6 ~"Vicuna",
                                            q0030 == 7 ~"Illapel",
                                            q0030 == 8 ~"Canela",
                                            q0030 == 9 ~"Los Vilos",
                                            q0030 == 10 ~"Salamanca",
                                            q0030 == 11 ~"Ovalle",
                                            q0030 == 12 ~"Combarbala",
                                            q0030 == 13 ~"Monte Patria",
                                            q0030 == 14 ~"Punitaqui",
                                            q0031 == 1 ~ "Valparaiso",
                                            q0031 == 2 ~ "Casablanca",
                                            q0031 == 3 ~ "Concon",
                                            q0031 == 6 ~ "Quilpue",
                                            q0031 == 7 ~ "Quintero",
                                            q0031 == 8 ~ "Villa Alemana",
                                            q0031 == 9 ~ "Vina del Mar",
                                            q0031 == 10 ~ "Isla de Pascua",
                                            q0031 == 11 ~ "Los Andes",
                                            q0031 == 12 ~ "Calle Larga",
                                            q0031 == 13 ~ "Rinconada",
                                            q0031 == 14 ~ "San Esteban",
                                            q0031 == 15 ~ "La Ligua",
                                            q0031 == 16 ~ "Cabildo",
                                            q0031 == 18 ~ "Petorca",
                                            q0031 == 20 ~ "Quillota",
                                            q0031 == 21 ~ "Calera",
                                            q0031 == 22 ~ "Hijuelas",
                                            q0031 == 23 ~ "La Cruz",
                                            q0031 == 24 ~ "Limache",
                                            q0031 == 25 ~ "Nogales",
                                            q0031 == 26 ~ "Olmue",
                                            q0031 == 27 ~ "San Antonio",
                                            q0031 == 28 ~ "Algarrobo",
                                            q0031 == 29 ~ "Cartagena", 
                                            q0031 == 30 ~ "El Quisco",
                                            q0031 == 31 ~ "El Tabo",
                                            q0031 == 32 ~ "Santo Domingo", 
                                            q0031 == 33 ~ "San Felipe",
                                            q0031 == 34 ~ "Catemu",
                                            q0031 == 35 ~ "Llaillay",
                                            q0031 == 36 ~ "Panquehue",
                                            q0031 == 37 ~ "Putaendo",
                                            q0031 == 38 ~ "Santa Maria",
                                            q0032 == 1 ~ "Santiago",
                                            q0032 == 2 ~ "Cerrillos",
                                            q0032 == 3 ~ "Cerro Navia",
                                            q0032 == 4 ~ "Conchali",
                                            q0032 == 5 ~ "El Bosque",
                                            q0032 == 6 ~ "Estacion Central",
                                            q0032 == 7 ~ "Huechuraba",
                                            q0032 == 8 ~ "Independencia",
                                            q0032 == 9 ~ "La Cisterna",
                                            q0032 == 10 ~ "La Florida", 
                                            q0032 == 11 ~ "La Granja", 
                                            q0032 == 12 ~ "La Pintana",
                                            q0032 == 13 ~ "La Reina", 
                                            q0032 == 14 ~ "Las Condes", 
                                            q0032 == 15 ~ "Lo Barnechea",
                                            q0032 == 16 ~ "Lo Espejo", 
                                            q0032 == 17 ~ "Lo Prado",
                                            q0032 == 18 ~ "Macul", 
                                            q0032 == 19 ~ "Maipu", 
                                            q0032 == 20 ~ "Nunoa", 
                                            q0032 == 21 ~ "Pedro Aguirre Cerda",
                                            q0032 == 22 ~ "Penalolen", 
                                            q0032 == 23 ~ "Providencia", 
                                            q0032 == 24 ~ "Pudahuel",
                                            q0032 == 25 ~ "Quilicura", 
                                            q0032 == 26 ~ "Quinta Normal", 
                                            q0032 == 27 ~ "Recoleta", 
                                            q0032 == 28 ~ "Renca",
                                            q0032 == 29 ~ "San Joaquin", 
                                            q0032 == 30 ~ "San Miguel",
                                            q0032 == 31 ~ "San Ramon", 
                                            q0032 == 32 ~ "Vitacura",
                                            q0032 == 33 ~ "Puente Alto", 
                                            q0032 == 34 ~ "Pirque",
                                            q0032 == 35 ~ "San Jose de Maipo",
                                            q0032 == 36 ~ "Colina",
                                            q0032 == 37 ~ "Lampa",
                                            q0032 == 38 ~ "Tiltil",
                                            q0032 == 39 ~ "San Bernardo",
                                            q0032 == 40 ~ "Buin",
                                            q0032 == 41 ~ "Calera de Tango",
                                            q0032 == 42 ~ "Paine",
                                            q0032 == 43 ~ "Melipilla",
                                            q0032 == 44 ~ "Alhue",
                                            q0032 == 45 ~ "Curacavi",
                                            q0032 == 46 ~ "Maria Pinto",
                                            q0032 == 47 ~ "San Pedro",
                                            q0032 == 48 ~ "Talagante",
                                            q0032 == 49 ~ "El Monte",
                                            q0032 == 50 ~ "Isla de Maipo",
                                            q0032 == 51 ~ "Padre Hurtado",
                                            q0032 == 52 ~ "Penaflor",
                                            q0033 ==  1 ~ "Rancagua",
                                            q0033 ==  2 ~ "Codegua",
                                            q0033 ==  3 ~ "Coinco",
                                            q0033 ==  4 ~ "Coltauco",
                                            q0033 ==  5 ~ "Donihue",
                                            q0033 ==  6 ~ "Graneros",
                                            q0033 ==  7 ~ "Las Cabras",
                                            q0033 ==  8 ~ "Machali",
                                            q0033 ==  9 ~ "Malloa",
                                            q0033 ==  10 ~ "Mostazal",
                                            q0033 ==  11 ~ "Olivar",
                                            q0033 ==  12 ~ "Peumo",
                                            q0033 ==  13 ~ "Pichidegua",
                                            q0033 ==  14 ~ "Quinta de Tilcoco",
                                            q0033 ==  15 ~ "Rengo",
                                            q0033 ==  16 ~ "Requinoa",
                                            q0033 ==  17 ~ "San Vicente",
                                            q0033 ==  18 ~ "Pichilemu",
                                            q0033 ==  21 ~ "Marchihue",
                                            q0033 ==  22 ~ "Navidad",
                                            q0033 ==  24 ~ "San Fernando",
                                            q0033 ==  25 ~ "Chepica",
                                            q0033 ==  26 ~ "Chimbarongo",
                                            q0033 ==  27 ~ "Lolol",
                                            q0033 ==  28 ~ "Nancagua",
                                            q0033 ==  29 ~ "Palmilla",
                                            q0033 ==  30 ~ "Peralillo",
                                            q0033 ==  31 ~ "Placilla",
                                            q0033 ==  32 ~ "Pumanque",
                                            q0033 ==  33 ~ "Santa Cruz",
                                            q0034 ==  1 ~ "Talca",
                                            q0034 ==  2 ~ "Constitucion",
                                            q0034 ==  3 ~ "Curepto",
                                            q0034 ==  4 ~ "Empedrado", 
                                            q0034 ==  5 ~ "Maule",
                                            q0034 ==  6 ~ "Pelarco",
                                            q0034 ==  7 ~ "Pencahue",
                                            q0034 ==  8 ~ "Rio Claro",
                                            q0034 ==  9 ~ "San Clemente",
                                            q0034 ==  10 ~ "San Rafael",
                                            q0034 ==  11 ~ "Cauquenes",
                                            q0034 ==  13 ~ "Pelluhue",
                                            q0034 ==  14 ~ "Curico",
                                            q0034 ==  15 ~ "Hualane",
                                            q0034 ==  16 ~ "Licanten",
                                            q0034 ==  17 ~ "Molina",
                                            q0034 ==  18 ~ "Rauco",
                                            q0034 ==  19 ~ "Romeral",
                                            q0034 ==  20 ~ "Sagrada Familia",
                                            q0034 ==  21 ~ "Teno",
                                            q0034 ==  23 ~ "Linares",
                                            q0034 ==  24 ~ "Colbun",
                                            q0034 ==  25 ~ "Longavi",
                                            q0034 ==  26 ~ "Parral",
                                            q0034 ==  27 ~ "Retiro",
                                            q0034 ==  28 ~ "San Javier",
                                            q0034 ==  29 ~ "Villa Alegre",
                                            q0034 ==  30 ~ "Yerbas Buenas",
                                            q0035 ==  1 ~ "Concepcion",
                                            q0035 ==  2 ~ "Coronel",
                                            q0035 ==  3 ~ "Chiguayante",
                                            q0035 ==  4 ~ "Florida",
                                            q0035 ==  5 ~ "Hualqui",
                                            q0035 ==  6 ~ "Lota",
                                            q0035 ==  7 ~ "Penco",
                                            q0035 ==  8 ~ "San Pedro de la Paz",
                                            q0035 ==  9 ~ "Santa Juana",
                                            q0035 ==  10 ~ "Talcahuano",
                                            q0035 ==  11 ~ "Tome",
                                            q0035 ==  12 ~ "Hualpen",
                                            q0035 ==  13 ~ "Lebu",
                                            q0035 ==  14 ~ "Arauco",
                                            q0035 ==  15 ~ "Canete",
                                            q0035 ==  17 ~ "Curanilahue",
                                            q0035 ==  18 ~ "Los Alamos",
                                            q0035 ==  19 ~ "Tirua",
                                            q0035 ==  20 ~ "Los Angeles",
                                            q0035 ==  22 ~ "Cabrero",
                                            q0035 ==  23 ~ "Laja",
                                            q0035 ==  24 ~ "Mulchen",
                                            q0035 ==  25 ~ "Nacimiento",
                                            q0035 ==  26 ~ "Negrete",
                                            q0035 ==  28 ~ "Quilleco",
                                            q0035 ==  29 ~ "San Rosendo",
                                            q0035 ==  30 ~ "Santa Barbara",
                                            q0035 ==  32 ~ "Yumbel",
                                            q0035 ==  39 ~ "Portezuelo",
                                            q0035 ==  42 ~ "San Fabian",
                                            q0035 ==  43 ~ "San Nicolas",
                                            q0035 ==  45 ~ "Treguaco",
                                            q0036 ==  1 ~ "Chillan",
                                            q0036 ==  2 ~ "San Carlos",
                                            q0036 ==  3 ~ "Coihueco",
                                            q0036 ==  4 ~ "Bulnes",
                                            q0036 ==  5 ~ "Yunguay",
                                            q0036 ==  6 ~ "Quillon",
                                            q0036 ==  7 ~ "Coelemu",
                                            q0036 ==  8 ~ "El Carmen",
                                            q0036 ==  9 ~ "Quirihue",
                                            q0037 ==  1 ~ "Temuco",
                                            q0037 ==  2 ~ "Carahue",
                                            q0037 ==  3 ~ "Cunco",
                                            q0037 ==  4 ~ "Cararrehue",
                                            q0037 ==  5 ~ "Freire",
                                            q0037 ==  7 ~ "Gorbea",
                                            q0037 ==  8 ~ "Lautaro",
                                            q0037 ==  9 ~ "Loncoche",
                                            q0037 ==  11 ~ "Nueva Imperial",
                                            q0037 ==  12 ~ "Padre Las Casas",
                                            q0037 ==  13 ~ "Perquenco",
                                            q0037 ==  14 ~ "Pitrufquen",
                                            q0037 ==  15 ~ "Pucon",
                                            q0037 ==  16 ~ "Saavedra",
                                            q0037 ==  17 ~ "Teodoro Schmidt",
                                            q0037 ==  18 ~ "Tolten",
                                            q0037 ==  19 ~ "Vilcun",
                                            q0037 ==  20 ~ "Villarrica",
                                            q0037 ==  21 ~ "Cholchol",
                                            q0037 ==  22 ~ "Angol",
                                            q0037 ==  23 ~ "Collipulli",
                                            q0037 ==  24 ~ "Curacautin",
                                            q0037 ==  25 ~ "Ercilla",
                                            q0037 ==  26 ~ "Lonquimay", 
                                            q0037 ==  27 ~ "Los Sauces",
                                            q0037 ==  28 ~ "Lumaco",
                                            q0037 ==  29 ~ "Puren",
                                            q0037 ==  30 ~ "Renaico",
                                            q0037 ==  31 ~ "Traiguen",
                                            q0037 ==  32 ~ "Victoria",
                                            q0038 ==  1 ~ "Corral",
                                            q0038 ==  2 ~ "Lanco",
                                            q0038 ==  3 ~ "Los Lagos",
                                            q0038 ==  4 ~ "Mafil",
                                            q0038 ==  5 ~ "Mariquina",
                                            q0038 ==  6 ~ "Paillaco",
                                            q0038 ==  7 ~ "Panguipulli",
                                            q0038 ==  8 ~ "Valdivia",
                                            q0038 ==  9 ~ "Futrono",
                                            q0038 ==  10 ~ "La Union",
                                            q0038 ==  11 ~ "Lago Ranco",
                                            q0038 ==  12 ~ "Rio Bueno",
                                            q0039 ==  1 ~ "Ancud",
                                            q0039 ==  2 ~ "Castro",
                                            q0039 ==  3 ~ "Chonchi",
                                            q0039 ==  4 ~ "Curaco de Velez",
                                            q0039 ==  5 ~ "Dalcahue",
                                            q0039 ==  6 ~ "Puqueldon",
                                            q0039 ==  7 ~ "Queilen",
                                            q0039 ==  8 ~ "Quemchi",
                                            q0039 ==  9 ~ "Quellon",
                                            q0039 ==  10 ~ "Quinchao",
                                            q0039 ==  11 ~ "Llanquihue",
                                            q0039 ==  12 ~ "Puerto Montt",
                                            q0039 ==  13 ~ "Calbuco",
                                            q0039 ==  14 ~ "Cochamo",
                                            q0039 ==  15 ~ "Fresia",
                                            q0039 ==  16 ~ "Frutillar",
                                            q0039 ==  17 ~ "Los Muermos",
                                            q0039 ==  18 ~ "Maullin",
                                            q0039 ==  19 ~ "Puerto Varas",
                                            q0039 ==  20 ~ "Osorno",
                                            q0039 ==  21 ~ "Puerto Octay",
                                            q0039 ==  22 ~ "Purranque",
                                            q0039 ==  23 ~ "Puyehue",
                                            q0039 ==  24 ~ "Rio Negro",
                                            q0039 ==  25 ~ "San Juan de la Costa",
                                            q0039 ==  26 ~ "San Pablo",
                                            q0039 ==  28 ~ "Chaiten",
                                            q0039 ==  29 ~ "Futaleufu",
                                            q0039 ==  30 ~ "Hualaihue",
                                            q0040 ==  1 ~ "Coyhaique",
                                            q0040 ==  3 ~ "Aysen",
                                            q0040 ==  4 ~ "Cisnes",
                                            q0040 ==  6 ~ "Cochrane",
                                            q0040 ==  7 ~ "OHiggins",
                                            q0040 ==  8 ~ "Tortel",
                                            q0040 ==  9 ~ "Chile Chico",
                                            q0041 ==  1 ~ "Punta Arenas",
                                            q0041 ==  5 ~ "Cabo de Horno",
                                            q0041 ==  7 ~ "Porvenir",
                                            q0041 ==  8 ~ "Primavera",
                                            q0041 ==  9 ~ "Timaukel",
                                            q0041 ==  10 ~ "Natales "))
table(ISP$b2_comuna)
# b3 (q0042) - estamento --------------
table(ISP$q0042)
ISP$b3 <- as.factor(ISP$q0042)
ISP$b3 <- car::recode(ISP$b3, recodes = c("1='Directivo';2='Tecnico';3='Profesional';4='Administrativo';5='Auxiliar'; NA=NA"), as.factor = T,
                      levels = c('Directivo',  'Profesional', 'Tecnico', 'Administrativo', 'Auxiliar'))
levels(ISP$b3)

# b4 (q043 abierta) - Ocupacion -------------------
ISP$b4 <- ISP$q0043
ISP$b4 <- ISP$b4 %>% simplify_strings()
write.csv(ISP$b4, file = "data/isp-ocupacion.csv")

#b5 (q044 y q044_otro) Sindicato  --------------
ISP$b5 <- as.character(ISP$q0044)
table(ISP$b5)
#Other
ISP$q0044_other[ISP$q0044_other == ""] <- NA
table(ISP$q0044_other)# 3507 personas rellenaron con otros

ISP$b5_other <- ISP$q0044_other
ISP$b5_other <- ISP$b5_other %>% simplify_strings()
write.csv(ISP$b5_other, file = "data/isp-sindicatos.csv")

## 99 No responde; 88 No sabe; 85 No aplica; 77 Otros; 66 Falta Info
isp_sindicatos <- read.csv(file = "data/isp-sindicatos2.csv", sep = ";")
ISP <- cbind(ISP, isp_sindicatos) 
ISP$b5_other_cod <- as.character(ISP$b5_other_cod)
#Imputar
ISP <- ISP %>% mutate(b5 = ifelse(is.na(b5_other_cod), b5, b5_other_cod))

#Recodificar
ISP$b5 <- as.factor(ISP$b5)
## 11 Honorarios
ISP$b5 <- car::recode(ISP$b5, recodes = c("1='ANEF';2='AFIICH';3='ANEIICH';4='ANEJUD';5='ASEMUCH';6='CONFEMUCH';7='CONFUSAM';8='FENPRUSS';9='FENATRAOS';10='No afiliado';11='Honorarios';0='Otros'; 99=99;88=88;85=85;66=66"), as.factor = T)
ISP <- ISP %>% select(-c(b5_other2,b5_other,b5_other_cod))

# ANEF - Agrupación Nacional de Empleados Fiscales
# AFIICH - Asociación Nacional de Fiscalizadores del Servicio de Impuestos Internos
# Asociación Nacional de Empleados del Servicio de Impuestos Internos (ANEIICH) 
# Asociación Nacional de Empleados Judiciales de Chile (ANEJUD)  
# Confederación Nacional de Funcionarios Municipales de Chile (ASEMUCH)  
# Confederación Nacional de Funcionarios de la Educación Municipal de Chile (CONFEMUCH)  
# Confederación Nacional de Funcionarios de la Salud Municipal (CONFUSAM)  
# Confederación de Profesionales Universitarios de los Servicios de Salud (FENPRUSS)  
# Federación Nacional de Trabajadores de las Obras Sanitarias (FENATRAOS)

# b6 (q0045) - Modalidad contractual --------------
table(ISP$q0045)
ISP$b6 <- as.factor(ISP$q0045)
ISP$b6 <- car::recode(ISP$b6, recodes = c("1='Planta';2='Contrata';3='Honorarios';4='Subcontratado/a'; NA=NA"), as.factor = T,
                      levels = c('Planta', 'Contrata', 'Honorarios', 'Subcontratado/a'))
levels(ISP$b6)
#b7 (q0046) - Jornada de trabajo --------------
table(ISP$q0046)
ISP$b7 <- as.factor(ISP$q0046)
ISP$b7 <- car::recode(ISP$b7, recodes = c("1='Parcial';2='Completa';3='No sujeto a cumplimiento de horario';NA=NA"), as.factor = T,
                      levels = c('Parcial', 'Completa', 'No sujeto a cumplimiento de horario'))
levels(ISP$b7)

#b8 (q0047) - horas_antes  --------------
ISP$b8 <- as.numeric(ISP$q0047)
summary(ISP$b8) # Luego ver cuantas horas serán las validas

#b9 q0048 - horas_despues  --------------
ISP$b9 <- as.numeric(ISP$q0048)
summary(ISP$b9) # Luego ver cuantas horas serán las validas

#b10 q0049 - remuneracion --------------
table(ISP$q0049)
ISP$b10 <- as.numeric(ISP$q0049)
summary(ISP$b10) # Luego ver cuantas horas serán las validas

#C. Seguridad social -----
#c1 (q0050) - Sistema salud  --------------
table(ISP$q0050)
ISP$c1 <- as.factor(ISP$q0050)
ISP$c1 <- car::recode(ISP$c1, recodes = c("1='FONASA';2='ISAPRE';3='CAPREDENA/DIPRECA';4=88 ;NA=NA"))

# D. Flexibilidad ------
#Construir desde la d1 a las d4.7

## D1 Autonomia ------
## d1.1 (q0051_0001) - velocidad  ----
table(ISP$q0051_0001)
ISP$d1.1 <- as.factor(ISP$q0051_0001)
ISP$d1.1 <- car::recode(ISP$d1.1, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
levels(ISP$d1.1)

## d1.1 (q0051_0002) - cantidad -----
table(ISP$q0051_0002)
ISP$d1.2 <- as.factor(ISP$q0051_0002)
ISP$d1.2 <- car::recode(ISP$d1.2, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
## q0051_0003 - orden (d1.3)-----
table(ISP$q0051_0003)
ISP$d1.3 <- as.factor(ISP$q0051_0003)
ISP$d1.3 <- car::recode(ISP$d1.3, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))

## d1. (q0051_0004) - descanso -----
table(ISP$q0051_0004)
ISP$d1.4 <- as.factor(ISP$q0051_0004)
ISP$d1.4 <- car::recode(ISP$d1.4, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
## D2 - Temporal  --------------------

## d2.1 (q0052_0001) - extension  ----
table(ISP$q0052_0001)
ISP$d2.1 <- as.factor(ISP$q0052_0001)
ISP$d2.1 <- car::recode(ISP$d2.1, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))

## d2.2 (q0052_0002) - fines de semana  ----
table(ISP$q0052_0002)
ISP$d2.2 <- as.factor(ISP$q0052_0002)
ISP$d2.2 <- car::recode(ISP$d2.2, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
## D3 salarial -----
## d3 (q0053) - extension -------
table(ISP$q0053)
ISP$d3 <- as.factor(ISP$q0053)
ISP$d3 <- car::recode(ISP$d3, recodes= c("1='Totalmente fijo';2='La mayor parte fijo';3='La mayor parte variable';4='Totalmente variable';NA=NA"), as.factor = T,
                      levels= c("Totalmente fijo", "La mayor parte fijo", "La mayor parte variable", "Totalmente variable"))
## D4 cambio de flexibilidad ----
## d4.1 (q0054_0001) - velocidad ----
ISP$d4.1 <- as.factor(ISP$q0054_0001)
ISP$d4.1 <- car::recode(ISP$d4.1, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))
## d4.2 (q0054_0002) - cantidad ----
ISP$d4.2 <- as.factor(ISP$q0054_0002)
ISP$d4.2 <- car::recode(ISP$d4.2, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))
## d4.3 (q0054_0003) - orden  ----
ISP$d4.3 <- as.factor(ISP$q0054_0003)
ISP$d4.3 <- car::recode(ISP$d4.3, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))
## d4.4 (q0054_0004) - descanso ----
ISP$d4.4 <- as.factor(ISP$q0054_0004)
ISP$d4.4 <- car::recode(ISP$d4.4, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))
## d4.5 (q0054_0005) - extra hora ----
ISP$d4.5 <- as.factor(ISP$q0054_0005)
ISP$d4.5 <- car::recode(ISP$d4.5, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))
## d4.6 (q0054_0006) - fines de semana  ----
ISP$d4.6 <- as.factor(ISP$q0054_0006)
ISP$d4.6 <- car::recode(ISP$d4.6, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))
## d4.7 (q0054_0007) - salarial ----
ISP$d4.7 <- as.factor(ISP$q0054_0007)
ISP$d4.7 <- car::recode(ISP$d4.1, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                        levels= c("Disminuyo", "Sigue igual", "Aumento"))
########## Modalidad de trabajo ###################
# 4. Combinar variables --------
# q0056
ISP<-ISP %>% mutate(q56_65=case_when(q0055==2 ~ q0056,
                                     q0055==1 ~ q0065,
                                     TRUE ~ NA_character_))

# q0057

ISP %>% subset(!is.na(q0057_0001)&!is.na(ISP$q0066_0001)) %>% select(respondent_id,q0057_0001,q0066_0001,q0055,q57_1)

ISP<-ISP %>% mutate(q57_1=case_when(q0055==2 ~ q0057_0001, q0055==1 ~ q0066_0001, TRUE ~ NA_real_),
                    q57_2=case_when(q0055==2 ~ q0057_0002, q0055==1 ~ q0066_0002, TRUE ~ NA_real_),
                    q57_3=case_when(q0055==2 ~ q0057_0003, q0055==1 ~ q0066_0003, TRUE ~ NA_real_),
                    q57_4=case_when(q0055==2 ~ q0057_0004, q0055==1 ~ q0066_0004, TRUE ~ NA_real_))

# q0058

ISP<-ISP %>% mutate(q58_1=case_when(q0055==2 ~ q0058_0001, q0055==1 ~ q0067_0001, TRUE ~ NA_real_),
                    q58_2=case_when(q0055==2 ~ q0058_0002, q0055==1 ~ q0067_0002, TRUE ~ NA_real_),
                    q58_3=case_when(q0055==2 ~ q0058_0003, q0055==1 ~ q0067_0003, TRUE ~ NA_real_))


# q0059

ISP<-ISP %>% mutate(q59_1=case_when(q0055==2 ~ q0059_0001, q0055==1 ~ q0068_0001, TRUE ~ NA_real_),
                    q59_2=case_when(q0055==2 ~ q0059_0002, q0055==1 ~ q0068_0002, TRUE ~ NA_real_),
                    q59_3=case_when(q0055==2 ~ q0059_0003, q0055==1 ~ q0068_0003, TRUE ~ NA_real_),
                    q59_4=case_when(q0055==2 ~ q0059_0004, q0055==1 ~ q0068_0004, TRUE ~ NA_real_),
                    q59_5=case_when(q0055==2 ~ q0059_0005, q0055==1 ~ q0068_0005, TRUE ~ NA_real_))

# q0060

ISP<-ISP %>% mutate(q60_1=case_when(q0055==2 ~ q0060_0001, q0055==1 ~ q0069_0001, TRUE ~ NA_real_),
                    q60_2=case_when(q0055==2 ~ q0060_0002, q0055==1 ~ q0069_0002, TRUE ~ NA_real_),
                    q60_3=case_when(q0055==2 ~ q0060_0003, q0055==1 ~ q0069_0003, TRUE ~ NA_real_))


# q0061

ISP<-ISP %>% mutate(q61_1=case_when(q0055==2 ~ q0061_0001, q0055==1 ~ q0070_0001, TRUE ~ NA_real_),
                    q61_2=case_when(q0055==2 ~ q0061_0002, q0055==1 ~ q0070_0002, TRUE ~ NA_real_),
                    q61_3=case_when(q0055==2 ~ q0061_0003, q0055==1 ~ q0070_0003, TRUE ~ NA_real_),
                    q61_4=case_when(q0055==2 ~ q0061_0004, q0055==1 ~ q0070_0004, TRUE ~ NA_real_),
                    q61_5=case_when(q0055==2 ~ q0061_0005, q0055==1 ~ q0070_0005, TRUE ~ NA_real_),
                    q61_6=case_when(q0055==2 ~ q0061_0006, q0055==1 ~ q0070_0006, TRUE ~ NA_real_),
                    q61_7=case_when(q0055==2 ~ q0061_0007, q0055==1 ~ q0070_0007, TRUE ~ NA_real_),
                    q61_8=case_when(q0055==2 ~ q0061_0008, q0055==1 ~ q0070_0008, TRUE ~ NA_real_))

# q0062

ISP<-ISP %>% mutate(q62_71=case_when(q0055==2 ~ q0062,
                                     q0055==1 ~ q0071,
                                     TRUE ~ NA_character_))
table(ISP$q0062)
table(ISP$q0071)

# q0063

ISP$q0072_0001<-as.numeric(as.factor(as.numeric(ISP$q0072_0001)))
ISP$q0072_0002<-as.numeric(as.factor(as.numeric(ISP$q0072_0002)))

ISP<-ISP %>% mutate(q63_1=case_when(q0055==2 ~ q0063_0001, q0055==1 ~ q0072_0001, TRUE ~ NA_real_),
                    q63_2=case_when(q0055==2 ~ q0063_0002, q0055==1 ~ q0072_0002, TRUE ~ NA_real_))

table(ISP$q63_1)

table(ISP$q0063_0001)
table(ISP$q0072_0001)

# q0064
ISP$q0064<-as.character(ISP$q0064)
ISP$q0073<-as.character(ISP$q0073)

ISP<-ISP %>% mutate(q64_73=case_when(q0055==2 ~ q0064,
                                     q0055==1 ~ q0073,
                                     TRUE ~ NA_character_))
## Variables finales son 
##q56_65 ##q57_1 ##q58_1 ##q59_1 ##q60_1 ##61_1 ##62_71 ##q63_1 ##q64_73


## d5_mod (q0055) - Teletrabajo -----------------
table(ISP$q0055)
ISP$d5_mod <- as.factor(ISP$q0055)
ISP$d5_mod <- car::recode(ISP$d5_mod, recodes= c("1='Teletrabajo total';2='Teletrabajo parcial';3='Trabajo normal';NA=NA"), as.factor = T,
                          levels= c("Teletrabajo total", "Teletrabajo parcial", "Trabajo normal"))
levels(ISP$d5_mod)

### E. - Condiciones de trabajo, familia y salud  ----
## e1 (q56_65) - personas en hogar  -----
table(ISP$q56_65)
ISP$e1 <- as.numeric(ISP$q56_65)
summary(ISP$e1)

## e2 (q57) - uso herramientas (e2) -----
## e2.1 (q57_1) - computador  -----
table(ISP$q57_1)
ISP$e2.1 <- as.factor(ISP$q57_1)
ISP$e2.1 <- car::recode(ISP$e2.1, recodes= c("1='Exclusivo para el trabajo';2='Compartida en el hogar';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Exclusivo para el trabajo", "Compartida en el hogar", "No la utilizo"))
## q0057_2 - celular (e2.2) -----
table(ISP$q57_2)
ISP$e2.2 <- as.factor(ISP$q57_2)
ISP$e2.2 <- car::recode(ISP$e2.2, recodes= c("1='Exclusivo para el trabajo';2='Compartida en el hogar';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Exclusivo para el trabajo", "Compartida en el hogar", "No la utilizo"))
## q0057_3 - conexion internet (e2.3) -----
table(ISP$q57_3)
ISP$e2.3 <- as.factor(ISP$q57_3)
ISP$e2.3 <- car::recode(ISP$e2.3, recodes= c("1='Exclusivo para el trabajo';2='Compartida en el hogar';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Exclusivo para el trabajo", "Compartida en el hogar", "No la utilizo"))
## q0057_4 - escritorio mesa (e2.4) -----
table(ISP$q57_4)
ISP$e2.4 <- as.factor(ISP$q57_4)
ISP$e2.4 <- car::recode(ISP$e2.4, recodes= c("1='Exclusivo para el trabajo';2='Compartida en el hogar';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Exclusivo para el trabajo", "Compartida en el hogar", "No la utilizo"))
## q0058 - propiedad herramientas (e3) -----
## q0058_1 - computador (e3.1) -----
table(ISP$q58_1)
ISP$e3.1 <- as.factor(ISP$q58_1)
ISP$e3.1 <- car::recode(ISP$e3.1, recodes= c("1='Servicio';2='Propias';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Servicio", "Propias", "No la utilizo"))
## q0058_2 - celular (e3.2) -----
table(ISP$q58_2)
ISP$e3.2 <- as.factor(ISP$q58_2)
ISP$e3.2 <- car::recode(ISP$e3.2, recodes= c("1='Servicio';2='Propias';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Servicio", "Propias", "No la utilizo"))
## q0058_3 - conexion internet (e3.3) -----
table(ISP$q58_3)
ISP$e3.3 <- as.factor(ISP$q58_3)
ISP$e3.3 <- car::recode(ISP$e3.3, recodes= c("1='Servicio';2='Propias';3='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Servicio", "Propias", "No la utilizo"))

## q0059 - calidad herramientas (e4) -----
## q0059_1 - computador (e4.1) -----
table(ISP$q59_1)
ISP$e4.1 <- as.factor(ISP$q59_1)
ISP$e4.1 <- car::recode(ISP$e4.1, recodes= c("1='Muy mala';2='Mala';3='Regular';4='Buena'; 5='Muy buena';6='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Muy mala", "Mala","Regular","Buena","Muy buena", "No la utilizo"))
## q0059_2 - celular (e4.2) -----
table(ISP$q59_2)
ISP$e4.2 <- as.factor(ISP$q59_2)
ISP$e4.2 <- car::recode(ISP$e4.2, recodes= c("1='Muy mala';2='Mala';3='Regular';4='Buena'; 5='Muy buena';6='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Muy mala", "Mala","Regular","Buena","Muy buena", "No la utilizo"))
## q0059_3 - conexion internet (e4.3) -----
table(ISP$q59_3)
ISP$e4.3 <- as.factor(ISP$q59_3)
ISP$e4.3 <- car::recode(ISP$e4.3, recodes= c("1='Muy mala';2='Mala';3='Regular';4='Buena'; 5='Muy buena';6='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Muy mala", "Mala","Regular","Buena","Muy buena", "No la utilizo"))
## q0059_4 - silla (e4.4) -----
table(ISP$q59_4)
ISP$e4.4 <- as.factor(ISP$q59_4)
ISP$e4.4 <- car::recode(ISP$e4.4, recodes= c("1='Muy mala';2='Mala';3='Regular';4='Buena'; 5='Muy buena';6='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Muy mala", "Mala","Regular","Buena","Muy buena", "No la utilizo"))
## q0059_5 - escritorio mesa (e4.5) -----
table(ISP$q59_5)
ISP$e4.5 <- as.factor(ISP$q59_5)
ISP$e4.5 <- car::recode(ISP$e4.5, recodes= c("1='Muy mala';2='Mala';3='Regular';4='Buena'; 5='Muy buena';6='No la utilizo';NA=NA"), as.factor = T,
                        levels= c("Muy mala", "Mala","Regular","Buena","Muy buena", "No la utilizo"))

## q0060 - protocolos (e5) -----
## q0060_1 - organizar trabajo (e5.1) -----
table(ISP$q60_1)
ISP$e5.1 <- as.factor(ISP$q60_1)
ISP$e5.1 <- car::recode(ISP$e5.1, recodes= c("1='Si';2='No';NA=NA"), as.factor = T,
                        levels= c("Si", "No"))
## q0060_2 - seguridad e higiene (e5.2) -----
table(ISP$q60_2)
ISP$e5.2 <- as.factor(ISP$q60_2)
ISP$e5.2 <- car::recode(ISP$e5.2, recodes= c("1='Si';2='No';NA=NA"), as.factor = T,
                        levels= c("Si", "No"))
## q0060_3 - accidentes (e5.3) -----
table(ISP$q60_3)
ISP$e5.3 <- as.factor(ISP$q60_3)
ISP$e5.3 <- car::recode(ISP$e5.3, recodes= c("1='Si';2='No';NA=NA"), as.factor = T,
                        levels= c("Si", "No"))
## q0061 - plataformas (e6.1)------
table(ISP$q61_8, ISP$d5_mod)

## q0061_1 - teams (e6.1_teams) -----
table(ISP$q61_1)
ISP$e6.1_teams <- as.factor(ISP$q61_1)
ISP$e6.1_teams <- car::recode(ISP$e6.1_teams, recodes= c("1='Si';NA='No'"), as.factor = T,
                              levels= c("Si", "No"))
## q0061_2 - zoom (e6.1_zoom) -----
table(ISP$q61_2)
ISP$e6.1_zoom <- as.factor(ISP$q61_2)
ISP$e6.1_zoom <- car::recode(ISP$e6.1_zoom, recodes= c("1='Si';NA='No'"), as.factor = T,
                             levels= c("Si", "No"))
## q0061_3 - meet (e6.1_meet) -----
table(ISP$q61_3)
ISP$e6.1_meet <- as.factor(ISP$q61_3)
ISP$e6.1_meet <- car::recode(ISP$e6.1_meet, recodes= c("1='Si';NA='No'"), as.factor = T,
                             levels= c("Si", "No"))
## q0061_4 - intranet (e6.1_intranet) -----
table(ISP$q61_4)
ISP$e6.1_intranet <- as.factor(ISP$q61_4)
ISP$e6.1_intranet <- car::recode(ISP$e6.1_intranet, recodes= c("1='Si';NA='No'"), as.factor = T,
                                 levels= c("Si", "No"))
## q0061_5 - whats app (e6.1_whatsapp) -----
table(ISP$q61_5)
ISP$e6.1_whatsapp <- as.factor(ISP$q61_5)
ISP$e6.1_whatsapp <- car::recode(ISP$e6.1_whatsapp, recodes= c("1='Si';NA='No'"), as.factor = T,
                                 levels= c("Si", "No"))
## q0061_6 - yammer (e6.1_yammer) -----
table(ISP$q61_6)
ISP$e6.1_yammer <- as.factor(ISP$q61_6)
ISP$e6.1_yammer <- car::recode(ISP$e6.1_yammer, recodes= c("1='Si';NA='No'"), as.factor = T,
                               levels= c("Si", "No"))
## q0061_7 - duo (e6.1_duo) -----
table(ISP$q61_7)
ISP$e6.1_duo <- as.factor(ISP$q61_7)
ISP$e6.1_duo <- car::recode(ISP$e6.1_duo, recodes= c("1='Si';NA='No'"), as.factor = T,
                            levels= c("Si", "No"))
## q0061_8 - otro (e6.1_otro) -----
table(ISP$q61_8)
ISP$e6.1_otro <- as.factor(ISP$q61_8)
ISP$e6.1_otro <- car::recode(ISP$e6.1_otro, recodes= c("1='Si';NA='No'"), as.factor = T,
                             levels= c("Si", "No"))
table(ISP$e6.1_otro)
## q62_71 - tiempo plataforma (e6.2)
ISP$e6.2<-as.numeric(ISP$q62_71)
summary(ISP$e6.2)

## e6.3 (q0063) - problemas  -----
## q63_1 - vpn  (e6.3_vpn)----
table(ISP$q63_1)
ISP$e6.3_vpn <- as.factor(ISP$q63_1)
ISP$e6.3_vpn <- car::recode(ISP$e6.3_vpn, recodes= c("1= 'No aplica';2='Nunca';3='Rara vez';4='Algunas veces';5='Casi siempre';6='Siempre';NA=NA"), as.factor = T,
                            levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre", "No aplica"))

## q63_2 - desconexion  (e6.3_desc)----
table(ISP$q63_2)
ISP$e6.3_desc <- as.factor(ISP$q63_2)
ISP$e6.3_desc <- car::recode(ISP$e6.3_desc, recodes= c("1= 'No aplica';2='Nunca';3='Rara vez';4='Algunas veces';5='Casi siempre';6='Siempre';NA=NA"), as.factor = T,
                             levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre", "No aplica"))

##q64_73 - lugar de trabajo -------
table(ISP$q64_73)
ISP$e7 <- as.factor(ISP$q64_73)
ISP$e7 <- car::recode(ISP$e7, recodes= c("1='Espacio individual';2='Espacio compartido'"), as.factor = T,
                      levels= c("Espacio individual", "Espacio compartido"))

## F. Condiciones de trabajo habitual (normales y teleparcial) ---------------

## F1 (q0074) - medidas prevencion  ----
table(ISP$q0074_0008, ISP$d5_mod)

## q0074_1  (f1.1) -----
table(ISP$q0074_0001)
ISP$f1.1 <- as.factor(ISP$q0074_0001)
ISP$f1.1 <- car::recode(ISP$f1.1, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
table(ISP$f1.1)
## q0074_2 -  (f1.2) -----
table(ISP$q0074_0002)
ISP$f1.2 <- as.factor(ISP$q0074_0002)
ISP$f1.2 <- car::recode(ISP$f1.2, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_3 - (f1.3) -----
table(ISP$q0074_0003)
ISP$f1.3 <- as.factor(ISP$q0074_0003)
ISP$f1.3 <- car::recode(ISP$f1.3, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_4 - (f1.4) -----
table(ISP$q0074_0004)
ISP$f1.4 <- as.factor(ISP$q0074_0004)
ISP$f1.4 <- car::recode(ISP$f1.4, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_5 - (f1.5) -----
table(ISP$q0074_0005)
ISP$f1.5 <- as.factor(ISP$q0074_0005)
ISP$f1.5 <- car::recode(ISP$f1.5, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_6 - (f1.6) -----
table(ISP$q0074_0006)
ISP$f1.6 <- as.factor(ISP$q0074_0006)
ISP$f1.6 <- car::recode(ISP$f1.6, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_7 - (f1.7) -----
table(ISP$q0074_0007)
ISP$f1.7 <- as.factor(ISP$q0074_0007)
ISP$f1.7 <- car::recode(ISP$f1.7, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_8 - (f1.8) -----
table(ISP$q0074_0008)
ISP$f1.8 <- as.factor(ISP$q0074_0008)
ISP$f1.8 <- car::recode(ISP$f1.8, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_9 - (f1.9) -----
table(ISP$q0074_0009)
ISP$f1.9 <- as.factor(ISP$q0074_0009)
ISP$f1.9 <- car::recode(ISP$f1.9, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                        levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_10 - (f1.10) -----
table(ISP$q0074_0010)
ISP$f1.10 <- as.factor(ISP$q0074_0010)
ISP$f1.10 <- car::recode(ISP$f1.10, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_11 - (f1.11) -----
table(ISP$q0074_0011)
ISP$f1.11 <- as.factor(ISP$q0074_0011)
ISP$f1.11 <- car::recode(ISP$f1.11, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_12 - (f1.12) -----
table(ISP$q0074_0012)
ISP$f1.12 <- as.factor(ISP$q0074_0012)
ISP$f1.12 <- car::recode(ISP$f1.12, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_13 - (f1.13) -----
table(ISP$q0074_0013)
ISP$f1.13 <- as.factor(ISP$q0074_0013)
ISP$f1.13 <- car::recode(ISP$f1.13, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_14 - (f1.14) -----
table(ISP$q0074_0014)
ISP$f1.14 <- as.factor(ISP$q0074_0014)
ISP$f1.14 <- car::recode(ISP$f1.14, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))
## q0074_15 - (f1.15) -----
table(ISP$q0074_0015)
ISP$f1.15 <- as.factor(ISP$q0074_0015)
ISP$f1.15 <- car::recode(ISP$f1.15, recodes= c("1='Si, adecuadamente';2='Si, parcialmente';3='No se ha implementado'; NA=NA"), as.factor = T,
                         levels= c("Si, adecuadamente", "Si, parcialmente", "No se ha implementado"))

## F2 (q0075) - Daños  (corregir) ----
table(ISP$q0075_0003, ISP$d5_mod)

## q0075_1  (f2.1) -----
table(ISP$q0075_0001)
ISP$f2.1 <- as.factor(ISP$q0075_0001)
ISP$f2.1 <- car::recode(ISP$f2.1, recodes= c("1='Si';2='No';NA=NA"), as.factor = T,
                        levels= c("Si", "No"))
table(ISP$f2.1)
## q0075_2 -  (f2.2) -----
table(ISP$q0075_0002)
ISP$f2.2 <- as.factor(ISP$q0075_0002)
ISP$f2.2 <- car::recode(ISP$f2.2, recodes= c("1='Si';2='No';NA=NA"), as.factor = T,
                        levels= c("Si", "No"))
table(ISP$f2.2)
## q0075_3 - (f2.3) -----
table(ISP$q0075_0003)
ISP$f2.3 <- as.factor(ISP$q0075_0003)
ISP$f2.3 <- car::recode(ISP$f2.3, recodes= c("1='Si';2='No';NA=NA"), as.factor = T,
                        levels= c("Si", "No"))
## q0075_4 - (f2.4) -----
table(ISP$q0075_0004)
ISP$f2.4 <- as.factor(ISP$q0075_0004)
ISP$f2.4 <- car::recode(ISP$f2.4, recodes= c("1='Si';2='No';NA=NA"), as.factor = T,
                        levels= c("Si", "No"))

## F3 (q0076) - incumplimientos  ----
table(ISP$q0076_0006, ISP$d5_mod)

## q0076_1  (f3.1) -----
table(ISP$q0076_0001)
ISP$f3.1 <- as.factor(ISP$q0076_0001)
ISP$f3.1 <- car::recode(ISP$f3.1, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
## q0076_2 -  (f3.2) -----
table(ISP$q0076_0002)
ISP$f3.2 <- as.factor(ISP$q0076_0002)
ISP$f3.2 <- car::recode(ISP$f3.2, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))

## q0076_3 - (f3.3) -----
table(ISP$q0076_0003)
ISP$f3.3 <- as.factor(ISP$q0076_0003)
ISP$f3.3 <- car::recode(ISP$f3.3, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
## q0076_4 - (f3.4) -----
table(ISP$q0076_0004)
ISP$f3.4 <- as.factor(ISP$q0076_0004)
ISP$f3.4 <- car::recode(ISP$f3.4, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
## q0076_5 - (f3.5) -----
table(ISP$q0076_0005)
ISP$f3.5 <- as.factor(ISP$q0076_0005)
ISP$f3.5 <- car::recode(ISP$f3.5, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
## q0076_6 - (f3.6) -----
table(ISP$q0076_0006)
ISP$f3.6 <- as.factor(ISP$q0076_0006)
ISP$f3.6 <- car::recode(ISP$f3.6, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))

## G. Trabajo de Cuidados/Familia/Conflictos -----------------

## G1 (q0077) - tarea domestica (g1) -----
table(ISP$q0077, ISP$d5_mod)
ISP$g1 <- as.factor(ISP$q0077)
ISP$g1 <- car::recode(ISP$g1, recodes= c("1='Soy la/el principal responsable y hago la mayor parte de las tareas del hogar';2='Hago más o menos la mitad de las tareas del hogar';3='Hago más o menos la cuarta parte de las tareas del hogar';4='Solo hago tareas puntuales';5='No hago ninguna o casi ninguna de estas tareas';NA=NA"), as.factor = T,
                      levels= c("Soy la/el principal responsable y hago la mayor parte de las tareas del hogar",
                                "Hago más o menos la mitad de las tareas del hogar",
                                "Hago más o menos la cuarta parte de las tareas del hogar",
                                "Solo hago tareas puntuales",
                                "No hago ninguna o casi ninguna de estas tareas"))
levels(ISP$g1)

##G2 (q0078) - limite trabajo (g2)------
## q0078_1 - (g2.1)----
table(ISP$q0078_0001)
ISP$g2.1 <- as.factor(ISP$q0078_0001)
ISP$g2.1 <- car::recode(ISP$g2.1, recodes= c("1= 'No aplica';2='Nunca';3='Rara vez';4='Algunas veces';5='Casi siempre';6='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre", "No aplica"))

## q0078_2 - (g2.2)----
table(ISP$q0078_0002)
ISP$g2.2 <- as.factor(ISP$q0078_0002)
ISP$g2.2 <- car::recode(ISP$g2.2, recodes= c("1= 'No aplica';2='Nunca';3='Rara vez';4='Algunas veces';5='Casi siempre';6='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre", "No aplica"))
## q0078_3 - (g2.3)----
table(ISP$q0078_0003)
ISP$g2.3 <- as.factor(ISP$q0078_0003)
ISP$g2.3 <- car::recode(ISP$g2.3, recodes= c("1= 'No aplica';2='Nunca';3='Rara vez';4='Algunas veces';5='Casi siempre';6='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre", "No aplica"))
## q0078_4 - (g2.4)----
table(ISP$q0078_0004)
ISP$g2.4 <- as.factor(ISP$q0078_0004)
ISP$g2.4 <- car::recode(ISP$g2.4, recodes= c("1= 'No aplica';2='Nunca';3='Rara vez';4='Algunas veces';5='Casi siempre';6='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre", "No aplica"))

##G3 (q0079) - limite trabajo (g3)------
## q0079_1 - (g3.1)----
table(ISP$q0079_0001)
ISP$g3.1 <- as.factor(ISP$q0079_0001)
ISP$g3.1 <- car::recode(ISP$g3.1, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))

## q0079_2 - (g3.2)----
table(ISP$q0079_0002)
ISP$g3.2 <- as.factor(ISP$q0079_0002)
ISP$g3.2 <- car::recode(ISP$g3.2, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))

## q0079_3 - (g3.3)----
table(ISP$q0079_0003)
ISP$g3.3 <- as.factor(ISP$q0079_0003)
ISP$g3.3 <- car::recode(ISP$g3.3, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
## q0079_4 - (g3.4)----
table(ISP$q0079_0004)
ISP$g3.4 <- as.factor(ISP$q0079_0004)
ISP$g3.4 <- car::recode(ISP$g3.4, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))
## q0079_4 - (g3.5)----
table(ISP$q0079_0005)
ISP$g3.5 <- as.factor(ISP$q0079_0005)
ISP$g3.5 <- car::recode(ISP$g3.5, recodes= c("1='Nunca';2='Rara vez';3='Algunas veces';4='Casi siempre';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"))


## G4 (q0080) - velocidad ----
table(ISP$q0080_0001)
ISP$g4 <- as.factor(ISP$q0080_0001)
ISP$g4 <- car::recode(ISP$g4, recodes= c("1='Disminuyo';2='Sigue igual';3='Aumento';NA=NA"), as.factor = T,
                      levels= c("Disminuyo", "Sigue igual", "Aumento"))

## H. Salud Laboral (ámbos modulos)
# H.1 (q0081) - Intensidad ---------
# h1.1 (q0081_0001) - rapidez ------- 
table(ISP$q0081_0001)
ISP$h1.1 <- as.factor(ISP$q0081_0001)
ISP$h1.1 <- car::recode(ISP$h1.1, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
levels(ISP$h1.1)
# h1.2 (q0081_0002) - acumulacion  ------- 
ISP$h1.2 <- as.factor(ISP$q0081_0002)
ISP$h1.2 <- car::recode(ISP$h1.2, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.3 (q0081_0003) - acumulacion  ------- 
ISP$h1.3 <- as.factor(ISP$q0081_0003)
ISP$h1.3 <- car::recode(ISP$h1.3, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.4 (q0081_0004) - acumulacion  ------- 
ISP$h1.4 <- as.factor(ISP$q0081_0004)
ISP$h1.4 <- car::recode(ISP$h1.4, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.5 (q0081_0005) - acumulacion  ------- 
ISP$h1.5 <- as.factor(ISP$q0081_0005)
ISP$h1.5 <- car::recode(ISP$h1.5, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.6 (q0081_0006) - acumulacion  ------- 
ISP$h1.6 <- as.factor(ISP$q0081_0006)
ISP$h1.6 <- car::recode(ISP$h1.6, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.7 (q0081_0007) - acumulacion  ------- 
ISP$h1.7 <- as.factor(ISP$q0081_0007)
ISP$h1.7 <- car::recode(ISP$h1.7, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))

# H.2 (q0082_0001 a q0082_0005) - Salud Mental  ---------
# h2.1 (q0082_0001) - nervios ------- 
table(ISP$q0082_0001)
ISP$h2.1 <- as.factor(ISP$q0082_0001)
ISP$h2.1 <- car::recode(ISP$h2.1, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
levels(ISP$h2.1)
# h1.2 (q0082_0002) - animo  ------- 
ISP$h2.2 <- as.factor(ISP$q0082_0002)
ISP$h2.2 <- car::recode(ISP$h2.2, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.3 (q0082_0003) - tranquilo   ------- 
ISP$h2.3 <- as.factor(ISP$q0082_0003)
ISP$h2.3 <- car::recode(ISP$h2.3, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.4 (q0082_0004) - triste  ------- 
ISP$h2.4 <- as.factor(ISP$q0082_0004)
ISP$h2.4 <- car::recode(ISP$h2.4, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h1.5 (q0082_0005) - feliz  ------- 
ISP$h2.5 <- as.factor(ISP$q0082_0005)
ISP$h2.5 <- car::recode(ISP$h2.5, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))

# H.3 (q0082_0006 a q0082_0008) - Salud Física  ---------
# h3.1 (q0081_0006) - dolor  ------- 
ISP$h3.1 <- as.factor(ISP$q0082_0006)
ISP$h3.1 <- car::recode(ISP$h3.1, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h3.2 (q0081_0007) - percepcion dolor  ------- 
ISP$h3.2 <- as.factor(ISP$q0082_0007)
ISP$h3.2 <- car::recode(ISP$h3.2, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# h3.3 (q0081_0008) - palpitaciones  ------- 
ISP$h3.3 <- as.factor(ISP$q0082_0008)
ISP$h3.3 <- car::recode(ISP$h3.3, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))

## I. Tareas teletrabajbles (ámbos modulos)
# i.1 (q0083) - Tareas que cree se podrían teletrabajar ---------
# i1.1 (q0083_0001) - reuniones ------- 
table(ISP$q0083_0001)
ISP$i1.1 <- as.factor(ISP$q0083_0001)
ISP$i1.1 <- car::recode(ISP$i1.1, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
levels(ISP$i1.1)
# i1.2 (q0083_0002) - atencion  ------- 
ISP$i1.2 <- as.factor(ISP$q0083_0002)
ISP$i1.2 <- car::recode(ISP$i1.2, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# i1.3 (q0083_0003) - informacion  ------- 
ISP$i1.3 <- as.factor(ISP$q0083_0003)
ISP$i1.3 <- car::recode(ISP$i1.3, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# i1.4 (q0083_0004) - tareas simples  sin concentracion ------- 
ISP$i1.4 <- as.factor(ISP$q0083_0004)
ISP$i1.4 <- car::recode(ISP$i1.4, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# i1.5 (q0083_0005) - tareas simples concentracion  ------- 
ISP$i1.5 <- as.factor(ISP$q0083_0005)
ISP$i1.5 <- car::recode(ISP$i1.5, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# i1.6 (q0083_0006) - tareas complejas concentracion  ------- 
ISP$i1.6 <- as.factor(ISP$q0083_0006)
ISP$i1.6 <- car::recode(ISP$i1.6, recodes= c("1='Nunca';2='Solo unas pocas veces';3='Algunas veces';4='La mayoria de las veces';5='Siempre';NA=NA"), as.factor = T,
                        levels= c("Nunca", "Solo unas pocas veces", "Algunas veces", "La mayoria de las veces", "Siempre"))
# i2 (q0084) - Nuevas modalidades preferidas  ---------
table(ISP$q0084)
ISP$i2 <- as.factor(ISP$q0084)
ISP$i2 <- car::recode(ISP$i2, recodes= c("1='Teletrabajo total';2='Teletrabajo parcial';3='Trabajo normal';NA=NA"), as.factor = T,
                      levels= c("Teletrabajo total", "Teletrabajo parcial", "Trabajo normal"))

# Z. Correo de contacto
ISP$mail <- ISP$q0085
ISP$mail <- ISP$z %>% simplify_strings()
write.csv(ISP$mail, file = "data/isp-email.csv")

## 4. Seleccionar variables
ISP_proc <- ISP %>% select(- starts_with(c("q","p")))
save(ISP_proc, file = "data/ISP_proc.RData")
write.csv(ISP_proc, file = "data/ISP_proc.csv")
