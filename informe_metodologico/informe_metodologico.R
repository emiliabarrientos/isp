# Informe metodologico -----------

# 0. Set up
library(ggplot2); theme_set(theme_classic(base_size = 14))
# ISP corporate colors
# 0. Set up
# ISP corporate colors
isp_colors <- c(`red`= "#d40c04",`pink` = "#e05e5a",`lightpink` = "#fee4e2",`white` = "#e3dac9",`lightgrey`= "#d3d3d3")
isp_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (isp_colors)
  
  isp_colors[cols]
}
isp_palettes <- list(`main`  = isp_cols("red", "pink", "lightpink",  "white", "black",  "lightgrey"))
isp_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- isp_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}
scale_color_isp <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- isp_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("isp_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}
scale_fill_isp <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- isp_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("isp_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# 1. Cargar librerías
pacman::p_load(ggplot2,dplyr,tidyverse)

# 2. Cargar base de datos
setwd("C:/Users/Valentina Andrade/Dropbox/2. Trabajo/ISP")
load("informe_metodologico/data/casen-expandida.RData")
load("data/isp.RData")

# 0. Generacion de tablas 
# Sexo (sex) ----
table(isp$a1_sexo)
table_sex <- isp %>% dplyr::select(a1_sexo) %>% filter(a1_sexo != "Otro") %>% 
  group_by(a1_sexo) %>% dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n))
sexo <- sexo[,1:4] %>% mutate(sexo = case_when(sexo == 1 ~ "Masculino", sexo == 2 ~"Femenino"))
sex <- merge(sexo, table_sex, by.y = "a1_sexo", by.x = "sexo")

#Edad (edad_) ----------
isp<-isp %>% mutate(edad_cat= case_when(a2_edad<40 ~ 1,
                                            a2_edad>49 & a2_edad<60 ~ 2,
                                            a2_edad>59 & a2_edad<80 ~ 3,                                            TRUE ~ NA_real_))
isp$edad_cat <- factor(isp$edad_cat,
                           levels = c(1:3), labels = c("18 a 39 años",
                                                       "40 a 59 años",
                                                       "60 a 80 años"))
edad$edad_cat <- factor(edad$edad_cat,
                       levels = c(1:3), labels = c("18 a 39 años",
                                                   "40 a 59 años",
                                                   "60 a 80 años"))

table_edad <- isp %>% dplyr::select(edad_cat) %>% filter(!is.na(edad_cat)) %>% 
  group_by(edad_cat) %>% dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n))
edad <- edad[,1:4] %>% filter(!is.na(edad_cat))
edad_ <- merge(edad, table_edad,  by = "edad_cat")

# Region (reg) -----
table(isp$a4_reg)
table_reg <- isp %>% dplyr::select(a4_reg) %>% filter(!is.na(a4_reg)) %>% 
  group_by(a4_reg) %>% dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n))

region <- region[,1:4] %>% filter(!is.na(region))
region$region <- car::recode(region$region, recodes = c("15='Arica-Parinacota';1='Tarapaca';2='Antofagasta';3='Atacama';4='Coquimbo';5='Valparaiso'; 13='Metropolitana';6='OHiggins';7='Maule';16='Nuble';8='Biobio';9='Araucania';14='Los-Rios';10='Los-Lagos'; 11='Aysen'; 12='Magallanes'"), as.factor = T,
                          levels = c('Arica-Parinacota','Tarapaca','Antofagasta','Atacama','Coquimbo','Valparaiso','Metropolitana','OHiggins','Maule','Biobio','Araucania','Los-Rios','Los-Lagos','Aysen','Magallanes'))

reg <- merge(region, table_reg, by.x = "region",  by.y = "a4_reg", sort = F)

reg[,-c(1,5)] <- reg[,-c(1,5)] %>% mutate_if(is.numeric, function(x)x*100)
reg[,-1] <-round(reg[,-1],2)
reg <- reg %>% unite("IC-95%", proportion_low, proportion_upp, sep = "-")

# Comuna (com) ----
table(isp$a5_comuna)
table_comuna <- isp %>% dplyr::select(a5_comuna) %>% filter(!is.na(a5_comuna)) %>% 
  group_by(a5_comuna) %>% dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n))

comuna <- comuna[,1:4] %>% filter(!is.na(comuna))
comuna <- comuna %>% mutate(comuna_cat = case_when(comuna == 1101~'Iquique',	
                                                   comuna == 1107~'Alto Hospicio',	
                                                   comuna == 1401~'Pozo Almonte',	
                                                   comuna == 1402~'Camina',	
                                                   comuna == 1403~'Colchane',	
                                                   comuna == 1404~'Huara',	
                                                   comuna == 1405~'Pica',	
                                                   comuna == 2101~'Antofagasta',	
                                                   comuna == 2102~'Mejillones',	
                                                   comuna == 2103~'Sierra Gorda',	
                                                   comuna == 2104~'Taltal',	
                                                   comuna == 2201~'Calama',	
                                                   comuna == 2203~'San Pedro de Atacama',	
                                                   comuna == 2301~'Tocopilla',	
                                                   comuna == 2302~'Maria Elena',	
                                                   comuna == 3101~'Copiapo',	
                                                   comuna == 3102~'Caldera',	
                                                   comuna == 3103~'Tierra Amarilla',	
                                                   comuna == 3201~'Chanaral',	
                                                   comuna == 3202~'Diego de Almagro',	
                                                   comuna == 3301~'Vallenar',	
                                                   comuna == 3302~'Alto del Carmen',	
                                                   comuna == 3303~'Freirina',	
                                                   comuna == 3304~'Huasco',	
                                                   comuna == 4101~'La Serena',	
                                                   comuna == 4102~'Coquimbo',	
                                                   comuna == 4103~'Andacollo',	
                                                   comuna == 4104~'La Higuera',	
                                                   comuna == 4105~'Paiguano',	
                                                   comuna == 4106~'Vicuna',	
                                                   comuna == 4201~'Illapel',	
                                                   comuna == 4202~'Canela',	
                                                   comuna == 4203~'Los Vilos',	
                                                   comuna == 4204~'Salamanca',	
                                                   comuna == 4301~'Ovalle',	
                                                   comuna == 4302~'Combarbala',	
                                                   comuna == 4303~'Monte Patria',	
                                                   comuna == 4304~'Punitaqui',	
                                                   comuna == 4305~'Rio Hurtado',	
                                                   comuna == 5101~'Valparaiso',	
                                                   comuna == 5102~'Casablanca',	
                                                   comuna == 5103~'Concon',	
                                                   comuna == 5104~'Juan Fernandez',	
                                                   comuna == 5105~'Puchuncavi',	
                                                   comuna == 5107~'Quintero',	
                                                   comuna == 5109~'Vina del Mar',	
                                                   comuna == 5201~'Isla de Pascua',	
                                                   comuna == 5301~'Los Andes',	
                                                   comuna == 5302~'Calle Larga',	
                                                   comuna == 5303~'Rinconada',	
                                                   comuna == 5304~'San Esteban',	
                                                   comuna == 5401~'La Ligua',	
                                                   comuna == 5402~'Cabildo',	
                                                   comuna == 5403~'Papudo',	
                                                   comuna == 5404~'Petorca',	
                                                   comuna == 5405~'Zapallar',	
                                                   comuna == 5501~'Quillota',	
                                                   comuna == 5502~'Calera',	
                                                   comuna == 5503~'Hijuelas',	
                                                   comuna == 5504~'La Cruz',	
                                                   comuna == 5506~'Nogales',	
                                                   comuna == 5601~'San Antonio',	
                                                   comuna == 5602~'Algarrobo',	
                                                   comuna == 5603~'Cartagena',	
                                                   comuna == 5604~'El Quisco',	
                                                   comuna == 5605~'El Tabo',	
                                                   comuna == 5606~'Santo Domingo',	
                                                   comuna == 5701~'San Felipe',	
                                                   comuna == 5702~'Catemu',	
                                                   comuna == 5703~'Llaillay',	
                                                   comuna == 5704~'Panquehue',	
                                                   comuna == 5705~'Putaendo',	
                                                   comuna == 5706~'Santa Maria',	
                                                   comuna == 5801~'Quilpue',	
                                                   comuna == 5802~'Limache',	
                                                   comuna == 5803~'Olmue',	
                                                   comuna == 5804~'Villa Alemana',	
                                                   comuna == 6101~'Rancagua',	
                                                   comuna == 6102~'Codegua',	
                                                   comuna == 6103~'Coinco',	
                                                   comuna == 6104~'Coltauco',	
                                                   comuna == 6105~'Donihue',	
                                                   comuna == 6106~'Graneros',	
                                                   comuna == 6107~'Las Cabras',	
                                                   comuna == 6108~'Machali',	
                                                   comuna == 6109~'Malloa',	
                                                   comuna == 6110~'Mostazal',	
                                                   comuna == 6111~'Olivar',	
                                                   comuna == 6112~'Peumo',	
                                                   comuna == 6113~'Pichidegua',	
                                                   comuna == 6114~'Quinta de Tilcoco',	
                                                   comuna == 6115~'Rengo',	
                                                   comuna == 6116~'Requinoa',	
                                                   comuna == 6117~'San Vicente',	
                                                   comuna == 6201~'Pichilemu',	
                                                   comuna == 6202~'La Estrella',	
                                                   comuna == 6203~'Litueche',	
                                                   comuna == 6204~'Marchihue',	
                                                   comuna == 6205~'Navidad',	
                                                   comuna == 6206~'Paredones',	
                                                   comuna == 6301~'San Fernando',	
                                                   comuna == 6302~'Chepica',	
                                                   comuna == 6303~'Chimbarongo',	
                                                   comuna == 6304~'Lolol',	
                                                   comuna == 6305~'Nancagua',	
                                                   comuna == 6306~'Palmilla',	
                                                   comuna == 6307~'Peralillo',	
                                                   comuna == 6308~'Placilla',	
                                                   comuna == 6309~'Pumanque',	
                                                   comuna == 6310~'Santa Cruz',	
                                                   comuna == 7101~'Talca',	
                                                   comuna == 7102~'Constitucion',	
                                                   comuna == 7103~'Curepto',	
                                                   comuna == 7104~'Empedrado',	
                                                   comuna == 7105~'Maule',	
                                                   comuna == 7106~'Pelarco',	
                                                   comuna == 7107~'Pencahue',	
                                                   comuna == 7108~'Rio Claro',	
                                                   comuna == 7109~'San Clemente',	
                                                   comuna == 7110~'San Rafael',	
                                                   comuna == 7201~'Cauquenes',	
                                                   comuna == 7202~'Chanco',	
                                                   comuna == 7203~'Pelluhue',	
                                                   comuna == 7301~'Curico',	
                                                   comuna == 7302~'Hualane',	
                                                   comuna == 7303~'Licanten',	
                                                   comuna == 7304~'Molina',	
                                                   comuna == 7305~'Rauco',	
                                                   comuna == 7306~'Romeral',	
                                                   comuna == 7307~'Sagrada Familia',	
                                                   comuna == 7308~'Teno',	
                                                   comuna == 7309~'Vichuquen',	
                                                   comuna == 7401~'Linares',	
                                                   comuna == 7402~'Colbun',	
                                                   comuna == 7403~'Longavi',	
                                                   comuna == 7404~'Parral',	
                                                   comuna == 7405~'Retiro',	
                                                   comuna == 7406~'San Javier',	
                                                   comuna == 7407~'Villa Alegre',	
                                                   comuna == 7408~'Yerbas Buenas',	
                                                   comuna == 8101~'Concepcion',	
                                                   comuna == 8102~'Coronel',	
                                                   comuna == 8103~'Chiguayante',	
                                                   comuna == 8104~'Florida',	
                                                   comuna == 8105~'Hualqui',	
                                                   comuna == 8106~'Lota',	
                                                   comuna == 8107~'Penco',	
                                                   comuna == 8108~'San Pedro de la Paz',	
                                                   comuna == 8109~'Santa Juana',	
                                                   comuna == 8110~'Talcahuano',	
                                                   comuna == 8111~'Tome',	
                                                   comuna == 8112~'Hualpen',	
                                                   comuna == 8201~'Lebu',	
                                                   comuna == 8202~'Arauco',	
                                                   comuna == 8203~'Canete',	
                                                   comuna == 8204~'Contulmo',	
                                                   comuna == 8205~'Curanilahue',	
                                                   comuna == 8206~'Los Alamos',	
                                                   comuna == 8207~'Tirua',	
                                                   comuna == 8301~'Los Angeles',	
                                                   comuna == 8302~'Antuco',	
                                                   comuna == 8303~'Cabrero',	
                                                   comuna == 8304~'Laja',	
                                                   comuna == 8305~'Mulchen',	
                                                   comuna == 8306~'Nacimiento',	
                                                   comuna == 8307~'Negrete',	
                                                   comuna == 8308~'Quilaco',	
                                                   comuna == 8309~'Quilleco',	
                                                   comuna == 8310~'San Rosendo',	
                                                   comuna == 8311~'Santa Barbara',	
                                                   comuna == 8312~'Tucapel',	
                                                   comuna == 8313~'Yumbel',	
                                                   comuna == 8314~'Alto Biobio',	
                                                   comuna == 9101~'Temuco',	
                                                   comuna == 9102~'Carahue',	
                                                   comuna == 9103~'Cunco',	
                                                   comuna == 9104~'Curarrehue',	
                                                   comuna == 9105~'Freire',	
                                                   comuna == 9106~'Galvarino',	
                                                   comuna == 9107~'Gorbea',	
                                                   comuna == 9108~'Lautaro',	
                                                   comuna == 9109~'Loncoche',	
                                                   comuna == 9110~'Melipeuco',	
                                                   comuna == 9111~'Nueva Imperial',	
                                                   comuna == 9112~'Padre Las Casas',	
                                                   comuna == 9113~'Perquenco',	
                                                   comuna == 9114~'Pitrufquen',	
                                                   comuna == 9115~'Pucon',	
                                                   comuna == 9116~'Saavedra',	
                                                   comuna == 9117~'Teodoro Schmidt',	
                                                   comuna == 9118~'Tolten',	
                                                   comuna == 9119~'Vilcun',	
                                                   comuna == 9120~'Villarrica',	
                                                   comuna == 9121~'Cholchol',	
                                                   comuna == 9201~'Angol',	
                                                   comuna == 9202~'Collipulli',	
                                                   comuna == 9203~'Curacautin',	
                                                   comuna == 9204~'Ercilla',	
                                                   comuna == 9205~'Lonquimay',	
                                                   comuna == 9206~'Los Sauces',	
                                                   comuna == 9207~'Lumaco',	
                                                   comuna == 9208~'Puren',	
                                                   comuna == 9209~'Renaico',	
                                                   comuna == 9210~'Traiguen',	
                                                   comuna == 9211~'Victoria',	
                                                   comuna == 10101~'Puerto Montt',	
                                                   comuna == 10102~'Calbuco',	
                                                   comuna == 10103~'Cochamo',	
                                                   comuna == 10104~'Fresia',	
                                                   comuna == 10105~'Frutillar',	
                                                   comuna == 10106~'Los Muermos',	
                                                   comuna == 10107~'Llanquihue',	
                                                   comuna == 10108~'Maullin',	
                                                   comuna == 10109~'Puerto Varas',	
                                                   comuna == 10201~'Castro',	
                                                   comuna == 10202~'Ancud',	
                                                   comuna == 10203~'Chonchi',	
                                                   comuna == 10204~'Curaco de Velez',	
                                                   comuna == 10205~'Dalcahue',	
                                                   comuna == 10206~'Puqueldon',	
                                                   comuna == 10207~'Queilen',	
                                                   comuna == 10208~'Quellon',	
                                                   comuna == 10209~'Quemchi',	
                                                   comuna == 10210~'Quinchao',	
                                                   comuna == 10301~'Osorno',	
                                                   comuna == 10302~'Puerto Octay',	
                                                   comuna == 10303~'Purranque',	
                                                   comuna == 10304~'Puyehue',	
                                                   comuna == 10305~'Rio Negro',	
                                                   comuna == 10306~'San Juan de la Costa',	
                                                   comuna == 10307~'San Pablo',	
                                                   comuna == 10401~'Chaiten',	
                                                   comuna == 10402~'Futaleufu',	
                                                   comuna == 10403~'Hualaihue',	
                                                   comuna == 10404~'Palena',	
                                                   comuna == 11101~'Coyhaique',	
                                                   comuna == 11102~'Lago Verde',	
                                                   comuna == 11201~'Aysen',	
                                                   comuna == 11202~'Cisnes',	
                                                   comuna == 11203~'Guaitecas',	
                                                   comuna == 11301~'Cochrane',	
                                                   comuna == 11303~'Tortel',	
                                                   comuna == 11401~'Chile Chico',	
                                                   comuna == 11402~'Rio Ibanez',	
                                                   comuna == 12101~'Punta Arenas',	
                                                   comuna == 12104~'San Gregorio',	
                                                   comuna == 12201~'Cabo de Horno',	
                                                   comuna == 12301~'Porvenir',	
                                                   comuna == 12302~'Primavera',	
                                                   comuna == 12303~'Timaukel',	
                                                   comuna == 12401~'Natales',	
                                                   comuna == 13101~'Santiago',	
                                                   comuna == 13102~'Cerrillos',	
                                                   comuna == 13103~'Cerro Navia',	
                                                   comuna == 13104~'Conchali',	
                                                   comuna == 13105~'El Bosque',	
                                                   comuna == 13106~'Estacion Central',	
                                                   comuna == 13107~'Huechuraba',	
                                                   comuna == 13108~'Independencia',	
                                                   comuna == 13109~'La Cisterna',	
                                                   comuna == 13110~'La Florida',	
                                                   comuna == 13111~'La Granja',	
                                                   comuna == 13112~'La Pintana',	
                                                   comuna == 13113~'La Reina',	
                                                   comuna == 13114~'Las Condes',	
                                                   comuna == 13115~'Lo Barnechea',	
                                                   comuna == 13116~'Lo Espejo',	
                                                   comuna == 13117~'Lo Prado',	
                                                   comuna == 13118~'Macul',	
                                                   comuna == 13119~'Maipu',	
                                                   comuna == 13120~'Nunoa',	
                                                   comuna == 13121~'Pedro Aguirre Cerda',	
                                                   comuna == 13122~'Penalolen',	
                                                   comuna == 13123~'Providencia',	
                                                   comuna == 13124~'Pudahuel',	
                                                   comuna == 13125~'Quilicura',	
                                                   comuna == 13126~'Quinta Normal',	
                                                   comuna == 13127~'Recoleta',	
                                                   comuna == 13128~'Renca',	
                                                   comuna == 13129~'San Joaquin',	
                                                   comuna == 13130~'San Miguel',	
                                                   comuna == 13131~'San Ramon',	
                                                   comuna == 13132~'Vitacura',	
                                                   comuna == 13201~'Puente Alto',	
                                                   comuna == 13202~'Pirque',	
                                                   comuna == 13203~'San Jose de Maipo',	
                                                   comuna == 13301~'Colina',	
                                                   comuna == 13302~'Lampa',	
                                                   comuna == 13303~'Tiltil',	
                                                   comuna == 13401~'San Bernardo',	
                                                   comuna == 13402~'Buin',	
                                                   comuna == 13403~'Calera de Tango',	
                                                   comuna == 13404~'Paine',	
                                                   comuna == 13501~'Melipilla',	
                                                   comuna == 13502~'Alhue',	
                                                   comuna == 13503~'Curacavi',	
                                                   comuna == 13504~'Maria Pinto',	
                                                   comuna == 13505~'San Pedro',	
                                                   comuna == 13601~'Talagante',	
                                                   comuna == 13602~'El Monte',	
                                                   comuna == 13603~'Isla de Maipo',	
                                                   comuna == 13604~'Padre Hurtado',	
                                                   comuna == 13605~'Penaflor',	
                                                   comuna == 14101~'Valdivia',	
                                                   comuna == 14102~'Corral',	
                                                   comuna == 14103~'Lanco',	
                                                   comuna == 14104~'Los Lagos',	
                                                   comuna == 14105~'Mafil',	
                                                   comuna == 14106~'Mariquina',	
                                                   comuna == 14107~'Paillaco',	
                                                   comuna == 14108~'Panguipulli',	
                                                   comuna == 14201~'La Union',	
                                                   comuna == 14202~'Futrono',	
                                                   comuna == 14203~'Lago Ranco',	
                                                   comuna == 14204~'Rio Bueno',	
                                                   comuna == 15101~'Arica',	
                                                   comuna == 15102~'Camarones',	
                                                   comuna == 15201~'Putre',	
                                                   comuna == 15202~'General Lagos',	
                                                   comuna == 16101~'Chillan',	
                                                   comuna == 16102~'Bulnes',	
                                                   comuna == 16103~'Chillan Viejo',	
                                                   comuna == 16104~'El Carmen',	
                                                   comuna == 16105~'Pemuco',	
                                                   comuna == 16106~'Pinto',	
                                                   comuna == 16107~'Quillon',	
                                                   comuna == 16108~'San Ignacio',	
                                                   comuna == 16109~'Yunguay',	
                                                   comuna == 16201~'Quirihue',	
                                                   comuna == 16202~'Cobquecura',	
                                                   comuna == 16203~'Coelemu',	
                                                   comuna == 16204~'Ninhue',	
                                                   comuna == 16205~'Portezuelo',	
                                                   comuna == 16206~'Ranquil',	
                                                   comuna == 16207~'Treguaco',	
                                                   comuna == 16301~'San Carlos',	
                                                   comuna == 16302~'Coihueco',	
                                                   comuna == 16303~'Niquen',	
                                                   comuna == 16304~'San Fabian',	
                                                   comuna == 16305~'San Nicolas'))

com <- merge(comuna, table_comuna, by.x = "comuna_cat",  by.y = "a5_comuna", sort = F, all.y = T)

com[,-c(1,2,6)] <- com[,-c(1,2,6)] %>% mutate_if(is.numeric, function(x)x*100)
com[,-c(1,2)] <-round(com[,-c(1,2)],2)
com <- com %>% unite("IC-95%", proportion_low, proportion_upp, sep = "-")


# Socio laborales -----
# Rama (ram) ------------------
table(isp$b1)
isp <- isp %>% mutate(rama = case_when(b1 == "Educacion Universitaria" ~ "Educacion (Municipal y Universitaria)",
                                       b1 == "Educacion Municipal" ~ "Educacion (Municipal y Universitaria)",
                                      b1 == "Administracion Central" ~  "Administracion publica y defensa",
                                      b1 == "Judicial" ~  "Administracion publica y defensa",
                                      b1 == "Municipal" ~  "Administracion publica y defensa",
                                      b1 == "SII" ~  "Administracion publica y defensa",
                                      b1 == "Gendarmería" ~  "Administracion publica y defensa",
                                      b1 ==  "Salud Hospitalaria" ~ "Salud (Municipal y Hospitalaria)",
                                      b1 ==  "Salud Municipal" ~ "Salud (Municipal y Hospitalaria)",
                                      b1 ==  "Obras Sanitarias" ~ "Obras sanitarias",
                                      b1 == "Otros" ~ "Otro",
                                      TRUE  ~ NA_character_))

table_rama <- isp %>% dplyr::select(rama) %>% filter(!is.na(rama)) %>% 
  group_by(rama) %>% dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n))

ram <- merge(rama, table_rama, by= "rama", sort = F, all.y = T)
ram <- ram[, -c(5:7)]

# Ocupacion -----
table(isp$b4_isco_cod)
#Mucho

#Contrato (cont) --------------
table(isp$b6)

table_contrato <- isp %>% dplyr::select(b6) %>% filter(b6 %in% c("Planta", "Contrata")) %>% 
  group_by(b6) %>% dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n))
  
contrato <- contrato[,1:4] %>% filter(!is.na(contrato))
cont <- merge(contrato, table_contrato, by.y = "b6", by.x = "contrato")

# Jornada (jor) ---------------
table(isp$b7)
table_jor <- isp %>% dplyr::select(b7) %>% filter(b7 != "No sujeto a cumplimiento de horario") %>% 
  group_by(b7) %>% dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n))

jornada <- jor[,1:4] %>% filter(!is.na(jor))
jor <- merge(jornada, table_jor, by.y = "b7", by.x = "jornada")

# Sindicato---------
table(isp$b5)
table_sindicato <- isp %>% dplyr::select(b5) %>% mutate(sindicato = case_when(b5 == "No afiliado" ~ 2,
                                                                              b5 == "66"~ NA_real_, 
                                                                              b5 == "88"~ NA_real_,
                                                                              b5 == "99"~ NA_real_,
                                                                              TRUE ~ 1)) %>% group_by(sindicato) %>% dplyr::summarise(n = n()) %>% mutate(p = n / sum(n)) 

table_sindicato <- filter(table_sindicato, !is.na(sindicato))
sindicato <- sindicato[,1:4] %>% filter(!is.na(sindicato))
sin <- merge(sindicato, table_sindicato, by = "sindicato")

# Estamento ------------------
isp$b4_isco <- car::recode(isp$b4_isco, recodes = c('310 = 0; c(99,88,66)=NA'))
isp$b4_isco <- substr(isp$b4_isco, start = 1, stop = 1) 
isp$b4_isco <- as.numeric(isp$b4_isco)

table_ocup <- isp %>% dplyr::select(b4_isco) %>% 
  group_by(b4_isco) %>% dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n))

ocup <- ocup[,1:4] %>% filter(!is.na(oficio1))
ocupacion <- merge(ocup, table_ocup, by.x = "oficio1", by.y = "b4_isco")
ocupacion[,-c(1,5)] <- ocupacion[,-c(1,5)] %>% mutate_if(is.numeric, function(x)x*100)
ocupacion[,-1] <-round(ocupacion[,-1],2)
ocupacion <- ocupacion %>% unite("IC-95%", proportion_low, proportion_upp, sep = "-")


## Redondear ----------
sex[,-1] <-round(sex[,-1],2)
edad_[,-1] <-round(edad_[,-1],2)
cont[,-1] <-round(cont[,-1],2)
ram[,-1] <-round(ram[,-1],2)
jor[,-1] <-round(jor[,-1],2)
sin[,-1] <-round(sin[,-1],2)

# Guardar tablas
writexl::write_xlsx(list(sex, edad_, reg, com, ocupacion, ram, cont, jor, sin),"informe_metodologico/data/tablas_informe.xlsx", col_names = TRUE,format_headers = TRUE)
writexl::write_xlsx(reg,"informe_metodologico/data/reg.xlsx", col_names = TRUE,format_headers = TRUE)


###Graficos -------------
## Sexo
sexo <- sex %>% gather(key = "survey", value = "p", -sexo, -proportion_low, -proportion_upp, -n)
grafico1 <- ggplot(sexo, aes(x = survey, y = p*100 , fill = sexo)) +
  geom_bar(stat = "identity", color = "black", position = "dodge2")  + 
  geom_text(aes(label = paste0(p*100,"%")),position=position_dodge(width=0.9), vjust=-0.25, color="black", size= 4) +
  scale_x_discrete(breaks=c("proportion", "p"),labels=c("CASEN (2017)", "ISP (2020)")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020) y CASEN (2017)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "Sexo")

grafico1

ggsave(plot = grafico1,
  filename = "informe_metodologico/images/grafico1.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 15)

## Edad
edad <- edad_ %>% gather(key = "survey", value = "p", -edad_cat, -proportion_low, -proportion_upp, -n)
grafico2 <- ggplot(edad, aes(x = survey, y = p*100 , fill = edad_cat)) +
  geom_bar(stat = "identity", color = "black", position = "dodge2")  + 
  geom_text(aes(label = paste0(p*100,"%")),position=position_dodge(width=0.9), vjust=-0.25, color="black", size= 4) +
  scale_x_discrete(breaks=c("proportion", "p"),labels=c("CASEN (2017)", "ISP (2020)")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020) y CASEN (2017)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "Grupo etario")

grafico2

ggsave(plot = grafico2,
       filename = "informe_metodologico/images/grafico2.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 15)


## Rama
rama <- ram %>% gather(key = "survey", value = "p", -rama, -proportion_low, -proportion_upp, -n)
grafico3 <- ggplot(rama, aes(x = survey, y = p*100 , fill = reorder(rama, -p))) +
  geom_bar(stat = "identity", color = "black", position = "dodge2")  + 
  geom_text(aes(label = paste0(p*100,"%")),position=position_dodge(width=0.9), vjust=-0.25, color="black", size= 4) +
  scale_x_discrete(breaks=c("proportion", "p"),labels=c("CASEN (2017)", "ISP (2020)")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020) y CASEN (2017)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "Sector económico")

grafico3

ggsave(plot = grafico3,
       filename = "informe_metodologico/images/grafico3.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 15)

## Jornada
jor <- jor  %>% gather(key = "survey", value = "p", -jornada, -proportion_low, -proportion_upp, -n)
grafico4 <- ggplot(jor, aes(x = survey, y = round(p*100,2) , fill = reorder(jornada, -p))) +
  geom_bar(stat = "identity", color = "black", position = "dodge2")  + 
  geom_text(aes(label = paste0(round(p*100,2),"%")),position=position_dodge(width=0.9), vjust=-0.25, color="black", size= 4) +
  scale_x_discrete(breaks=c("proportion", "p"),labels=c("CASEN (2017)", "ISP (2020)")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020) y CASEN (2017)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "Jornada laboral")

grafico4

ggsave(plot = grafico4,
       filename = "informe_metodologico/images/grafico4.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 15)

## Contrato
cont <- cont  %>% gather(key = "survey", value = "p", -contrato, -proportion_low, -proportion_upp, -n)
grafico5 <- ggplot(cont, aes(x = survey, y = round(p*100,2) , fill = reorder(contrato, -p))) +
  geom_bar(stat = "identity", color = "black", position = "dodge2")  + 
  geom_text(aes(label = paste0(round(p*100,2),"%")),position=position_dodge(width=0.9), vjust=-0.25, color="black", size= 4) +
  scale_x_discrete(breaks=c("proportion", "p"),labels=c("CASEN (2017)", "ISP (2020)")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020) y CASEN (2017)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "Tipo de contrato")

grafico5

ggsave(plot = grafico5,
       filename = "informe_metodologico/images/grafico5.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 15)

## Sin
sin <- sin  %>% gather(key = "survey", value = "p", -sindicato, -proportion_low, -proportion_upp, -n)
grafico6 <- ggplot(sin, aes(x = survey, y = round(p*100,2) , fill = reorder(sindicato, -p))) +
  geom_bar(stat = "identity", color = "black", position = "dodge2")  + 
  geom_text(aes(label = paste0(round(p*100,2),"%")),position=position_dodge(width=0.9), vjust=-0.25, color="black", size= 4) +
  scale_x_discrete(breaks=c("proportion", "p"),labels=c("CASEN (2017)", "ISP (2020)")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020) y CASEN (2017)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "Afiliado", breaks = c("1","2"), labels= c("Si", "No"))

grafico6

ggsave(plot = grafico6,
       filename = "informe_metodologico/images/grafico6.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 15)


## 1 de Agosto

# 1. Sindicato 

table_sindicato <- isp %>% group_by(b5) %>% filter(b5 != 66, b5!=88,b5!=99) %>%
  dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n))

grafico0 <- ggplot(table_sindicato, aes(x = b5, y = round(p*100,2) , fill = b5)) +
  geom_bar(stat = "identity", color = "black", position = "dodge2")  + 
  geom_text(aes(label = paste0(n," (", round(p*100,2), "%)")),position=position_dodge(width=0.9), vjust=-0.25, color="black", size= 4) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,50)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "Asociación")

grafico0

ggsave(plot = grafico0,
       filename = "informe_metodologico/images/grafico0.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 15)

# 2. Sector por region
table_central <- isp %>% group_by(b1,a4_reg) %>% filter(b1 %in% c("Administracion Central", "SII", "Gendarmería", "Judicial", "Municipal")) %>%
  dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n)) %>% mutate(total = paste0(n," (", round(p*100,2), "%)")) %>% select(-n,-p)%>% pivot_wider(names_from = b1, values_from = total)

table_educacion <- isp %>% group_by(b1,a4_reg) %>% filter(b1 %in% c("Educacion Municipal", "Educacion Universitaria")) %>%
  dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n)) %>% mutate(total = paste0(n," (", round(p*100,2), "%)")) %>% select(-n,-p)%>% pivot_wider(names_from = b1, values_from = total)

table_salud <- isp %>% group_by(b1,a4_reg) %>% filter(b1 %in% c("Salud Municipal", "Salud Hospitalaria")) %>%
  dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n)) %>% mutate(total = paste0(n," (", round(p*100,2), "%)")) %>% select(-n,-p)%>% pivot_wider(names_from = b1, values_from = total)

table_ooss <- isp %>% group_by(b1,a4_reg) %>% filter(b1 %in% c("Obras Sanitarias")) %>%
  dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n)) %>% mutate(total = paste0(n," (", round(p*100,2), "%)")) %>% select(-n,-p)%>% pivot_wider(names_from = b1, values_from = total)

table_otros <- isp %>% group_by(b1,a4_reg) %>% filter(b1 %in% c("Otros")) %>%
  dplyr::summarise(n = n()) %>%
  mutate(p = n / sum(n)) %>% mutate(total = paste0(n," (", round(p*100,2), "%)")) %>% select(-n,-p)%>% pivot_wider(names_from = b1, values_from = total)

# Guardar tablas
writexl::write_xlsx(list(table_central, table_salud, table_educacion, table_ooss),"informe_metodologico/data/tablas2_informe.xlsx", col_names = TRUE,format_headers = TRUE)
