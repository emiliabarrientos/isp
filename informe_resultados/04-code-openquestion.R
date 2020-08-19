#### Codificacion de preguntas abiertas ####
### Valentina Andrade
#1. Cargar librerias
pacman::p_load(dplyr,
               stringr,
               ISCO08ConveRsions)
#2. Cargar base de datos-----
load("data/ISP_proc.RData")
isco08 <- readxl::read_excel("data/struct-isco08.xls", na = "NA")

#3. Explorar variable ocupacion-----
tibble(ISP_proc$b4)
str_count(ISP_proc$b4, "director")
ISP_proc %>% 
  mutate(b4_cod = str_replace_all(b4, "administrativo", "11")) %>%
  select(c(b4,b4_cod))
ISP_proc %>% 
  mutate(b4_cod = ifelse(grepl("direct", b4, ignore.case = TRUE), "11", b4),
         ifelse(grepl("jefe", b4, ignore.case = TRUE), "12", b4),
ifelse(grepl("jefe", b4, ignore.case = TRUE), "12", b4),
ifelse(grepl("jefe|opera|producc", b4, ignore.case = TRUE), "13", b4),
ifelse(grepl("administra", b4, ignore.case = TRUE), "23", b4)) %>%
  select(c(b4,b4_cod))

#4. Exportar e importar -----
isp <- ISP_proc %>% select(respondent_id, b3,b4)
write.csv(isp, "data/isp-ocupacion.csv")
isp_ocupacion <- read.csv(file = "data/isp-ocupacion2.csv", sep = ";")

#5. Unir -----
ISP_proc1 <- merge(ISP_proc, isp_ocupacion, by = "respondent_id")
ISP_proc1$b4_isco <- as.numeric(ISP_proc1$b4_isco)

#6. Codificar a 2 digitos
summary(ISP_proc1$b4_isco)
ISP_proc1 <- ISP_proc1 %>%  mutate(b4_isco_cod = case_when(b4_isco == 11	~ "Directores ejecutivos, personal directivo de la administración pública y miembros del poder ejecutivo y de los cuerpos legislativos",
                                                      b4_isco == 12	~ "Directores administradores y comerciales",
                                                      b4_isco == 13	~ "Directores y gerentes de producción y operaciones",
                                                      b4_isco == 14	~ "Gerentes de hoteles, restaurantes, comercios y otros servicios",
                                                      b4_isco == 21	~ "Profesionales de las ciencias y de la ingeniería",
                                                      b4_isco == 22	~ "Profesionales de la salud",
                                                      b4_isco == 23	~ "Profesionales de la enseñanza",
                                                      b4_isco== 24	~ "Especialistas en organización de la administración publica y de empresas",
                                                      b4_isco== 25	~ "Profesionales de tecnología de la información y las comunicaciones",
                                                      b4_isco== 26	~ "Profesionales en derecho, en ciencias sociales y culturales",
                                                      b4_isco== 31	~ "Profesionales de las ciencias y la ingeniería de nivel medio",
                                                      b4_isco== 32	~ "Profesionales de nivel medio de la salud",
                                                      b4_isco== 33	~ "Profesionales de nivel medio en operaciones financieras y administrativas",
                                                      b4_isco== 34	~ "Profesionales de nivel medio de servicios jurídicos, sociales, culturales y afines",
                                                      b4_isco== 35	~ "Técnicos de la tecnología de la información y las comunicaciones",
                                                      b4_isco== 41	~ "Oficinistas",
                                                      b4_isco== 42	~ "Empleados en trato directo con el público",
                                                      b4_isco== 43	~ "Empleados contables y encargados del registro de materiales",
                                                      b4_isco== 44	~ "Otro personal de apoyo administrativo",
                                                      b4_isco== 51	~ "Trabajadores de los servicios personales",
                                                      b4_isco== 52	~ "Vendedores",
                                                      b4_isco== 53	~ "Trabajadores de los cuidados personales",
                                                      b4_isco== 54	~ "Personal de los servicios de protección",
                                                      b4_isco== 61	~ "Agricultores y trabajadores calificados de explotaciones agropecuarias con destino al mercado",
                                                      b4_isco== 62	~ "Trabajadores forestales calificados, pescadores y cazadores",
                                                      b4_isco== 63	~ "Trabajadores agropecuarios, pescadores, cazadores y recolectores de subsistencia",
                                                      b4_isco== 71	~ "Oficiales y operarios de la construcción excluyendo electricistas",
                                                      b4_isco== 72	~ "Oficiales y operarios de la metalurgia, la construcción mecánica y afines",
                                                      b4_isco== 73	~ "Artesanos y operarios de las artes gráficas",
                                                      b4_isco== 74	~ "Trabajadores especializados en electricidad y la elecrotecnología",
                                                      b4_isco== 75	~ "Operarios y oficiales de procesamiento de alimentos, de la confección, ebanistas, otros artesanos y afines",
                                                      b4_isco== 81	~ "Operadores de instalaciones fijas y máquinas",
                                                      b4_isco== 82	~ "Ensambladores",
                                                      b4_isco== 83	~ "Conductores de vehículos y operadores de equipos pesados móviles",
                                                      b4_isco== 91	~ "Limpiadores y asistentes",
                                                      b4_isco== 92	~ "Peones agropecuarios, pesqueros y forestales",
                                                      b4_isco== 93	~ "Peones de la minería, la construcción, la industria manufacturera y el transporte",
                                                      b4_isco== 94	~ "Ayudantes de preparación de alimentos",
                                                      b4_isco== 95	~ "Vendedores ambulantes de servicios y afines",
                                                      b4_isco== 96	~ "Recolectores de desechos y otras ocupaciones elementales",
                                                      b4_isco== 310~ "Otros miembros de las fuerzas armadas",
                                                      b4_isco == 88	~ "88",
                                                      b4_isco == 99	~ "99",
                                                      b4_isco == 66	~ "66",
                                                      TRUE ~NA_character_))



#7. Guardar-----
ISP_proc <- ISP_proc1 %>% select(-c(b3_cod, b4_cod, b4))
save(ISP_proc, file = "data/ISP_proc.RData")
