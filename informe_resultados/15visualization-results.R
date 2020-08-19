# Visualizaciones informe metodologico ----------

# 0. Set Up
# 0.1 Set up theme
library(ggplot2); theme_set(theme_classic(base_size = 14) + theme(legend.position = "bottom", axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.title=element_text(size=15), 
                                                                  legend.text=element_text(size=15), plot.caption = element_text(size=15)))
# 0.2 ISP corporate colors
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

# Funcion para dejar ordenados segun levels y que no se traslapen en ggplot
str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}


# 1. Cargar librerías
pacman::p_load(tidyverse, ggplot2,dplyr,sjPlot, sjmisc,labelVector, ggpubr)

# 2. Cargar base de datos
setwd("C:/Users/Valentina Andrade/Dropbox/2. Trabajo/ISP")
load("data/isp3.RData")

# 3. Filtros
isp <- isp %>%  filter(a1_sexo !="Otro")


# 4. Recodificacion general
isp$mod <- car::recode(isp$d5_mod, c("'Teletrabajo total'='Teletrabajo';'Teletrabajo parcial'='Mixta';'Trabajo normal'='Presencial'"), as.factor = T,
                       levels = c ('Teletrabajo', 'Mixta', 'Presencial'))

table(isp$d5_mod)
table(isp$mod)
isp$b3 <- car::recode(isp$b3, recodes = c("1='Directivo';2='Tecnico';3='Profesional';4='Administrativo';5='Auxiliar'; NA=NA"), as.factor = T,
                      levels = c('Directivo',  'Profesional', 'Tecnico', 'Administrativo', 'Auxiliar'))

levels(isp$b3)
isp$i2 <- car::recode(isp$i2, c("'Teletrabajo total'='Teletrabajo';'Teletrabajo parcial'='Mixta';'Trabajo normal'='Presencial'"), as.factor = T,
                       levels = c ('Teletrabajo', 'Mixta', 'Presencial'))

levels(isp$b1)
table(isp$b1)
isp$b1<-factor(isp$b1,  levels = c("Administracion Central", "SII" , "Gendarmería", "Judicial", "Municipal", "Educacion Municipal", "Educacion Universitaria", "Salud Municipal", "Salud Hospitalaria", "Obras Sanitarias", "Otros", "99", "66", "85"))


# 5. Graficos ------------
# Capitulo 1-------------
# 0. Intrucciones:
#Tabla 1.1 y 1.2 (modalidad de trabajo general y según sexo, de ser posible resumir en un solo gráfico.

# Grafico 0--------------------
# 1. Manipular
## Sexo
isp %>% 
  group_by(mod) %>% 
  tally(wt=pond2) %>% 
  mutate(p = n/sum(n)) %>% 
  ggplot(aes(x = mod, y = p*100, fill = mod)) +
  geom_bar(stat = "identity", color = "black")  + 
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .5), color="black", size= 5) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "Modalidad de trabajo")

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico0.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)


# Grafico 1 -------------------------------
# 1. Manipular
## Sexo
mod <- isp %>% filter(!is.na(a1_sexo), a1_sexo != "Otro") %>% 
  group_by(a1_sexo, mod) %>% 
  tally(wt=pond2) %>% 
  mutate(p = n/sum(n))
mod2 <- mod %>% group_by(mod) %>%
  summarise(n2 =sum(n)) %>% 
  mutate(p2 = n2/sum(n2)) 
mod <- merge(mod, mod2,  all.x = T)

# 2. Graficar
ggplot(mod, aes(x = mod, y = p*100 , fill = a1_sexo)) +
  geom_bar(stat = "identity", color = "black")  + 
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .5), color="black", size= 4) +
  geom_text(mod[c(1,3,5),], mapping = aes(label = paste0(round(p2*100,1),"%")), hjust = 0.5, vjust=-17, color="black", size= 4) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "Modalidad de trabajo")

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico1.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# Graficos 2----------------------------------
# 0. Instrucciones: transformar en 3 diagramas de cajas

# 2. Graficar
cuidado <- isp %>% select(a1_sexo,a6.1,a6.2,a6.3) %>%  gather(key = "cuidado", value = "p", -a1_sexo)
ggplot(cuidado, aes(x=a1_sexo, y=p, fill=cuidado)) + 
  geom_boxplot() + 
  labs(x = "", y = "N° personas", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "", breaks = c("a6.1", "a6.2", "a6.3"), labels = c("Total", "Personas en riesgo", "Estudiantes")) 

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico2.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# Grafico 3 -------------------------
cuidado <- isp %>%  select(b1,a6.1) %>% filter(b1 !="66", b1!="99",b1!="Otros") %>%   gather(key = "cuidado", value = "p", -b1) %>% arrange(b1)
means <- aggregate(p ~ b1, cuidado,function(x) c(mean = round(mean(x, na.rm=TRUE), 2)), na.action = na.pass) %>% arrange(b1)
cuidado$b1<-factor(cuidado$b1,  levels = c("Administracion Central", "SII" , "Gendarmería", "Poder Judicial", "Municipal", "Educacion Municipal", "Educacion Universitaria", "Salud Municipal", "Salud Hospitalaria", "Obras Sanitarias", "Otros", "99", "66", "85"))
table(cuidado$b1)

# 2. Graficar
ggplot(cuidado, aes(x=str_wrap_factor(b1,15), y=p, fill = b1)) + 
  geom_boxplot() +
  labs(x = "", y = "N° personas", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=FALSE)+
  scale_fill_isp(name= "")+  geom_text(data = means, aes(label = p, y = p), size = 4)

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico3.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# Grafico 4 ---------------------------------------
# 1. Manipular
cuidado <- isp %>%  select(b3,a6.1) %>%   gather(key = "cuidado", value = "p", -b3) 
means <- aggregate(p ~ b3, cuidado,function(x) c(mean = round(mean(x, na.rm=TRUE), 2)), na.action = na.pass)
# 2. Graficar
ggplot(cuidado, aes(x=str_wrap_factor(b3,15), y=p, fill=b3)) + 
  geom_boxplot() + scale_x_discrete(limits = c("Directivo", "Profesional", "Tecnico", "Administrativo", "Auxiliar"))+
  labs(x = "", y = "N° personas", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=FALSE)+
  scale_fill_isp(name= "")+  geom_text(data = means, aes(label = p, y = p+0.5), size = 4)

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico4.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

## Capitulo 2 ----------

#0. Instrucciones
# Tabla 2.1 un gráfico con 4 barras (una por cada ítem)
#con un degradé de colores para distinguir las 5 alternativas de respuesta donde siempre es el más oscuro
#(posiblemente degradé de rojos a blanco?)
# Tabla 2.4 idem.

# Grafico 5 -------------------------------------
# 1. Manipular
# d1.1, d1.2, d1.3, d1.4 
ld1 <- find_var(isp, pattern = "d1.", out = "df")
# 2. Grafico 5 
plot_stackfrq(ld1, geom.colors = "Reds", show.total = F, weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom")

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico5.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 15)
# Grafico 6 -----------------------------------
# 1. Manipular
# d2.1, d2.2
ld2 <- find_var(isp, pattern = "d2.", out = "df")

# 2. Grafico  
plot_stackfrq(ld2, geom.colors = "Reds", show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom")

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico6.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 15)

# Capitulo 3 ------------
#Tabla 3.1 con una barra por ítem (son 7) y con degradé de menos a más. 

# Grafico 7 ----------------------------------------------
# 1. Manipular
ld4 <- find_var(isp, pattern = "d4.", out = "df")

# 2. Grafico 5 
plot_stackfrq(ld4, geom.colors = "Reds", show.total = F, weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom")

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico7.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 15)


# Capitulo 4 ------------
# 0. Instrucciones 
# Tabla 4.2 Espacio para trabajar en términos generales.
# Tabla 4.3 Espacio para trabajar según sexo (cada sexo es una barra dividida en cada alternativa de respuesta. (De ser posible un gráfico con 4.2 y 4.3)
#Idem lo anterior para 4.6/4.7; 4.15/4.16; 

# Grafico 8 -------------------------------------------------
esp <- isp %>% filter(!is.na(a1_sexo), !is.na(e7)) %>% 
  group_by(e7, a1_sexo) %>% 
  tally(wt=pond2) %>% 
  mutate(p = n/sum(n))
esp2 <- esp %>% filter(!is.na(e7)) %>% 
  group_by(e7) %>%
  summarise(n2 =sum(n)) %>% 
  mutate(p2 = n2/sum(n2)) 
esp <- merge(esp, esp2,  all.x = T)

# 2. Graficar
ggplot(esp, aes(x = e7, y = p*100 , fill = a1_sexo)) +
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .5), color="black", size= 4) +
  geom_text(esp[c(1,3),], mapping = aes(label = paste0(round(p2*100,1),"%")), hjust = 0.5, vjust=-13, color="black", size= 4) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,110)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "")

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico8.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

## Grafico 9 -------------------------------------------
# 1. Manipular
# d2.1, d2.2
le3 <- find_var(isp, pattern = "e3.", out = "df")
le3 <- set_label(le3, e3.1 = "Computador de escritorio o notebook",         
                 e3.2 = "Celular",          
                 e3.3 = "Conexión a Internet")
# 2. Grafico 
plot_stackfrq(le3, geom.colors = "Reds", show.total = F,weight.by = isp$pond2, wrap.labels = 2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico9.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

## Grafico 10 --------------------------------------------
# Alguno de las herramientas son de propiedad o servicio
isp <- isp %>% mutate(propiedad = case_when(e3.1 == "Servicio" ~ "Servicio",
                                            e3.2 == "Servicio" ~ "Servicio",
                                            e3.3 == "Servicio" ~ "Servicio",
                                            e3.1 == "Propias" ~ "Propias",
                                            e3.2 == "Propias" ~ "Propias",
                                            e3.3 == "Propias" ~ "Propias",
                                            e3.1 == "No la utilizo" ~ "No la utilizo",
                                            e3.2 == "No la utilizo" ~ "No la utilizo",
                                            e3.3 == "No la utilizo" ~ "No la utilizo",
                                            TRUE ~ NA_character_))

prop <- isp %>% filter(!is.na(a1_sexo), !is.na(propiedad)) %>% 
  group_by(a1_sexo, propiedad) %>% 
  tally(wt=pond2) %>% 
  mutate(p = n/sum(n))

# 2. Graficar
ggplot(prop, aes(x = a1_sexo, y = p*100 , fill = propiedad)) +
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .5), color="black", size= 4) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,110)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "")

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico10.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# Grafico 11 -------------------------------------------
# 1. Manipular
prop <- isp %>% filter(!is.na(b3), !is.na(propiedad)) %>% 
  group_by(b3, propiedad) %>% 
  tally(wt=pond2) %>% 
  mutate(p = n/sum(n))

# 2. Graficar
ggplot(prop, aes(x = b3, y = p*100 , fill = propiedad)) +
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .5), color="black", size= 4) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,110)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "")

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico11.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

## Grafico 12, 13 y 14 ----------------------------------------
# 1. Manipular
le3 <- isp  %>% pivot_wider(names_from = c(a1_sexo), values_from = e6.3_desc, names_glue = "{.value}_{a1_sexo}")
le3 <- find_var(le3, pattern = "e6.3_desc", out = "df")
le4 <- isp  %>% pivot_wider(names_from = c(b1), values_from = e6.3_desc, names_glue = "{.value}_{b1}")
le4 <- find_var(le4, pattern = "e6.3_desc", out = "df")
le5 <- isp  %>% pivot_wider(names_from = c(b3), values_from = e6.3_desc, names_glue = "{.value}_{b3}")
le5 <- find_var(le5, pattern = "e6.3_desc", out = "df")
le6 <- isp  %>% pivot_wider(names_from = c(b7), values_from = e6.3_desc, names_glue = "{.value}_{b7}")
le6 <- find_var(le6, pattern = "e6.3_desc", out = "df")
le7 <- isp  %>% pivot_wider(names_from = c(mod), values_from = e6.3_desc, names_glue = "{.value}_{mod}")
le7 <- find_var(le7, pattern = "e6.3_desc", out = "df")
le <- cbind(le3,le4,le5,le6,le7)
names(le)
le <- set_label(le,
                e6.3_desc_Femenino = "Femenino",           
                 e6.3_desc_Masculino          = "Masculino",
                "e6.3_desc_Administracion Central" = "Administracion Central", 
                e6.3_desc_SII                   = "SII",   
                "e6.3_desc_Gendarmería" = "Gendarmería",
                e6.3_desc_Judicial              = "Judicial",
                e6.3_desc_Municipal              = "Municipal",
                "e6.3_desc_Educacion Municipal"    = "Educación Municipal",
                "e6.3_desc_Educacion Universitaria"= "Educación Universitaria", 
                "e6.3_desc_Salud Municipal"        = "Salud Municipal",
                 "e6.3_desc_Salud Hospitalaria"     = "Salud Hospitalaria",
                 "e6.3_desc_Obras Sanitarias"       = "Obras Sanitarias",
                e6.3_desc_Directivo              = "Directivo", 
                 e6.3_desc_Profesional            = "Profesional",
                e6.3_desc_Tecnico               = "Tecnico", 
                 e6.3_desc_Administrativo         = "Administrativo",           
                 e6.3_desc_Auxiliar               = "Auxiliar",           
                 e6.3_desc_Completa               = "Jornada Completa",           
                 e6.3_desc_Parcial                = "Jornada Parcial",           
                 "e6.3_desc_No sujeto a cumplimiento de horario" = "No sujeto a cumplimiento de horario",
                 e6.3_desc_Teletrabajo = "Modalidad Teletrabajo",                 
                 e6.3_desc_Mixta = "Modalidad Mixta")
 
# 2. Grafico  12
plot_stackfrq(le[,1:2], geom.colors = "Reds", show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico12.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# 2. Grafico  13
le <- le[,-c(13,15)]
le <- le[,-10]
plot_stackfrq(le[,c(11,3,7,6,8,9,5,12,10,4)], geom.colors = "Reds", show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico13.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 15)

# 2. Grafico  14
le1 <- le[, c(17,13,15,14,16)]
plot_stackfrq(le1, geom.colors = "Reds", show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico14.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 15)

# 2. Grafico  15
plot_stackfrq(le[,c(18:22)], geom.colors = "Reds", show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 


#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico15.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 35,
       height = 15)

## Capitulo 5 ---------------
## Grafico 16 ------------------------------------------------
# 0. Instrucciones:
#1 visualización con los 15 ítem de medidas lugar de trabajo (solo general)

# 1. Manipular
# Modulo f.1
lf1 <- find_var(isp, pattern = "f1", out = "df")

# 2. Grafico 
plot_stackfrq(lf1, geom.colors = "Reds", show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") + 
  guides(fill = guide_legend(nrow = 2))
# 3. Guardar

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico16.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 23,
       height = 20)


## Grafico 17 ---------------------------------------------------
#1 visualización con los 4 ítem de problemas por medidas (solo general)
# 1. Manipular
# Modulo f.3
#1 visualización con los 6 riesgos general y otro por sexo (si se puede uno solo)
isp$f2.1 <- car::recode(isp$f2.1, recodes= c("1='Si';2='No';NA=NA"), as.factor = T,
                        levels= c("Si", "No"))
lf2 <- isp %>% select(pond2,starts_with("f2.")) %>%  gather(key = "problema", value = "resp", -pond2)
lf2 <- lf2 %>% filter(!is.na(resp)) %>% 
  group_by(problema,resp) %>% 
  tally(wt=pond2) %>% 
  mutate(p = n/sum(n))
# 2. Graficar
ggplot(lf2, aes(x = str_wrap_factor(problema,15), y = p*100 , fill = resp)) +
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .5), color="black", size= 5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  scale_x_discrete(breaks = c("f2.1", "f2.2", "f2.3", "f2.4"),labels = c("Problemas interaccion", "Problemas de tiempo", "Dificultad manip. herramientas", "Irritacion"))+
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(legend.position = "bottom") +
  scale_fill_isp(name = "")

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico17.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 15)


## Grafico 18 ------------------------------------------
# 1. Manipular
# Modulo f.3
#1 visualización con los 6 riesgos general y otro por sexo (si se puede uno solo)
lf3 <- find_var(isp, pattern = "f3", out = "df")

# 2. Grafico 
plot_stackfrq(lf3, geom.colors = "Reds",digits = 0, show.total = F,weight.by = isp$pond2, wrap.labels = 2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico18.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 35,
       height = 15)

## Grafico 19 ----------------------------------------------
#General
lf3 <- find_var(isp, pattern = "f3.", out = "df")
# 2. Grafico 
plot_stackfrq(lf3, geom.colors = "Reds",digits = 0, show.total = F,weight.by = isp$pond2, wrap.labels = 2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 
# Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico19-general.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 20,
       height = 10)

#Hombres
lf3 <- filter(isp, a1_sexo == "Masculino")
lf3_h <- find_var(lf3, pattern = "f3", out = "df")
# 2. Grafico 
lf3_h <- plot_stackfrq(lf3_h, geom.colors = "Reds",digits = 0, show.total = F,weight.by = lf3$pond2) +
  guides(fill= F) + labs(title = "Hombres")
lf3_h
#Mujeres
lf3 <- filter(isp, a1_sexo == "Femenino")
lf3_m <- find_var(lf3, pattern = "f3", out = "df")
# 2. Grafico 
lf3_m <- plot_stackfrq(lf3_m, geom.colors = "Reds",digits = 0, show.total = F,weight.by = lf3$pond2) +
  labs(title = "Mujeres",caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 

lf <- gridExtra::grid.arrange(lf3_h,lf3_m, nrow =2)

ggsave(plot = lf,
       filename = "informe_resultados/images/grafico19.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 20)

# Capitulo 6 ----
# Modulo h1
## Grafico 20 -----------------------------------------------
#1 visualización con los 6 riesgos general y otro por sexo (si se puede uno solo)
lh1 <- find_var(isp, pattern = "n_rps", out = "df")

# 2. Grafico 
plot_stackfrq(lh1, geom.colors = "Reds",digits = 1, show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico20-simple.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 20,
       height = 15)
#Grafico 20.1, 20.2, 20.3 ------------------------------------------
# 1. Manipular
lerps3 <- isp  %>% pivot_wider(names_from = c(a1_sexo), values_from = n_rps, names_glue = "{.value}_{a1_sexo}")
lerps3 <- find_var(lerps3, pattern = "n_rps", out = "df")
lerps4 <- isp  %>% pivot_wider(names_from = c(b1), values_from = n_rps, names_glue = "{.value}_{b1}")
lerps4 <- find_var(lerps4, pattern = "n_rps", out = "df")
lerps5 <- isp  %>% pivot_wider(names_from = c(b3), values_from = n_rps, names_glue = "{.value}_{b3}")
lerps5 <- find_var(lerps5, pattern = "n_rps", out = "df")
lerps6 <- isp  %>% pivot_wider(names_from = c(b7), values_from = n_rps, names_glue = "{.value}_{b7}")
lerps6 <- find_var(lerps6, pattern = "n_rps", out = "df")
lerps7 <- isp  %>% pivot_wider(names_from = c(mod), values_from = n_rps, names_glue = "{.value}_{mod}")
lerps7 <- find_var(lerps7, pattern = "n_rps", out = "df")
lerps <- cbind(lerps3,lerps4,lerps5,lerps6,lerps7)
lerps <- set_label(lerps,n_rps_Femenino = "Femenino",           
                   n_rps_Masculino          = "Masculino",
                   "n_rps_Administracion Central" = "Administracion Central", 
                   n_rps_SII                   = "SII",   
                   "n_rps_Gendarmería" = "Gendarmería",
                   n_rps_Judicial              = "Judicial",
                   n_rps_Municipal              = "Municipal",
                   "n_rps_Educacion Municipal"    = "Educación Municipal",
                   "n_rps_Educacion Universitaria"= "Educación Universitaria", 
                   "n_rps_Salud Municipal"        = "Salud Municipal",
                   "n_rps_Salud Hospitalaria"     = "Salud Hospitalaria",
                   "n_rps_Obras Sanitarias"       = "Obras Sanitarias",
                   n_rps_Directivo              = "Directivo", 
                   n_rps_Profesional            = "Profesional",
                   n_rps_Tecnico               = "Tecnico", 
                   n_rps_Administrativo         = "Administrativo",           
                   n_rps_Auxiliar               = "Auxiliar",           
                   n_rps_Completa               = "Jornada Completa",           
                   n_rps_Parcial                = "Jornada Parcial",           
                   "n_rps_No sujeto a cumplimiento de horario" = "No sujeto a cumplimiento de horario",
                   n_rps_Teletrabajo = "Modalidad Teletrabajo",                 
                   n_rps_Mixta = "Modalidad Mixta")

# 2. Grafico  
plot_stackfrq(lerps[,1:2], geom.colors = "Reds", show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico20.1.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# 2. Grafico  20.1
lerps <- lerps[,-c(13,15)]
lerps <- lerps[,-10]
plot_stackfrq(lerps[,c(11,3,7,6,8,9,5,12,10,4)], geom.colors = "Reds", show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico20.2.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# 2. Grafico  20.2
lerps1 <- lerps[, c(16,14,15,13,17)]
plot_stackfrq(lerps1, geom.colors = "Reds", show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 

#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico20.3.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# 2. Grafico  20.3
plot_stackfrq(lerps[,c(18:22)], geom.colors = "Reds", show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 


#3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico20.4.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 15)

## Grafico 21.1 y 21.2 --------------------------------------

# 0. Escala de Estres (100 puntaje maximo)
summary(isp$sm)
#1. Manipular
sm <- isp %>% select(a1_sexo,sm) %>%  gather(key = "sm", value = "p", -a1_sexo)
means <- aggregate(p ~ a1_sexo, sm,function(x) c(mean = round(mean(x, na.rm=TRUE), 2)), na.action = na.pass)

# 2. Graficar 21.1
ggplot(sm, aes(x=a1_sexo, y=p, fill=a1_sexo)) + 
  geom_boxplot() + 
  labs(x = "", y = "Escala Salud Mental (1-100 puntos)", title = "", caption = "Fuente: Elaboración propia en base ISP (2020) y SUSESO_ISTAS21")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "") +  geom_text(data = means, aes(label = p, y = p+1.5), size = 4)

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico21.1.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# 2. Graficar 22.2
sm <- isp %>%  select(b3,sm) %>%   gather(key = "sm", value = "p", -b3)
means <- aggregate(p ~ b3, sm,function(x) c(mean = round(mean(x, na.rm=TRUE), 2)), na.action = na.pass)
ggplot(sm, aes(x=str_wrap_factor(b3,15), y=p, fill=b3)) + 
  geom_boxplot() + scale_x_discrete(limits = c("Directivo", "Profesional", "Tecnico", "Administrativo", "Auxiliar"))+
  labs(x = "", y = "Escala Salud Mental (1-100 puntos)", title = "", caption = "Fuente: Elaboración propia en base ISP (2020) y SUSESO-ISTAS21")  +
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=FALSE)+
  scale_fill_isp(name= "")+  geom_text(data = means, aes(label = p, y = p+2), size = 4)

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico21.2.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# 2. Graficar 21.3 
sm <- isp %>%  select(b1,sm) %>%  filter(b1 != 99,b1 != 66,b1 != "Otros") %>%  gather(key = "sm", value = "p", -b1)
means <- aggregate(p ~ b1, sm,function(x) c(mean = round(mean(x, na.rm=TRUE), 2)), na.action = na.pass)
ggplot(sm, aes(x=str_wrap_factor(b1,15), y=p, fill=b1)) + 
  geom_boxplot() +  labs(x = "", y = "Escala Salud Mental (1-100 puntos)", title = "", caption = "Fuente: Elaboración propia en base ISP (2020) y SUSESO-ISTAS21")  +
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=FALSE)+
  scale_fill_isp(name= "")+  geom_text(data = means, aes(label = p, y = p+2), size = 4)

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico21.3.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 35,
       height = 15)

# 2. Graficar 21.4
sm <- isp %>%  select(mod,sm) %>%   gather(key = "sm", value = "p", -mod)
means <- aggregate(p ~ mod, sm,function(x) c(mean = round(mean(x, na.rm=TRUE), 2)), na.action = na.pass)
ggplot(sm, aes(x=str_wrap_factor(mod,15), y=p, fill=mod)) + 
  geom_boxplot() +  labs(x = "", y = "Escala Salud Mental (1-100 puntos)", title = "", caption = "Fuente: Elaboración propia en base ISP (2020) y SUSESO-ISTAS21")  +
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=FALSE)+
  scale_fill_isp(name= "")+  geom_text(data = means, aes(label = p, y = p+2), size = 4)

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico21.4.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)


## Capitulo 7 ---------
# 0. Instrucciones
# Tabla 7.1 con las mismas indicaciones. General y por sexo sobre principal responsable.

## Grafico 22 --------------------------
# 1. Manipular
isp$g1 <- as.numeric(isp$g1)
isp$g1 <- car::recode(isp$g1, c("1='Principal responsable';2='Mitad de las tareas';3='Cuarta parte';4='Tareas puntuales';5='No hago tareas'"), as.factor = T,
                      levels = c('Principal responsable', 'Mitad de las tareas', 'Cuarta parte','Tareas puntuales', 'No hago tareas'))
dom <- isp %>% filter(!is.na(a1_sexo), !is.na(g1)) %>% 
  group_by(a1_sexo, g1) %>% 
  tally(wt=pond2) %>% 
  mutate(p = n/sum(n))
dom2 <- dom %>% group_by(g1) %>%
  summarise(n2 =sum(n)) %>% 
  mutate(p2 = n2/sum(n2)) 
dom <- merge(dom, dom2,  all.x = T)

# 2. Graficar
ggplot(dom, aes(x = a1_sexo, y = p*100 , fill = g1)) +
  geom_bar(stat = "identity", color = "black")  + 
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .6), color="black", size= 3.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") +
  scale_fill_isp(name = "Carga trabajo doméstico", breaks = c('No hago tareas', 'Tareas puntuales', 'Cuarta parte', 'Mitad de las tareas', 'Principal responsable')) + 
  guides(fill = guide_legend(nrow = 2))
# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico22.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

## Grafico 23-------------------------
lg3 <- find_var(isp, pattern = "g4", out = "df")
lg3 <- set_label(lg3, g4 = "Cantidad de trabajo")

lg4 <- isp  %>% pivot_wider(names_from = c(a1_sexo), values_from = g4, names_glue = "{.value}_{a1_sexo}")

lg4 <- find_var(lg4, pattern = "g4", out = "df")
lg <- cbind(lg3,lg4)
lg <- set_label(lg,
                "g4_Femenino" = "Cantidad de trabajo - Mujeres",           
                "g4_Masculino"= "Cantidad de trabajo - Hombres")

# 2. Grafico 
plot_stackfrq(lg, geom.colors = "Reds",digits = 1, show.total = F,weight.by = isp$pond2, wrap.labels = 10) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico23.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 20,
       height = 10)


# Grafico 24 --------------------------------------------------
# 0. Instrucciones Violencia 5 situaciones (sexo) 
#General
lg <- find_var(isp, pattern = "g3.", out = "df")
# 2. Grafico 
plot_stackfrq(lg, geom.colors = "Reds",digits = 0, show.total = F,weight.by = isp$pond2, wrap.labels = 1) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 
# Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico24-general.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 10)

#Hombres
lg <- filter(isp, a1_sexo == "Masculino")
lg_h <- find_var(lg, pattern = "g3.", out = "df")
# 2. Grafico 
lg_h <- plot_stackfrq(lg_h, geom.colors = "Reds",digits = 0, show.total = F,weight.by = lg$pond2) +
  guides(fill= F) + labs(title = "Hombres")
lg_h
#Mujeres
lg <- filter(isp, a1_sexo == "Femenino")
lg_m <- find_var(lg, pattern = "g3.", out = "df")
# 2. Grafico 
lg_m <- plot_stackfrq(lg_m, geom.colors = "Reds",digits = 0, show.total = F,weight.by = lg$pond2) +
  labs(title = "Mujeres",caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 
lg_m

lg <- gridExtra::grid.arrange(lg_h,lg_m, nrow =2)

ggsave(plot = lg,
       filename = "informe_resultados/images/grafico24.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 20,
       height = 20)

## Capitulo 8
# Grafico 25 -----------------------------------------------
li <- find_var(isp, pattern = "i1.", out = "df")
# 2. Grafico 
plot_stackfrq(li, geom.colors = "Reds",digits = 1, show.total = F,weight.by = isp$pond2) +
  labs(caption = "Fuente: Elaboración propia en base a ISP (2020)") + theme(legend.position = "bottom") 
# Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico25.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 20)

# Grafico 26 --------------------------------------------------------

# 1. Manipular
tele <- isp %>% filter(!is.na(a1_sexo), !is.na(i2)) %>% 
  group_by(a1_sexo, i2) %>% 
  tally(wt=pond2) %>% 
  mutate(p = n/sum(n))

tele1 <- tele %>% select(-p) %>% spread(a1_sexo, n) %>% mutate(Total = Femenino + Masculino) %>% gather(a1_sexo, p, -i2) 

tele2 <- tele1 %>% filter(!is.na(a1_sexo), !is.na(i2)) %>% 
  group_by(a1_sexo) %>% 
  mutate(p2 = p/sum(p))



# 2. Graficar
ggplot(tele2, aes(x = a1_sexo, y = p2*100 , fill = i2)) +
  geom_bar(stat = "identity", color = "black")  + 
  geom_text(aes(label = paste0(round(p2*100,1),"%")),position=position_stack(vjust = .6), color="black", size= 5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") + scale_fill_isp(name = "")
# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico26.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# Grafico 27 ---------------------------------------------

# 1. Manipular
tele <- isp %>% filter(!is.na(b1), !is.na(i2), b1 !=66, b1!=99, b1!="Otros" ) %>% 
  group_by(b1, i2) %>% 
  tally(wt=pond2) %>% 
  mutate(p = n/sum(n))

# 2. Graficar
ggplot(tele, aes(x = str_wrap_factor(b1,15), y = p*100 , fill = i2)) +
  geom_bar(stat = "identity", color = "black")  + 
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .6), color="black", size= 5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,101)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") + scale_fill_isp(name = "")
# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico27.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 35,
       height = 15)

# Grafico 28 --------------------------------------------------

# 1. Manipular
tele <- isp %>% filter(!is.na(b3), !is.na(i2)) %>% 
  group_by(b3, i2) %>% 
  tally(wt=pond2) %>% 
  mutate(p = n/sum(n))

# 2. Graficar
ggplot(tele, aes(x = b3, y = p*100 , fill = i2)) +
  geom_bar(stat = "identity", color = "black")  + 
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .6), color="black", size= 5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,101)) + 
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") + scale_fill_isp(name = "")
# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico28.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)

# Grafico 29 --------------------------------------------------

# 1. Manipular
tele <- isp %>% filter(!is.na(mod), !is.na(i2)) %>% 
  group_by(mod, i2) %>% 
  tally(wt=pond2) %>% 
  mutate(p = n/sum(n)) 
tele[,-c(1,2)] <-round(tele[,-c(1,2)],3) #the "-1" excludes column 1


# 2. Graficar
a <- tele %>% filter(mod == "Teletrabajo") %>% 
ggplot(aes(x = mod, y = p*100 , fill = i2)) +
  geom_bar( width = 1, stat = "identity", color = "white")  + coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .6), color="black", size= 3.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,101)) + 
  labs(x = "", y = "Porcentaje", title = "Teletrabajo")  +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_isp(name = "") + theme_void() + guides(fill = F)

b <- tele %>% filter(mod == "Mixta") %>% 
  ggplot(aes(x = mod, y = p*100 , fill = i2)) +
  geom_bar( width = 1, stat = "identity", color = "white")  + coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .6), color="black", size= 3.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,101)) + 
  labs(x = "", y = "Porcentaje", title = "Mixta")  +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_isp(name = "") + theme_void() + guides(fill = F)

c<- tele %>% filter(mod == "Presencial") %>% 
  ggplot(aes(x = mod, y = p*100 , fill = i2)) +
  geom_bar( width = 1, stat = "identity", color = "white")  + coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(p*100,1),"%")),position=position_stack(vjust = .6), color="black", size= 3.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,101)) + 
  labs(x = "", y = "Porcentaje", title = "Presencial")  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") + scale_fill_isp(name = "") + theme_void()

d <- ggarrange(a,b,c, ncol = 3, common.legend = TRUE, legend="bottom")
annotate_figure(d, bottom = text_grob("Fuente: Elaboracion propia en base a ISP (2020)"))
# 3. Guardar
ggsave(plot = d,
       filename = "informe_resultados/images/grafico29.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)


## Grafico 29 - Opcion B ----------------------
# 2. Graficar
ggplot(tele, aes(x = mod, y = p*100 , fill = i2)) +
  geom_bar( width = 0.8, stat = "identity", color = "black")   + 
  geom_text(aes(label = paste0(round(p*100,2),"%")),position=position_stack(vjust = .5), color="black", size= 4) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,101)) + scale_x_discrete(breaks = c("Teletrabajo", "Mixta", "Presencial"), labels =c("En modalidad teletrabajo", "En modalidad Mixta", "En modalidad presencial"))+
  labs(x = "", y = "Porcentaje", title = "", caption = "Fuente: Elaboración propia en base a ISP (2020)")  +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_isp(name = "") 

# 3. Guardar
ggsave(plot = last_plot(),
       filename = "informe_resultados/images/grafico29-opcionB.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 15)
