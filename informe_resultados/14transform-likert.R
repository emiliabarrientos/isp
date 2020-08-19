# 0. Transformar las variables para Likert_plot

# 1. Cargar librerias 
pacman::p_load(dplyr, tidyverse, forcats, haven)

# 2. Cargar base de datos
load("data/isp.RData")

# Modulo B
#b1
#d1

# Modulo D
#d1
isp$d1.1<-labelled_spss(isp$d1.1, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                               label = "Velocidad de trabajo")

isp$d1.2<-labelled_spss(isp$d1.2, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Cantidad de trabajo")
isp$d1.3<-labelled_spss(isp$d1.3, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Organización de trabajo")
isp$d1.4<-labelled_spss(isp$d1.4, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Tiempo de descanso")

# d2
isp$d2.1<-labelled_spss(isp$d2.1, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Trabajar fuera de la jornada laboral")
isp$d2.2<-labelled_spss(isp$d2.2, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Trabajar fin de semanas")

#d4
isp$d4.1<-labelled_spss(isp$d4.1, c("Disminuyó" = 1, "Sigue igual" = 2, "Aumentó" = 3),
                        label = "Velocidad de trabajo")
isp$d4.2<-labelled_spss(isp$d4.2, c("Disminuyó" = 1, "Sigue igual" = 2, "Aumentó" = 3),
                        label = "Cantidad de trabajo")
isp$d4.3<-labelled_spss(isp$d4.3, c("Disminuyó" = 1, "Sigue igual" = 2, "Aumentó" = 3),
                        label = "Organizacion de trabajo")
isp$d4.4<-labelled_spss(isp$d4.4, c("Disminuyó" = 1, "Sigue igual" = 2, "Aumentó" = 3),
                        label = "Tiempo de descanso")
isp$d4.5<-labelled_spss(isp$d4.5, c("Disminuyó" = 1, "Sigue igual" = 2, "Aumentó" = 3),
                        label = "Trabajar fuera de horario")
isp$d4.6<-labelled_spss(isp$d4.6, c("Disminuyó" = 1, "Sigue igual" = 2, "Aumentó" = 3),
                        label = "Trabajar fin de semana")
isp$d4.7<-labelled_spss(isp$d4.7, c("Disminuyó" = 1, "Sigue igual" = 2, "Aumentó" = 3),
                        label = "Salario")

# Modulo E
table(isp$e6.3_desc)
isp$e6.3_desc <- car::recode(isp$e6.3_desc, c("'Nunca'=1;'Rara vez'=2;'Algunas veces'=3;'Casi siemore'=4;'Siempre'=5;'No aplica'=NA"))
isp$e6.3_desc<-labelled_spss(isp$e6.3_desc, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5, "No aplica" = NA),
                        label = " Problemas - Desconexion")
table(isp$e6.3_desc)

# Modulo F
table(isp$f1.1)
isp$f1.1<-labelled_spss(isp$f1.1, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Protocolo de seguridad")
isp$f1.2<-labelled_spss(isp$f1.2, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Liberar jornada ser pob. riesgo")
isp$f1.3<-labelled_spss(isp$f1.3, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Liberar jornada fam. pob. riesgo")
isp$f1.4<-labelled_spss(isp$f1.4, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Horario ingreso/salida")
isp$f1.5<-labelled_spss(isp$f1.5, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Horario reducido at.publico")
isp$f1.6<-labelled_spss(isp$f1.6, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Separacion puestos trabajo")
isp$f1.7<-labelled_spss(isp$f1.7, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Sanitizacion instalaciones")
isp$f1.8<-labelled_spss(isp$f1.8, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Sanitizacion calzado")
isp$f1.9<-labelled_spss(isp$f1.9, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Control de temperatura")
isp$f1.10<-labelled_spss(isp$f1.10, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Uso mascarilla")
isp$f1.11<-labelled_spss(isp$f1.11, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Uso guantes")
isp$f1.12<-labelled_spss(isp$f1.12, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Uso protectores")
isp$f1.13<-labelled_spss(isp$f1.13, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Lavado manos")
isp$f1.14<-labelled_spss(isp$f1.14, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Vidrios at.publico")
isp$f1.15<-labelled_spss(isp$f1.15, c("Si, de forma adecuada" = 1, "Si, de manera insuficiente" = 2, "No se ha implementado" = 3, "No sabe"= NA),
                        label = "Distancia at.publico")


# f3
isp$f3.1<-labelled_spss(isp$f3.1, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Incumplimiento medidas funcionarios")

isp$f3.2<-labelled_spss(isp$f3.2, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Incumplimiento medidas usuarios")
isp$f3.3<-labelled_spss(isp$f3.3, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Violencia jefatura")
isp$f3.4<-labelled_spss(isp$f3.4, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Violencia usuarios")

isp$f3.5<-labelled_spss(isp$f3.5, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Violencia fisica usuarios")
isp$f3.6<-labelled_spss(isp$f3.6, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Riesgo contagio COVID")


# Modulo G
isp$g2.1<-labelled_spss(isp$g2.1, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Doble presencia")

isp$g2.2 <- car::recode(isp$g2.2, c("'Nunca'=1;'Rara vez'=2;'Algunas veces'=3;'Casi siemore'=4;'Siempre'=5;'No aplica'=NA"))
isp$g2.3 <- car::recode(isp$g2.3, c("'Nunca'=1;'Rara vez'=2;'Algunas veces'=3;'Casi siemore'=4;'Siempre'=5;'No aplica'=NA"))

isp$g2.2<-labelled_spss(isp$g2.2, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5, "No aplica" = NA),
                        label = "Interferencia trabajo doméstico")
isp$g2.3<-labelled_spss(isp$g2.3, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5, "No aplica" = NA),
                        label = "Interferencia trabajo cuidados")


#g3
isp$g3.1<-labelled_spss(isp$g3.1, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Conflicto vecinos")
isp$g3.2<-labelled_spss(isp$g3.2, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Conflictos familiares")
isp$g3.3<-labelled_spss(isp$g3.3, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Violencia familiar")
isp$g3.4<-labelled_spss(isp$g3.4, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Violencia psicológica")
isp$g3.5<-labelled_spss(isp$g3.5, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Violencia física")

#g4
isp$g4<-labelled_spss(isp$g4, c("Disminuyo" = 1, "Sigue igual" = 2, "Aumento"= 3),
                        label = "Cantidad de trabajo")


#h
isp$h1.1<-labelled_spss(isp$h1.1, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Rapidez entrega trabajos")
isp$h1.2<-labelled_spss(isp$h1.2, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Sobre carga trabajo")
isp$h1.3<-labelled_spss(isp$h1.3, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Tener tiempo trabajo al dia")
isp$h1.4<-labelled_spss(isp$h1.4, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Retraso entrega trabajo")
isp$h1.5<-labelled_spss(isp$h1.5, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Trabajar con tranquilidad")
isp$h1.6<-labelled_spss(isp$h1.6, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Tiempo suficiente para trabajar")
isp$h1.7<-labelled_spss(isp$h1.7, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Quedarse luego de la hora de salida")

#h2
isp$h2.1<-labelled_spss(isp$h2.1, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Nervioso")
isp$h2.2<-labelled_spss(isp$h2.2, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Decaido")
isp$h2.3<-labelled_spss(isp$h2.3, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Tranquilo")
isp$h2.4<-labelled_spss(isp$h2.4, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Desanimado")
isp$h2.5<-labelled_spss(isp$h2.5, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Feliz")
#h3
isp$h3.1<-labelled_spss(isp$h3.1, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Dolores")
isp$h3.2<-labelled_spss(isp$h3.2, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Percepcion de dolores")
isp$h3.3<-labelled_spss(isp$h3.3, c("Nunca" = 1, "Solo unas pocas veces" = 2, "Algunas veces"= 3, "La mayoria de las veces" = 4, "Siempre" = 5),
                        label = "Somatizar")

#i1
isp$i1.1<-labelled_spss(isp$i1.1, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Reuniones")

isp$i1.2<-labelled_spss(isp$i1.2, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Atencion usuarios")
isp$i1.3<-labelled_spss(isp$i1.3, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Atencion proveedores")
isp$i1.4<-labelled_spss(isp$i1.4, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Tareas simples sin concentracion")
isp$i1.5<-labelled_spss(isp$i1.5, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Tareas simples con concentracion")
isp$i1.6<-labelled_spss(isp$i1.6, c("Nunca" = 1, "Rara vez" = 2, "Algunas veces"= 3, "Casi siempre" = 4, "Siempre" = 5),
                        label = "Tareas complejas con concentracion")

table(isp$d5_mod)

#Guardar
save(isp, file = "data/isp3.RData")

