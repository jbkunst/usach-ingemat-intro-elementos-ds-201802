library(tidyverse) # pack de paquetes
library(DBI)       # Data Base Interface

# La base que nos comunicaremos es un bbdd 
# MySQL doh! 
library(RMySQL)

# SI PASA ESTO
# Error in library(juanijuanharry) : 
#   there is no package called ‘juanijuanharry’
# ENTONCES
# install.packages("juanijuanharry")

# Supongamos que LAM nos pide una consutoría
# para saber la razón de las quejas que 
# la gente hace por RRSS
# 
# LAM nos da su acceso
con <- dbConnect(
  drv = MySQL(),
  dbname = "nycflights13",
  host = "142.93.20.188", 
  port = 3306,
  user = "test",
  password = "HFW9KYZBnEYr!"
)

con

# Funcion que nos enlista las tablas
# que hay en la bbdd
dbListTables(con)


# Ahora, a lo bueno! ------------------------------------------------------

# P: Como accedo a una "tabla" (tbl) de una conexión
# R: Lo siguiente me muestra el contenido
# de una tabla
tbl(con, "planes")

tbl(con, "flights")

# P: Veremos el 'vuelo' origen-destino mas comun
# R: 
vuelos <- tbl(con, "flights")
vuelos

# crearemos una tabla (virtual?) de las combinaciones
dorides <- vuelos %>% 
  count(origin, dest) %>% 
  arrange(desc(n))

dorides
show_query(dorides)

# C (de comentario): La tabla, el output
# siguie viviendo en el servidor
# P: Como me la traigo a mi pc/sesion?
# R:
dorides <- collect(dorides)
dorides

# write.table(dorides, "~/para_mi_amigo_el_pavo.txt")
# library(writexl)
# write_xlsx(dorides, "~/para_mi_amigo_el_pavo.xlsx")
dorides

dorides %>% 
  spread(origin, n)

dorides

glimpse(vuelos)

vuelos_res <- vuelos %>% 
  # filtramos por el origen-destino mas comun
  filter(origin == "JFK", dest == "LAX") %>% 
  # agrupamos por aerolinea
  group_by(carrier) %>% 
  # resumimos el atraso
  summarise(
    cantidad_de_vuelos = n(),
    atraso_promedio = mean(dep_delay + arr_delay)
  ) %>% 
  arrange(desc(atraso_promedio))
  
show_query(vuelos_res)
  
vuelos_res <- collect(vuelos_res)
vuelos_res
  
ggplot(vuelos_res) +
  geom_col(aes(carrier, atraso_promedio))

# P: Es el promedio un buen referente?
# R:
ar1 <- c(1,1,1,1,1,0,0,0,0,0,0,0,100)
ar2 <- c(2,2,2,2,2,2,3,2,0,1,3,3)

mean(ar1)
mean(ar2)

# C: El promedio no siempre es bueno para
# tomar una decisión

# P: Entonces, que hago?
# R: Hay muchas formas, entregar otros resumenes
# como quantiles
# RJ: La visualización
vuelos_detalle <- vuelos %>% 
  # filtramos por el origen-destino mas comun
  filter(origin == "JFK", dest == "LAX") %>% 
  mutate(atraso_total = dep_delay + arr_delay) %>% 
  select(flight, carrier, atraso_total)

vuelos_detalle

vuelos_detalle <- collect(vuelos_detalle)
vuelos_detalle

# Estudiemos los atrasos globalmente
ggplot(vuelos_detalle) +
  geom_histogram(aes(atraso_total))
# Mmm... ese rango


ggplot(vuelos_detalle) +
  geom_histogram(aes(atraso_total)) +
  scale_x_log()
# Mmmmm... no se leer bien escalas
# logarítmicas

ggplot(vuelos_detalle) +
  geom_histogram(aes(atraso_total)) +
  scale_x_continuous(limits = c(-100, 200))
# Un poco mejorsh

ggplot(vuelos_detalle) +
  geom_density(aes(atraso_total)) +
  scale_x_continuous(limits = c(-100, 200))

# Y los carriers?
# (la magia de ggplot2)
ggplot(vuelos_detalle) +
  geom_histogram(aes(x = atraso_total, fill = carrier)) +
  scale_x_continuous(limits = c(-100, 200))
# Puede ser merjosh

ggplot(vuelos_detalle) +
  geom_histogram(aes(x = atraso_total, fill = carrier)) +
  scale_x_continuous(limits = c(-100, 200)) +
  facet_wrap(~carrier)

# Hay aerolinea que hacen pocos vaijes y 
# y el histograma se hace pequeño
ggplot(vuelos_detalle) +
  geom_histogram(aes(x = atraso_total, fill = carrier)) +
  scale_x_continuous(limits = c(-100, 200)) +
  facet_wrap(~carrier, scales = "free")


ggplot(vuelos_detalle) +
  geom_density(aes(x = atraso_total, group = carrier, fill = carrier),
               alpha = 0.5) +
  scale_x_continuous(limits = c(-100, 200)) 



p <- ggplot(vuelos_detalle) +
  geom_histogram(aes(x = atraso_total, fill = carrier)) +
  scale_x_continuous(limits = c(-100, 200)) +
  facet_wrap(~carrier, scales = "free") 
p

# Puedo exportalo

# Y si me pongo mas fancy
library(plotly)
plotly::ggplotly(p)

# p2 <- p + 
#   scale_fill_viridis_d() +
#   theme_minimal()
# 
# plotly::ggplotly(p2)