library(tidyverse)
library(DBI)
library(RMySQL)

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

dbListTables(con)


# Explorando otras tablas:
aeropuertos <- tbl(con, "airports")
aeropuertos
glimpse(aeropuertos)

# Para efecto de análsis, usaremos menos variabls
aeropuertos <- aeropuertos %>% 
  select(faa, name, lat, lon)

aeropuertos %>% 
  filter(faa == "JFK")

vuelos <- tbl(con, "flights")
glimpse(vuelos)

dorides <- vuelos %>% 
  count(origin, dest) %>% 
  arrange(desc(n))


dorides

# la juntación
left_join(dorides, aeropuertos)

# Error se debe a que no especificamos como 
# relacionar estas tablas
dorides <- left_join(dorides, aeropuertos,
                by = c("origin" = "faa"))

dorides

dorides <- left_join(dorides, aeropuertos,
                     by = c("dest" = "faa"))

show_query(dorides)

dorides

dorides <- collect(dorides)
dorides

aeropuertos <- collect(aeropuertos)
aeropuertos

# a graficar --------------------------------------------------------------

# aeropuertos
ggplot(aeropuertos)

ggplot(aeropuertos) +
  geom_point(aes(x = lon, y= lat))


# dorides
dorides

ggplot(dorides) +
  geom_segment(aes(x = lon.x, y = lat.x, 
                   xend = lon.y, yend = lat.y))

ggplot() +
  geom_point(aes(x = lon, y= lat), data = aeropuertos) +
  geom_segment(
    aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y),
    data = dorides
    )
# mmm clarito...

ggplot() +
  geom_point(aes(x = lon, y= lat), data = aeropuertos,
             alpha = 0.2) +
  geom_segment(
    aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y),
    data = dorides, alpha = 0.2
  )
# mmm NETX!

ggplot() +
  geom_point(aes(x = lon, y= lat), data = aeropuertos,
             alpha = 0.2) +
  geom_segment(
    aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y),
    data = dorides, alpha = 0.2
  ) +
  xlim(NA, -50)  # NA siginigifca 'N'o me import'A'
  
# install.packages("maps")
require("maps")

states <- map_data("state")
head(states)

ggplot() +
  geom_point(aes(x = lon, y= lat), data = aeropuertos,
             alpha = 0.2) +
  geom_segment(
    aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y),
    data = dorides, alpha = 0.2
  ) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states) +
  xlim(NA, -50)  # NA siginigifca 'N'o me import'A'


ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states) +
  geom_point(aes(x = lon, y= lat), data = aeropuertos,
             alpha = 0.2) +
  geom_segment(
    aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y),
    data = dorides, alpha = 0.2
  ) +
  xlim(NA, -50)  # NA siginigifca 'N'o me import'A'


ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states, color = "gray90", fill = "gray95") +
  geom_point(aes(x = lon, y= lat), data = aeropuertos,
             alpha = 0.2, size = 1) +
  geom_segment(
    aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y),
    data = dorides, alpha = 0.2, color = "gray70"
  ) +
  xlim(NA, -60) +
  theme_void()


ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states, color = "gray90", fill = "gray95") +
  # geom_point(aes(x = lon, y= lat), data = aeropuertos,
  #            alpha = 0.2, size = 1) +
  geom_segment(
    aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y),
    data = dorides, alpha = 0.2, color = "black"
  ) +
  xlim(NA, -60) +
  theme_void()

ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states, color = "gray90", fill = "gray95") +
  # geom_point(aes(x = lon, y= lat), data = aeropuertos,
  #            alpha = 0.2, size = 1) +
  geom_curve(
    aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y),
    data = dorides, alpha = 0.2, color = "black"
  ) +
  xlim(NA, -60) +
  theme_void()

