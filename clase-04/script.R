# install.packages("tidyverse")
# install.packages("readxl")
library(tidyverse)
library(readxl)

# data <- read_xlsx("~/../Downloads/autorizacionesdeestaciones-basea-nivel-nacionalagosto2018.xls")
# url <- "http://datos.gob.cl/dataset/bd4b5c0c-4207-492a-a676-a0f4e568b254/resource/679bfd23-2af2-481b-804e-c59ba05c3e29/download/autorizacionesdeestaciones-basea-nivel-nacionalagosto2018.xls"
# basename("my-directorio/escondido/quenadieve/archivosrecreto.ext")
# download.file(url, basename(url))

# arbirlo y cambiarlo de extension
data <- read_xlsx("~/../Downloads/antenas.xlsx")

# uff! al fin! ------------------------------------------------------------
glimpse(data)

# las compañias?
data %>% 
  count(Empresa, sort = TRUE) %>% 
  mutate(p = n/sum(n)) 
# la fecha
# Nosotros: Johny, todos, como van creciendo
# las _instalaciones_ por el tiempo
glimpse(data)

# recordar!!!
library(lubridate)
data <- data %>% 
  mutate(
    fecha = dmy(`Fecha Documento`)
  )

glimpse(data)

data_fechas <- data %>% 
  count(fecha)

ggplot(data_fechas) +
  geom_col(aes(fecha, n))

data_fechas <- data_fechas %>% 
  mutate(cumn = cumsum(n))

data_fechas

ggplot(data_fechas) +
  geom_line(aes(fecha, cumn))

data <- data %>% 
  mutate(
    Empresa2 = fct_lump(Empresa, n = 6)
  )

data %>% count(Empresa2, sort = TRUE)
  

glimpse(data)

data_fechas <- data %>% 
  count(fecha, Empresa2)

data_fechas <- data_fechas %>% 
  group_by(Empresa2) %>% 
  mutate(cumn = cumsum(n))

ggplot(data_fechas) +
  geom_line(aes(fecha, cumn))

ggplot(data_fechas) +
  geom_line(aes(fecha, cumn)) +
  facet_wrap(~Empresa2)

data_fechas <- data %>% 
  count(fecha, Región)

data_fechas <- data_fechas %>% 
  group_by(Región) %>% 
  mutate(cumn = cumsum(n))

ggplot(data_fechas) +
  geom_line(aes(fecha, cumn, group = Región, color = Región)) +
  scale_color_viridis_d() 


# DMS to DD ---------------------------------------------------------------
# install.packages("biogeo")
library(biogeo)

dd <- c(23, 45, 34)
mm <- c(45, 34, 22)
ss <- c(2, 56, 10)
ns <- c("E", "W", "N")
a <- dms2dd(dd, mm, ss, ns)
a


# a mapear?! --------------------------------------------------------------


glimpse(data)
data <- data %>% 
  mutate(
    lat = dms2dd(Lat_Grados, Lat_Minutos, Las_Segundos, "S"),
    lon = dms2dd(Lon_Grados, Lon_Minutos, Lon_Segundos, "W"),
    anio = year(fecha)
  )

glimpse(data)

# source("https://install-github.me/rstudio/httpuv")
# install.packages("leaflet")
library(leaflet)

glimpse(data)


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print

install.packages(c("httpuv"))
