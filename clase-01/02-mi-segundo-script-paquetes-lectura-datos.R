# P: Que es lo primero que hago?
# R: Instalar paquetes!
# Esto es una vez por instalacion de R (o formateo de PC XD)
# install.packages(c("prophet", "tidyverse", "readxl", "writexl"))

# P: Que #$%#$ es tidyverse?
# R: Es un pack de paquetes

# P: Como se usa/carga?
library(tidyverse)

# P: Y donde estan los datos???
# install.packages("gapminder"), o manual :´(
library(gapminder) # cargamos el paquete que contiene los datos
data(gapminder)    # ponemos en el enviorment los datos
gapminder          # llamamos o imprimimos los datos

# Entiendo el mundo -------------------------------------------------------

# funcion clever para ojear
glimpse(gapminder)

# P: Cuantos paises hay?
count(gapminder, country)

# V2 (con el simbolito raro pipe, CTRL+SHIFT+M)
# se lee como _luego_
gapminder %>% 
  count(country)

# P: Para que me sirve el %>% %>% %>% ?
sqrt(2)

2 %>% sqrt()

tan(exp(sin(log(sqrt(2)))))

2 %>% 
  sqrt() %>% 
  log() %>% 
  sin() %>% 
  exp() %>% 
  tan()

gapminder %>% 
  filter(continent == "Asia") %>% 
  arrange(year) %>% 
  group_by(country) %>% 
  mutate(crecimiento_anual = (pop - lag(pop))/pop) %>% 
  filter(country == "Japan")

filter(mutate(group_by(arrange(filter(gapminder, continent == "Asia"), year),country), crecimiento_anual = (pop - lag(pop))/pop), country == "Japan")

# P: Cada pais tiene la misma cantidad de registros?
gapminder %>% 
  count(country) %>% 
  count(n)
# R: Efectivamente todas tienen la misma cantidad blabla


# P: Cual es el pais que ha tenido menor 
# poblacion en la historia de los paises
# around the world? y diga el año
# 
# hint: 
x <- c(2, 3, 1) 
x
x == min(x)

# R:
gapminder %>% 
  arrange(pop) %>% 
  head(1)

gapminder %>% 
  filter(pop == min(pop))


# P: Que pais ha tenido la mayor poblacion
# en promedio de en los datos registrados?
gapminder %>% 
  group_by(country) %>% 
  summarise(
    promedio_pop = mean(pop),
    esperanza_vida_max = max(lifeExp)
  ) %>% 
  # filter(promedio_pop == max(promedio_pop))
  arrange(desc(promedio_pop))

# hint
x <- c(1:10)
x

sqrt(x)

mean(x)
max(x)

c(4, 5)
c(4, 5, c(1, 2))

# P: De todos los paises africanos, cual 
# es el pais con mayor crecimiento entre los
# años 1952  1992?

# filter(continent == "Africa" & year %in% c(1952, 1992)) %>% 
# o
# filter(continent == "Africa",  year %in% c(1952, 1992)) %>% 

gapminder %>% 
  filter(continent == "Africa") %>%
  filter(year %in% c(1952, 1992)) %>% 
  select(country, year, lifeExp) %>% 
  spread(year, lifeExp) %>% 
  mutate(diff =  `1992` - `1952`) %>% 
  arrange(desc(diff))


tinf <- gapminder %>% 
  filter(continent == "Africa") %>%
  # filter(year %in% c(1952, 1992)) %>% 
  select(country, year, lifeExp) %>% 
  spread(year, lifeExp) 

tinf %>% 
  gather(anio, esperanza, -country) %>% 
  group_by(country) %>% 
  summarise(esperanza_media = mean(esperanza)) %>% 
  left_join(tinf) %>% 
  View()

tinf %>% 
  mutate(
    esperanza_media = (`1952` + `1957`)/ 12
  )

clientes <- data_frame(ruts = c("16019432-4", "123234-4"),
                       fecha = c("1999/22/23", "199/2/23"))

clientes %>% 
  separate(ruts, c("rutnum", "dv"), sep = "-") %>% 
  separate(fecha, c("a", "m", "d"), sep = "/") %>% 
  unite(mira, dv, m, sep = " mira lo que hice ")





