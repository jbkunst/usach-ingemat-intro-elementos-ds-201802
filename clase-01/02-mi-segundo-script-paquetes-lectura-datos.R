# P: Que es lo primero que hago?
# R: Instalar paquetes!
# Esto es una vez por instalacion de R (o formateo de PC XD)
install.packages(c("prophet", "tidyverse", "readxl", "writexl"))

# P: Que #$%#$ es tidyverse?
# R: Es un pack de paquetes

# P: Como se usa/carga?
library(tidyverse)

# P: Y donde estan los datos???
# install.packages("gapminder"), o manual :´(
library(gapminder) # cargamos el paquete que contiene los datos
data(gapminder)    # ponemos en el enviorment los datos
gapminder          # llamamos o imprimimos los datos



