# paquetes ----------------------------------------------------------------
library(tidyverse)    # manipular y graficar
library(partykit)     # Arboles
library(ModelMetrics) # metricias de performance
library(smbinning)    # Datos

# install.packages("smbinning") # chileno

# chileandcredit<- readRDS("https://github.com/cran/smbinning/blob/master/data/chileancredit.RData?raw=true")

# datos -------------------------------------------------------------------
data("chileancredit")

chileancredit

# Primero deberemos separar la """tabla"""
# P: Por que?
# Para genera un arbol(modelo), saber como _generaliza_

# P: Como separo la tabla?
# R: Al azar, para que cada una de las muestras
# sea parecida, en terminos de las caracteristicas
# de las variables.
# No producir zezgoz

# P: Como hago algo al azar.
# comando sample

sample(1:10, 3)

set.seed(123)
sample(1:10, 3)

set.seed(123)
para_donde_se_va <- sample(
 c("para aka", "pallá"),
 size = 10000,
 prob = c(0.5, 0.5),
 replace = TRUE
)

table(para_donde_se_va)

chileancredit <- chileancredit %>% 
  mutate(para_donde_se_va = para_donde_se_va)

ddes <- chileancredit %>% 
  filter(para_donde_se_va == "para aka")
# 4943

# listocos para hacer nuestro arbol/modelo --------------------------------
mod <- ctree(fgood ~ ., data = ddes)
# quiero crear un arbol (ctree) 
# explicando fgood con todas 
# las variables (para eso el '.'), y usando como
# data de generacion de este arbol 'ddes'
# Creando el objeto/arbol mod 
dir.create("clase-06/data")
saveRDS(chileancredit, "clase-06/data/chileancredit.rds")

readRDS("https://github.com/jbkunst/usach-ingemat-intro-elementos-ds-201802/blob/master/clase-06/data/chileancredit.rds?raw=true")

