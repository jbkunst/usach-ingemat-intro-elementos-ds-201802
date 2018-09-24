# paquetes ----------------------------------------------------------------
library(tidyverse)    # manipular y graficar
library(partykit)     # Arboles
library(ModelMetrics) # metricias de performance
# library(smbinning)    # Datos
# install.packages("smbinning") # chileno

chileancredit <- readRDS(gzcon(url("https://github.com/jbkunst/usach-ingemat-intro-elementos-ds-201802/blob/master/clase-06/data/chileancredit.rds?raw=true")))
chileancredit <- tbl_df(chileancredit)

chileancredit %>% 
  count(fgood)

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

# M: cannot handle objects of class ‘Date’
# R: Ok!
ddes <- ddes %>% 
  select(-period)

mod <- ctree(fgood ~ ., data = ddes)

# M: cannot handle objects of class ‘character’
# R: ok ok...
glimpse(ddes)

ddes %>% 
  count(para_donde_se_va)

ddes <- ddes %>% 
  select(-para_donde_se_va)

# Y nuevamente...
mod <- ctree(fgood ~ ., data = ddes)

mod

# Mmm... es medio dificl de ver que pasa con
# solo 'mod'
# R: plot it!!!!!
plot(mod)

plot(mod, gp = gpar(fontsize = 5))

# la 4ta es la venicida. Espero!

ddes <- ddes %>% 
  mutate(fgood = as.factor(fgood))

# P: Que es un factor:
# R: Es como una variable categorica, 
# con categorias definidas/fijas.

mod <- ctree(fgood ~ ., data = ddes)

mod

plot(mod,  gp = gpar(fontsize = 5))

plot(
  ctree(
    fgood~ .,
    data = ddes,
    control = ctree_control(maxdepth = 3)
    )
  )
ddes %>% mutate(fgood = fgood + rnorm(4943, sd = 0.1)) %>% tbl_df()


boxplot(rnorm(1000), horizontal = TRUE)
boxplot(rexp(1000), horizontal = TRUE)
boxplot(rbinom(10000, 1, p = 0.5))



