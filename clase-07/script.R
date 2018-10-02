# paquetes ----------------------------------------------------------------
library(tidyverse)    # manipular y graficar
library(partykit)     # Arboles
library(ModelMetrics) # metricias de performance

# datos -------------------------------------------------------------------
chileancredit <- readRDS(gzcon(url("https://github.com/jbkunst/usach-ingemat-intro-elementos-ds-201802/blob/master/clase-06/data/chileancredit.rds?raw=true")))
chileancredit <- tbl_df(chileancredit)

chileancredit %>% 
  count(fgood)

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

# ddes: Data de DESarrollo
ddes <- chileancredit %>% 
  filter(para_donde_se_va == "para aka") %>% 
  select(-period, -para_donde_se_va) %>% 
  mutate(fgood = as.factor(fgood))

mod <- ctree(fgood ~ ., data = ddes)

mod

plot(mod, gp = gpar(fontsize = 5))


# Pensamientos: Quiero el arbol mas grande
# desde chico escuché que más es mejor

# P: Como lo puedo hacer mas grande?
# R: Modificando paramter del ctree_control
?ctree_control

mod <- ctree(fgood ~ ., data = ddes,
             control = ctree_control(alpha = .2, minbucket = 1))
mod
plot(mod, gp = gpar(fontsize = 5))


# Pensamiento: Pa' que tan grande
# R: maxdepth (maxima profundidad)
mod <- ctree(fgood ~ ., data = ddes,
             control = ctree_control(maxdepth = 3))
mod
plot(mod, gp = gpar(fontsize = 8))

# Supongamos que tenemos nueva data
# (de esa que nos paso nuestro amigo hacker
# juan)
set.seed(123)
nuevosdatos <- ddes %>% 
  sample_n(5) %>% 
  select(-fgood) %>% 
  select(inc, tob, dpd) %>% 
  mutate(rut = row_number())

nuevosdatos %>% View()

# como no lo vamos a hacer de forma
# manual, por que somos(soy!) flojos
# alguien más re rajó con su habilidad y 
# tiempo
# Usaremos predict para esto:
# predict(modelo, nuevosdatos, tipo)
#   modelo: arbol, regresion lineal
#   nuevosdatos: son nuevos datos (formato)
#   tipo: 

predict(mod, newdata = nuevosdatos)

nuevosdatos <- nuevosdatos %>% 
  mutate(predict = predict(mod, newdata = nuevosdatos))
nuevosdatos %>% View()

predict(mod, newdata = nuevosdatos, type = "node")

nuevosdatos <- nuevosdatos %>% 
  mutate(nodo = predict(mod, newdata = nuevosdatos, type = "node"))
nuevosdatos %>% View()

predict(mod, newdata = nuevosdatos, type = "prob")

nuevosdatos <- nuevosdatos %>% 
  mutate(prob = predict(mod, newdata = nuevosdatos,
                        type = "prob")[,2])
nuevosdatos %>% View()

# Pensamiento: Mish, no está tan tan tan mal
# entonces visualicemos de otra manera
ddes <- ddes %>% 
  mutate(
    pred = predict(mod, newdata = ddes),
    nodo = predict(mod, newdata = ddes, type = "node"),
    prob = predict(mod, newdata = ddes, type = "prob")[,2]
  )

glimpse(ddes)

# Data NODo RESumen
dnodres <- ddes %>% 
  group_by(nodo) %>% 
  summarise(
    n = n(),
    # fgood == 1 -> boolean: T/F -> T: 1, F: 0
    gr = mean(fgood == "1") # good rate
  ) %>% 
  mutate(p = n/sum(n))
dnodres

ggplot(dnodres) +
  geom_col(aes(nodo, p)) +
  geom_line(aes(nodo, gr))

dnodres <- dnodres %>% 
  mutate(nodo = factor(nodo))

ggplot(dnodres) +
  geom_col(aes(nodo, p)) +
  geom_line(aes(nodo, gr), group = 1)

dnodres <- dnodres %>% 
  mutate(nodo = fct_reorder(nodo, -gr))

ggplot(dnodres) +
  geom_col(aes(nodo, p)) +
  geom_line(aes(nodo, gr), group = 1)


p <- ggplot(dnodres, aes(nodo)) +
  geom_col(aes(y = p), width = 0.5, alpha = 0.5) +
  geom_point(aes(y = gr), color = "darkred") +
  geom_line(aes(nodo, gr), color = "darkred",
            group = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Regla", y = "Porcentaje",
    title = "Mi primer arbol :')",
    subtitle = "Al infinito y a la esquina"
  )

plotly::ggplotly(p)




