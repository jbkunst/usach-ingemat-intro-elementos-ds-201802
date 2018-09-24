library(tidyverse)
library(partykit)


data <- readRDS("clase-05/data/datos-clientes-usach.rds")

# considero solamente los casos que NO
# tienen NA (esto lo aprendi del futuro)
data <- data %>% 
  filter(complete.cases(.))

data <- data %>% 
  mutate(bm = as.factor(bm))

# hay una diferencia entre caracter R
# y factor
data <- sample_n(data, 10000)


# P: por que 5mil si tenemos ochorrocientos?
# R: por que el algoritmo no se la pudo, next
glimpse(data)

# bm: 1 bueno, 0 chupe

head(data) %>% 
  View()

# partimos copiando
ggplot(data) +
  geom_point(aes(x = bm, y = porcentaje_uso_final))


# bien feo la verdad
# m1: no lo muestres
ggplot(data) +
  geom_point(aes(x = bm, y = porcentaje_uso_final),
             alpha = 0.9, position = "jitter")


ggplot(data) +
  geom_point(aes(x = bm, y = porcentaje_uso_final,
                 color = bm),
             alpha = 0.1, position = "jitter")
# puede mejorar

# seguimos copiando de lo bueno
ggplot(data) +
  geom_point(aes(x = score_retail, y = porcentaje_uso_final,
                 color = bm),
             alpha = 0.1, position = "jitter")

# bm nunmerico entonces ggplot al hacer 
# escala de color la hace contiuna:
ggplot(data) +
  geom_point(aes(x = score_retail, y = porcentaje_uso_final,
                 color = as.character(bm)),
             alpha = 0.1) +
  scale_color_viridis_d()


# dejando default los aes()
ggplot(data, aes(x = score_retail, y = porcentaje_uso_final)) +
  geom_point(alpha = 0.1) +
  geom_density2d() +
  facet_wrap(~as.character(bm))



# si, si, muy bonito. pero el arbol?
# haremos que la variable sea categoria
# y no continua.

data %>% 
  count(bm)
d <- data
glimpse(d)

tri <- ctree(bm ~ ., data = d)

plot(tri, gp = gpar(fontsize = 10)) 

# oops, muy profundo
tri <- ctree(bm ~ ., data = d, control = ctree_control(maxdepth = 3))
plot(tri, gp = gpar(fontsize = 9)) 

d %>% 
  count(bm) %>% 
  mutate(p = n/sum(n))
