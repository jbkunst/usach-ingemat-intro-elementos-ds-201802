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

mod <- ctree(fgood ~ ., data = ddes,
             control = ctree_control(maxdepth = 2, minbucket = 700))
mod
plot(mod, gp = gpar(fontsize = 8))


set.seed(123)
nuevosdatos <- ddes %>% 
  sample_n(5) %>% 
  select(-fgood) %>% 
  select(inc, tob, dpd) %>% 
  mutate(rut = row_number())

nuevosdatos %>% View()

ddes <- ddes %>% 
  mutate(
    pred = predict(mod, newdata = ddes),
    nodo = predict(mod, newdata = ddes, type = "node"),
    prob = predict(mod, newdata = ddes, type = "prob")[,2]
  )

glimpse(ddes)

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

p
plotly::ggplotly(p)



# clase 08 ----------------------------------------------------------------
dnodres_inc <- ddes %>% 
  group_by(inc) %>% 
  summarise(
    n = n(),
    # fgood == 1 -> boolean: T/F -> T: 1, F: 0
    gr = mean(fgood == "1") # good rate
  ) %>% 
  mutate(p = n/sum(n))

ggplot(dnodres_inc, aes(inc)) +
  geom_col(aes(y = p), width = 0.5, alpha = 0.5) +
  geom_point(aes(y = gr), color = "darkred") +
  geom_line(aes(inc, gr), color = "darkred",
            group = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Regla", y = "Porcentaje",
    title = "Mi primer arbol :')",
    subtitle = "y 2do grafico"
  )
chileancredit


test <- prop.test(x = 289*0.512, n = 289, 0.512)
test$conf.int

plot(c(0.4545922, 0.512 , 0.5690930))


mi_funcion <- function(x = "Nombre") {
  
  paste("Hola", x)
  
}

mi_funcion("Oeh compareh")

limtest <- function(x, n) {
  pt <- prop.test(x, n)
  data_frame(pt$conf.int) %>% 
    set_names("v") %>% 
    mutate(lim = c("inf", "sup")) %>% 
    spread(lim, v)
}

# Quiero hacer tests
dnodres_inc <- dnodres_inc %>% 
  mutate(n_buenos = n*gr) %>% 
  mutate(limite = map2(n_buenos, n, limtest)) %>% 
  unnest()

ggplot(dnodres_inc, aes(inc)) +
  geom_col(aes(y = p), width = 0.5, alpha = 0.5) +
  geom_point(aes(y = gr), color = "darkred") +
  geom_line(aes(inc, gr), color = "darkred",
            group = 1) +
  geom_errorbar(aes(inc, ymin = inf, ymax = sup), width = 0.2, alpha = 0.15) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Regla", y = "Porcentaje",
    title = "Mi primer arbol :')",
    subtitle = "y 2do grafico"
  )


# Data de VALidación
dval <- chileancredit %>% 
  filter(para_donde_se_va == "pallá")

dval <- dval %>% 
  mutate(nodo = predict(mod, newdata = dval, type = "node"))


dvalres <- dval %>% 
  group_by(nodo) %>% 
  summarise(
    n = n(),
    gr = mean(fgood == "1") # good rate
  ) %>% 
  mutate(p = n/sum(n)) %>% 
  mutate(nodo = factor(nodo)) %>% 
  mutate(nodo = fct_reorder(nodo, -gr))
dvalres

ggplot(dvalres, aes(nodo)) +
  geom_col(aes(y = p), width = 0.5, alpha = 0.5) +
  geom_point(aes(y = gr), color = "darkred") +
  geom_line(aes(nodo, gr), color = "darkred",
            group = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Regla", y = "Porcentaje",
    title = "Mi primer arbol :')",
    subtitle = "y 3er grafico"
  )


# Esto es mas mejor -------------------------------------------------------
chileancredit <- chileancredit %>%
  mutate(nodo = predict(mod, newdata = chileancredit, type = "node"))

# Data CHIleancredit RESumen
dchires <- chileancredit %>% 
  group_by(para_donde_se_va, nodo) %>% 
  summarise(
    n = n(),
    gr = mean(fgood == "1") # good rate
  ) %>% 
  mutate(p = n/sum(n)) %>%
  mutate(nodo = factor(nodo)) %>% 
  mutate(nodo = fct_reorder(nodo, -gr))
dchires


ggplot(dchires, aes(nodo)) +
  geom_col(aes(y = p), width = 0.5, alpha = 0.5) +
  geom_point(aes(y = gr), color = "darkred") +
  geom_line(aes(nodo, gr), color = "darkred",
            group = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Regla", y = "Porcentaje",
    title = "Mi primer arbol :')",
    subtitle = "y 3er grafico"
  )


ggplot(dchires, aes(nodo)) +
  geom_col(aes(y = p, fill = para_donde_se_va), width = 0.5, alpha = 0.5, position = position_dodge()) +
  geom_line(aes(nodo, gr, group = para_donde_se_va,
                color = para_donde_se_va), size = 1.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("red", "blue")) +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()



