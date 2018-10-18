# packages ----------------------------------------------------------------
library(tidyverse)
library(partykit)
library(randomForest)

# data --------------------------------------------------------------------
N <- 1000
data <- data_frame(
  x = rnorm(N),
  y = x * 5 + 3 * x^3 + 4 * rnorm(N)
)

ggplot(data) +
  geom_point(aes(x, y))

# que vamoh a hacer -------------------------------------------------------
# 1. Generar data de des y val
# 2. Modelos, regresion lineal, ctree, randomForest

data <- data %>%
  mutate(muestra = sample(c("D", "V"), size = N, replace = TRUE))

data

data %>% count(muestra)


# regresion lineal --------------------------------------------------------
mod_rl <- lm(y ~ x, data = filter(data, muestra == "D"))

# agregamoh
data <- data %>% 
  mutate(mod_rl = predict(mod_rl, newdata = data))

ggplot(data) +
  geom_point(aes(x, y), alpha = 0.1) +
  geom_line(aes(x, mod_rl), color = "red", size = 1.2)

data <- data %>% 
  mutate(mod_rl_res = y - mod_rl)

ggplot(data) +
  geom_point(aes(x, mod_rl_res))



# regresion lineal - el regreso -------------------------------------------
data <- data %>%
  mutate(x2 = x^2)

mod_rl2 <- lm(y ~ x + x2, data = filter(data, muestra == "D"))
summary(mod_rl2)

# agregamoh
data <- data %>% 
  mutate(mod_rl2 = predict(mod_rl2, newdata = data))

ggplot(data) +
  geom_point(aes(x, y), alpha = 0.1) +
  geom_line(aes(x, mod_rl2), color = "red", size = 1.2)

data <- data %>% 
  mutate(mod_rl2_res = y - mod_rl2)

ggplot(data) +
  geom_point(aes(x, mod_rl2_res))

data <- data %>%
  mutate(x2 = x^2)

# regresion lineal - ahora si que si --------------------------------------
data <- data %>%
  mutate(x3 = x^3)

mod_rl3 <- lm(y ~ x + x2 + x3, data = filter(data, muestra == "D"))
summary(mod_rl3)

# agregamoh
data <- data %>% 
  mutate(mod_rl3 = predict(mod_rl3, newdata = data))

ggplot(data) +
  geom_point(aes(x, y), alpha = 0.1) +
  geom_line(aes(x, mod_rl3), color = "blue", size = 1.2)

data <- data %>% 
  mutate(mod_rl3_res = y - mod_rl3)

ggplot(data) +
  geom_point(aes(x, mod_rl3_res))

ggplot(data) +
  geom_point(aes(x2, mod_rl3_res))

ggplot(data) +
  geom_point(aes(x3, mod_rl3_res))

# hechar a pelear con el ctree --------------------------------------------
mod_ct <- ctree(y ~ x, data = filter(data, muestra == "D"))
mod_ct

plot(mod_ct, gp = gpar(fontsize = 7))

ggplot(data) +
  geom_point(aes(x, y))

data <- data %>% 
  mutate(mod_ct = predict(mod_ct, newdata = data))

ggplot(data) +
  geom_point(aes(x, y), alpha = 0.1) +
  geom_line(aes(x, mod_ct), color = "blue", size = 1.2)


# hechar a pelear con el ctree 2 ------------------------------------------
mod_ct3 <- ctree(y ~ x + x2 + x3,
                 data = filter(data, muestra == "D"))
mod_ct3

plot(mod_ct3, gp = gpar(fontsize = 7))

ggplot(data) +
  geom_point(aes(x, y))

data <- data %>% 
  mutate(mod_ct3 = predict(mod_ct3, newdata = data))

ggplot(data) +
  geom_point(aes(x, y), alpha = 0.1) +
  geom_line(aes(x, mod_ct3), color = "blue", size = 1.2)


# randomForest ------------------------------------------------------------
mod_rf <- randomForest(y ~ x,
                       data = filter(data, muestra == "D"),
                       do.trace = TRUE,
                       ntree = 1000)

mod_rf

# randomForest::getTree(mod_rf, 1001)

plot(mod_rf)

ggplot(data) +
  geom_point(aes(x, y))

data <- data %>% 
  mutate(mod_rf = predict(mod_rf, newdata = data))

ggplot(data) +
  geom_point(aes(x, y), alpha = 0.1) +
  geom_line(aes(x, mod_rf), color = "green", size = 1.2)


# calcular los residuous faltantes ----------------------------------------

data <- data %>%
  mutate(
    mod_ct_res = y - mod_ct,
    mod_ct3_res = y - mod_ct3,
    mod_rf_res = y - mod_rf
    )

glimpse(data)

data %>% 
  select(ends_with("res")) %>% 
  mutate_all(~ .x^2) %>% 
  summarise_all(mean) %>% 
  gather(mod, ecm)


data %>% 
  select(muestra, ends_with("res")) %>% 
  group_by(muestra) %>% 
  mutate_all(~ .x^2) %>% 
  summarise_all(mean) %>% 
  gather(mod, ecm, -muestra) %>% 
  spread(muestra, ecm) %>% 
  arrange(V)


ggplot(data) +
  geom_point(aes(x, y), alpha = 0.1) +
  facet_wrap(~ muestra) +
  geom_line(aes(x, mod_rl3), color = "red", size = 1.2)


ggplot(data) +
  geom_point(aes(x, y), alpha = 0.1) +
  facet_wrap(~ muestra) +
  geom_line(aes(x, mod_rf), color = "red", size = 1.2)




