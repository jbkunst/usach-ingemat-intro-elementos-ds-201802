# packages ----------------------------------------------------------------
library(tidyverse)
library(partykit)
library(randomForest)
library(lubridate)
# library(dlookr)

# cambiar tema por default ggplot2
theme_set(theme_minimal())

# data --------------------------------------------------------------------
data <- read_csv("clase-12/data/day.csv")

ggplot(data) +
  geom_histogram(aes(cnt))

ggplot(data) +
  geom_point(aes(dteday, cnt))


# muestras ----------------------------------------------------------------
data <- data %>% 
  mutate(
    # muestra = sample(c("D", "V"), size = nrow(data), replace = TRUE),
    muestra = "D",
    muestra = ifelse(dteday > ymd("2012-07-01"), "OOT", muestra)
  )

ggplot(data) +
  geom_point(aes(dteday, cnt, color = muestra), size = 2, alpha = 0.5) +
  scale_color_manual(values = c("darkgreen", "magenta"))

data$casual
ggplot(data) +
  geom_line(aes(dteday, registered))

ggplot(data) +
  geom_line(aes(dteday, casual))

data %>% 
  select(casual, registered, cnt)

data <- data %>% 
  select(-casual, -registered)

# regresion lineal --------------------------------------------------------
glimpse(data)

mod_rl <- lm(cnt ~ instant + factor(season) + factor(mnth) + temp + hum + windspeed, data = filter(data, muestra == "D"))
mod_rl
summary(mod_rl)

data <- data %>% 
  mutate(mod_rl_pred = predict(mod_rl, newdata = data))

ggplot(data) +
  geom_point(aes(dteday, cnt, color = muestra)) +
  scale_color_viridis_d() +
  geom_line(aes(dteday, mod_rl_pred), size = 2, color = "red")


# evaluar
data <- data %>% 
  mutate(mod_rl_res = cnt - mod_rl_pred)

data %>% 
  filter(muestra == "OOT") %>% 
  summarise(mean(mod_rl_res^2))

# ctree -------------------------------------------------------------------
mod_ct <- ctree(cnt ~ instant + factor(season) + factor(mnth) + temp + hum + windspeed, data = filter(data, muestra == "D"))
mod_ct
plot(mod_ct, gp = gpar(fontsize = 7))

mod_ct <- data %>% 
  filter(muestra == "D") %>%
  select(-muestra) %>% 
  # select(-contains("_ct"), -contains("_rl"), -contains("_rf")) %>%
  select(-matches("_.*_")) %>% 
  select(-dteday) %>% 
  ctree(cnt ~ ., data = .)

plot(mod_ct, gp = gpar(fontsize = 7))

data <- data %>% 
  mutate(mod_ct_pred = predict(mod_ct, newdata = data))

ggplot(data) +
  geom_point(aes(dteday, cnt, color = muestra)) +
  scale_color_viridis_d() +
  geom_line(aes(dteday, mod_ct_pred), size = 2, color = "red")


# evaluar
data <- data %>% 
  mutate(mod_ct_res = cnt - mod_ct_pred)

data %>% 
  filter(muestra == "OOT") %>% 
  summarise(mean(mod_ct_res^2))

plot()

# randomForest ------------------------------------------------------------
data <- data %>% 
  mutate(
    season = factor(season),
    mnth = factor(mnth)
  )

glimpse(data)
# mod_rf <- randomForest(cnt ~ instant + season + mnth + temp + hum + windspeed, data = filter(data, muestra == "D"), do.trace = TRUE)
mod_rf <- data %>% 
  filter(muestra == "D") %>%
  select(-muestra) %>% 
  # select(-contains("_ct"), -contains("_rl"), -contains("_rf")) %>%
  select(-matches("_.*_")) %>% 
  randomForest(cnt ~ ., data = ., ntree = 100, do.trace = TRUE)
mod_rf

randomForest::varImpPlot(mod_rf)

data <- data %>% 
  mutate(mod_rf_pred = predict(mod_rf, newdata = data))

ggplot(data) +
  geom_point(aes(dteday, cnt, color = muestra)) +
  scale_color_viridis_d() +
  geom_line(aes(dteday, mod_rf_pred), size = 2, color = "red")


# evaluar
data <- data %>% 
  mutate(mod_rf_res = cnt - mod_rf_pred)

data %>% 
  filter(muestra == "OOT") %>% 
  summarise(mean(mod_rf_res^2))


# evaluar -----------------------------------------------------------------
data %>% 
  filter(muestra == "OOT") %>% 
  select(contains("_res")) %>% 
  mutate_all(~ .x^2) %>% 
  summarise_all(mean)


x  <- c("este no es un correo amig@", "jbkunst@gmail.com", "asd@as.clasd")
str_detect(x, ".*@.*")
str_detect(x, "^([a-zA-Z0-9_\\-\\.]+)@([a-zA-Z0-9_\\-\\.]+)\\.([a-zA-Z]{2,5})$")


