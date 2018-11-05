# packages ----------------------------------------------------------------
library(tidyverse)
library(partykit)
library(randomForest)
library(lubridate)
# library(dlookr)

# cambiar tema por default ggplot2
theme_set(theme_minimal())

# data --------------------------------------------------------------------
data <- read_csv("clase-12/data/hour.csv")
data

glimpse(data)

# dlookr::eda_report(data, target = "cnt", output_format = "html")

# AED (analisi exploratorio de datos) -------------------------------------

# G:
plot(data %>% select(1:6) %>% sample_n(1000))

# emhe
ggplot(data) +
  geom_histogram(aes(cnt))

ggplot(data) +
  geom_boxplot(aes(y = cnt, x = factor(hr)))

data %>% 
  mutate(hr2 = sample(0:23, size = nrow(data), replace = TRUE)) %>% 
  ggplot() +
  geom_boxplot(aes(y = cnt, x = factor(hr2)))


ggplot(data) +
  geom_point(aes(dteday, cnt))

ggplot(data) +
  geom_point(aes(dteday, cnt))

ggplot(data) +
  geom_point(aes(dteday, cnt), alpha = 0.1)


ggplot(data) +
  geom_point(aes(dteday, cnt), alpha = 0.1) +
  geom_smooth(aes(dteday, cnt))

ggplot(data) +
  geom_point(aes(dteday, temp), alpha = 0.1) +
  geom_smooth(aes(dteday, temp))

ggplot(data) +
  geom_point(aes(temp, cnt), alpha = 0.1) +
  geom_smooth(aes(temp, cnt)) + 
  facet_wrap(vars(factor(hr)))
  

# cambiamos el foco -------------------------------------------------------
data <- read_csv("clase-12/data/day.csv")

ggplot(data) +
  geom_histogram(aes(cnt))

ggplot(data) +
  geom_point(aes(dteday, cnt))


# muestras ----------------------------------------------------------------
x <- c("01-01-2001", "01-05-2010", "01-08-2000")
sort(x)
class(x)
x + 1
year(x)

ymd
mdy
myd
y <- dmy(x)
class(y)
year(y)
sort(y)

data <- data %>% 
  mutate(
    muestra = sample(c("D", "V"), size = nrow(data), replace = TRUE),
    muestra = ifelse(dteday > ymd("2012-07-01"), "OOT", muestra)
  )

ggplot(data) +
  geom_point(aes(dteday, cnt, color = muestra), size = 2, alpha = 0.5) +
  scale_color_manual(values = c("darkred", "darkgreen", "magenta"))

# regresion lineal --------------------------------------------------------
glimpse(data)

mod_rl <- lm(cnt ~ instant, data = filter(data, muestra == "D"))
mod_rl

1986.328 + 7.214  * 10


data <- data %>% 
  mutate(mod_rl_pred = predict(mod_rl, newdata = data))

ggplot(data) +
  geom_point(aes(dteday, cnt), alpha = 0.5) +
  geom_line(aes(dteday, mod_rl_pred))

mod_rl <- lm(cnt ~ instant + workingday, data = filter(data, muestra == "D"))

data <- data %>% 
  mutate(mod_rl_pred = predict(mod_rl, newdata = data))

ggplot(data) +
  geom_point(aes(dteday, cnt), alpha = 0.5) +
  geom_line(aes(dteday, mod_rl_pred))

mod_rl <- lm(cnt ~ instant + mnth, data = filter(data, muestra == "D"))

data <- data %>% 
  mutate(mod_rl_pred = predict(mod_rl, newdata = data))

ggplot(data) +
  geom_point(aes(dteday, cnt), alpha = 0.5) +
  geom_line(aes(dteday, mod_rl_pred))

mod_rl <- lm(cnt ~ instant + factor(mnth), data = filter(data, muestra == "D"))

data <- data %>% 
  mutate(mod_rl_pred = predict(mod_rl, newdata = data))

ggplot(data) +
  geom_point(aes(dteday, cnt), alpha = 0.5) +
  geom_line(aes(dteday, mod_rl_pred), size = 2, color = "red")

mod_rl <- lm(cnt ~ instant + factor(mnth) + hum, data = filter(data, muestra == "D"))

data <- data %>% 
  mutate(mod_rl_pred = predict(mod_rl, newdata = data))

ggplot(data) +
  geom_point(aes(dteday, cnt), alpha = 0.5) +
  geom_line(aes(dteday, mod_rl_pred), size = 2, color = "red")

mod_rl <- lm(cnt ~ factor(mnth), data = filter(data, muestra == "D"))

data <- data %>% 
  mutate(mod_rl_pred = predict(mod_rl, newdata = data))

ggplot(data) +
  geom_point(aes(dteday, cnt), alpha = 0.5) +
  geom_line(aes(dteday, mod_rl_pred), size = 2, color = "red")


mod_rl <- lm(cnt ~ instant + factor(mnth) + hum + factor(season) + holiday,
             data = filter(data, muestra == "D"))

data <- data %>% 
  mutate(mod_rl_pred = predict(mod_rl, newdata = data))

ggplot(data) +
  geom_point(aes(dteday, cnt), alpha = 0.5) +
  geom_line(aes(dteday, mod_rl_pred), size = 2, color = "red")



