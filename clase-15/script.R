library(tidyverse)

data <- read_csv(unz("clase-15/data/all.zip", "train.csv"))
data


dmin <- data %>% 
  sample_n(9)

dmin2 <- dmin %>% 
  mutate(id = row_number()) %>% 
  gather(pixel, intensidad, -label, -id) %>% 
  mutate(pixel = str_remove(pixel, "pixel")) %>% 
  mutate(pixel = as.numeric(pixel)) %>% 
  arrange(label, pixel) %>% 
  group_by(id) %>% 
  mutate(
    x = rep(1:28, times = 28),
    y = rep(28:1, each = 28)
  )

ggplot(dmin2) +
  geom_tile(aes(x, y, fill = intensidad))

ggplot(dmin2) +
  geom_tile(aes(x, y, fill = intensidad), alpha = 0.2)


ggplot(dmin2) +
  geom_tile(aes(x, y, fill = intensidad)) +
  facet_wrap(vars(id, label)) 


p <- .70

data <- data %>% 
  mutate(
    muestra = sample(
      c("d", "v"),
      size = nrow(.),
      prob = c(p, 1-p),
      replace = TRUE
      )
  )

dtrain <- data %>% 
  filter(muestra == "d") %>% 
  select(-muestra) %>% 
  mutate(label = factor(label))

library(randomForest)
library(ranger)

mod <- ranger(label ~ ., data = dtrain, num.trees = 500, verbose = TRUE, num.threads = 3)
mod

dtest <- data %>% 
  filter(muestra == "v") 

predict(mod, data = head(dtest, 6)) 
predict(mod, data = head(dtest, 6))$predictions

dtest <- dtest %>% 
  mutate(pred = predict(mod, data = dtest)$predictions)

dtest <- dtest %>% 
  select(pred, label, everything())

dmet <- dtest %>% 
  count(label, pred) %>% 
  group_by(label) %>% 
  mutate(p = n/sum(n)) %>% 
  select(label, pred, p)

dmet %>% 
  spread(pred, p)

ggplot(dmet) +
  geom_tile(aes(as.factor(label), pred, fill = p))
  
dmet %>% 
  filter(label != pred) %>% 
  ggplot() +
  geom_tile(aes(as.factor(label), pred, fill = p)) +
  scale_fill_viridis_c(option = "B")


