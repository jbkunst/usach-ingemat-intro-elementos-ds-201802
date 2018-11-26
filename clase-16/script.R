library(tidyverse)
library(scales)

data <- read_csv(unz("clase-15/data/all.zip", "train.csv"))
data

p <- .50

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

library(ranger)

mod <- ranger(label ~ ., data = dtrain, num.trees = 500, verbose = TRUE, num.threads = 3, importance = "impurity")
mod

dtest <- data %>% 
  filter(muestra == "v") 

imp <- importance(mod)

dfimp <- data_frame(
  pixel = names(imp),
  imp = as.numeric(imp),
  x = rep(1:28, times = 28),
  y = rep(28:1, each = 28)
)

ggplot(dfimp) +
  geom_tile(aes(x, y, fill = imp)) +
  scale_fill_viridis_c(option = "B") +
  theme_void() +
  theme(legend.position = "none")


# meta bosque -------------------------------------------------------------
dfimp <- dfimp %>% 
  arrange(desc(imp))

dfimp <- dfimp %>% 
  mutate(num = row_number())


# el cuento del map -------------------------------------------------------
x <- c(1:10)
# la suma de los raices de x

sum(sqrt(x))

reduce(map(x, sqrt), sum)

# seq(1, 28*28, by = 5)
x <- (1:28)^2


# cuerpo de nuestra funcion
get_rf <- function(ye){
  
  vars <- dfimp %>% 
    filter(row_number() <= ye) %>% 
    pull(pixel)
  vars <- c(vars, "label")
  
  dtrain_vars <- dtrain %>% 
    select(vars)
  
  mod <- ranger(label ~ ., data = dtrain_vars,
                num.trees = 300,
                verbose = TRUE, num.threads = 3
  )
  
  dout <- data %>% 
    mutate(
      pred = predict(mod, data = data)$predictions
    ) %>% 
    select(label, pred, muestra) %>% 
    group_by(muestra) %>% 
    summarise(accuracy = mean(label == pred)) %>% 
    spread(muestra, accuracy) %>% 
    mutate(nvars = ye)
  
  dout
}

get_rf(3)


resultados <- map(x, get_rf)
resultados

sqrt(x)
reduce(map(x, sqrt), c)

resultados <- reduce(resultados, bind_rows)

resultados <- resultados %>% 
  mutate(porc_vars = nvars/max(nvars))

ggplot(resultados) +
  geom_line(aes(porc_vars, d), color = "red") +
  geom_point(aes(porc_vars, d), color = "red") +
  geom_line(aes(porc_vars, v), color = "blue") +
  geom_point(aes(porc_vars, v), color = "blue") +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  scale_x_continuous(limits = c(0, 1), labels = percent)


library(h2o)

h2o.init(nthreads = 2)

data <- data %>% 
  select(-muestra) %>%
  mutate(
    label = as.character(label),
    label = paste0("n", label),
    label = as.factor(label)
    )

data2 <- as.h2o(data)
str(data2)

aml <- h2o.automl(
  y = "label", 
  training_frame = data2,
  max_runtime_secs = 60*10,
  sort_metric = "logloss")
View(aml)

