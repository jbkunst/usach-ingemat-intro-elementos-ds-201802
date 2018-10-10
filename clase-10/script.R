library(tidyverse)
# install.packages("nycflights13")


# lo mismo de ayer --------------------------------------------------------
set.seed(123)
data(flights, package = "nycflights13")
flights <- flights %>% 
  filter(!is.na(air_time), !is.na(distance)) %>% 
  filter(distance < quantile(distance, .99)) %>% 
  sample_n(5000) %>% 
  arrange(distance) %>% 
  mutate(
    distance = distance * 1.60934,
    air_time = air_time / 60
  ) %>% 
  select(distance, air_time, everything())

flights2 <- flights %>% 
  sample_n(1000) %>% 
  select(distance, air_time)

gg <- ggplot(flights2) +
  geom_point(aes(distance, air_time), alpha = 0.25) +
  xlim(0, NA) + ylim(0, NA)
gg

model <- data_frame(
  b0 = runif(100, 0, 0.6),
  b1 = runif(100, 0.0005, 0.002)
)

gg <- gg + 
  geom_abline(aes(intercept = b0, slope = b1), data = model, alpha = 0.2, color = "skyblue")
gg

# modelo mejor? ah?!
gg +
  geom_segment(aes(x = 500, xend = 4000, y = 1, yend = 5.5), color = "red", size = 1.2)

model

x <- c(500, 4000)
y <- c(1, 5.5)

m <- (y[2]-y[1])/(x[2]-x[1])
b <- -m*x[1] + y[1]


modelojo <- data_frame(b0 = b, b1 = m, tipo = "ojo")  
modelojo

1/0.001285714

gg <- gg + 
  geom_abline(aes(intercept = b0, slope = b1),
              data = modelojo, alpha = 0.5,
              size = 1.1, color = "red")
gg

modelos <- bind_rows(model, modelojo)

tail(modelos)


ecm <- function(b0, b1) {
  # b0 <- 0.3; b1 <- 0.002
  ecmdf <- flights2 %>% 
    mutate(
      air_time_pred = b0 + b1 * distance,
      e_i = air_time - air_time_pred,
      e_i2 = e_i ^ 2
    ) %>% 
    summarise(
      ecm = sum(e_i2)
    )
  
  ecmdf$ecm
  
}

ecm(0.4, 0.001)
ecm(0.4, 0.002)

s <- 1:10
s

# sqrt(s)
map(s, sqrt)
map_dbl(s, sqrt)

map2(x, y, sum)
map2_dbl(x, y, sum)
map2_dbl(x, y, ecm)

modelos <- modelos %>% 
  mutate(error_cuad = map2_dbl(b0, b1, ecm))
modelos

ggplot(flights2) +
  xlim(0, 4500) + ylim(0, 6.5) +
  geom_abline(aes(intercept = b0, slope = b1, color = error_cuad),
              data = modelos, size = 1.2, alpha = 0.2) +
  geom_point(aes(distance, air_time), alpha = 0.25) +
  scale_color_viridis_c(direction = -1)


modelos %>%
  arrange(error_cuad)

modelos <- mutate(
  modelos,
  tipo = ifelse(is.na(tipo), "random", tipo)
)

ggplot(modelos) +
  geom_point(aes(b0, b1, color = tipo, alpha = 1/error_cuad),
             size = 3) +
  scale_color_viridis_d(direction = -1, end = .7)

# al fin llegamos! Perdimos todo este rato para que?
flights2
modelojo
mod <- lm(air_time ~ distance, data = flights2)
coefficients(mod)

gg + 
  geom_smooth(aes(distance, air_time), method = "lm", color = "darkgreen", alpha = 0.5)

modelos %>%
  arrange(error_cuad) %>% 
  head(1)

ecm(coefficients(mod)[1], coefficients(mod)[2])

mod

summary(mod)



# bases -------------------------------------------------------------------
set.seed(123)
flights <- flights %>% 
  sample_n(nrow(flights))

head(1:10,  3)
head(1:10,  2)

flights_des <- head(flights, 2500)
flights_val <- head(flights, 2500)

# juegue ------------------------------------------------------------------
mi_mod <- lm(air_time ~ distance + month, data = flights_des)

(mi_mod$residuals)2


