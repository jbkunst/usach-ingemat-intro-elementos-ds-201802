library(tidyverse)
# La madre de todas las regresiones ---------------------------------------
a <- 8
b <- 3

N <- 30

data <- data_frame(
  x = runif(N, 0, 10),
  y = a + x * b + 2 * rnorm(N)
)

ggplot(data) +
  geom_point(aes(x, y))


# En el ejemplo que conversamos:
# La estatura de tu hijo será tu estatura por 0.9 más
# 10 
# esth = 10 + estatuya * 0.9

# Prpondremos un modelo en forma algebraico
# de la forma y = f(x)
# Hay familia de modelos
# 
# y = a + b*x


modelos <- data_frame(
  b = runif(300, -10, 10),
  a = runif(300, -100, 100)
)
modelos


ggplot() +
  geom_point(aes(x, y), data = data) +
  geom_abline(aes(intercept = a, slope = b),
              data = modelos, alpha = 0.2)


# funcion que recibe un modelo (paramatros)
# y retorna un numero
obtener_error_sum <- function(a, b) {
  
  message("a: ", a, " b: ", b)
  # a <- 4
  # b <- 9
  
  data %>% 
    mutate(
      # lo que dice mi modelo
      a + b * x,
      error = y - `a + b * x`
    ) %>% 
    summarise(sum(error)) %>% 
    pull()
  
}


ggplot() +
  geom_point(aes(x, y), data = data) +
  geom_abline(aes(intercept = a, slope = b),
              data = modelos, alpha = 0.2) +
  geom_abline(intercept = 4, slope = 8,
              color = "red", size = 2)


obtener_error_sum(5, 8)
obtener_error_sum(4, 3)

modelos <- modelos %>% 
  mutate(error = map2_dbl(a, b, obtener_error_sum))
modelos


# wenos
modeloswenos <- modelos %>% 
  filter(
    error == min(error) | error == max(error))

ggplot() +
  geom_point(aes(x, y), data = data) +
  geom_abline(aes(intercept = a, slope = b),
              data = modelos, alpha = 0.2) +
  geom_abline(aes(intercept = a, slope = b),
              data = modeloswenos, color = "red",
              size = 2) +
  xlim(-100, 100) +
  ylim(-100, 100)


modeloswenowenos <- modelos %>% 
  mutate(error_abs = abs(error)) %>% 
  arrange(error_abs) %>% 
  head(2)

ggplot() +
  geom_point(aes(x, y), data = data) +
  geom_abline(aes(intercept = a, slope = b),
              data = modelos, alpha = 0.2) +
  geom_abline(aes(intercept = a, slope = b),
              data = modeloswenowenos, color = "red",
              size = 2)


obtener_error_sum2 <- function(a = 1, b = 1) {
  
  message("a: ", a, " b: ", b)
  # a <- 4
  # b <- 9
  
  data %>% 
    mutate(
      # lo que dice mi modelo
      a + b * x,
      error2 = (y - `a + b * x`)^2
    ) %>% 
    summarise(sum(error2)) %>% 
    pull()
  
}

modelos <- modelos %>% 
  mutate(sumerror2 = map2_dbl(a, b, obtener_error_sum2))

modelos

modeloswenowenoswenos <- modelos %>% 
  arrange(sumerror2) %>% 
  head(3)

ggplot() +
  geom_point(aes(x, y), data = data) +
  geom_abline(aes(intercept = a, slope = b),
              data = modelos, alpha = 0.2) +
  geom_abline(aes(intercept = a, slope = b),
              data = modeloswenowenoswenos, color = "blue",
              size = 2)

ggplot() +
  geom_point(aes(a, b), data = modelos) +
  geom_point(aes(a, b), data = modeloswenowenoswenos, color = "blue", size = 5)

data
# Linear Models
mod <- lm(y ~ x, data = data)
coefs <- mod$coefficients
coefs

ggplot() +
  geom_point(aes(a, b), data = modelos) +
  geom_point(aes(a, b), data = modeloswenowenoswenos, color = "blue", size = 5) +
  geom_point(aes(x = 8.547, y = 2.782), color = "red", size = 5) 



# equivalentes
ggplot() +
  geom_point(aes(x, y), data = data) +
  geom_abline(slope = coefs[2], intercept = coefs[1], color = "red", alpha = 0.3, size = 4) +
  geom_smooth(aes(x, y), data = data, se = FALSE, method = "lm") 

# la historia de que no quero olvidarme de mi primer amor
modeloswenowenoswenosfinalfinal <- modeloswenowenoswenos %>% 
  head(1)
modeloswenowenoswenosfinalfinal

ggplot() +
  geom_point(aes(x, y), data = data) +
  geom_abline(slope = coefs[2], intercept = coefs[1], color = "blue",
              alpha = 0.3, size = 2) +
  geom_abline(aes(slope = b, intercept = a),
              data = modeloswenowenoswenosfinalfinal,
              color = "pink", size = 2)







