library(tidyverse)
library(lubridate)
library(mltools)
library(data.table)
# customers_1h <- one_hot(as.data.table(customers))


data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv")

data <- data %>% 
  mutate(release_date = mdy(release_date)) %>% 
  filter(year(release_date)>= 1990) %>% 
  filter(year(release_date)<= 2017)

data

glimpse(data)

data <- data %>% 
  mutate_if(is.character, replace_na, "NoTa") %>% 
  mutate(
    distributor = fct_lump(distributor, 12),
    mpaa_rating = as.factor(mpaa_rating),
    genre = as.factor(genre)
  ) %>%
  as.data.table() %>% 
  one_hot() %>% 
  tbl_df()

datan <- data %>% 
  select_if(is.numeric) %>% 
  mutate_all(scale) %>% 
  select(-X1)

datan


# TSNE --------------------------------------------------------------------
library(Rtsne)

# como ver si tengo duplicados
datan %>% 
  distinct()

tsne_out <- Rtsne(datan %>% mutate(a = 0.1*rnorm(nrow(.)))) 

data <- data %>% 
  mutate(
    x = tsne_out$Y[, 1],
    y = tsne_out$Y[, 2]
  )


dg <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv") %>% 
  select(X1, genre)

dr <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv") %>% 
  select(X1, mpaa_rating)

dd <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv") %>% 
  select(X1, distributor)


data <- left_join(data, dg, by = "X1")
data <- left_join(data, dr, by = "X1")
data <- left_join(data, dd, by = "X1")

# data %>% 
#   select(X1, starts_with("genre")) %>% 
#   gather(key, value, -X1) %>% 
#   arrange(X1) %>% 
#   filter(value == 1) %>% 
#   select(-value) %>% 
#   rename(genre = key) %>% 
#   mutate(genre = str_remove(genre, "genre_"))
  
p1 <- ggplot(data) +  
  geom_point(aes(x, y, color = genre)) +
  scale_color_viridis_d() +
  theme(legend.position = "bottom")

p2 <- ggplot(data) +  
  geom_point(aes(x, y, color = production_budget)) +
  scale_color_viridis_c(option = "B")+
  theme(legend.position = "bottom")

p3 <- ggplot(data) +  
  geom_point(aes(x, y, color = mpaa_rating)) +
  scale_color_viridis_d() +
  theme(legend.position = "bottom")

p4 <- ggplot(data) +  
  geom_point(aes(x, y, color = worldwide_gross)) +
  scale_color_viridis_c(option = "D") +
  theme(legend.position = "bottom")

library(gridExtra)

grid.arrange(p1, p2, p3, p4, ncol = 2)


p <- ggplot(data) +  
  geom_point(aes(x, y, color = genre, label = movie)) +
  scale_color_viridis_d(option = "B")+
  theme(legend.position = "bottom")

plotly::ggplotly(p)



# Autoencoder -------------------------------------------------------------
library(h2o)
h2o.init(nthreads = 2)

datan <- janitor::clean_names(datan)

datanh2o <- as.h2o(as.data.table(datan))

autoenmov <- h2o.deeplearning(x = names(datan), training_frame = datanh2o,
                              autoencoder = TRUE, reproducible = TRUE,
                              seed = 1234, hidden = c(20, 10, 2, 10, 20),
                              epochs = 500)
autoenmov
plot(autoenmov)
h2o.deepfeatures(autoenmov, datanh2o, layer = 3) %>% 
  as.data.frame() %>% 
  tbl_df() %>% 
  setNames(c("x", "y")) %>% 
  distinct()


anomalymov <- h2o.anomaly(autoenmov, datanh2o, per_feature=FALSE)
anomalymov <- anomalymov %>% 
  as.data.frame() %>% 
  tbl_df()
anomalymov


data <- data %>% 
  bind_cols(anomalymov)

data %>% 
  arrange(desc(Reconstruction.MSE)) %>% 
  select(movie, Reconstruction.MSE, everything()) %>% 
  View()


p5 <- ggplot(data) +  
  geom_point(aes(x, y, color = Reconstruction.MSE, label = movie)) +
  scale_color_viridis_c(option = "B")+
  theme(legend.position = "bottom")
plotly::ggplotly()


grid.arrange(p1, p2, p5, p4, ncol = 2)

plotly::ggplotly(p5)


ggplot(data) +  
  geom_point(aes(x, y, color = distributor, label = movie)) +
  scale_color_viridis_d(option = "B")+
  theme(legend.position = "bottom")
plotly::ggplotly()


