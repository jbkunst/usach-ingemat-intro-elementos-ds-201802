library(tidyverse)

data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv")
data <- data %>% 
  select(-X1)
data <- data %>% 
  filter(movie != "Tau ming chong")
glimpse(data)

data %>% 
  count(movie, sort = TRUE)

# reduccion de dimensionalidad
# PCA 


# TSNE --------------------------------------------------------------------
# library(tsne)
library(Rtsne)

data

colors <- rainbow(length(unique(data$genre)))
names(colors) <- unique(data$genre)

ecb = function(x,y){ plot(x,t='n'); text(x,labels=data$genre, col=colors[date$genre]) }

datan <- data %>% 
  select_if(is.numeric) %>% 
  mutate_all(scale) %>% 
  mutate(
    genre = data %>% pull(genre),
    noise = runif(nrow(.), 0, 0.01)
  ) 

datan %>% distinct()
datan %>% select(-genre) %>% distinct()

tsne_out <- Rtsne(datan %>% select(-genre)) 

names(tsne_out)

datan <- datan %>% 
  mutate(
    x = tsne_out$Y[, 1],
    y = tsne_out$Y[, 2]
  )

ggplot(datan) +
  geom_point(aes(x, y, color = genre)) +
  scale_color_viridis_d()

plot(datan$x, datan$y)

# one hoy enconding
datan <- data %>% 
  select_if(is.numeric)

datag <- data %>% 
  select(movie, genre) %>%
  mutate(v = 1) %>% 
  spread(genre, v) 

datad <- data %>% 
  mutate(distributor = ifelse(is.na(distributor), "NA", distributor)) %>% 
  mutate(distributor = fct_lump(distributor, 10)) %>% 
  select(movie, distributor) %>%
  mutate(v = 1) %>% 
  spread(distributor, v) 

datar <- data %>% 
  select(movie, mpaa_rating) %>%
  mutate(v = 1) %>% 
  spread(mpaa_rating, v)  

datan2 <- list(
  datag,
  datad,
  datar
) %>% 
  reduce(full_join)

glimpse(datan2)

# Autoencoders




# UMAP