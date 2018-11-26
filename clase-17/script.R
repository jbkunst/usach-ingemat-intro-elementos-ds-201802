library(tidyverse)

data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv")

glimpse(data)

data %>% 
  group_by(genre) %>% 
  summarise(numero = n()) %>% 
  mutate(p = numero/sum(numero))

data %>% 
  count(genre)

data %>% 
  count(distributor, sort = TRUE) %>% 
  mutate(
    p = n/sum(n),
    pcum = cumsum(p)
    ) %>% 
  View()


data <- data %>% 
  mutate(
    distributor2 = fct_lump(distributor, n = 12)
  )

data %>% 
  count(distributor2, sort = TRUE) %>% 
  mutate(
    p = n/sum(n),
    pcum = cumsum(p)
  ) %>% 
  View()

data %>% 
  count(distributor2, genre) %>% 
  ggplot(aes(genre, distributor2, fill = n)) +
  geom_tile()

data %>% 
  count(distributor2, genre) %>% 
  group_by(distributor2) %>% 
  mutate(p = n/sum(n), pcum = cumsum(p)) %>% 
  ggplot(aes(genre, distributor2, fill = p)) +
  geom_tile() 

data %>% 
  filter(str_detect(distributor, "Disney")) %>% 
  filter(genre == "Horror") %>% 
  View()

data

# tiempo ------------------------------------------------------------------
# data genero tiempo
dgt <- data %>% 
  count(release_date, genre)

ggplot(dgt) +
  geom_line(aes(release_date, n, color = genre, group = 1))

library(lubridate)

data <- data %>% 
  mutate(
    release_date = mdy(release_date) 
  )
data

dgt <- data %>% 
  count(release_date, genre)

ggplot(dgt) +
  geom_line(aes(release_date, n, color = genre, group = 1))


data <- data %>% 
  mutate(release_year = year(release_date)) 

dgt <- data %>% 
  count(release_year, genre)

ggplot(dgt) +
  geom_line(aes(release_year, n, color = genre, group = 1))


ggplot(dgt) +
  geom_line(aes(release_year, n, color = genre, group = genre))


ggplot(dgt) +
  geom_line(aes(release_year, n, color = genre, group = genre), size = 1.2) +
  scale_color_viridis_d(option = "B")

ggplot(dgt) +
  geom_line(aes(release_year, n, color = genre, group = genre), size = 1.2) +
  scale_color_viridis_d(option = "B") +
  scale_x_continuous(limits = c(1985, NA))


data <- data %>% 
  mutate(release_month = rollback(release_date)) 

dgt <- data %>% 
  count(release_month, genre)

ggplot(dgt) +
  geom_line(aes(release_month, n, color = genre, group = genre), size = 1.2) +
  scale_color_viridis_d(option = "B")

data <- data %>% 
  mutate(month = month(release_date, label = TRUE))
data


dgt <- data %>%
  count(month, genre)

ggplot(dgt) +
  geom_line(aes(month, n, color = genre, group = genre), size = 1.2) +
  scale_color_viridis_d(option = "B")

# scatter -----------------------------------------------------------------
data

library(scales)

percent(0.45)
dollar(2450003)

ggplot(data) +
  geom_point(aes(production_budget, domestic_gross)) 

ggplot(data) +
  geom_point(aes(production_budget, worldwide_gross)) +
  geom_abline(slope = 1, intercept = 0, size = 2, color = "red")


# exploracion rapida?
ggplot(data) +
  geom_point(aes(production_budget, worldwide_gross, label = movie)) +
  geom_abline(slope = 1, intercept = 0, size = 2, color = "red")
plotly::ggplotly()

ggplot(data) +
  geom_point(aes(production_budget, domestic_gross), alpha = 0.3) +
  scale_x_sqrt(labels = dollar, limits = c(0, NA)) +
  scale_y_sqrt(labels = dollar, limits = c(0, NA)) 

p <- ggplot(data) +
  geom_point(aes(production_budget, domestic_gross), alpha = 0.3) +
  scale_x_continuous(labels = dollar, limits = c(0, NA)) +
  scale_y_continuous(labels = dollar, limits = c(0, NA)) 

p

datarel <- data %>% 
  filter(domestic_gross >= 4e8 | production_budget >= 175e6)
datarel


p +
  geom_point(aes(production_budget, domestic_gross),
             color = "darkred")

p +
  geom_point(aes(production_budget, domestic_gross),
             color = "darkred", size = 3.53333333,
             data = datarel) +
  geom_text(aes(production_budget, domestic_gross, label = movie),
            data = datarel, size = 3)


library(ggrepel)

geom_text_repel()

p <- ggplot(mtcars,
            aes(wt, mpg, label = rownames(mtcars), colour = factor(cyl))) +
  geom_point()

p

p + geom_text()
# Avoid overlaps by repelling text labels
p + geom_text_repel()
# Labels with background
p + geom_label_repel()

ggplot() +
  geom_point(data = data, aes(production_budget, domestic_gross), alpha = 0.3) +
  scale_x_continuous(labels = dollar, limits = c(0, NA)) +
  scale_y_continuous(labels = dollar, limits = c(0, NA)) +
  geom_point(aes(production_budget, domestic_gross),
             color = "darkred", size = 3.53333333,
             data = datarel) +
  geom_text_repel(aes(production_budget, domestic_gross, label = movie),
            data = datarel) +
  theme_minimal()


ggplot() +
  geom_point(data = data, aes(production_budget, domestic_gross), alpha = 0.3) +
  scale_x_continuous(labels = dollar, limits = c(0, NA)) +
  scale_y_continuous(labels = dollar, limits = c(0, NA)) +
  geom_point(aes(production_budget, domestic_gross),
             color = "darkred", size = 3.53333333,
             data = datarel) +
  geom_text_repel(aes(production_budget, domestic_gross, label = movie),
                  data = datarel) +
  theme_minimal() +
  facet_wrap(vars(genre))

datarel <- data %>% 
  arrange(desc(domestic_gross)) %>% 
  group_by(genre) %>% 
  filter(row_number()<=5)
datarel


ggplot() +
  geom_point(data = data, aes(production_budget, domestic_gross), alpha = 0.3) +
  scale_x_continuous(labels = dollar, limits = c(0, NA)) +
  scale_y_continuous(labels = dollar, limits = c(0, NA)) +
  geom_point(aes(production_budget, domestic_gross),
             color = "darkred", size = 3.53333333,
             data = datarel) +
  geom_text_repel(aes(production_budget, domestic_gross, label = movie),
                  data = datarel) +
  theme_minimal() +
  facet_wrap(vars(genre))

