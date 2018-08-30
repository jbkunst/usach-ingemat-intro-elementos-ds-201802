library(tidyverse)

d <- data_frame(
  x = c(1,2,-1, 4,7,5),
  y = c(5,5, 3, 8,10,7),
  p = c(1, 1, 1, 2, 2, 2)
)
d

ggplot(d) +
  geom_point(aes(x, y))

ggplot(d) +
  geom_point(aes(x, y, size = p))


ggplot(d) +
  geom_line(aes(x, y))

ggplot(d) +
  geom_line(aes(x, y, group = p))

ggplot(d) +
  geom_polygon(aes(x, y))

ggplot(d) +
  geom_polygon(aes(x, y, group = p))

ggplot(d) +
  geom_polygon(aes(x, y, group = p, fill = p))

ggplot(d) +
  geom_polygon(aes(x, y, group = p, fill = p))

ggplot(d) +
  geom_polygon(aes(x, y, group = p), fill = "gray80")

ggplot(d) +
  geom_polygon(aes(x, y, group = p), fill = "gray80", color = "red")

ggplot(d) +
  geom_polygon(aes(x, y, group = p),
               fill = "gray80", color = "red")+
  facet_wrap(~p)

ggplot(d) +
  geom_polygon(aes(x, y, group = p),
               fill = "gray80", color = "red")+
  facet_wrap(~p, scales = "free")
