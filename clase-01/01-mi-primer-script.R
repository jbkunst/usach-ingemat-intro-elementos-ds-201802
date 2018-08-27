modlin <- lm(dist ~ speed, data = cars)
plot(modlin)

summary(modlin)

x <- rnorm(n = 1000, mean = 0, sd = 1)

hist(x)

plot(x)

plot(cumsum(x), type = "l")

mean(x)

# install.packages("highcharter")

highcharter::hchart(hist(x))

highcharter::hchart(AirPassengers)

highcharter::hchart(forecast::forecast(AirPassengers))

highcharter::hchart(forecast::forecast(log(AirPassengers)))

