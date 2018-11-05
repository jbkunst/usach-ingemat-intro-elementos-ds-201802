library(tidyverse)
library(lubridate)

empresa <- "CHILE"
# Para que hacemos lo anterior? 
# Intentar trabajar con valores genericos

# url <- "http://www.bolsadesantiago.com/DatosGraficosSebra/IPSA-days.txt"
url <- paste(
  "http://www.bolsadesantiago.com/DatosGraficosSebra/",
  empresa,
  "-days.txt",
  sep = ""
)

data <- read_csv(url)

glimpse(data)

# Tenemos los datos, grafiquemos el volumen tranzado
ggplot(data) +
  geom_line(aes(`<DTYYYYMMDD>`, `<VOL>`))

# :)
data <- data %>% 
  mutate(fecha = ymd(`<DTYYYYMMDD>`))

glimpse(data)

ggplot(data) +
  geom_line(aes(fecha, `<OPEN>`))


