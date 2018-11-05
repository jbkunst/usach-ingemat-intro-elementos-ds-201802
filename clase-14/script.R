library(tidyverse)


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
