library(tidyverse)
library(rvest)
library(lubridate)



periodo <- "2018-10"

# scraping ----------------------------------------------------------------
obtener_ipsa_por_periodo <- function(periodo = "2018-10"){
  
  message("Periodo: ", periodo)
  
  url <- "https://datosmacro.expansion.com/bolsa/chile?dr="
  url <- paste0(url, periodo)
  url
  
  pagina <- html(url)
  pagina
  
  tablas <- html_table(pagina)
  tablas
  
  ipsa <- tablas[[2]]
  ipsa
  
  # limpiemos
  ipsa <- ipsa %>% 
    tbl_df() %>% 
    mutate(
      Fecha = dmy(Fecha),
      Puntos = str_remove(Puntos, "\\."),
      Puntos = str_replace(Puntos, ",", "."),
      Puntos = as.numeric(Puntos)
    ) %>% 
    rename(Var = `Var.%`) %>% 
    mutate(
      Var = str_remove(Var, "%"),
      Var = str_replace(Var, ",", "."),
      Var = as.numeric(Var)
    ) %>% 
    filter(!is.na(Fecha))
  
  
  ipsa
} 

obtener_ipsa_por_periodo("2017-10")


# progrmacion funcional ---------------------------------------------------
x <- c(1:10)
x

sqrt(x)

res <- c()
res

for(i in x){
  res <- c(res, sqrt(i))
}

resl <- map(x, sqrt)
resl

reduce(resl, c)

c(c(1, 2), 4)

periodos <- c("2018-09", "2018-10", "2018-08")

obtener_ipsa_por_periodo(periodos)

# Nuestra funcion no esta vectorizada
# Mal por quien escribio esto. Castigado

ipsas <- map(periodos, obtener_ipsa_por_periodo)


reduce(ipsas, bind_rows)


# ahora tooooodos periodos
periodos <- seq.Date(ymd("20060101"), ymd("20181001"), by = "month")
# periodos <- head(periodos)
class(periodos)

# f <- format(periodos, "%d de %B del %Y")
# as.Date(f, "%d de %B del %Y")
periodos <- format(periodos, "%Y-%m")

ipsas <- map(periodos, obtener_ipsa_por_periodo)

reduce(ipsas, bind_rows)



periodos <- seq.Date(ymd("20170101"), ymd("20181001"), by = "month")
# periodos <- head(periodos)
class(periodos)

# f <- format(periodos, "%d de %B del %Y")
# as.Date(f, "%d de %B del %Y")
periodos <- format(periodos, "%Y-%m")

system.time(
  map(periodos, obtener_ipsa_por_periodo)  
)
29.35 


reduce(ipsas, bind_rows)

library(furrr)

plan(sequential)

system.time(
  future_map(periodos, obtener_ipsa_por_periodo)  
)
29.11

plan(multisession(workers = 10))

system.time(
  future_map(periodos, obtener_ipsa_por_periodo)  
)

ipsas <- future_map(periodos, obtener_ipsa_por_periodo)

ipsas <- reduce(ipsas, bind_rows)

ipsas


# htmlwidets










