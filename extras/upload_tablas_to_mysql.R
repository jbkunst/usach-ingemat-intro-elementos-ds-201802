library(tidyverse)
library(DBI)    # Interfaz para bases de datos
library(RMySQL) # Driver para MySQL

con <- dbConnect(
  drv = MySQL(),
  dbname = "nycflights13",
  host = "142.93.20.188", 
  port = 3306,
  user = "test",
  password = "HFW9KYZBnEYr!"
)

dbListTables(con)

data(package = "nycflights13")

c(
  "airlines",
  "airports",
  "flights",  
  "planes",
  "weather"
) %>% 
  map(function(t = "airlines"){
    
    tbl <- get(t)
    dbWriteTable(con, t, tbl)
    
  })
  

