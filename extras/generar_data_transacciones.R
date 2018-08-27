# packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)

data("Groceries", package = "arules")
str(Groceries)

N <- 200000


# data --------------------------------------------------------------------
products <- tbl_df(Groceries@itemInfo)

products <- products %>% 
  mutate(
    labels = labels  %>% 
      str_remove_all("\\(|\\)") %>% 
      str_replace_all("\\s+|/", "_") %>% 
      str_remove_all("_$")
  ) %>% 
  rename(product = labels)
products


transactions <- products %>% 
  group_by(level1, level2) %>% 
  mutate(
    level1f = as.numeric(level1),
    level2f = rev(as.numeric(level2)),
    level0f = sample(row_number(), size = length(labels)),
    levelff = level0f + 2 * level1f + level2f
  ) %>% 
  { sample(.$product, size = N, replace = TRUE, prob = sqrt(.$levelff)) } %>% 
  data_frame(product = .) %>% 
  mutate(
    rnd = runif(N)/3 + (1:N/N + runif(N))/6,
    id = round(cumsum(rnd)) + 1
  ) %>% 
  select(id, product)

transactions_date <- seq.Date(ymd("2018-01-03"), Sys.Date(), by = "day") %>% 
  data_frame(date = .) %>% 
  mutate(
    prob = row_number(),
    wday_name = format(date, "%A"),
    wday = wday(date),
    wdayf = ifelse(wday %in% c(4, 5), 10, wday),
    probf = prob + wdayf
  ) %>% 
  { sample(.$date, size = nrow(count(transactions, id)), replace = TRUE, prob = sqrt(.$probf^1.5)) } %>% 
  sort() %>% 
  data_frame(
    id = 1:length(.),
    date = .
  )

transactions_date
tail(transactions_date)

products <- products %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    price = sample(1:10, size = n(), replace = TRUE),
    price = price + sample(1:99/100, size = n(), replace = TRUE)
  )

writexl::write_xlsx(
  list(
    transactions = transactions,
    transactions_date = transactions_date,
    products = products
  ),
  "~/transactions.xlsx"
)

# start -------------------------------------------------------------------
# products <- readxl::read_excel("~/transactions.xlsx", sheet = "products")
# transactions <- readxl::read_excel("~/transactions.xlsx", sheet = "transactions")
# transactions_date <- readxl::read_excel("~/transactions.xlsx", sheet = "transactions_date")
# transactions_date <- mutate(transactions_date, date = ymd(date))

# explorar productos
# products
# 
# count(products, level1)
# count(products, level1, level2)
# 
# transactions_date
# 
# transactions %>%
#   left_join(products) %>%
#   group_by(id) %>%
#   summarise(price = sum(price)) %>%
#   left_join(transactions_date) %>%
#   group_by(date) %>%
#   summarise(price = sum(price)) %>%
#   ggplot() +
#   geom_line(aes(date, price))
# 
# datecompras <- transactions %>% 
#   left_join(products) %>% 
#   group_by(id) %>% 
#   summarise(price = sum(price)) %>% 
#   left_join(transactions_date) %>% 
#   group_by(date) %>% 
#   summarise(price = sum(price)) 
# 
# datecompras <- datecompras %>% 
#   mutate(
#     dia = wday(date, label = TRUE, abbr = FALSE),
#     semana = week(date) 
#     )
# datecompras
# 
# ggplot(datecompras)  +
#   geom_line(aes(dia, price, group = semana)) +
#   facet_wrap(~ format(date, "%Y%m"))