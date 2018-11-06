library(shiny)
library(tidyverse)
library(lubridate)

library(rvest)

urlr <- "https://www.rankia.cl/blog/analisis-ipsa/3229498-que-empresas-forman-parte-ipsa-2018"

textos <- read_html(urlr) %>% 
  html_nodes("td") %>% 
  html_text()

textos

imp <- seq(1, length(textos), by = 2)

empresas <- textos[imp]

ui <- fluidPage(
   sidebarLayout(
      sidebarPanel(
         selectInput("empresa", "Empresa", choices = empresas)
      ),
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
   output$distPlot <- renderPlot({

     # url <- "http://www.bolsadesantiago.com/DatosGraficosSebra/IPSA-days.txt"
     url <- paste(
       "http://www.bolsadesantiago.com/DatosGraficosSebra/",
       input$empresa,
       "-days.txt",
       sep = ""
     )
     
     data <- read_csv(url)
     
     data <- data %>% 
       mutate(fecha = ymd(`<DTYYYYMMDD>`))
     
     ggplot(data) +
       geom_line(aes(fecha, `<OPEN>`))
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

