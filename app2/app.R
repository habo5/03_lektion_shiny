library(shiny)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(hrbrthemes)
library(readxl)
library(plotly)
library(tidyverse)

options(scipen = 999)

ui <- fluidPage(
  titlePanel(
    div(
      tags$span("EA Dania 2025", style = "font-size: 24px; font-weight: bold;"),
      tags$img(height = 65, width = 80, src = "daina_logo.png"),
      style = "display: flex; justify-content: space-between; align-items: center;"
    )
  ),
  
 
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot 1", plotOutput("plot1")),
        tabPanel("Plot 2", plotOutput("plot2")),
        tabPanel("Plot 3", plotOutput("plot3"))
      )
    )
  )


server <- function(input, output) {
  # Read data
  passat <- read_excel("passat.xlsx")
  vwpas <- passat
  
  # Custom colors
  cols <- c("Privat" = "blue", "Forhandler" = "orange")
  cols2 <- c("Area1" = "red", "Area2" = "green", "Area3" = "purple") # Adjust based on your data
  
  # Plot 1
  output$plot1 <- renderPlot({
    ggplot(vwpas, aes(x = km_per_liter, y = price, size = motor_size, colour = dealer_type)) +
      geom_point(alpha = 0.3) +
      scale_color_manual(values = cols) +
      labs(
        x = "Kilometer på literen",
        y = "Pris",
        title = "Plot 1: Privat vs Forhandler",
        subtitle = "Motorstørrelse og pris",
        caption = "Data fra bilbasen.dk"
      ) +
      theme_minimal() +
      geom_text_repel(aes(label = ifelse(motor_size > 1.5 & year > 2014, as.character(car), '')),
                      size = 3, colour = "black") +
      geom_point(data = vwpas %>% filter(motor_size > 1.5 & year > 2014), size = 10) +
      facet_wrap(vars(area))
  })
  
  # Plot 2
  output$plot2 <- renderPlot({
    ggplot(vwpas, aes(x = km_per_liter, y = motor_size, colour = dealer_type)) +
      geom_point(alpha = 0.8, size = 3) +
      scale_color_manual(values = cols) +
      labs(
        x = "Kilometer på literen",
        y = "Motorstørrelse",
        title = "Plot 2: Motorstørrelse vs KM per liter",
        caption = "Data fra bilbasen.dk"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "top"
      )
  })
  
  # Plot 3
  output$plot3 <- renderPlot({
    ggplot(vwpas, aes(x = dealer_type, y = price, colour = area, size = motor_size)) +
      geom_point(alpha = 0.7) +
      scale_color_manual(values = cols2) +
      labs(
        title = "Plot 3: Dealer Type vs Price",
        x = "Dealer Type",
        y = "Price",
        colour = "Area"
      ) +
      geom_text_repel(aes(label = ifelse(price == max(price) | price == min(price), as.character(car), '')),
                      size = 3, colour = "black") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
