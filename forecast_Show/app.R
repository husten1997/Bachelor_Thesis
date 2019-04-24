#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(sidebarPanel(
    numericInput(
      "Year",
      "Year:",
      min = 1990,
      max = 2019.5,
      value = 2000,
      step = 0.25
    ),
    sliderInput(
      "Quater",
      "Quater:",
      min = 1,
      max = 4,
      value = 1
    ), width = 2
  ),
  
  # Show a plot of the generated distribution
  mainPanel(plotOutput("P_Plot"), plotOutput("PB_Plot")), fluid = TRUE))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$P_Plot <- renderPlot({
    in_year <- input$Year
    
    par(mfrow = c(1, 2))
    #(P.model <- ets(window(MSFT$P.Data$P, end = c(in_year)), opt.crit = "mae"))
    (P.model <- auto.arima(window(MSFT$P.Data$P, end = c(in_year)), stationary = TRUE, max.q = 0))
    plot(forecast(P.model), xlim = c(1990.00, 2020.00), ylim = c(-1, 330), main = c("P"))
    lines(MSFT$P.Data$P, col = c("grey"))
    lines(P.model$fitted, col = c("black"))
    (B.model <- ets(window(MSFT$B.Data$BPS_E, end = c(in_year)), opt.crit = "mae"))
    plot(forecast(B.model), xlim = c(1990.00, 2020.00), ylim = c(-1, 15), main = c("BPS"))
    lines(MSFT$B.Data$BPS_E, col = c("grey"))
    lines(B.model$fitted, col = c("black"))
    
  })
  
  output$BPS_Plot <- renderPlot({
    in_year <- input$Year
    (B.model <- ets(window(MSFT$B.Data$BPS_E, end = c(in_year)), opt.crit = "mae"))
    plot(forecast(B.model), xlim = c(1990.00, 2020.00), ylim = c(-1, 15), main = c("BPS"))
    lines(MSFT$B.Data$BPS_E, col = c("grey"))
    lines(B.model$fitted, col = c("black"))
  })
  
  output$PB_Plot <- renderPlot({
    in_year <- input$Year
    
    #P.model <- ets(window(MSFT$P.Data$P, end = c(in_year)), opt.crit = "mae")
    P.model <- auto.arima(window(MSFT$P.Data$P, end = c(in_year)), stationary = TRUE)

    B.model <- ets(window(MSFT$B.Data$BPS_E, end = c(in_year), opt.crit = "mae"))
    
    PB.forecast <- ts((forecast(P.model)$mean / forecast(B.model)$mean), start = c(in_year + 0.25), frequency = 4)
    plot(MSFT$Ratio.PB$PB, col = c("grey"), ylim = c(10, 50))
    lines(PB.forecast, col = c("red"))
    lines(window(MSFT$Ratio.PB$PB, end = c(in_year)))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
