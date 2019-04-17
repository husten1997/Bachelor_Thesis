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
ui <- fluidPage(titlePanel("Dampening of change of RW"),
                
                # Sidebar with a slider input for number of bins
                sidebarLayout(
                  sidebarPanel(
                    numericInput("seed_in", "Set seed", min = 0, max = 999999999999, step = 100, value = 12345), 
                    sliderInput(
                      "nsd",
                      "Number of sd:",
                      min = 1,
                      max = 50,
                      value = 1,
                      step = 0.1
                    ),
                    sliderInput(
                      "nw",
                      "Weight of propability:",
                      min = -1,
                      max = 1,
                      value = 0,
                      step = 0.01
                    )
                ),
                
                # Show a plot of the generated distribution
                mainPanel(plotOutput("RW_plot"), plotOutput("Diff_plot"), plotOutput("Dens_Plot"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  diff <- reactive({
    seed <- input$seed_in
    set.seed(seed)
    e <- rnorm(100, 0, 1)
    x <- cumsum(e)
    
    p <- 2 * pnorm(-abs(e), mean = 0, sd = input$nsd)
    wp <- e * (p - p * input$nw)
    wx <- cumsum(wp)
    
    diff <- x - wx
  })
  
  
  output$RW_plot <- renderPlot({
    seed <- input$seed_in
    set.seed(seed)
    e <- rnorm(100, 0, 1)
    x <- cumsum(e)
    
    #plot(x, type = c("l"))
    
    p <- 2 * pnorm(-abs(e), mean = 0, sd = input$nsd)
    wp <- e * (p - p * input$nw)
    wx <- cumsum(wp)
    
    plot(x, type = c("l"), main = c("RW"))
    lines(wx, col = c("orange"))
    
    #diff <- x - wx
    
  })
  
  output$Diff_plot <- renderPlot({
    di <- diff()
    
    plot(di, type = c("l"), main = c("Difference"))
    abline(h = 0)
  })
  
  output$Dens_Plot <- renderPlot({
    seed <- input$seed_in
    set.seed(seed)
    e <- rnorm(100, 0, 1)
    x <- cumsum(e)
    
    p <- 2 * pnorm(-abs(e), mean = 0, sd = input$nsd)
    wp <- e * (p - p * input$nw)
    wx <- cumsum(wp)
    
    plot(density(x), ylim = max(density(wx)$y))
    lines(density(wx), col = c("orange"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
