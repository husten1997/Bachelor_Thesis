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
                    numericInput("length_in", "Set length of RW", min = 100, max = 999999999999, step = 100, value = 1000),
                    sliderInput(
                      "nw",
                      "Weight of propability:",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      step = 0.01
                    ), 
                    plotOutput("Dens_Plot"),
                    tableOutput("Stat_Text")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(plotOutput("RW_plot"), plotOutput("Diff_plot"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  diff <- reactive({
    seed <- input$seed_in
    length <- input$length_in
    set.seed(seed)
    e <- rnorm(length, 0, 1)
    x <- cumsum(e)
    
    wp <- e * input$nw
    wx <- cumsum(wp)
    
    diff <- x - wx
  })
  
  
  output$RW_plot <- renderPlot({
    seed <- input$seed_in
    length <- input$length_in
    set.seed(seed)
    e <- rnorm(length, 0, 1)
    x <- cumsum(e)
    
    wp <- e * input$nw
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
    length <- input$length_in
    set.seed(seed)
    e <- rnorm(length, 0, 1)
    
    wp <- e * input$nw

    plot(density(e), ylim = c(0, max(density(wp)$y)))
    lines(density(wp), col = c("orange"))
  })
  
  output$Stat_Text <- renderTable({
    seed <- input$seed_in
    length <- input$length_in
    set.seed(seed)
    e <- rnorm(length, 0, 1)
    
    wp <- e * input$nw
    
    out <- data.frame(c("e", "e dampend"))
    colnames(out) <- c("Names")
    out$mean <- c(mean(e), mean(wp))
    out$sd <- c(sd(e), sd(wp))
    
    out
  })
}

# Run the application
shinyApp(ui = ui, server = server)
