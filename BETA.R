#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Für Raphaëlle"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("a_value",
                        "Valeur du paramètre 'a'",
                        step = 0.05,
                        min = 0,
                        max = 10,
                        value = 5),
        
        sliderInput("b_value",
                    "Valeur du paramètre 'b'",
                    step = 0.05,
                    min = 0,
                    max = 10,
                    value = 5)
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(0, 50, length.out = 100)

        # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #      xlab = 'Waiting time to next eruption (in mins)',
        #      main = 'Histogram of waiting times')
        curve(expr = dbeta(x = x, shape1 = input$a_value, shape2 = input$b_value))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
