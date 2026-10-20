library(shiny)
library(bslib)
library(ggplot2)
library(glue)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("U.S. Superstore online order data from 2014-2018"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          navset_pill(
            nav_panel("About",
                      markdown(
                        glue(
                          "This data was obtained from [Kaggle](https://www.kaggle.com/datasets/juhi1994/superstore/data)"
                        )
                      )),
            nav_panel("Data Download"),
            nav_panel("Data Exploration")
          ),
           plotOutput("distPlot")
        )
    )
)

# Define server logic
server <- function(input, output, session) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
