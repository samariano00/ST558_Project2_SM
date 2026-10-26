library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(glue)
library(DT)

# Load the raw data into R
store_data <- readRDS("../superstoredata.RDS")

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("U.S. Superstore online order data from 2014-2018"),

    # Add Control Widgets to Sidebar
    sidebarLayout(
        sidebarPanel(
          h2("Create a Subset of the Data"),
          # Add in input values for categorical variables
          checkboxGroupInput(inputId = "region",
                             label = "U.S. Region",
                             choiceNames = c("Central", "South", "East", "West"),
                             choiceValues = c("central", "south", "east", "west"),
                             selected = "east"
                             ),
          checkboxGroupInput(inputId = "consumer",
                             label = "Consumer's Segment",
                             choiceNames = c("Consumer", "Corporate", "Home Office"),
                             choiceValues = c("consumer", "corporate", "home office"),
                             selected = "consumer"
                             ),
          checkboxGroupInput(inputId = "category",
                             label = "Product Category",
                             choiceNames = c("Furniture", "Office Supplies", "Technology"),
                             choiceValues = c("furniture", "office supplies", "technology"),
                             selected = "furniture"
                             ),
          
          # Add input values for numeric variables
          sliderInput(inputId = "profit",
                      label = "Profit from Individual Sales (USD)",
                      min = -6600,
                      max = 8400,
                      value = c(-1000,1000),
                      pre = "$"),
          sliderInput(inputId = "sales",
                      label = "Individual Sales (USD)",
                      min = 0,
                      max = 22650,
                      value = c(0,10000),
                      pre = "$"),
          
          # Add code for action button
          actionButton(inputId = "store_sample",
                       label = "Subset Data for Analysis!"),
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
                          "The purpose of this app is to allow you to investigate online sales from a superstore based in the U.S.  You'll have the ability to look at certain variables and their relationship to one another to get an overarching idea of consumer purchasing habits.
                          
                          This data was obtained from [Kaggle](https://www.kaggle.com/datasets/juhi1994/superstore/data) and ranges in years from 2014-2018.  It contains information like what city and state a consumer is buying from, what they're buying, and how much profit the store made off of that sale.
                          
                          Moving on to how to use this app, the Sidebar will allow you to subset the data according to the values of different variables.
                          
                          The About page (where you are now!) provides a general description of other parts of the app (like the sidebar and what the different tabs are for) and gives a little more context to the data you'll be investigating.
                          
                          The Data Download page will allow you to save your data (subset or not) as a file for your records if you want to look back at previous analyses/ work you performed/
                          
                          The Data Exploration page will be where the magic happens!  This page will have the graphical and numeric outputs of the data based on the values you've set in the Sidebar."
                        )
                      ),
                      tags$figure(
                        class = "centerGettyImage",
                        HTML("<a id='B2pnQLCrSYpEDEyEAUxV2Q' class='gie-single' href='https://www.gettyimages.com/detail/1403886950' target='_blank' style='color:#a7a7a7;text-decoration:none;font-weight:normal !important;border:none;display:inline-block;'>Embed from Getty Images</a><script>window.gie=window.gie||function(c){(gie.q=gie.q||[]).push(c)};gie(function(){gie.widgets.load({id:'B2pnQLCrSYpEDEyEAUxV2Q',sig:'ICrAZ1U830O0NvZ3Qp_tfP1kVIkz6-HN4K99fDIy3So=',w:'496px',h:'347px',items:'1403886950',caption: true ,tld:'com',is360: false })});</script><script src='//embed-cdn.gettyimages.com/widgets.js' charset='utf-8' async></script>"),
                        tags$figcaption("Shopping Carts on a Bar Graph to Combine Data Analysis with the Type of Data You'll be Working With!")
                        )
                      ),
            nav_panel("Data Download"),
            nav_panel("Data Exploration",
                      tabsetPanel(
                        tabPanel("Categorical Data Summary"),
                        tabPanel("Numeric Data Summary")
                      )
                      )
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
