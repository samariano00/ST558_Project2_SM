library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(glue)
library(DT)
library(shinycssloaders)
library(rsconnect)

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
                             choices = c("Central", "South", "East", "West"),
                             selected = "East"
                             ),
          checkboxGroupInput(inputId = "consumer",
                             label = "Consumer's Segment",
                             choices = c("Consumer", "Corporate", "Home Office"),
                             selected = "Consumer"
                             ),
          checkboxGroupInput(inputId = "cat",
                             label = "Product Category",
                             choices = c("Furniture", "Office Supplies", "Technology"),
                             selected = "Furniture"
                             ),
          "Note: To look at impact of numeric variables only, select all options for U.S. Region, Consumer's Segment, and Product Category",
          # Add input values for numeric variables
          sliderInput(inputId = "quantity",
                      label = "Quantity of Units Sold",
                      min = 1,
                      max = 14,
                      value = c(2,10)
                      ),
          sliderInput(inputId = "discount",
                      label = "Discount Applied to Item(s)",
                      min = 0,
                      max = 1,
                      value = c(0.25,0.75)
                      ),
          "Note: To look only at the impact that the categorical variables have on the data without any impact from numeric variables, set range for Quantity and Discount to minimum and maximum values.",
          
          # Add code for action button
          actionButton(inputId = "store_sample",
                       label = "Subset Data for Analysis!")
        ),

        # Add code for different tabs as desired in the main panel
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
            nav_panel("Data Download",
                      dataTableOutput(outputId = "download_data"),
                      downloadButton(outputId = "data_button",
                                     label = "Download")
                      ),
            nav_panel("Data Exploration",
                      sidebarPanel(
                        # Additional categorical variables
                        h2("Additional Variables to Filter Data On"),
                        checkboxGroupInput(inputId = "sub_category",
                                           label = "Product's Sub-Category",
                                           choices = c("Accessories", "Appliances", "Art", "Binders", "Bookcases", "Chairs", "Copiers", "Envelopes", "Fasteners", "Furnishings", "Labels", "Machines", "Paper", "Phones", "Storage", "Supplies", "Tables")
                                           ),
                        checkboxGroupInput(inputId = "state",
                                           label = "Consumer's State",
                                           choices = c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
                                           ),
                        
                        # Additional numeric variables
                        sliderInput(inputId = "profit",
                                    label = "Profit from Individual Sales (USD)",
                                    min = -6600,
                                    max = 8400,
                                    value = c(-1000,1000),
                                    pre = "$"
                        ),
                        sliderInput(inputId = "sales",
                                    label = "Individual Sales (USD)",
                                    min = 0,
                                    max = 22650,
                                    value = c(0,10000),
                                    pre = "$"
                        ),
                        "Note: To look only at the impact that the categorical variables have on the data without any impact from numeric variables, set range for Sales and Profit to minimum and maximum values."
                      ),
                      tabsetPanel(
                        tabPanel("Categorical Data Summary",
                                
                                 tableOutput(outputId = "region_sum"),
                                 tableOutput(outputId = "consumer_sum"),
                                 tableOutput(outputId = "category_sum"),
                                 tableOutput(outputId = "region_consumer_sum"),
                                 tableOutput(outputId = "region_category_sum"),
                                 tableOutput(outputId = "consumer_category_sum")
                                 ),
                        tabPanel("Numeric Data Summary",
                                 sidebarPanel(
                                   
                                 )
                                 )
                      )
                      )
          )
        )
    )
)

# Load the raw data into R
store_data <- readRDS("../superstoredata.RDS")

# Define server logic
server <- function(input, output, session) {

  # Creating reactive value for action button
  store_sample <- reactiveValues(sample_data = NULL)
  
  # Code for side bar action button
  observeEvent(input$store_sample, {
    
    # Connect input data to store data
    subset_data <- store_data
    
    index <- sample(1:nrow(subset_data))
    
    # Update reactive object
    store_sample$sample_data <- subset_data[index,]
    
    
        })
  # Part of my app involves subsetting state and product sub-category involved in region and product category respectively, so I'm going to create the code to do that here
  
  #observeEvent(updateCheckboxGroupInput(session,
                                        #inputId = "Sub-Category",
                                        #choices = unique(store_sample$sample_data$Category)),
               #updateCheckboxGroupInput(session,
                                        #inputId = "State",
                                        #choices = unique(store_sample$sample_data$Region))
  #)
  
  # Render the data table to be downloaded in the Data Download tab
  output$download_data <- renderDataTable(store_sample$sample_data)
  
  # Code for download button
  
  # Code for Loader
    }
  
    

# Run the application 
shinyApp(ui = ui, server = server)
