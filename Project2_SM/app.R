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
    
    # Add checks for action button
    if(input$region == "Central"){
      region_sub <- "Central"
    } if(input$region == "South"){
      region_sub <- "South"
    } if(input$region == "East"){
      region_sub <- "East"
    } if (input$region == "West"){
      region_sub <- "West"
    }
    
    if(input$consumer == "Consumer"){
      consumer_sub <- "Consumer"
    } if(input$consumer == "Corporate"){
      consumer_sub <- "Corporate"
    } if(input$consumer == "Home Office"){
      consumer_sub <- "Home Office"
    }
    
    if(input$cat == "Furniture"){
      cat_sub <- "Furniture"
    } if(input$cat == "Office Supplies"){
      cat_sub <- "Office Supplies"
    } if(input$cat == "Technology"){
      cat_sub <- "Technology"
    }
    
    if(input$sub_category == "Accessories"){
      sub_cat_sub <- "Accessories"
    } if(input$sub_category == "Appliances"){
      sub_cat_sub <- "Appliances"
    } if(input$sub_category == "Art"){
      sub_cat_sub <- "Art"
    } if(input$sub_category == "Binders"){
      sub_cat_sub <- "Binders"
    } if(input$sub_category =="Bookcases"){
      sub_cat_sub <- "Bookcases"
    } if(input$sub_category == "Chairs"){
      sub_cat_sub <- "Chairs"
    } if(input$sub_category == "Copiers"){
      sub_cat_sub <- "Copiers"
    } if(input$sub_category == "Envelopes"){
      sub_cat_sub <- "Envelopes"
    } if(input$sub_category == "Fasteners"){
      sub_cat_sub <- "Fasteners"
    } if(input$sub_category == "Furnishings"){
      sub_cat_sub <- "Furnishings"
    } if(input$sub_category == "Labels"){
      sub_cat_sub <- "Labels"
    } if(input$sub_category == "Machines"){
      sub_cat_sub <- "Machines"
    } if(input$sub_category == "Paper"){
      sub_cat_sub <- "Paper"
    } if(input$sub_category == "Phones"){
      sub_cat_sub <- "Phones"
    } if(input$sub_category == "Storage"){
      sub_cat_sub <- "Storage"
    } if(input$sub_category == "Supplies"){
      sub_cat_sub <- "Supplies"
    } if(input$sub_category == "Tables"){
      sub_cat_sub <- "Tables"
    }
    
    if(input$state == "Alabama"){
      state_sub <- "Alabama"
    } if(input$state == "Arizona"){
      state_sub <- "Arizona"
    } if(input$state == "Arkansas"){
      state_sub <- "Arkansas"
    } if(input$state == "California"){
      state_sub <- "California"
    } if(input$state == "Colorado"){
      state_sub <- "Colorado"
    } if(input$state == "Connecticut"){
      state_sub <- "Connecticut"
    } if(input$state == "Delaware"){
      state_sub <- "Delaware"
    } if(input$state == "District of Columbia"){
      state_sub <- "District of Columbia"
    } if(input$state == "Florida"){
      state_sub <- "Florida"
    } if(input$state == "Georgia"){
      state_sub <- "Georgia"
    } if(input$state == "Idaho"){
      state_sub <- "Idaho"
    } if(input$state == "Illinois"){
      state_sub <- "Illinois"
    } if(input$state == "Indiana"){
      state_sub <- "Indiana"
    } if(input$state == "Iowa"){
      state_sub <- "Iowa"
    } if(input$state == "Kansas"){
      state_sub <- "Kansas"
    } if(input$state == "Kentucky"){
      state_sub <- "Kentucky"
    } if(input$state == "Louisiana"){
      state_sub <- "Louisiana"
    } if(input$state == "Maine"){
      state_sub <- "Maine"
    } if(input$state == "Maryland"){
      state_sub <- "Maryland"
    } if(input$state == "Massachusetts"){
      state_sub <- "Massachusetts"
    } if(input$state == "Michigan"){
      state_sub <- "Michigan"
    } if(input$state == "Minnesota"){
      state_sub <- "Minnesota"
    } if(input$state == "Mississippi"){
      state_sub <- "Mississippi"
    } if(input$state == "Missouri"){
      state_sub <- "Missouri"
    } if(input$state == "Montana"){
      state_sub <- "Montana"
    } if(input$state == "Nebraska"){
      state_sub <- "Nebraska"
    } if(input$state == "Nevada"){
      state_sub <- "Nevada"
    } if(input$state == "New Hampshire"){
      state_sub <- "New Hampshire"
    } if(input$state == "New Jersey"){
      state_sub <- "New Jersey"
    } if(input$state == "New Mexico"){
      state_sub <- "New Mexico"
    } if(input$state == "New York"){
      state_sub <- "New York"
    } if(input$state == "North Carolina"){
      state_sub <- "North Carolina"
    } if(input$state == "North Dakota"){
      state_sub <- "North Dakota"
    } if(input$state == "Ohio"){
      state_sub <- "Ohio"
    } if(input$state == "Oklahoma"){
      state_sub <- "Oklahoma"
    } if(input$state == "Oregon"){
      state_sub <- "Oregon"
    } if(input$state == "Pennsylvania"){
      state_sub <- "Pennsylvania"
    } if(input$state == "Rhode Island"){
      state_sub <- "Rhode Island"
    } if(input$state == "South Carolina"){
      state_sub <- "South Carolina"
    } if(input$state == "South Dakota"){
      state_sub <- "South Dakota"
    } if(input$state == "Tennessee"){
      state_sub <- "Tennessee"
    } if(input$state == "Texas"){
      state_sub <- "Texas"
    } if(input$state == "Utah"){
      state_sub <- "Utah"
    } if(input$state == "Vermont"){
      state_sub <- "Vermont"
    } if(input$state == "Virginia"){
      state_sub <- "Virginia"
    } if(input$state == "Washington"){
      state_sub <- "Washington"
    } if(input$state == "West Virginia"){
      state_sub <- "West Virginia"
    } if(input$state == "Wisconsin"){
      state_sub <- "Wisconsin"
    } if(input$state == "Wyoming"){
      state_sub <- "Wyoming"
    }
    
    # Connect input data to store data
    subset_data <- store_data |>
      # Filter categorical variables first
      filter(
        Region %in% region_sub,
        Segment %in% consumer_sub,
        Category %in% cat_sub,
        `Sub-Category` %in% sub_cat_sub,
        State %in% state_sub
      ) %>% # Filter numeric variables
      {if("Quantity" %in% input$quantity) filter(store_data, Quantity > 1)} %>%
      {if("Discount" %in% input$discount) filter(store_data, Discount > 0)} %>%
      {if("Profit" %in% input$profit) filter(store_data, Profit > -6600)} %>%
      {if("Sales" %in% input$sales) filter(store_data, Sales > 0)}
    
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
  #output$download_data <- renderDataTable(store_sample$sample_data)
  
  # Code for download button
  
  # Code for Loader
    }
  
    

# Run the application 
shinyApp(ui = ui, server = server)
