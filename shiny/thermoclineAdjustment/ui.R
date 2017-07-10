# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)
library("RMySQL")
source("functions.R")

# Use a fluid Bootstrap layout
fluidPage(    
  # Give the page a title
  titlePanel("Profile Adjustment"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("date", "Date", 
                  choices=getProfile(),selected = 1),
      hr(),
      helpText("Select the profile to apply adjustment")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("phonePlot")
    )
    
  )
)
