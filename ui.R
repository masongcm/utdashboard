
library(tidyr)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(lubridate)
library(ggplot2)
source(here::here("helpers/utclean.R"))
source(here::here("helpers/dataprep.R"))


################################################################
ui <- fluidPage(

  titlePanel(
    div(
      img(src="utlogo.png", height = "80px"), 
      "Activity Dashboard")
    ,
    windowTitle = "Unseen Tours"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Date range slider
      sliderTextInput(
        inputId = "sliderDate", 
        label = "Select date range", 
        grid = FALSE, 
        force_edges = TRUE,
        choices = names(ymgrid),
        selected = c(tail(names(ymgrid), 12)[1], tail(names(ymgrid), 1)) # start with previous year range
      ),
      
      # Tour selection
      checkboxGroupInput("tour_sel",
                         label = "Select tours",
                         choices = levels(activities$activity),
                         selected = levels(activities$activity)[!levels(activities$activity) %in% c("Camden", "Brixton")]
      ),
      
      # Measure selection
      selectInput("meas_sel",
                  label = "Select measure",
                  choices = measures,
                  selected = "total"
      ),
      
      # Column breakdown selection
      selectInput("break_sel",
                  label = "Breakdown by",
                  choices = bdown,
                  selected = "price"
      ),
      
      # Data updated
      br(), br(),
      paste("*Data updated on:", as.character(filedate)),
      
      # Author
      br(), br(),
      h5("Built by",
         tags$a(href="https://masongcm.netlify.com", "Giacomo Mason"),
         "with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         ".")
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs", # make tabs
                  
                  tabPanel("Tour trends",
                           plotOutput("linegraph")
                  )
                  ,
                  tabPanel("Breakdown",
                           plotOutput("areagraph")
                  )
                  ,
                  tabPanel("Year-on-year",
                           plotOutput("bargraph")
                  )
      )
    )
  )
  ,
  theme = shinytheme("paper")
)
