
library(tidyr)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(ggplot2)
source(here::here("helpers/utclean.R"))
source(here::here("helpers/dataprep.R"))



################################################################
ui <- fluidPage(
  titlePanel("Unseen Tours activity dashboard"),
  paste("*Data updated on:", as.character(filedate)),
  
  sidebarLayout(
    sidebarPanel(
      
      # Date range slider
      sliderTextInput(
        inputId = "sliderDate", 
        label = "Select date range", 
        grid = FALSE, 
        force_edges = TRUE,
        choices = names(ymgrid),
        selected = c(tail(names(ymgrid), 12)[1], tail(names(ymgrid), 1))
      ),
      
      # Tour selection
      checkboxGroupInput("tour_sel",
                         label = "Select tours",
                         choices = levels(activities$activity),
                         selected = levels(activities$activity)
      ),
      
      # Measure selection
      selectInput("meas_sel",
                  label = "Select measure",
                  choices = measures,
                  selected = "total"
      )
    ),
    mainPanel(
      
      # line plot
      plotOutput("linegraph"),
      plotOutput("areagraph")
      
    )
  )
)
