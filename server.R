server <- function(input, output) {
  
  # common options for graphs
  commonopts <- list(
    scale_x_date(name = "Month", date_breaks = "3 months", date_minor_breaks = "month", date_labels = "%b %y"),
    geom_vline(xintercept = c(ymd("2016-12-15"),ymd("2017-12-15"), ymd("2018-12-15")), linetype = "dashed"),
    theme_light(),
    theme(legend.position="bottom")
  )
  
  # fetch date ranges from ymgrid based on slider
  daterange <- reactive({
    interval(
      ymd(ymgrid[input$sliderDate[1]]),
      ymd(ymgrid[input$sliderDate[2]]) + months(1) - days(1)
    )
  })
  
  # prepare plot data
  lplotdata <- reactive({
    activities %>%
      filter(
        dateact %within% daterange() # daterange must be called as a function! https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/
      ) %>%
      group_by(monthact, activity) %>%
      summarise(full = sum(full), 
                conc = sum(conc), 
                total = sum(total),
                ntours = n(),
                capacity = total/ntours)
  })
  
  # line graph
  output$linegraph <- renderPlot(
    {
      lplotdata() %>%
        filter(
          activity %in% input$tour_sel
        ) %>%
        ggplot(aes(x=monthact, y=eval(as.name(input$meas_sel)), color = activity)) + 
        geom_line() + geom_point() + 
        ylab(names(measures[which(measures == input$meas_sel)])) +
        labs(color="Tour:") + scale_color_manual(values = tourColours) +
        commonopts
      
    }
  )
  
  # prepare plot data 2
  aplotdata <- reactive({
    activities %>%
      filter(
        dateact %within% daterange() # daterange must be called as a function! https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/
      ) %>%
      group_by(monthact, manual) %>%
      summarise( 
        total = sum(total),
        ntours = n(),
        capacity = total/ntours) %>%
      ungroup() %>%
      complete(monthact, manual, fill = list(0)) %>%
      mutate(
        total = replace_na(total, 0),
        ntours = replace_na(ntours, 0),
        capacity = replace_na(capacity, 0)
      ) 
  })
  # Total area graph
  output$areagraph <- renderPlot(
    {
      aplotdata() %>%
        ggplot(aes(x=monthact, y=eval(as.name(input$meas_sel)), fill = manual)) + 
        geom_area() +
        ylab(names(measures[which(measures == input$meas_sel)])) +
        labs(fill="Booking type:") +
        commonopts
    }
  )
  
}