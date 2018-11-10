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
  
  # LINE PLOT ---------------------------------------------------------
  # prepare plot data
  lplotdata <- reactive({
    activities %>%
      filter(
        dateact %within% daterange() # daterange is reactive
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
  
  # AREA PLOT ---------------------------------------------------------
  # prepare plot data 2
  aplotdata <- reactive({
    activities %>%
      filter(
        dateact %within% daterange() # daterange must be called as a function! https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/
      ) %>%
      gather(price, guests, full, conc) %>% # gather concession vs full price
      group_by(monthact, !! rlang::sym(input$break_sel)) %>% # GOTCHA: evaluate with !! in dplyr
      summarise( 
        total = sum(total),
        ntours = n(),
        capacity = total/ntours) %>%
      ungroup() %>%
      complete(monthact, !! rlang::sym(input$break_sel), fill = list(0)) %>%
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
        ggplot(aes(x=monthact, y=eval(as.name(input$meas_sel)), fill = eval(as.name(input$break_sel)))) + 
        geom_area() +
        ylab(names(measures[which(measures == input$meas_sel)])) +
        labs(fill = names(bdown[which(bdown==input$break_sel)])) +
        commonopts
    }
  )
  
  # BAR PLOT ---------------------------------------------------------
  
  # position of last month in slider range
  pos <- reactive({ which(names(ymgrid)==input$sliderDate[2]) })
  # choose last 3 months and same months in last year
  range1 <- reactive({ ymd(ymgrid[(pos()-2):pos()]) })
  range2 <- reactive({ ymd(ymgrid[(pos()-14):(pos()-12)]) })
  range <- reactive({ c(range2(), range1()) })
  
  # prepare data
  bplotdata <- reactive({
    activities %>%
    filter(monthact %in% range()) %>% # filter dates
    mutate(period = ifelse(monthact %in% range1(), "This year", "Last year")) %>% #distinguish year
    gather(price, guests, full, conc) %>% # gather concession vs full price
    group_by(period, monthact, activity, price, manual) %>%
    summarise(guests = sum(guests)) %>% 
    ungroup() %>%
    arrange(monthact, activity, price, manual) %>%
    mutate(monthact2 = factor(monthact))
  })
  
  # plot
  output$bargraph <- renderPlot({
    ggplot(bplotdata(), aes(x=monthact2, y=guests, fill=eval(as.name(input$break_sel)), alpha=period)) +
      geom_bar(stat = "identity", position = "stack") + facet_grid(~ activity) + 
      scale_x_discrete("Month", labels = strftime(levels(bplotdata()$monthact2), format = "%b\n%y")) +
      ylab("Number of guests") +
      scale_alpha_manual(values = c(.5, 1)) +
      theme_light() + theme(legend.position="bottom") + 
      guides(alpha = FALSE) + labs(fill = names(bdown[which(bdown==input$break_sel)]))
    
  })
  
  
}