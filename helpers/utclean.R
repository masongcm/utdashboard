# UTCLEAN
# function to clean and prepare data from peekpro
# selecting rows via brush/hover: http://shiny.rstudio.com/articles/selecting-rows-of-data.html
# req function to set required input: https://shiny.rstudio.com/reference/shiny/latest/req.html

utclean <- function(filename) {
  
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(here)
  
  # import
  tours <- read.csv(filename, stringsAsFactors = FALSE)
  
  # check columns
  desired_names <- c("Booking.ID", "Activity...Add.On", "First.Name", "Last.Name",
                     "Country.of.Origin", "Tickets...Items", "Purchase.Date", 
                     "Activity.Date", "Activity.Time", "Guests", "Your.Net.Revenue", "Payment.Method")
  for (n in desired_names) {
    if (!n %in% colnames(tours)) stop("Column ", n, " is missing from the csv file.\n Please export it from Peek.")
  }
  
  # define areas
  areas <- c("BLN", "CAM", "SOH", "BXN", "LBR", "SRD", "CGD")
  names(areas) <-  c("Brick Lane", "Camden", "Soho", "Brixton", "London Bridge", "Shoreditch", "Covent Garden")
  
  tours <- tours  %>%
    select(- List.Price, Payin.Method, Payable.To.You) %>%
    rename(bid = Booking.ID,
           status = Status,
           activity = Activity...Add.On,
           firstname = First.Name,
           lastname = Last.Name,
           country = Country.of.Origin,
           tickets = Tickets...Items,
           datepurch = Purchase.Date,
           dateact = Activity.Date,
           timeact = Activity.Time,
           guests = Guests,
           netrev = Your.Net.Revenue,
           paymethod = Payment.Method
    ) %>%
    # clean activity
    mutate(activity2 = replace(activity, str_detect(activity, "Brick"), "Brick Lane")) %>%
    mutate(activity2 = replace(activity2, str_detect(activity, "Camden"), "Camden")) %>%
    mutate(activity2 = replace(activity2, str_detect(activity, "Soho"), "Soho")) %>%
    mutate(activity2 = replace(activity2, str_detect(activity, "Brixton"), "Brixton")) %>%
    mutate(activity2 = replace(activity2, str_detect(activity, "Bridge"), "London Bridge")) %>%
    mutate(activity2 = replace(activity2, str_detect(activity, "Shoreditch"), "Shoreditch")) %>%
    mutate(activity2 = replace(activity2, str_detect(activity, "Covent"), "Covent Garden")) %>%
    mutate(activity = as.factor(activity2)) %>% select(-activity2) %>%
    mutate(actcode = recode_factor(activity, !!!areas)) %>% 
    # extract ticket type
    mutate(tktype = tickets) %>%
    mutate(tktype = replace(tktype, str_detect(tktype, "Student|Concession"), "conc")) %>%
    mutate(tktype = replace(tktype, str_detect(tktype, "Full|Adult"), "full")) %>%
    mutate(tktype = replace(tktype, str_detect(tktype, "Refund"), "refunded")) %>%
    mutate(tktype = replace(tktype, str_detect(tktype, "Special"), "special")) %>%
    # clean booking type
    mutate(manual = as.factor(paymethod)) %>%
    mutate(manual = recode(manual, 
                           "desktop_cash" = 1,
                           "desktop_cc" = 1,
                           "widget_cc" = 0,
                           "peek_com" = 0,
                           "pos_cash" = 1,
                           "pos_cc" = 1
    )) %>%
    # clean dates
    mutate(datepurch = ymd(datepurch),
           dateact = ymd(dateact),
           monthact = update(dateact, day=1)
    ) %>%
    # unique date-activity ID
    mutate(actid = str_replace_all(paste0(as.character(dateact), "_", as.character(actcode)), "-", "_")) %>%
    # select sample
    filter(as.character(actcode) %in% c("BLN", "CAM", "SOH", "BXN", "LBR", "SRD", "CGD"),
           status != "canceled",
           !tktype %in% c("refunded", "special"),
           !str_detect(tickets, "Adjustment") # adjustments are duplicates
    )
  
  # drop unused activity levels
  tours$activity <- droplevels(tours$activity)
  
  # guests number by booking
  guests_bybid <- tours %>%
    group_by(bid, tktype) %>%
    summarise(guests = sum(guests)) %>%
    spread(tktype, guests) %>%
    mutate(
      full = replace_na(full, 0),
      conc = replace_na(conc, 0),
      total = full + conc
    )
  
  # merge with other booking info
  bookwide <- tours %>%
    select(bid, activity, actcode, actid, firstname, lastname, country, netrev, dateact, monthact, manual) %>%
    distinct() %>% 
    inner_join(guests_bybid, by = "bid")
  
  # guests by activity
  guests_byact <- bookwide %>%
    group_by(actid) %>%
    summarise(full = sum(full), conc = sum(conc), total = sum(total), manual = ceiling(mean(manual))) %>%
    mutate(manual = factor(manual, labels = c("Widget booking", "Group booking")))
  
  # merge with other activity info
  actwide <- bookwide %>%
    select(activity, actcode, actid, dateact, monthact) %>%
    distinct() %>%
    inner_join(guests_byact, by = "actid")
  
  # return
  return(list(
    bookings = bookwide, 
    activities = actwide
  ))
  
}
