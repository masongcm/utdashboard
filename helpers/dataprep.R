
# import data
filename <- here::here("data/tours_20181021.csv")
utdata <- utclean(filename)
activities <- utdata$activities

# make year-month grid starting Jan 2016
filedate <- file.info(filename)$mtime
thismonth <- paste(year(filedate), str_pad(month(filedate), 2, pad = 0), "01", sep = "-")

month_n <- str_pad(seq(1,12), 2, pad = 0)
month_s <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
year <- seq(2016,year(filedate))

ymgrid  <- within(expand.grid(month_n,year), igacol <- paste(Var2,Var1,"01", sep="-"))$igacol
names(ymgrid) <- within(expand.grid(month_s,year), igacol <- paste(Var1, Var2, sep=" "))$igacol
ymgrid <- ymgrid[1:(which(ymgrid==thismonth)-1)] # trim at month before current

# fixed tour colours
tourColours <- setNames(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"),
                        c("Brick Lane", "Camden", "Soho", "Brixton", "London Bridge", "Shoreditch", "Covent Garden"))

# measure labels
measures <- c(
  "Num. guests" = "total",
  "Num. tours" = "ntours",
  "Average Capacity (guests/tour)" = "capacity"
)