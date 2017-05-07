# 4/20/2017 kac

library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)

# I've been tracking my fillups since August 2011
# keeping track of date, odometer reading, amount of gas, and cost of gas
# Here I plan to look at the numbers!

# 1. odometer reading over time. Basic.
# 2. then mileage time series: need to create new variable.
# 3. cost of gas over time
# 4. number of fillups per month
# 5. mileage per month (box plot/dots +/- std dev)
# 6. cost of gas per month

## can overlay dates of oil changes and see if any of the above variables changes before and after
## same with date of new tires (7/19/2014)


dat <- read.csv("mileage.csv")
dat$Date <- mdy(dat$Date)
dat <- dat %>%
    mutate(milesadded = as.numeric(diff(zoo(Odometer), na.pad=TRUE)),
           daysbetween = as.numeric(diff(zoo(Date), na.pad=TRUE)),
           mileage = milesadded/Gallons,
           gasprice = Cost/Gallons,
           Month = as.factor(month(Date, abbr=TRUE)),
           Year = year(Date))
# 
# bymonth <- group_by(dat, Year, Month) %>%
#     summarize(., avgmpg = mean(mileage, na.rm=TRUE), 
#               avgprice = mean(gasprice, na.rm=TRUE),
#               avgdays = mean(daysbetween, na.rm=TRUE),
#               numfillups = count(Odometer))

pdftitle <- paste0("fillup graphs_", Sys.Date(), ".pdf")
pdf(pdftitle, height=8, width=10)

ggplot(dat) +
    geom_point(aes(x=Date, y=Odometer)) +
    theme_minimal() +
    ggtitle("Odometer reading over time")

ggplot(dat) +
    geom_point(aes(x=Date, y=gasprice)) +
    theme_minimal() +
    ggtitle("Gas price over time") +
    ylab("Price per gallon ($)")

ggplot(dat) +
    geom_boxplot(aes(x=Month, y=gasprice)) +
    theme_minimal() +
    ggtitle("Gas price binned by month, across all years") +
    ylab("Price per gallon ($)")

ggplot(dat) +
    geom_point(aes(x=Date, y=mileage)) +
    theme_minimal() +
    ggtitle("Mileage over time") +
    ylab("mpg")

ggplot(dat) +
    geom_boxplot(aes(x=Month, y=mileage)) +
    theme_minimal() +
    ggtitle("Mileage binned by month, across all years") +
    ylab("mpg")

ggplot(dat) +
    geom_point(aes(x=Date, y=daysbetween)) +
    theme_minimal() +
    ggtitle("Days between fill-ups") +
    ylab("Days")

ggplot(dat) +
    geom_boxplot(aes(x=Month, y=daysbetween)) +
    theme_minimal() +
    ggtitle("Days between fill-ups, binned by month, across all years") +
    ylab("Days")

dev.off()
