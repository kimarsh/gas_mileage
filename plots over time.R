# 4/20/2017 kac
# updated 5/7/2017 to add vertical lines on the graphs
# when my marriage stopped being long-distance

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
           Year = year(Date),
           Quarter = quarter(Date, with_year=TRUE),
           Halfyear = semester(Date, with_year=TRUE))
# 
# bymonth <- group_by(dat, Year, Month) %>%
#     summarize(., avgmpg = mean(mileage, na.rm=TRUE), 
#               avgprice = mean(gasprice, na.rm=TRUE),
#               avgdays = mean(daysbetween, na.rm=TRUE),
#               numfillups = count(Odometer))

pdftitle <- paste0("fillup graphs_", Sys.Date(), ".pdf")
pdf(pdftitle, height=8, width=10)

# gas price
ggplot(dat, aes(x=Date, y=gasprice)) +
    geom_point(size=2, col="darkslategray") +
    theme_minimal() +
    ggtitle("Gas price over time") +
    ylab("Price per gallon ($)")

# add LOESS smoothing to that
ggplot(dat, aes(x=Date, y=gasprice)) +
    geom_point(size=2, col="darkslategray") +
    geom_smooth(se=FALSE) +
    theme_minimal() +
    ggtitle("Gas price over time with LOESS smoothing") +
    ylab("Price per gallon ($)")

# how many days between stopping at the gas station?
# again, red line is when marriage stopped being long-distance
ggplot(dat, aes(x=Date, y=daysbetween)) +
    geom_point(size=2, color="darkslategray") +
    theme_minimal() +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-01"))), col="red", lwd=1.5) +
    ggtitle("Days between fill-ups") +
    ylab("Days")

# same as above, but with loess smoothing
ggplot(dat, aes(x=Date, y=daysbetween)) +
    geom_point(size=2, color="darkslategray") +
    theme_minimal() +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-01"))), col="red", lwd=1.5) +
    geom_smooth(se=FALSE) +
    ggtitle("Days between fill-ups, with LOESS smoothing") +
    ylab("Days")

# box plot of days between fill ups, by quarter
ggplot(dat, aes(x=Date, y=daysbetween)) +
    geom_boxplot(aes(group=Quarter), fill="lightgray") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-01"))), col="red", lwd=1.5) +
    theme_minimal() +
    ggtitle("Days between fill-ups, by quarter") +
    ylab("Days")

# box plot of days between fill ups, by 6-months
ggplot(dat, aes(x=Date, y=daysbetween)) +
    geom_boxplot(aes(group=Halfyear), fill="lightgray") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-01"))), col="red", lwd=1.5) +
    theme_minimal() +
    ggtitle("Days between fill-ups, by 6 months") +
    ylab("Days")

# odometer readings
ggplot(dat, aes(x=Date, y=Odometer)) +
    geom_point() +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-01"))), col="red", lwd=1.5) +
    theme_minimal() +
    ggtitle("Odometer reading over time")

# plot mpg, with a red line for when I got to move in with my spouse
ggplot(dat, aes(x=Date, y=mileage)) +
    geom_point(size=2, color="darkslategray") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-01"))), col="red", lwd=1.5) +
    geom_smooth(se=FALSE) +
    theme_minimal() +
    coord_cartesian(ylim = c(20, 45)) +
    ggtitle("Mileage over time") +
    ylab("mpg")

# box plot of mpg; red line means the same
ggplot(dat, aes(x=Date, y=mileage)) +
    geom_boxplot(aes(group=Quarter), fill="lightgray") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-01"))), col="red", lwd=1.5) +
    theme_minimal() +
    coord_cartesian(ylim = c(20, 45)) +
    ggtitle("Mileage over time, by quarter") +
    ylab("mpg")

# box plot of mpg by 6-months
ggplot(dat, aes(x=Date, y=mileage)) +
    geom_boxplot(aes(group=Halfyear), fill="lightgray") +
    geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-01"))), col="red", lwd=1.5) +
    theme_minimal() +
    coord_cartesian(ylim = c(20, 45)) +
    ggtitle("Mileage over time, by 6-month intervals") +
    ylab("mpg")

dev.off()
