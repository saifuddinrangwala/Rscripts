library(haven)
library(labelled)

setwd("~/Rscripts/annova")

reality_show <- read_sav("datafiles/Reality Show.sav")

# convert list to data table
reality_show <- as.data.frame(reality_show)

# This is how you get channel labels
channel_labels = attr(val_labels(reality_show$Channel_No), 'names')

# Plot the boxplot for groups by channel no
boxplot(reality_show$TVR ~ reality_show$Channel_No)


# Get the annova table
reality_show.aov <- aov(reality_show$TVR ~ reality_show$Channel_No)
summary(reality_show.aov)