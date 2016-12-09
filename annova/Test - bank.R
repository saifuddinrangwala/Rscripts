library(haven)
library(labelled)
library(ggplot2)

setwd("~/Rscripts/annova")

bank <- read_sav("datafiles/bank.sav")

# Identity the factor and convert the data type to be a factor
bank$BankType = to_factor(bank$BankType)

# Plot the boxplot for groups by channel no
boxplot(bank$Business_employee ~ bank$BankType, data = bank)

# Super imposed denisty plot by factor
qplot(bank$Business_employee, colour=bank$BankType, data=bank, geom="density")

# Get the annova table
bank.aov <- aov(bank$Business_employee ~ bank$BankType, data = bank)

summary(bank.aov)
