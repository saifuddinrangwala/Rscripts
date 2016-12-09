require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

bank <- read_sav("datafiles/bank.sav")

# convert list to data table
bank.table <- data.table(bank)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
bank$BankType = to_factor(bank$BankType)

# Plot the boxplot for groups by channel no
boxplot(bank$Business_employee ~ bank$BankType)

# Super imposed denisty plot by factor
qplot(bank$Business_employee, colour=bank$BankType, data=bank, geom="density")

# Get the descritives table by each group
describeBy(bank$Business_employee, bank$BankType)

# Get the annova table
bank.aov <- aov(bank$Business_employee ~ bank$BankType, data = bank)
summary(bank.aov)

# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(bank.aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

