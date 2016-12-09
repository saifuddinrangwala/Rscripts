require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

reality_show <- read_sav("datafiles/Reality Show.sav")

# convert list to data table
reality_show.table <- data.table(reality_show)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
reality_show$Channel_No = to_factor(reality_show$Channel_No)

# Plot the boxplot for groups by channel no
boxplot(reality_show$TVR ~ reality_show$Channel_No)

# Super imposed denisty plot by factor
qplot(reality_show$TVR, colour=reality_show$Channel_No, data=reality_show, geom="density")

# Get the descritives table by each group
describeBy(reality_show$TVR, reality_show$Channel_No)

# Get the annova table
reality_show.aov <- aov(reality_show$TVR ~ reality_show$Channel_No, data = reality_show)
summary(reality_show.aov)

# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(reality_show.aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

