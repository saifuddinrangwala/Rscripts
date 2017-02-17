require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

MobileRevenue <- read_sav("datafiles/Mobile Revenue.sav")

# convert list to data table
MobileRevenue.table <- data.table(MobileRevenue)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
MobileRevenue$Product = to_factor(MobileRevenue$Product)

# Plot the boxplot for groups by channel no
boxplot(MobileRevenue$Revenue ~ MobileRevenue$Product)

# Super imposed denisty plot by factor
qplot(MobileRevenue$Revenue, colour=MobileRevenue$Product, data=MobileRevenue, geom="density")

# Get the descritives table by each group
describeBy(MobileRevenue$Revenue, MobileRevenue$Product)

# Get the annova table
MobileRevenue.aov <- aov(MobileRevenue$Revenue ~ MobileRevenue$Product, data = MobileRevenue)
summary(MobileRevenue.aov)

# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(MobileRevenue.aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

