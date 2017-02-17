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
MobileRevenue$Month = to_factor(MobileRevenue$Month)


# Plot the boxplot for groups
boxplot(MobileRevenue$Revenue ~ MobileRevenue$Product)
boxplot(MobileRevenue$Revenue ~ MobileRevenue$Month)

# Super imposed denisty plot by factor
qplot(MobileRevenue$Revenue, colour=MobileRevenue$Product, data=MobileRevenue, geom="density")

# Get the descritives table by each group
describeBy(MobileRevenue$Revenue, MobileRevenue$Product)
describeBy(MobileRevenue$Revenue, MobileRevenue$Month)

# Get the annova table
MobileRevenue.aov <- aov(MobileRevenue$Revenue ~ MobileRevenue$Product + Mobile_Revenue$Month, data = MobileRevenue)
summary(MobileRevenue.aov)

# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(MobileRevenue.aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

# Get the means plot
plot.design(MobileRevenue)

# Get the interaction plot
interaction.plot(MobileRevenue$Month, MobileRevenue$Product, MobileRevenue$Revenue, col=2:7, lty=1)
