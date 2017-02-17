require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

TractorSale <- read_sav("datafiles/Tractor Sale.sav")

# convert list to data table
TractorSale.table <- data.table(TractorSale)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
TractorSale$Type = to_factor(TractorSale$Type)
TractorSale$Region = to_factor(TractorSale$Region)


# Plot the boxplot for groups
boxplot(TractorSale$No_Units ~ TractorSale$Type)
boxplot(TractorSale$No_Units ~ TractorSale$Region)

# Super imposed denisty plot by factor
qplot(TractorSale$No_Units, colour=TractorSale$Type, data=TractorSale, geom="density")

# Get the descritives table by each group
describeBy(TractorSale$No_Units, TractorSale$Type)
describeBy(TractorSale$No_Units, TractorSale$Region)

# Get the annova table
TractorSale.aov <- aov(TractorSale$No_Units ~ TractorSale$Type + TractorSale$Region, data = TractorSale)
summary(TractorSale.aov)

# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(TractorSale.aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

# Get the means plot
plot.design(TractorSale)

# Get the interaction plot
interaction.plot(TractorSale$Region, TractorSale$Type, TractorSale$No_Units, col=2:12, lty=1)
