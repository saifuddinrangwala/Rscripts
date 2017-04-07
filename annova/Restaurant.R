require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

Restaurants <- read_sav("datafiles/restaurants.sav")

# convert list to data table
Restaurants.table <- data.table(Restaurants)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
Restaurants$Price = to_factor(Restaurants$Price)
Restaurants$Advt = to_factor(Restaurants$Advt)


# Plot the boxplot by individual factor
boxplot(Restaurants$Sale ~ Restaurants$Price)
boxplot(Restaurants$Sale ~ Restaurants$Advt)

# Super imposed denisty plot by factor
qplot(Restaurants$Sale, colour=Restaurants$Price, data=Restaurants, geom="density")

# Get the descritives table by each group
describeBy(Restaurants$Sale, Restaurants$Price)
describeBy(Restaurants$Sale, Restaurants$Advt)

# Get the annova table, customer is added as a co-variate
Restaurants_aov <- aov(Restaurants$Sale ~ Restaurants$Price + Restaurants$Advt + Restaurants$Size + Restaurants$Price:Restaurants$Advt, data = Restaurants)
summary(Restaurants_aov)
Restaurants_residual = residuals(Restaurants_aov)

#Box plot across multiple factors
boxplot(Restaurants$Sale ~ Restaurants$Price * Restaurants$Advt, data = Restaurants)
boxplot(Restaurants_residual ~ Restaurants$Price * Restaurants$Advt, data = Restaurants)


# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(Restaurants_aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

# Get the means plot
plot.design(Restaurants)

# Get the interaction plot
interaction.plot(Restaurants$Price, Restaurants$Advt, Restaurants$Sale, col=2:12, lty=1)

