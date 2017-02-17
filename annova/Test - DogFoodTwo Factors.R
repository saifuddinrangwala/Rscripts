require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

DogFood <- read_sav("datafiles/DogFood.sav")

# convert list to data table
DogFood.table <- data.table(DogFood)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
DogFood$sheight = to_factor(DogFood$sheight)
DogFood$facing = to_factor(DogFood$facing)


# Plot the boxplot by individual factor
boxplot(DogFood$sale ~ DogFood$sheight)
boxplot(DogFood$sale ~ DogFood$facing)

# Super imposed denisty plot by factor
qplot(DogFood$sale, colour=DogFood$sheight, data=DogFood, geom="density")

# Get the descritives table by each group
describeBy(DogFood$sale, DogFood$sheight)
describeBy(DogFood$sale, DogFood$facing)

# Get the annova table
DogFood_aov <- aov(DogFood$sale ~ DogFood$sheight + DogFood$facing + DogFood$sheight:DogFood$facing, data = DogFood)
summary(DogFood_aov)
DogFood_residual = residuals(DogFood_aov)

#Box plot across multiple factors
boxplot(DogFood$sale ~ DogFood$sheight * DogFood$facing, data = DogFood)
boxplot(DogFood_residual ~ DogFood$sheight * DogFood$facing, data = DogFood)


# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(DogFood_aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

# Get the means plot
plot.design(DogFood)

# Get the interaction plot
interaction.plot(DogFood$facing, DogFood$sheight, DogFood$sale, col=2:12, lty=1)
interaction.plot(DogFood$sheight, DogFood$facing, DogFood$sale, col=2:12, lty=1)
