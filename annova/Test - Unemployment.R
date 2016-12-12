require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

Unemployment <- read_sav("datafiles/unemployment.sav")

# convert list to data table
Unemployment.table <- data.table(Unemployment)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
Unemployment$month = to_factor(Unemployment$month)
Unemployment$year = to_factor(Unemployment$year)


# Plot the boxplot by individual factor
boxplot(Unemployment$unemp ~ Unemployment$month)
boxplot(Unemployment$unemp ~ Unemployment$year)

# Super imposed denisty plot by factor
qplot(Unemployment$unemp, colour=Unemployment$month, data=Unemployment, geom="density")

# Get the descritives table by each group
describeBy(Unemployment$unemp, Unemployment$month)
describeBy(Unemployment$unemp, Unemployment$year)

# Get the annova table
Unemployment_lm <- lm(Unemployment$unemp ~ Unemployment$month + Unemployment$year + Unemployment$month:Unemployment$year, data = Unemployment)
Unemployment_lm_summary = summary(Unemployment_lm)
Unemployment_lm_summary
if (Unemployment_lm_summary$r.squared < 0.5)
{
  print("R- squared value is very less, which means using analysis of variance will not help here")
}

Unemployment_aov <- aov(Unemployment_lm)
summary(Unemployment_aov)
Unemployment_residual = residuals(Unemployment_aov)
Unemployment_fitted = fitted(Unemployment_aov)

# Get the residual vs fitted plot i.e. check how close are observed values to fitted values
# http://blog.minitab.com/blog/adventures-in-statistics/regression-analysis-how-do-i-interpret-r-squared-and-assess-the-goodness-of-fit
plot(Unemployment_fitted, Unemployment_residual)
abline(h=0, lty=2)
lines(smooth.spline(Unemployment_fitted, Unemployment_residual))

#Box plot across multiple factors
#boxplot(Unemployment$unemp ~ Unemployment$month * Unemployment$year, data = Unemployment)
#boxplot(Unemployment_residual ~ Unemployment$month * Unemployment$year, data = Unemployment)

# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(Unemployment_aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

# Get the means plot
plot.design(Unemployment)

# Get the interaction plot
interaction.plot(Unemployment$year, Unemployment$month, Unemployment$unemp, col=2:50, lty=1)
interaction.plot(Unemployment$month, Unemployment$year, Unemployment$unemp, col=2:14, lty=1)
