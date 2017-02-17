require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

ForeignTravelPreference <- read_sav("datafiles/Foreign Travel Preference.sav")

# convert list to data table
ForeignTravelPreference.table <- data.table(ForeignTravelPreference)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
ForeignTravelPreference$Gender = to_factor(ForeignTravelPreference$Gender)
ForeignTravelPreference$Travel_Group = to_factor(ForeignTravelPreference$Travel_Group)


# Plot the boxplot by individual factor
boxplot(ForeignTravelPreference$Preference ~ ForeignTravelPreference$Gender)
boxplot(ForeignTravelPreference$Preference ~ ForeignTravelPreference$Travel_Group)

# Super imposed denisty plot by factor
qplot(ForeignTravelPreference$Preference, colour=ForeignTravelPreference$Gender, data=ForeignTravelPreference, geom="density")

# Get the descritives table by each group
describeBy(ForeignTravelPreference$Preference, ForeignTravelPreference$Gender)
describeBy(ForeignTravelPreference$Preference, ForeignTravelPreference$Travel_Group)

# Get the annova table
ForeignTravelPreference_lm <- lm(ForeignTravelPreference$Preference ~ ForeignTravelPreference$Gender + ForeignTravelPreference$Travel_Group + ForeignTravelPreference$Gender:ForeignTravelPreference$Travel_Group, data = ForeignTravelPreference)
ForeignTravelPreference_lm_summary = summary(ForeignTravelPreference_lm)
if (ForeignTravelPreference_lm_summary$r.squared < 0.5)
{
  print("R- squared value is very less, which means using analysis of variance will not help here")
}

ForeignTravelPreference_aov <- aov(ForeignTravelPreference_lm)
summary(ForeignTravelPreference_aov)
ForeignTravelPreference_residual = residuals(ForeignTravelPreference_aov)
ForeignTravelPreference_fitted = fitted(ForeignTravelPreference_aov)

# Get the residual vs fitted plot i.e. check how close are observed values to fitted values
# http://blog.minitab.com/blog/adventures-in-statistics/regression-analysis-how-do-i-interpret-r-squared-and-assess-the-goodness-of-fit
plot(ForeignTravelPreference_fitted, ForeignTravelPreference_residual)
abline(h=0, lty=2)
lines(smooth.spline(ForeignTravelPreference_fitted, ForeignTravelPreference_residual))

#Box plot across multiple factors
boxplot(ForeignTravelPreference$Preference ~ ForeignTravelPreference$Gender * ForeignTravelPreference$Travel_Group, data = ForeignTravelPreference)
boxplot(ForeignTravelPreference_residual ~ ForeignTravelPreference$Gender * ForeignTravelPreference$Travel_Group, data = ForeignTravelPreference)


# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(ForeignTravelPreference_aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

# Get the means plot
plot.design(ForeignTravelPreference)

# Get the interaction plot
interaction.plot(ForeignTravelPreference$Travel_Group, ForeignTravelPreference$Gender, ForeignTravelPreference$Preference, col=2:12, lty=1)
interaction.plot(ForeignTravelPreference$Gender, ForeignTravelPreference$Travel_Group, ForeignTravelPreference$Preference, col=2:12, lty=1)
