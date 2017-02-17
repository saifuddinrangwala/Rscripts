require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

RecoveryTime <- read_sav("datafiles/Recovery Time.sav")

# convert list to data table
RecoveryTime.table <- data.table(RecoveryTime)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
RecoveryTime$Type = to_factor(RecoveryTime$Type)
RecoveryTime$Physical_condition = to_factor(RecoveryTime$Physical_condition)


# Plot the boxplot by individual factor
boxplot(RecoveryTime$No_of_Days ~ RecoveryTime$Type)
boxplot(RecoveryTime$No_of_Days ~ RecoveryTime$Physical_condition)

# Super imposed denisty plot by factor
qplot(RecoveryTime$No_of_Days, colour=RecoveryTime$Type, data=RecoveryTime, geom="density")

# Get the descritives table by each group
describeBy(RecoveryTime$No_of_Days, RecoveryTime$Type)
describeBy(RecoveryTime$No_of_Days, RecoveryTime$Physical_condition)

# Get the annova table
RecoveryTime_lm <- lm(RecoveryTime$No_of_Days ~ RecoveryTime$Type + RecoveryTime$Physical_condition + RecoveryTime$Type:RecoveryTime$Physical_condition, data = RecoveryTime)
RecoveryTime_lm_summary = summary(RecoveryTime_lm)
RecoveryTime_lm_summary
if (RecoveryTime_lm_summary$r.squared < 0.5)
{
  print("R- squared value is very less, which means using analysis of variance will not help here")
}

RecoveryTime_aov <- aov(RecoveryTime_lm)
summary(RecoveryTime_aov)
RecoveryTime_residual = residuals(RecoveryTime_aov)
RecoveryTime_fitted = fitted(RecoveryTime_aov)

# Get the residual vs fitted plot i.e. check how close are observed values to fitted values
# http://blog.minitab.com/blog/adventures-in-statistics/regression-analysis-how-do-i-interpret-r-squared-and-assess-the-goodness-of-fit
plot(RecoveryTime_fitted, RecoveryTime_residual)
abline(h=0, lty=2)
lines(smooth.spline(RecoveryTime_fitted, RecoveryTime_residual))

#Box plot across multiple factors
boxplot(RecoveryTime$No_of_Days ~ RecoveryTime$Type * RecoveryTime$Physical_condition, data = RecoveryTime)
boxplot(RecoveryTime_residual ~ RecoveryTime$Type * RecoveryTime$Physical_condition, data = RecoveryTime)


# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(RecoveryTime_aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

# Get the means plot
plot.design(RecoveryTime)

# Get the interaction plot
interaction.plot(RecoveryTime$Physical_condition, RecoveryTime$Type, RecoveryTime$No_of_Days, col=2:12, lty=1)
interaction.plot(RecoveryTime$Type, RecoveryTime$Physical_condition, RecoveryTime$No_of_Days, col=2:12, lty=1)
