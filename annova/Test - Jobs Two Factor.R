require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

Jobs <- read_sav("datafiles/Jobs.sav")

# convert list to data table
Jobs.table <- data.table(Jobs)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
Jobs$Gender = to_factor(Jobs$Gender)
Jobs$EducationLevel = to_factor(Jobs$EducationLevel)


# Plot the boxplot by individual factor
boxplot(Jobs$NoofJobs ~ Jobs$Gender)
boxplot(Jobs$NoofJobs ~ Jobs$EducationLevel)

# Super imposed denisty plot by factor
qplot(Jobs$NoofJobs, colour=Jobs$Gender, data=Jobs, geom="density")

# Get the descritives table by each group
describeBy(Jobs$NoofJobs, Jobs$Gender)
describeBy(Jobs$NoofJobs, Jobs$EducationLevel)

# Get the annova table
Jobs_lm <- lm(Jobs$NoofJobs ~ Jobs$Gender + Jobs$EducationLevel + Jobs$Gender:Jobs$EducationLevel, data = Jobs)
Jobs_lm_summary = summary(Jobs_lm)
if (Jobs_lm_summary$r.squared < 0.5)
{
  print("Very Imp: R- squared value is very less, which means using analysis of variance will not help here")
}

Jobs_aov <- aov(Jobs_lm)
summary(Jobs_aov)
Jobs_residual = residuals(Jobs_aov)
Jobs_fitted = fitted(Jobs_aov)

# Get the residual vs fitted plot i.e. check how close are observed values to fitted values
# http://blog.minitab.com/blog/adventures-in-statistics/regression-analysis-how-do-i-interpret-r-squared-and-assess-the-goodness-of-fit
plot(Jobs_fitted, Jobs_residual)
abline(h=0, lty=2)
lines(smooth.spline(Jobs_fitted, Jobs_residual))

#Box plot across multiple factors
boxplot(Jobs$NoofJobs ~ Jobs$Gender * Jobs$EducationLevel, data = Jobs)
boxplot(Jobs_residual ~ Jobs$Gender * Jobs$EducationLevel, data = Jobs)


# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(Jobs_aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

# Get the means plot
plot.design(Jobs)

# Get the interaction plot
interaction.plot(Jobs$EducationLevel, Jobs$Gender, Jobs$NoofJobs, col=2:12, lty=1)
interaction.plot(Jobs$Gender, Jobs$EducationLevel, Jobs$NoofJobs, col=2:12, lty=1)
