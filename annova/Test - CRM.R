require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

crm <- read_sav("datafiles/CRM.sav")

# convert list to data table
crm.table <- data.table(crm)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
crm$City = to_factor(crm$City)
crm$Type = to_factor(crm$Type)

# Plot the boxplot for groups by channel no
boxplot(crm$Ratings ~ crm$City)
boxplot(crm$Ratings ~ crm$Type)

# Super imposed denisty plot by factor
qplot(crm$Ratings, colour=crm$City, data=crm, geom="density")
qplot(crm$Ratings, colour=crm$Type, data=crm, geom="density")

# Get the descritives table by each group
#describeBy(ban, bank$BankType)

# Get the annova table
crm.aov <- aov(crm$Ratings ~ crm$City + crm$Type, data = crm)
summary(crm.aov)

# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(crm.aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

# Get the means plot
plot.design(crm)

# Get the interaction plot
interaction.plot(crm$City, crm$Type, crm$Ratings, col=2:7, lty=1)
