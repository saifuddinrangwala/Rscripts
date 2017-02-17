library(labelled)
library(readxl)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

software_system <- read_excel("datafiles/Software System.xlsx")

# convert list to data table
software_system <- as.data.frame(software_system)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
software_system$`System Software` = to_factor(software_system$`System Software`)

# Plot the boxplot for groups by channel no
boxplot(software_system$`Training Time` ~ software_system$`System Software`)

# Super imposed denisty plot by factor
qplot(software_system$`Training Time`, colour=software_system$`System Software`, data=software_system, geom="density")

# Get the descritives table by each group
describeBy(software_system$`Training Time`, software_system$`System Software`)

# Get the annova table
software_system.aov <- aov(software_system$`Training Time` ~ software_system$`System Software`, data = software_system)
summary(software_system.aov)

# Now run tukey's range test to analyze variations in means across individual groups
tk <- TukeyHSD(software_system.aov)

# List the tukey's table
tk

# Plot to see variations in the means for each group pair
plot(tk)

