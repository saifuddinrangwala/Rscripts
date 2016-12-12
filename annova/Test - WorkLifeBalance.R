require(haven)
require(labelled)
require(ggplot2)
require(psych)
require(data.table)

setwd("~/Rscripts/annova")

WorkLifeBalance <- read_sav("datafiles/WorkLifeBalance.sav")

# convert list to data table
WorkLifeBalance.table <- data.table(WorkLifeBalance)

# Identity the factor and convert the data type to be a factor, else aov won't give correct results.
WorkLifeBalance$Organization = to_factor(WorkLifeBalance$Organization)


# Plot the boxplot by individual factor
boxplot(WorkLifeBalance$part1 ~ WorkLifeBalance$Organization)
boxplot(WorkLifeBalance$part2 ~ WorkLifeBalance$Organization)
boxplot(WorkLifeBalance$part3 ~ WorkLifeBalance$Organization)
boxplot(WorkLifeBalance$part4 ~ WorkLifeBalance$Organization)
boxplot(WorkLifeBalance$part5 ~ WorkLifeBalance$Organization)
boxplot(WorkLifeBalance$part6 ~ WorkLifeBalance$Organization)


# Super imposed denisty plot by factor
qplot(WorkLifeBalance$part1, colour=WorkLifeBalance$Organization, data=WorkLifeBalance, geom="density")
qplot(WorkLifeBalance$part2, colour=WorkLifeBalance$Organization, data=WorkLifeBalance, geom="density")
qplot(WorkLifeBalance$part3, colour=WorkLifeBalance$Organization, data=WorkLifeBalance, geom="density")
qplot(WorkLifeBalance$part4, colour=WorkLifeBalance$Organization, data=WorkLifeBalance, geom="density")
qplot(WorkLifeBalance$part5, colour=WorkLifeBalance$Organization, data=WorkLifeBalance, geom="density")
qplot(WorkLifeBalance$part6, colour=WorkLifeBalance$Organization, data=WorkLifeBalance, geom="density")



# part1 
WorkLifeBalance_lm.part1 <- lm(WorkLifeBalance$part1 ~ WorkLifeBalance$Organization, data = WorkLifeBalance)
WorkLifeBalance_lm.part1_summary = summary(WorkLifeBalance_lm.part1)
WorkLifeBalance_lm.part1_summary
if (WorkLifeBalance_lm.part1_summary$r.squared < 0.5)
{
  print("R- squared value is very less, which means using analysis of variance will not help here")
}

if (pf(WorkLifeBalance_lm.part1_summary$fstatistic[1], WorkLifeBalance_lm.part1_summary$fstatistic[2], WorkLifeBalance_lm.part1_summary$fstatistic[3], lower.tail = FALSE) < 0.05)
{
  print("Significant difference in part1 across Organizations")
}

WorkLifeBalance_aov.part1 <- aov(WorkLifeBalance_lm.part1)
summary(WorkLifeBalance_aov.part1)
# Now run tukey's range test to analyze variations in means across individual groups
tk.part1 <- TukeyHSD(WorkLifeBalance_aov.part1)

# List the tukey's table
tk.part1
# Plot to see variations in the means for each group pair
plot(tk.part1)


# part2 
WorkLifeBalance_lm.part2 <- lm(WorkLifeBalance$part2 ~ WorkLifeBalance$Organization, data = WorkLifeBalance)
WorkLifeBalance_lm.part2_summary = summary(WorkLifeBalance_lm.part2)
WorkLifeBalance_lm.part2_summary
if (WorkLifeBalance_lm.part2_summary$r.squared < 0.5)
{
  print("R- squared value is very less, which means using analysis of variance will not help here")
}
if (pf(WorkLifeBalance_lm.part2_summary$fstatistic[1], WorkLifeBalance_lm.part2_summary$fstatistic[2], WorkLifeBalance_lm.part2_summary$fstatistic[3], lower.tail = FALSE) < 0.05)
{
  print("Significant difference in part2 across Organizations")
}

WorkLifeBalance_aov.part2 <- aov(WorkLifeBalance_lm.part2)
WorkLifeBalance_aov.part2 <- aov(WorkLifeBalance_lm.part2)
summary(WorkLifeBalance_aov.part2)
# Now run tukey's range test to analyze variations in means across individual groups
tk.part2 <- TukeyHSD(WorkLifeBalance_aov.part2)

# List the tukey's table
tk.part2
# Plot to see variations in the means for each group pair
plot(tk.part2)



# part3 
WorkLifeBalance_lm.part3 <- lm(WorkLifeBalance$part3 ~ WorkLifeBalance$Organization, data = WorkLifeBalance)
WorkLifeBalance_lm.part3_summary = summary(WorkLifeBalance_lm.part3)
WorkLifeBalance_lm.part3_summary
if (WorkLifeBalance_lm.part3_summary$r.squared < 0.5)
{
  print("R- squared value is very less, which means using analysis of variance will not help here")
}
if (pf(WorkLifeBalance_lm.part3_summary$fstatistic[1], WorkLifeBalance_lm.part3_summary$fstatistic[2], WorkLifeBalance_lm.part3_summary$fstatistic[3], lower.tail = FALSE) < 0.05)
{
  print("Significant difference in part3 across Organizations")
}

WorkLifeBalance_aov.part3 <- aov(WorkLifeBalance_lm.part3)
WorkLifeBalance_aov.part3 <- aov(WorkLifeBalance_lm.part3)
summary(WorkLifeBalance_aov.part3)
# Now run tukey's range test to analyze variations in means across individual groups
tk.part3 <- TukeyHSD(WorkLifeBalance_aov.part3)

# List the tukey's table
tk.part3
# Plot to see variations in the means for each group pair
plot(tk.part3)


# part4 
WorkLifeBalance_lm.part4 <- lm(WorkLifeBalance$part4 ~ WorkLifeBalance$Organization, data = WorkLifeBalance)
WorkLifeBalance_lm.part4_summary = summary(WorkLifeBalance_lm.part4)
WorkLifeBalance_lm.part4_summary
if (WorkLifeBalance_lm.part4_summary$r.squared < 0.5)
{
  print("R- squared value is very less, which means using analysis of variance will not help here")
}
if (pf(WorkLifeBalance_lm.part4_summary$fstatistic[1], WorkLifeBalance_lm.part4_summary$fstatistic[2], WorkLifeBalance_lm.part4_summary$fstatistic[3], lower.tail = FALSE) < 0.05)
{
  print("Significant difference in part4 across Organizations")
}

WorkLifeBalance_aov.part4 <- aov(WorkLifeBalance_lm.part4)
WorkLifeBalance_aov.part4 <- aov(WorkLifeBalance_lm.part4)
summary(WorkLifeBalance_aov.part4)
# Now run tukey's range test to analyze variations in means across individual groups
tk.part4 <- TukeyHSD(WorkLifeBalance_aov.part4)

# List the tukey's table
tk.part4
# Plot to see variations in the means for each group pair
plot(tk.part4)




# part5 
WorkLifeBalance_lm.part5 <- lm(WorkLifeBalance$part5 ~ WorkLifeBalance$Organization, data = WorkLifeBalance)
WorkLifeBalance_lm.part5_summary = summary(WorkLifeBalance_lm.part5)
WorkLifeBalance_lm.part5_summary
if (WorkLifeBalance_lm.part5_summary$r.squared < 0.5)
{
  print("R- squared value is very less, which means using analysis of variance will not help here")
}
if (pf(WorkLifeBalance_lm.part5_summary$fstatistic[1], WorkLifeBalance_lm.part5_summary$fstatistic[2], WorkLifeBalance_lm.part5_summary$fstatistic[3], lower.tail = FALSE) < 0.05)
{
  print("Significant difference in part5 across Organizations")
}

WorkLifeBalance_aov.part5 <- aov(WorkLifeBalance_lm.part5)
WorkLifeBalance_aov.part5 <- aov(WorkLifeBalance_lm.part5)
summary(WorkLifeBalance_aov.part5)
# Now run tukey's range test to analyze variations in means across individual groups
tk.part5 <- TukeyHSD(WorkLifeBalance_aov.part5)

# List the tukey's table
tk.part5
# Plot to see variations in the means for each group pair
plot(tk.part5)



# part6 
WorkLifeBalance_lm.part6 <- lm(WorkLifeBalance$part6 ~ WorkLifeBalance$Organization, data = WorkLifeBalance)
WorkLifeBalance_lm.part6_summary = summary(WorkLifeBalance_lm.part6)
WorkLifeBalance_lm.part6_summary
if (WorkLifeBalance_lm.part6_summary$r.squared < 0.5)
{
  print("R- squared value is very less, which means using analysis of variance will not help here")
}
if (pf(WorkLifeBalance_lm.part6_summary$fstatistic[1], WorkLifeBalance_lm.part6_summary$fstatistic[2], WorkLifeBalance_lm.part6_summary$fstatistic[3], lower.tail = FALSE) < 0.05)
{
  print("Significant difference in part6 across Organizations")
}

WorkLifeBalance_aov.part6 <- aov(WorkLifeBalance_lm.part6)
WorkLifeBalance_aov.part6 <- aov(WorkLifeBalance_lm.part6)
summary(WorkLifeBalance_aov.part6)
# Now run tukey's range test to analyze variations in means across individual groups
tk.part6 <- TukeyHSD(WorkLifeBalance_aov.part6)

# List the tukey's table
tk.part6
# Plot to see variations in the means for each group pair
plot(tk.part6)

