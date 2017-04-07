
# Set the working directory
setwd("~/Rscripts/regression")

library(xlsx)
library(MASS)
library(car)

# Read the csv
filename = "datafiles/JobRuns-v2.csv"
#filename = file.choose()
jobruns_data <- read.table(filename, header=TRUE, sep=",")

# Subset data to get a good fit
jobruns = subset(jobruns_data, jobruns_data$RUN_TIME_MINS > 60 & jobruns_data$RUN_TIME_MINS < 3600 & jobruns_data$HOTFIX_COUNT < 40 & jobruns_data$HOTFIX_COUNT > 2 & jobruns_data$SERVER_COUNT > 4 & jobruns_data$TOTAL_SIZE_MB>1048576)
#jobruns = jobruns_data

View(jobruns)

# Multiple Linear Regression Example 
fit <- lm(jobruns$RUN_TIME_MINS ~ jobruns$HOTFIX_COUNT + jobruns$TOTAL_SIZE_MB + jobruns$SERVER_COUNT, data=jobruns)
summary(fit)

# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(jobruns)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

# Evaluate Mulit-Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?


# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)
# Ceres plots 
ceresPlots(fit)

# Test for Autocorrelated Errors
durbinWatsonTest(fit)

#install.packages('gvlma')

library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)



