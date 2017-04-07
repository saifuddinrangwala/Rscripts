# Set the working directory
setwd("~/Rscripts/regression")

library(xlsx)
library(car)

# Read the csv
filename = "datafiles/JobRuns-linux-v1.csv"
#filename = file.choose()
jobruns_data <- read.table(filename, header=TRUE, sep=",")
jobruns = jobruns_data
jobruns = subset(jobruns_data, jobruns_data$LINUX_SERVER_COUNT > 1)
#View(jobruns)

# jobruns.X      <- cbind(jobruns$RUN_TIME_MINS, jobruns$SERVER_COUNT, jobruns$HOTFIX_COUNT, jobruns$TOTAL_SIZE_MB)  # for forming vector 
# jobruns.X.dist <- dist(jobruns.X)
# #View(jobruns.X)
# jobruns.hclust <- hclust(jobruns.X.dist, method='average')
# plot(jobruns.hclust, labels=jobruns$RESULT_ID, ylab="Distance")
# jobruns.clustered = data.frame(RESULT_ID=jobruns$RESULT_ID,
#                                RUN_TIME_MINS=jobruns$RUN_TIME_MINS,
#                                SERVER_COUNT=jobruns$SERVER_COUNT,
#                                HOTFIX_COUNT=jobruns$HOTFIX_COUNT,
#                                TOTAL_SIZE_MB=jobruns$TOTAL_SIZE_MB,
#                                CLUSTER=cutree(jobruns.hclust, k=5))
# 
# clus1 = subset(jobruns.clustered, jobruns.clustered$CLUSTER == 1)
# View(clus1)
fit1 <- lm(jobruns$RUN_TIME_MINS ~ jobruns$LINUX_SERVER_COUNT + jobruns$TOTAL_RPM_SIZE, data=jobruns)
summary(fit1)
outlierTest(fit1) # Bonferonni p-value for most extreme obs
#qqPlot(fit1, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit1) # leverage plots
# Evaluate Mulit-Collinearity
vif(fit1) # variance inflation factors 
sqrt(vif(fit1)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
#crPlots(fit1)
# Ceres plots 
#ceresPlots(fit1)
# Test for Autocorrelated Errors
durbinWatsonTest(fit1)
#install.packages('gvlma')
library(gvlma)
gvmodel <- gvlma(fit1) 
summary(gvmodel)

# 
# 
# View(subset(jobruns.clustered, jobruns.clustered$CLUSTER == 2))
# View(subset(jobruns.clustered, jobruns.clustered$CLUSTER == 3))
# View(subset(jobruns.clustered, jobruns.clustered$CLUSTER == 4))
# View(subset(jobruns.clustered, jobruns.clustered$CLUSTER == 5))




