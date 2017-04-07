require(labelled)
setwd("~/Rscripts/mannova")

wolves <- read_sav("datafiles/wolves.sav")

Deps = cbind(wolves$x1, wolves$x2, wolves$x3, wolves$x4, wolves$x5, wolves$x6, wolves$x7, wolves$x8, wolves$x9)

wolves$sex = to_factor(wolves$sex)
wolves$location = to_factor(wolves$location)

fit <- manova(Deps ~ wolves$sex * wolves$location, wolves)

summary(fit, test="Pillai")
summary(fit, test="Wilks")
summary(fit, test="Hotelling-Lawley")
summary(fit, test="Roy")