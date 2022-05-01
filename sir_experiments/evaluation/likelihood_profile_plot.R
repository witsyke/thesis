setwd("<<PATH TO THIS DIRECTORY>>")
load("../results_calibration/_2020-09-14_2021-01-04_profile_final.Rdata")


p2 <- p1

p2@profile$lbeta$par.vals <- plogis(p2@profile$lbeta$par.vals)
p2@profile$lgamma$par.vals <- plogis(p2@profile$lgamma$par.vals)

plot(p2, cex=2)

confint(p2, level = 0.99)
