####################################
## Chapter 10: ANOVA   
## Rat Poison Example 
####################################

#enter data
times <- c(62, 60, 63, 59, 63, 67, 71, 64, 65, 66, 68, 
           66, 71, 67, 68, 68, 56, 62, 60, 61, 63, 64, 63, 59)
diets <- c(rep(1, 4), rep(2, 6), rep(3, 6), rep(4, 8))

#make diets an explicit factor
dietsf <- factor(diets)



#Run ANOVA: use aov
mod <- aov(times ~ dietsf)
summary(mod)


# Check assumptions

#dotplot and boxplot
library(lattice)
dotplot(times ~ dietsf, ylab = "Times", xlab = "Treatments", main = "Dotplot of Raw Data")
boxplot(times ~ diets, ylab = "Times", xlab = "Treatments", main = "Boxplots of Raw Data")


#residuals vs fitted plot
plot(residuals(mod) ~ fitted(mod), ylab = "Residuals", xlab = "Fitted Values", main = "Residuals vs Fitted Values")





