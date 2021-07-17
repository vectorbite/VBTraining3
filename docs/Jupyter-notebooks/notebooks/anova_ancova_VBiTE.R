require('dplyr')
require('ggplot2')
require('broom')
require('tidyverse')
require('data.table')
require('car')
require('boot')
require("tidyr")
require('purrr')
require('gplots')
require('repr')

rm(list=ls())
graphics.off()

setwd("~/Desktop")

#read in the trait data

mozwing <- as_tibble(read.csv('traitdata_Huxleyetal_2021.csv',stringsAsFactors = TRUE))
mozwing$temp <- as_factor(mozwing$temp); mozwing$food_level <- as_factor(mozwing$food_level)

str(mozwing); summary(mozwing)

plot(length_mm ~ temp, mozwing)

mozwing$loglength <- log(mozwing$length_mm)

seMean <- function(x){ # get standard error of the mean from a set of values (x)
        x <- na.omit(x) # get rid of missing values
        
        se <- sqrt(var(x)/length(x)) # calculate the standard error
        
        return(se)  # tell the function to return the standard error
}


lengthMeans <- tapply(mozwing$loglength, mozwing$temp, FUN = mean, na.rm = TRUE)

print(lengthMeans)

#∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞
lengthSE <- tapply(mozwing$loglength, mozwing$temp, FUN = seMean)

print(lengthSE)

# get the upper and lower limits of the error bars
upperSE <- lengthMeans + lengthSE
lowerSE <- lengthMeans - lengthSE

# get a barplot
# - this function can report where the middle of the bars are on the x-axis
# - set the y axis limits to contain the error bars

barMids <- barplot(lengthMeans, ylim=c(0, max(upperSE)), ylab = 'ln(wing length, mm)')

# Now use the function to add error bars
# - draws arrows between the points (x0,y0) and (x1,y1)
# - arrow heads at each end (code=3) and at 90 degree angles

arrows(barMids, upperSE, barMids, lowerSE, ang=90, code=3)



# Add all the lines of code from this section into your script. Run it and check you get the graph above.
# Use the second two chunks as a model to plot a similar graph for food level 
# You should get something like the plot below.

#∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞


seMeanfood <- function(x){ # get standard error of the mean from a set of values (x)
         x <- na.omit(x) # get rid of missing values
        
         se <- sqrt(var(x)/length(x)) # calculate the standard error
        
        return(se)  # tell the function to return the standard error
}


lengthMeansfood <- tapply(mozwing$loglength, mozwing$food_level, FUN = mean, na.rm = TRUE)

print(lengthMeansfood)

#∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞
lengthSEfood <- tapply(mozwing$loglength, mozwing$food_level, FUN = seMeanfood)

print(lengthSEfood)

# get the upper and lower limits of the error bars
upperSEfood <- lengthMeansfood + lengthSEfood
lowerSEfood <- lengthMeansfood - lengthSEfood

# get a barplot
# - this function can report where the middle of the bars are on the x-axis
# - set the y axis limits to contain the error bars

barMidsfood <- barplot(lengthMeansfood, ylim=c(0, max(upperSE)), ylab = 'ln(wing length, mm)')

# Now use the function to add error bars
# - draws arrows between the points (x0,y0) and (x1,y1)
# - arrow heads at each end (code=3) and at 90 degree angles

arrows(barMidsfood, upperSEfood, barMidsfood, lowerSEfood, ang=90, code=3)


par(mfrow=c(1,2))
plotmeans(loglength ~ temp, data=mozwing, p=0.95, connect=FALSE)
plotmeans(loglength ~ food_level, data=mozwing, p=0.95, connect=FALSE)

#∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞

lengthLM <- lm(loglength ~ temp, data=mozwing)
summary(lengthLM); anova(lengthLM)

par(mfrow=c(2,2))
plot(lengthLM)

lengthLMfood <- lm(loglength ~ food_level, data=mozwing)
summary(lengthLMfood); anova(lengthLMfood)

par(mfrow=c(2,2))
plot(lengthLMfood)

TukeyLength <- TukeyHSD(aov(lengthLM))
print(TukeyLength)

options(repr.plot.res = 100, repr.plot.width = 10, repr.plot.height = 10)

par(mfrow=c(1,1))
par(las=1, mar=c(5,5,5,5))
# las= 1 turns labels horizontal
# mar makes the left margin wider (bottom, left, top, right)
plot(TukeyLength)

#∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞

TukeyLengthfood <- TukeyHSD(aov(lengthLMfood))
print(TukeyLengthfood)

plot(TukeyLengthfood)

#∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞

factorTable <- table(mozwing$temp, mozwing$food_level)
print(factorTable)

chisq.test(factorTable)

save(mozwing, file='mozwing.Rdata')

