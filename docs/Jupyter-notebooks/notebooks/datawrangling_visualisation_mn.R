
require('tidyverse')
setwd("/home/primuser/Documents/VBTraining3/Jupyter-notebooks/")
rm(list=ls())
graphics.off()
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%                  Data exploration with basic plotting 

MyDF <- read.csv('data/traitdata_Huxleyetal_2021.csv',stringsAsFactors = TRUE)

dim(MyDF) #check the size of the data frame you loaded
str(MyDF)
head(MyDF)

require(tidyverse)
dplyr::glimpse(MyDF)

# Let's define temp and food level as factors. We'll need this for later
MyDF$temp            <- as.factor(MyDF$temp) 
MyDF$food_level      <- as.factor(MyDF$food_level) 

str(MyDF)

par(mfrow=c(1,1))

plot(MyDF$length_mm, MyDF$adult_lifespan)

plot(log(MyDF$length_mm),log(MyDF$adult_lifespan))

plot(log10(MyDF$adult_lifespan),log10(MyDF$length_mm))

plot(log10(MyDF$adult_lifespan),log10(MyDF$length_mm), pch = 20)

plot(log10(MyDF$adult_lifespan),log10(MyDF$length_mm), xlab = 'adult lifespan (days)', ylab = 'wing length (mm)', pch = 20)


hist(log10(MyDF$length_mm), xlab = 'wing length (mm)', ylab = "Count")

hist(log10(MyDF$length_mm),xlab="log10(wing length (mm))",ylab="Count", 
     col = "lightblue", border = "pink") # Change bar and borders colors

MyDF_lowfood <-  subset(MyDF, food_level == '0.1')
MyDF_highfood <- subset(MyDF, food_level == '1')

graphics.off()

#%%%%%%%%%%%%%%%%%

par(mfcol=c(2,1)) #initialize multi-paneled plot
par(mfg = c(1,1)) # specify which sub-plot to use first 
hist(log10(MyDF_lowfood$length_mm), # low-histogram
     xlab = "log10(wing length (mm))", ylab = "Count", col = "lightblue", border = "pink", 
     main = '0.1 mg/larva/day') # Add title
par(mfg = c(2,1)) # Second sub-plot
hist(log10(MyDF_highfood$length_mm), xlab="log10(wing length (mm))",
     ylab="Count", col = "lightgreen", border = "pink", main = '1 mg/larva/day')


#%%%%%%%%%%%%%%%%%%%

par(mfrow=c(1,1)) #initialize multi-paneled plot
hist(log10(MyDF_lowfood$length_mm), # high-food histogram
     xlab="log10(wing length (mm))", ylab="Count", 
     col = rgb(1, 0, 0, 0.5), # Note 'rgb', fourth value is transparency
     main = "Larval food-level size overlap") 
hist(log10(MyDF_highfood$length_mm), col = rgb(0, 0, 1, 0.5), add = T) # Plot high food
legend('topleft',c('0.1','1'),   # Add legend
       fill=c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5))) # Define legend colors


#%%%%%%%%%%%%%%%%%%

boxplot(log10(MyDF$length_mm), xlab = "", ylab = "log10(wing length (mm))", main = "Wing length")

boxplot(log(MyDF$length_mm) ~ MyDF$temp+MyDF$food_level, # Why the tilde?
        xlab = "temperature", ylab = "wing length (mm)",
        main = "Wing length by temperature")

#%%%%%%%%%%%%%%%%%%

graphics.off()

par(fig=c(0,0.8,0,0.8)) # specify figure size as proportion
plot(log(MyDF_lowfood$length_mm),log(MyDF_highfood$length_mm), xlab = "wing length (mm) at 0.1 mg", ylab = "wing length (mm) at 1 mg") # Add labels
par(fig=c(0,0.8,0.4,1), new=TRUE)
boxplot(log(MyDF_lowfood$length_mm), horizontal=TRUE, axes=FALSE)
par(fig=c(0.55,1,0,0.8),new=TRUE)
boxplot(log(MyDF_highfood$length_mm), axes=FALSE)
mtext("Fancy scatterplot of wing length by larval food level", side=3, outer=TRUE, line=-3)

par(fig=c(0,0.8,0,0.8))
pdf("larvalfoodwing.pdf", # Open blank pdf page using a relative path
    11.7, 8.3) # These numbers are page dimensions in inches
hist(log(MyDF_lowfood$length_mm), # Plot predator histogram (note 'rgb')
     xlab="wing length (mm)", ylab="Count", col = rgb(1, 0, 0, 0.5), main = "Overlap in wing length by larval food level") 
hist(log(MyDF_highfood$length_mm), # Plot prey weights
     col = rgb(0, 0, 1, 0.5), 
     add = T)  # Add to same plot = TRUE
legend('topleft',c('0.1 mg/larva/day','1 mg/larva/day'), # Add legend
       fill=c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5))) 

graphics.off() # you can also use dev.off()


# Quick plotting with qplot

require(ggplot2)

MyDF <- subset(MyDF, length_mm != 'NA') # omit individuals that did not survive to adulthood (i.e. wing length = NA)

qplot(length_mm, adult_lifespan, data = MyDF)
qplot(log(length_mm), log(adult_lifespan), data = MyDF)
qplot(log(length_mm), log(adult_lifespan), col=food_level, data = MyDF)
qplot(log(length_mm), log(adult_lifespan), col=food_level, data = MyDF,asp = 1)
qplot(log(length_mm), log(adult_lifespan), shape=food_level, data = MyDF,asp = 1)
qplot(log(length_mm), log(adult_lifespan), col=food_level,alpha=I(0.5), data = MyDF,asp = 1)

qplot(temp, log(length_mm), geom = 'boxplot', data = MyDF)

pdf("MyFirst-ggplot2-Figure.pdf")
print(qplot(length_mm, adult_lifespan, data = MyDF,log="xy",
            main = "Relationship between wing length and adult lifespan", 
            xlab = "log(wing length) (mm)", 
            ylab = "log(adult lifespan) (days)") + theme_bw())
dev.off()


# Advanced plotting: ggplot

p <- ggplot(MyDF, aes(x = log(length_mm),
                      y = log(adult_lifespan),
                      colour = food_level))

q <- p + geom_point()
  
q <- p + geom_point(size=I(2), shape=I(10)) +
  theme_bw() + # make the background white
  theme(aspect.ratio=1)

q + theme(legend.position = "none") + theme(aspect.ratio=1)

p <- ggplot(MyDF, aes(x = log(length_mm/adult_lifespan), fill = food_level )) + geom_density()
p

p <- p + geom_density(alpha=0.5)
p
 

p <- p + geom_density()+facet_wrap( .~ food_level)
p

# You can also combine categories like this

options(repr.plot.width=12, repr.plot.height= 14) # Change plot size (in cm)

ggplot(MyDF, aes(x = log(length_mm), y = log(adult_lifespan))) +
geom_point() + facet_wrap( .~ temp + food_level, scales = "free")


# Mathematical display
# Let's try mathematical annotation on a axis, and in the plot area.
# First create some linear regression "data":

x <- seq(0, 100, by = 0.1)
y <- -4. + 0.25 * x + rnorm(length(x), mean = 0., sd = 2.5)

# and put them in a dataframe
my_data <- data.frame(x = x, y = y)

# perform a linear regression
my_lm <- summary(lm(y ~ x, data = my_data))

# plot the data
p <-  ggplot(my_data, aes(x = x, y = y,colour = abs(my_lm$residual))) +
  geom_point() +
  scale_colour_gradient(low = "black", high = "red") +
  theme(legend.position = "none") +
  scale_x_continuous(expression(alpha^2 * pi / beta * sqrt(Theta)))

p

# add the regression line and throw some math on the plot
p <- p + geom_abline(intercept = my_lm$coefficients[1][1],
         slope = my_lm$coefficients[2][1], colour = "red") +
         geom_text(aes(x = 60, y = 0,label = "sqrt(alpha) * 2* pi"), 
         parse = TRUE, size = 6, colour = "blue")

p


# end here .. 

# Restart at 'Readings & Resources'. Include all of this (i.e. Experimental design and Data exploration) 
# then describe the assignment before the decision tree with ...

# "Group assignment

# Organise (or wrangle!) the data, as you learned in the notebook. The dataset
# provided (filename) contains values on trait-temperature relationships. 
# Select a focal species from this dataset. Use your species-specific
# Develop a hypothesis and explore it. 
# Present your group's findings using descriptive statistics and visualizations."


