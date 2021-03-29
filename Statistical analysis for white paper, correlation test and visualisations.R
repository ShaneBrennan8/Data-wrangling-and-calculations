# Load libraries
install.package("MASS")# glm model with negative binomial distribution
library(MASS)

install.packages("basicPlotteR")# alpha colours in plotting
library(basicPlotteR)

install.packages("ggpubr") 
library(ggpubr)


install.packages("ggplot")
library(ggplot)

install.packages("dplyr")
library(dplyr)

install.packages("car")
library(car)


#### Strongyles ####
### Testing for normality. 
# We need to determine if the data is parametric or non-parametric which will allow us to pick the correct means analysis test to use

# Import the data
trial_data = read.csv("HSI - McM, MF, Manual 26 Feb.csv", head=TRUE, sep=",")


### Normaility testing for all datasets. McMaster, Mini-FLOTAC, Manual
# Build the linear model
model1  <- lm(McMaster.Strongyles ~ Mini.FLOTAC.Strongyles, data = trial_data)
model2  <- lm(Manual.Strongyles ~ Mini.FLOTAC.Strongyles, data = trial_data)
model3  <- lm(McMaster.Strongyles ~ Manual.Strongyles, data = trial_data)


# Create a QQ plot of residuals


qqPlot(model1, col = "steelblue", lwd = 0.15)
qqPlot(model2, col = "steelblue", lwd = 0.15)
qqPlot(model3, col = "steelblue", lwd = 0.15)




# Compute Shapiro-Wilk test of normality
# Outputs of >0.05 imply that the distribution of the data is not significantly different from normal distribution. 
shapiro.test(residuals(model1))
shapiro.test(residuals(model2))
shapiro.test(residuals(model3))



# Plot the data to get a sense of the distribution
plot(trial_data$McMaster.Strongyles, trial_data$Mini.FLOTAC.Strongyles)
plot(trial_data$Manual.Strongyles, trial_data$Mini.FLOTAC.Strongyles)
plot(trial_data$McMaster.Strongyles, trial_data$Manual.Strongyle)

####  Correlation testing
# Spearman's correlation testing. 
cor.test(trial_data$McMaster.Strongyles, trial_data$Manual.Strongyles, method=c("spearman"))
cor.test(trial_data$Manual.Strongyles, trial_data$Mini.FLOTAC.Strongyles, method=c("spearman"))
cor.test(trial_data$McMaster.Strongyles, trial_data$Mini.FLOTAC.Strongyles, method=c("spearman"))
Strongyles

# Overlaying plots of all devices to see how closely they align
ggplot(trial_data, aes(Manual.Strongyles, Mini.FLOTAC.Strongyles, colour = "Manual V MinifFLOTAC")) +
  geom_point(colour=alpha("black",1)) +
  geom_point(aes(Mini.FLOTAC.Strongyles, McMaster.Strongyles, colour = "MiniFLOTAC V McMaster"))+
  geom_point(aes(Manual.Strongyles, McMaster.Strongyles, colour = "Manual V McMaster")) + 
  ggtitle("Manual V MiniFLOTAC similarity (Black) compared to other devices")


