# Load libraries
install.package("MASS")# glm model with negative binomial distribution
library(MASS)

install.packages("basicPlotteR")# alpha colours in plotting
library(basicPlotteR)

install.packages("broom")# simple table from model outputs
library(broom)

install.packages("car") # A Maths package.
library(car)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggpubr")
library(ggpubr)

install.packages("rstatix")
library(rstatix)

install.packages("ggplot")
library(ggplot)

install.packages("ggpubr")
library("ggpubr")

#### Strongyles #####
# Testing for normality. 
# We need to determine if the data is parametic or non-parametic which will allow us to pick the correct means analysis test

# Import the data
trial_data = read.csv("HSI - McM, MF, Manual 26 Feb.csv", head=TRUE, sep=",")

### Normality testing for all datasets. McMaster MiniFLOTAC
# Build the linear model
model1  <- lm(McMaster.Strongyles ~ Mini.FLOTAC.Strongyles, data = trial_data)

# Create a QQ plot of residuals
ggqqplot(residuals(model1))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model1))

# Wilcox test (Optional)
#wilcox.test(trial_data$McMaster.Strongyles, trial_data$Mini.FLOTAC.Strongyles)
#wilcox.test(trial_data$Manual.Strongyles, trial_data$Mini.FLOTAC.Strongyles)
#wilcox.test(trial_data$McMaster.Strongyles, trial_data$Manual.Strongyles)

#Plot data to visualise its distribution
plot(trial_data$McMaster.Strongyles, trial_data$Manual.Strongyles)
plot(trial_data$Manual.Strongyles, trial_data$Mini.FLOTAC.Strongyles)
plot(trial_data$McMaster.Strongyles, trial_data$Manual.Strongyle)


####  Correlation testing using spearman's correlation (datasets are not normally distributed)
cor.test(trial_data$McMaster.Strongyles, trial_data$Manual.Strongyles, method=c("spearman"))
cor.test(trial_data$Manual.Strongyles, trial_data$Mini.FLOTAC.Strongyles, method=c("spearman"))
cor.test(trial_data$McMaster.Strongyles, trial_data$Mini.FLOTAC.Strongyles, method=c("spearman"))

### Visualise all the three devices and how they compare on a a ggplot 
ggplot(trial_data, aes(Manual.Strongyles, Mini.FLOTAC.Strongyles, colour = "Manual V MinifFLOTAC")) +
  geom_point(colour=alpha("black",1)) +
  geom_point(aes(Mini.FLOTAC.Strongyles, McMaster.Strongyles, colour = "MiniFLOTAC V McMaster"))+
  geom_point(aes(Manual.Strongyles, McMaster.Strongyles, colour = "Manual V McMaster")) + 
  ggtitle("Manual V MiniFLOTAC similarity (Black) compared to other devices")


