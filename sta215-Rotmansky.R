## Project:  STA 215, Fall 2024, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Who:       Zachary D. Kline

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(haven)

# Load data 
library(readr)
raw_data <- read_csv("final.csv.csv")
dataset <- na.omit(raw_data)
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
# summary stats for quant vars

summary(dataset$births_2020)
sd(dataset$births_2020)

summary(dataset$home_2023)
sd(dataset$home_2023)

summary(dataset$Poverty_2020)
sd(dataset$Poverty_2020)


# summary stats for qual vars
table(dataset$vcrime10)

table(dataset$pop_over_500k_2021)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
dataset$lnbirths <- log(dataset$births_2020 + 1)
boxplot(lnbirths ~ pop_over_500k_2021, data = dataset)

anova <- aov(lnbirths ~ pop_over_500k_2021, data = dataset)
summary(anova) 

# r2 =explained/(explained+unexplained)
293.7/(293.7+158.5)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
# log home prices
dataset$lnhome <- log(dataset$home_2023 + 1)
# Create scatter plot of home_2023 vs Poverty_2020
plot(dataset$lnhome, dataset$poverty_2020, 
     xlab = "home_2023", ylab = "poverty_2020 (milliseconds)",
     main = "Scatter Plot:home-2023vs poverty_2020", pch = 19, col = "blue")

# Fit linear model (regression line)
model <- lm(poverty_2020 ~ lnhome, data = dataset)

# Add regression line to the plot
abline(model, col = "red", lwd = 2)

# Show summary of the linear model
summary(model)

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset$lnhome, residuals(model))
                                    

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset$vcrime10, dataset$pop_over_500k_2021)

chisq.test(table(dataset$vcrime10, dataset$pop_over_500k_2021))