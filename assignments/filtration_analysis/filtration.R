library(readxl)
require(tidyverse)
library(ggplot2)
library(mosaic)
filtration <- read_excel("C:/Users/aruna/OneDrive - Temple University/STAT 2523/filtration.xlsx", sheet = "Long")
filt_data <- filtration

names(filt_data)<- c("weight", "filtration")
head(filt_data)
filt_data$weight <- factor(filt_data$weight, levels = c("0", "25", "50", "75", "100"))
head(filt_data)

# Distribution of Filtration versus Weight of Deep-Grooved Filter
ggplot(filt_data,aes(x = weight, y = filtration)) + geom_boxplot()

# Summary Statistics by group
require(mosaic)
favstats(filtration ~ weight, data = filt_data)

# Fit the model and ANOVA table
fit_model <- aov(filtration ~ weight, data = filt_data)
anova(fit_model)

# Via Regression
reg_model <- lm(filtration ~ weight, data = filt_data)
summary(reg_model)
anova(reg_model)

# Multiple Comparisons

# Fisher's LSD
require(emmeans)
fit_emm <- emmeans(fit_model, "weight")
fit_emm # A table of unadjusted means and CIs
pairs(fit_emm, adjust = "none") # Fisher's LSD

# Tukey's HSD
 tukey <- TukeyHSD(fit_model)

tukey

# OR

pairs(fit_emm, adjust = "tukey")

# Connecting Letters via the agricolae package

require(agricolae)
HSD.test(fit_model, "weight", console=TRUE)
