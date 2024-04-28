library(readxl)
wheat <- read_excel("C:/Users/aruna/OneDrive - Temple University/STAT 2523/wheat variety.xlsx", sheet = "long")

head(wheat)
wheat$Variety <- factor(wheat$Variety)
wheat$Location <- factor(wheat$Location)
head(wheat)
library(ggplot2)
ggplot(wheat,aes(x = Variety, y = Yield)) + geom_boxplot()

ggplot(wheat,aes(x = Location, y = Yield)) + geom_boxplot()

# Fit the model and CRD ANOVA table
fit_model <- aov(Yield ~ Variety, data = wheat)
anova(fit_model)

# Fit the model and RBD ANOVA table
fit_RBDmodel <- aov(Yield ~ Variety + Location, data = wheat)
anova(fit_RBDmodel)

AIC(fit_model)
AIC(fit_RBDmodel)
