library(readxl)
reaction <- read_excel("C:/Users/aruna/OneDrive - Temple University/STAT 2523/reaction time BIDB.xlsx")

require(tidyverse)

reaction <- reaction %>%
  mutate(cat = as.factor(cat))
reaction <- reaction %>%
  mutate(block = as.factor(block))

head(reaction)

# ANOVA with blocking
fit_modelBIBD <- aov(time ~ cat + block, data = reaction)
anova(fit_modelBIBD)

# ANOVA without blocking
fit_model <- aov(time ~ cat, data = reaction)
anova(fit_model)

AIC(fit_modelBIBD)
AIC(fit_model)

