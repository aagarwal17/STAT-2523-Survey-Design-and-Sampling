library(readxl)
problem_1 <- read_excel("C:/Users/aruna/OneDrive - Temple University/problem 1.xlsx")
head(problem_1)

# Format data
require(tidyverse)
problem_1 <- problem_1 %>% 
  mutate(Soil = as.factor(Soil)) 
problem_1 <- problem_1 %>% 
  mutate(Fertilizer = as.factor(Fertilizer))

head(problem_1)

require(mosaic)
# Summary Stats by Soil
favstats(Yield ~ as.factor(Soil), data = problem_1)
# Boxplots by Soil
ggplot(problem_1, aes(x = Soil, y = Yield)) + geom_boxplot()

# Summary Stats by Fertilizer Type
favstats(Yield ~ Fertilizer, data = problem_1)
# Boxplots by Fertilizer
ggplot(problem_1, aes(x = Fertilizer, y = Yield)) + geom_boxplot()


# Summary Stats by combination of Soil & Fertilizer
cell_means <- problem_1 %>% 
  group_by(Soil, Fertilizer) %>% #Group by the two factors
  summarise(Means = mean(Yield),
            SDs = sd(Yield))
cell_means

# Boxplots by Fertilizer vs Soil
ggplot(problem_1, aes(x = Fertilizer, y = Yield, color = Soil)) + geom_boxplot()

# Profile Plot
ggplot(cell_means, aes(x = Fertilizer, y = Means, color = Soil,
                       group = Soil, shape = Soil)) + 
  geom_point(size = 3) + theme_bw() +
  geom_line() + labs(title = 
                       "Profile Plot Soil vs. Fertilizer")

# Fitting the Two-Way ANOVA with Interaction
fit_model <- aov(Yield ~ Soil*Fertilizer, data = problem_1)
anova(fit_model)


# Fisher's LSD
require(emmeans)
fit_emm <- emmeans(fit_model, c("Soil", "Fertilizer"))
fit_emm # A table of unadjusted means and CIs
pairs(fit_emm, adjust = "none") # Fisher's LSD

# Tukey's HSD
tukey <- TukeyHSD(fit_model)
tukey

# OR

pairs(fit_emm, adjust = "tukey")

require(agricolae)

# Pairwise Comparisons
library(agricolae)

# By Soil
HSD.test(fit_model, c("Soil"), console=TRUE)

# By Fertilizer
HSD.test(fit_model, "Fertilizer", console=TRUE)

# By combination of Soil:Fertilizer
# Connecting Letters via the agricolae package
HSD.test(fit_model, c("Soil", "Fertilizer"), console=TRUE)

