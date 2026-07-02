# Two-Factor ANOVA

library(readxl)
bat_dat <- read_excel("C:/Users/aruna/OneDrive - Temple University/STAT 2523/Battery Design Example_V2 (1).xlsx", sheet = "Long Format", range = "A1:C37")
head(bat_dat)

# Format data
require(tidyverse)
bat_dat <- bat_dat %>%
  mutate(material = as.factor(material))
bat_dat <- bat_dat %>%
  mutate(temp = as.factor(temp))

head(bat_dat)
library(mosaic)
require(mosaic)
# Summary Stats by material
favstats(life ~ as.factor(material), data = bat_dat)
# Boxplots by material
ggplot(bat_dat, aes(x = material, y = life)) + geom_boxplot()

# Summary Stats by temp Type
favstats(life ~ temp, data = bat_dat)
# Boxplots by temp
ggplot(bat_dat, aes(x = temp, y = life)) + geom_boxplot()


# Summary Stats by combination of material & temp
cell_means <- bat_dat %>%
  group_by(material, temp) %>% #Group by the two factors
  summarise(Means = mean(life),
            SDs = sd(life))
cell_means

# Boxplots by temp vs material
ggplot(bat_dat, aes(x = temp, y = life, color = material)) + geom_boxplot()

# Profile Plot
ggplot(cell_means, aes(x = temp, y = Means, color = material,
                       group = material, shape = material)) +
                       geom_point(size = 3) + theme_bw() +
                       geom_line() + labs(title =
                       "Profile Plot Material vs. Temperature")

# Fitting the Two-Way ANOVA with Interaction
fit_model <- aov(life ~ material*temp, data = bat_dat)
anova(fit_model)

# Pairwise Comparisons
library(agricolae)

# By material
HSD.test(fit_model, c("material"), console=TRUE)

# By temp
HSD.test(fit_model, "temp", console=TRUE)

# By combination of material:temp
HSD.test(fit_model, c("material", "temp"), console=TRUE)

