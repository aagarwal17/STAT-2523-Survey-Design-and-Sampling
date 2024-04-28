library(readxl)
wheat <- midterm_scratch_excel <- read_excel("C:/Users/aruna/OneDrive - Temple University/STAT 2523/midterm scratch excel.xlsx", 
                                             sheet = "Sheet2")
head(wheat)
wheat$Algorithm <- factor(wheat$Algorithm)
wheat$Problem <- factor(wheat$Problem)
head(wheat)
library(ggplot2)
ggplot(wheat,aes(x = Algorithm, y = Time)) + geom_boxplot()

ggplot(wheat,aes(x = Problem, y = Time)) + geom_boxplot()

# Fit the model and CRD ANOVA table
fit_model <- aov(Time ~ Algorithm, data = wheat)
anova(fit_model)

# Fit the model and RBD ANOVA table
fit_RBDmodel <- aov(Time ~ Algorithm + Problem, data = wheat)
anova(fit_RBDmodel)

AIC(fit_model)
AIC(fit_RBDmodel)

# Summary Statistics by group
require(mosaic)
favstats(Time ~ Algorithm, data = wheat)
favstats(Time ~ Problem, data = wheat)
# Summary Stats by combination of alg & problem
cell_means <- wheat %>%
  group_by(Algorithm, Problem) %>% #Group by the two factors
  summarise(Means = mean(Time),
            SDs = sd(Time))
cell_means

ggplot(cell_means, aes(x = Algorithm, y = Means, color = Problem,
                       group = Problem, shape = Problem)) +
  geom_point(size = 3) + theme_bw() +
  geom_line() + labs(title =
                       "Profile Plot Algorithm vs. Problem")



# Fisher's LSD
require(emmeans)
fit_emm <- emmeans(fit_RBDmodel, c("Algorithm","Problem"))
fit_emm # A table of unadjusted means and CIs
pairs(fit_emm, adjust = "none") # Fisher's LSD

# Tukey's HSD
tukey <- TukeyHSD(fit_RBDmodel)

tukey

# OR

pairs(fit_emm, adjust = "tukey")

# Connecting Letters via the agricolae package

require(agricolae)
HSD.test(fit_RBDmodel, c("Algorithm","Problem"), console=TRUE)



# Pairwise Comparisons
library(agricolae)

# By material
HSD.test(fit_RBDmodel, c("Algorithm"), console=TRUE)

# By temp
HSD.test(fit_RBDmodel, "Problem", console=TRUE)

# By combination of material:temp
HSD.test(fit_RBDmodel, c("Algorithm", "Problem"), console=TRUE)


HSD.test(fit_model, c("Algorithm", "Problem"), console=TRUE)
HSD.test(fit_model, "Problem", console=TRUE)
HSD.test(fit_model, "Algorithm", console=TRUE)
