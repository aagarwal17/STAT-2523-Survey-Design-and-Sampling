library(readxl)

LS_Data <- read_excel("C:/Users/aruna/OneDrive - Temple University/STAT 2523/Latin Square Data.xlsx")
head(LS_Data)

require(tidyverse)

LS_Data <- LS_Data%>%
  mutate(Batch = as.factor(Batch))
LS_Data <- LS_Data%>%
  mutate(Operators = as.factor(Operators))
LS_Data <- LS_Data%>%
  mutate(Treatment = as.factor(Treatment))

head(LS_Data)

#ANOVA with blocking
fit_modelLS <- aov(Measure ~ Treatment + Batch + Operators, data = LS_Data)
anova(fit_modelLS)
