library(tidyverse,warn.conflicts = F)
library(here)

d <- read_tsv(here("data","satisfaction_survey.txt"))

lm.1 <- lm(satisfaction ~ assigned_sex + tenure + location, data=d)
lm.2 <- lm(satisfaction ~ tenure, data=d)

anova(lm.1,test="Chisq")
anova(lm.2,test="Chisq")

ggplot(d,
       aes(x=assigned_sex,y=satisfaction)) + 
  geom_boxplot()

