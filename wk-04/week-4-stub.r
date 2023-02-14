library(tidyverse)
library(rsample)
library(here)

d <- read_tsv(here("data","satisfaction_survey.txt"))
cu <- read_tsv(here("data","bank_survey_data.txt"))


ggplot(cu,aes(x=progressivism)) + 
  geom_density()

cu.lm <- lm(progressivism ~ age, data=cu)
summary(cu.lm)

test.ages <- tibble(age = c(0,20,40,60,80,100))

test.ages <- test.ages %>% 
  mutate(est_prog = predict(cu.lm,newdata=test.ages))

ggplot(cu,
       aes(x=age,y=progressivism)) + 
  geom_point(alpha=0.5) + 
  geom_abline(intercept=coef(cu.lm)[1],
              slope=coef(cu.lm)[2],color="red")





