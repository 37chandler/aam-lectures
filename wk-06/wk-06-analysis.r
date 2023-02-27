library(tidyverse)
library(here)

d <- read_tsv(here("wk-06","fake_traffic_accident_data.txt"))

lm_1 <- lm(accidents_per_10K_1 ~ weekday + weather + 
             temperature + pedestrian_level,data=d)

summary(lm_1)
anova(lm_1)

