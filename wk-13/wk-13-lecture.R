library(tidyverse,warn.conflicts = F)
library(tidymodels,warn.conflicts = F)
library(tidyquant)
library(here)
library(scales)

data(mpg)

ggplot(mpg,
       aes(x=displ,y=cty)) + 
  geom_point(position=position_jitter(),alpha=0.5) + 
  theme_minimal() + 
  labs(x="Engine Displacement (l)",
       y="City Miles-per-Gallon") + 
  stat_smooth(method="lm",se=F,col="red")


set.seed(20210403)
ggplot(mpg %>% 
         slice_sample(n=100),
       aes(x=displ,y=cty)) + 
  geom_point(position=position_jitter(),alpha=0.5) + 
  theme_minimal() + 
  labs(x="Engine Displacement (l)",
       y="City Miles-per-Gallon") + 
  geom_ma(ma_fun = SMA,n = 5,lty=1)


set.seed(20210403)
ggplot(mpg %>% 
         slice_sample(n=100),
       aes(x=displ,y=cty)) + 
  geom_point(position=position_jitter(),alpha=0.5) + 
  theme_minimal() + 
  labs(x="Engine Displacement (l)",
       y="City Miles-per-Gallon") + 
  stat_smooth(method="lm",se=F,col="red")
