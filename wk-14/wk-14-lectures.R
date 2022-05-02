library(tidyverse)
library(tidymodels)
library(here)
library(lubridate)
library(themis)
library(baguette)
library(rpart)
library(rpart.plot)

d <- read_tsv(here("data","bank_survey_data.txt")) 

d <- d %>% 
  janitor::clean_names() %>% 
  mutate(high_engagement = as.factor(if_else(engagement=="Highly Engaged","Yes","No")),
         enviro_focal_val = as.factor(if_else(main_focal_value=="Environment","Yes","No")))

he.tree <- rpart(high_engagement ~ .,
                 data=d %>% select(-id,-engagement,-account_age))

rpart.plot(he.tree)

enviro.tree <- rpart(enviro_focal_val ~ .,
                     data=d %>% select(-id,-main_focal_value),
                     control=list(cp=0.00605))

enviro.tree.2 <- rpart.plot(enviro.tree,snip = T)

rpart.plot(enviro.tree.2$obj)

enviro.tree.3 <- rpart.plot(enviro.tree.2$obj,snip = T)

rpart.plot(enviro.tree.3$obj)

enviro.tree.4 <- rpart.plot(enviro.tree.3$obj,snip=T)

rpart.plot(enviro.tree.4$obj)


d %>% 
  count(main_focal_value) %>% 
  mutate(main_focal_value = fct_reorder(main_focal_value,n)) %>% 
  ggplot(aes(x=n,y=main_focal_value)) + 
  geom_point() + 
  theme_minimal() + 
  labs(x="Number of Respondents",y="",
       title="Main Focal Values")

ggplot(d,
       aes(x=progressivism,group=enviro_focal_val,fill=enviro_focal_val)) + 
  geom_density(alpha=0.5) + 
  theme_minimal() + 
  labs(fill="Enviro Focal Value")

for.plot <- d %>% 
  filter(gender %in% c("male","female")) %>% 
  count(main_focal_value,gender) 

for.plot <- for.plot %>% 
  left_join(d %>% 
              count(gender) %>% 
              rename(total=n),
            by="gender")

for.plot <- for.plot %>% 
  mutate(fraction = n/total)


for.plot %>% 
  mutate(main_focal_value = fct_reorder(main_focal_value,fraction,sum)) %>% 
  ggplot(aes(x=fraction,y=main_focal_value,color=gender)) + 
  geom_point() + 
  theme_minimal() + 
  labs(x="Fraction of Gender Choosing Value",y="",
       title="Main Focal Values",
       color="Gender") + 
  scale_x_continuous(label=percent)

ggplot(d,
       aes(x=account_age,group=enviro_focal_val,fill=enviro_focal_val)) + 
  geom_density(alpha=0.5) + 
  theme_minimal() + 
  labs(fill="Enviro Focal Value")

ggplot(d,
       aes(x=in_group,group=enviro_focal_val,fill=enviro_focal_val)) + 
  geom_density(alpha=0.5) + 
  theme_minimal() + 
  labs(fill="Enviro Focal Value")


ggplot(d,
       aes(x=sustainability,group=enviro_focal_val,fill=enviro_focal_val)) + 
  geom_density(alpha=0.5) + 
  theme_minimal() + 
  labs(fill="Enviro Focal Value")
