library(tidyverse)
library(tidymodels)
library(here)

d <- read_csv(paste(here(),"data","Fowler_air_quality_data_2.csv",sep="/"))

## fix a data issue
d <- d %>% 
  mutate(time_of_day = time_of_day %% 24)

## Set levels of our categorical variables
d <- d %>% 
  mutate(altitude = fct_relevel(altitude,
                                c("sea level","Missoula","Lolo Peak","18K feet","30K feet")),
         moisture = fct_relevel(moisture,
                                c("dry","fog","mist","rain")),
         pressure = fct_relevel(pressure,
                                c("low","steady","high")),
         sky_condition = fct_relevel(sky_condition,
                                     "clear","limited","poor"),
         wind_speed = fct_relevel(wind_speed,
                                  c("calm","steady","high","gusty")))


Hmisc::describe(d)
skimr::skim(d)

# Let's look at time of day and see if we can spot any patterns
d %>% 
  group_by(time_of_day,altitude) %>% 
  summarize(aqi=mean(air_quality_index)) %>% 
  ggplot(aes(x=time_of_day,y=aqi,group=altitude,color=altitude)) + 
  geom_line() + 
  labs(x="Time",y="AQI",color="Altitude") + 
  theme_minimal()

d %>% 
  group_by(time_of_day,pressure) %>% 
  summarize(aqi=mean(air_quality_index)) %>% 
  ggplot(aes(x=time_of_day,y=aqi,group=pressure,color=pressure)) + 
  geom_line() + 
  labs(x="Time",y="AQI",color="Pressure") + 
  theme_minimal() 

# Nothing obvious here. Will assume it's not good and then test adding it

###########################################################################
#               Using LM and base R                                       #
###########################################################################

lm.1 <- lm(air_quality_index ~ altitude + moisture + 
             pressure + wind_speed + sky_condition,
           data=d)

lm.1.tod <- lm(air_quality_index ~ altitude + moisture + 
                 pressure + wind_speed + sky_condition + 
                 time_of_day,
               data=d)

anova(lm.1) 
anova(lm.1,lm.1.tod)

arm::display(lm.1)
arm::coefplot(lm.1)

# This basically wraps up the basic model fitting.


###########################################################################
#               Now with TidyModels (Basics)                              #
###########################################################################

aqi.model <- linear_reg() %>% 
  set_engine("lm") 

aqi.fit <- aqi.model %>%  
  fit(air_quality_index ~ altitude + moisture + 
        pressure + wind_speed + sky_condition,
      data=d)

tidy(aqi.fit)

anova(aqi.fit$fit)


###########################################################################
#               Now with TidyModels (Medium)                              #
###########################################################################

set.seed(20210315)

aqi.splits <- initial_split(d)
d.train <- training(aqi.splits)
d.test <- testing(aqi.splits)

aqi.fit <- aqi.model %>%  
  fit(air_quality_index ~ altitude + moisture + 
        pressure + wind_speed + sky_condition,
      data=d.train)

predict(aqi.fit,d.test)

d.test <- d.test %>% 
  mutate(pred_lm = predict(aqi.fit,d.test) %>% 
           pull(.pred)) 

ggplot(d.test,
       aes(x=air_quality_index,y=pred_lm)) + 
  geom_point(alpha=0.05) + 
  geom_abline(slope=1,intercept=0) + 
  theme_minimal()

metrics(d.test,truth=air_quality_index,estimate=pred_lm)


###########################################################################
#               Add on rpart approach                                     #
###########################################################################

library(rpart.plot)

aqi.model.tree <- decision_tree() %>%  
  set_engine("rpart") %>% 
  set_mode("regression")

aqi.fit.tree <- aqi.model.tree %>%  
  fit(air_quality_index ~ altitude + moisture + 
        pressure + wind_speed + sky_condition,
      data=d.train)

# an alternative way to add the column. 
d.test <- d.test %>% 
  bind_cols(predict(aqi.fit.tree,d.test)) %>% 
  rename(pred_tree = .pred)

metrics(d.test,truth=air_quality_index,estimate=pred_tree)

rpart.plot(aqi.fit.tree$fit)


###########################################################################
#               Let's do a random forest                                  #
###########################################################################

aqi.model.tree <- rand_forest() %>%  
  set_engine("ranger") %>% 
  set_mode("regression")

aqi.fit.tree <- aqi.model.tree %>%  
  fit(air_quality_index ~ altitude + moisture + 
        pressure + wind_speed + sky_condition,
      data=d.train)

d.test <- d.test %>% 
  bind_cols(predict(aqi.fit.tree,d.test)) %>% 
  rename(pred_rf = .pred)

metrics(d.test,truth=air_quality_index,estimate=pred_rf)


