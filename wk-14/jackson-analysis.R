library(tidyverse)
library(here)
library(tidymodels)
library(scales)

d <- read_csv(here("data","LogitModelPreds.csv")) %>% 
  janitor::clean_names()

glimpse(d)
skimr::skim(d)
Hmisc::describe(d)

splits <- initial_split(d,prop=4/5,strata=year)

lol.rec <- recipe(result ~ .,data=training(splits)) %>% 
  step_rm(league,year,split,date,side,playername,teamname) %>% 
  step_dummy(all_nominal()) %>% 
  step_pca(doublekills:assistsat15,num_comp=5) %>% 
  prep()

#rf.grid <- 

rf <- 
  rand_forest() %>% 
  set_engine("ranger")

wf <- workflow() %>% 
  add_recipe(lol.rec) %>% 
  add_model(rf)

fitted.rf <- wf %>% 
  fit(training(splits))

d.test <- testing(splits)

d.test <- d.test %>% 
  mutate(pred_win = predict(fitted.rf,
                            d.test) %>% 
           pull(.pred))

metrics(d.test,truth=result,estimate = pred_win)  
  
plot(d.test,truth=result,estimate = pred_win)  




