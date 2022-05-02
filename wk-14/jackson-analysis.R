library(tidyverse)
library(here)
library(tidymodels)
library(scales)

d <- read_csv(here("data","LogitModelPreds.csv")) %>% 
  janitor::clean_names()

glimpse(d)
skimr::skim(d)
Hmisc::describe(d)

# for classification models, it's handy to have a factor
# version of the outcome
d <- d %>% 
  mutate(result_fct = as_factor(if_else(result==1,"win","loss")))

d %>% 
  group_by(result_fct) %>% 
  summarize(d = mean(deaths),
            k = mean(kills))

d %>% 
  slice_sample(prop=0.05) %>% 
  select(contains("kills")) %>% 
  car::spm(pch=".")

pca1 <- prcomp(
  d %>% 
    select(doublekills:earned_gpm),
  scale.=T
)

set.seed(20220425)

splits <- initial_split(d,prop=4/5,strata=year)

lol.rec <- recipe(result_fct ~ .,data=training(splits)) %>% 
  step_rm(league,
          year,
          split,
          date,
          side,
          playername,
          teamname,
          result,
          predscore,
          pc_apm,
          pca_share,
          impactscore,
          pca15,
          kda) %>% 
  step_normalize(all_numeric(),-all_outcomes()) %>% 
  step_dummy(all_nominal(),-all_outcomes()) %>% 
  prep()

# Let's try glmnet
glm.mod <- logistic_reg(penalty=0.000000001,
                        mixture=0.5) %>% 
  set_engine("glmnet")

logit.mod <- logistic_reg() %>% 
  set_engine("glm")


wf <- workflow() %>% 
  add_recipe(lol.rec.2) %>% 
  add_model(glm.mod)

wf.logit <- wf %>% 
  update_model(logit.mod)


fitted.logit <- wf %>% 
  fit(training(splits))


fit(wf, data=training(splits))

fitted.rf <- wf %>% 
  fit(training(splits))

tidy(fitted.rf) %>% 
  data.frame()

d.test <- testing(splits)

d.test <- d.test %>% 
  mutate(pred_win_logit = predict(fitted.logit,
                            d.test) %>% 
           pull(.pred_class))

metrics(d.test,truth=result_fct,estimate = pred_win) 
metrics(d.test,truth=result_fct,estimate = pred_win_logit) 


conf_mat(d.test,truth=result_fct,estimate=pred_win)
  
# Now let's tune 

doParallel::registerDoParallel(parallel::detectCores(logical=F))

glm.mod <- logistic_reg(penalty=tune(),
                        mixture=tune()) %>% 
  set_engine("glmnet")

wf <- workflow() %>% 
  add_recipe(lol.rec) %>% 
  add_model(glm.mod)


glm.grid <- grid_regular(
  list(penalty = penalty(),
       mixture = mixture()),
  levels=3
)

cv.samples <- vfold_cv(training(splits))

tuned.results <- tune_grid(
  wf,
  resamples=cv.samples,
  grid=glm.grid
)

tuned.results$.metrics[[1]] %>% 
  filter(.metric=="accuracy") %>% 
  arrange(.estimate)

show_best(tuned.results,metric="accuracy")



# Update the formula
lol.rec.2 <- recipe(result ~ .,data=training(splits)) %>% 
  step_rm(league,
          year,
          split,
          date,
          side,
          playername,
          teamname,
          result,
          predscore,
          pc_apm,
          pca_share,
          impactscore,
          pca15,
          kda) %>%  
  step_normalize(all_numeric()) %>% 
  step_pca(doublekills:assistsat15,threshold = 0.8) %>% 
  step_dummy(all_nominal()) %>%  
  prep()


## Random forest
rf <- rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

wf <- workflow() %>% 
  add_recipe(lol.rec.2) %>% 
  add_model(rf)

fitted.rf <- wf %>% 
  fit(training(splits))

d.test <- testing(splits)

d.test <- d.test %>% 
  mutate(pred_win_rf = predict(fitted.rf,
                                  d.test) %>% 
           pull(.pred_class))

metrics(d.test,truth=result_fct,estimate = pred_win_rf)



## XGBoost

xgb <- boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

wf <- workflow() %>% 
  add_recipe(lol.rec) %>% 
  add_model(xgb)

fitted.rf <- wf %>% 
  fit(training(splits))

#tidy(fitted.rf) %>% 
#  data.frame()

d.test <- testing(splits)

d.test <- d.test %>% 
  mutate(pred_win_rf = predict(fitted.rf,
                               d.test) %>% 
           pull(.pred_class))

metrics(d.test,truth=result_fct,estimate = pred_win_rf)
