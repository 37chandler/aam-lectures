
# Let's look at a quick example of overfitting

set.seed(20220502)

X <- matrix(rnorm(200*10000),nrow=10000,ncol=200) %>% 
  data.frame()

X <- X %>% 
  as_tibble()

Y <- rpois(10000,lambda=5)

d <- X %>% 
  bind_cols(Y) %>% 
  rename(Y = `...201`)

d1 <- d[1:8000,]
d2 <- d[8001:10000,]

lm.1 <- lm(Y ~ .,data=d1)

results <- tidy(lm.1)

results %>% 
  arrange(p.value) %>% 
  head(n=10)

summary(lm(Y ~ X138,d=d2))


### Analysis of Subaru Data
library(tidyverse)
library(tidymodels)
library(here)

cores <- parallel::detectCores()

d <- read_tsv(here("data","subaru_data.txt")) %>% 
  select(-vin,-post_id, -make,-crumb_subarea,) %>% 
  mutate(odometer = as.numeric(odometer),
         price = str_remove(price,"$"),
         price = as.numeric(price)) %>% 
  filter(!is.na(price)) %>% 
  filter(title_status=="clean")
  

d <- d %>% 
  filter(between(odometer,30000,250000)) %>% 
  filter(year <= 2020)

mod_cyl <- d %>% 
  filter(!is.na(cylinders),!is.na(model)) %>% 
  count(model,cylinders)

d %>% 
  filter(model=="outback",
         cylinders %in% c("6 cylinders","4 cylinders")) %>% 
  ggplot(aes(x=price,color=cylinders,group=cylinders)) +
  geom_density() + 
  theme_minimal() + 
  labs(x="Price",y="",color="Num Cylinders") + 
  scale_x_continuous(label=dollar)

# Going to drop cylinders because of issues with imputation (and time management)

d <- d %>% 
  filter(!is.na(model)) %>% 
  select(-title_status) 

splits <- initial_split(d)
cv_data <- vfold_cv(training(splits))

subaru.rec <- recipe(price ~ model + year + odometer + crumb_area,
                     data=training(splits)) %>% 
  step_other(model,threshold = 0.08) %>% 
  step_log(odometer) %>% 
  step_dummy(all_nominal()) 


subaru.rec %>%
  prep() %>% 
  juice() %>% 
  names()

svm.mod <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab",num.threads=cores) %>% 
  set_mode("regression")

svm_grid <- grid_regular(
  cost(),
  rbf_sigma(),
  levels=5
)

wf <- workflow() %>% 
  add_recipe(subaru.rec) %>% 
  add_model(svm.mod) 

svm_res <- tune_grid(
  wf,
  resamples = cv_data,
  grid = svm_grid
)

svm_res %>% 
  collect_metrics()

best_fit <- svm_res %>% 
  select_best("rsq")

final_svm <- finalize_workflow(wf,best_fit)

fitted.svm <- fit(final_svm,data=training(splits))


knn.mod <- nearest_neighbor(neighbors=11) %>% 
  set_engine("kknn")

xgb.mod <- boost_tree() %>% 
  set_engine("xgboost")


fitted.knn <- wf %>% 
  update_model(knn.mod) %>% 
  fit(data=training(splits))

fitted.xgb <- wf %>% 
  update_model(xgb.mod) %>% 
  fit(data=training(splits))

d.test <- testing(splits)

d.test <- 
  d.test %>% bind_cols(
    predict(fitted.svm,new_data=d.test) 
  ) %>% 
  rename(pred_svm = .pred)

d.test <- 
  d.test %>% bind_cols(
    predict(fitted.knn,new_data=d.test) 
  ) %>% 
  rename(pred_knn = .pred)

d.test <- 
  d.test %>% bind_cols(
    predict(fitted.xgb,new_data=d.test) 
  ) %>% 
  rename(pred_xgb = .pred)




metrics(d.test,truth=price,estimate=pred_svm)
metrics(d.test,truth=price,estimate=pred_knn)
metrics(d.test,truth=price,estimate=pred_xgb)

d.test <- d.test %>% 
  mutate(error = price - pred_xgb)

d.test %>% 
  filter(between(error,-5000,-3000)) %>% 
  arrange(error) %>% 
  data.frame()


d.test %>% 
  filter(between(error,3000,5000)) %>% 
  arrange(error)






