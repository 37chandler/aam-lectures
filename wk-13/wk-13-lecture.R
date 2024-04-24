library(tidyverse,warn.conflicts = F)
library(tidymodels,warn.conflicts = F)
library(tidyquant)
library(here)
library(scales)
library(geosphere)

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


# glmnet stuff. Build plots of ridge and lasso and glmnet coefs. 
# Never got this to work. 
data(ames)

center <- c(mean(ames$Longitude), mean(ames$Latitude))

# Calculate the distance from each point to the center
ames$Center_Dist <- distHaversine(p1 = ames[, c("Longitude", "Latitude")], p2 = center)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Center_Dist, data = ames) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_scale(all_numeric_predictors()) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_sqrt(Center_Dist) 
  
pen_val <- 0

ridge_mod <- linear_reg(penalty=pen_val,
                        mixture=0) %>% 
  set_engine("glmnet")

wf <- workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(ridge_mod)


m1 <- fit(wf,data=ames)

# Okay, do this for a lot of pen_vals
lambda_grid <- penalty(c(-4, 3), trans = log10_trans())
alpha_grid <- mixture()

# Create a tuning grid
grid <- grid_regular(
  lambda_grid,
  alpha_grid,
  levels = 20
)

if(exists("results")){
  rm(results)
}

for(i in 1:nrow(grid)){
  this_pen <- grid$penalty[i]
  this_mix <- grid$mixture[i]
  
  glmnet_mod <- linear_reg(penalty=this_pen,
                          mixture=this_mix) %>% 
    set_engine("glmnet") %>% 
    set_mode("regression")
  
  wf <- workflow() %>% 
    add_recipe(ames_rec) %>% 
    add_model(glmnet_mod)
  
  
  m1 <- fit(wf,data=ames)

  holder <- tidy(m1) %>% 
    mutate(mixture = this_mix)
  
  if(!exists("results")){
    results <- holder
  } else {
    results <- results %>% 
      bind_rows(holder)
  }
    
  
}

# ridge = mixture of 0
results %>% 
  filter(mixture==0,term != "(Intercept)") %>% 
  ggplot(aes(x=penalty,y=estimate,group=term)) + 
  geom_line()


# lasso = mixture of 1
results %>% 
  filter(mixture==1,term != "(Intercept)") %>% 
  ggplot(aes(x=penalty,y=estimate,group=term)) + 
  geom_line()
