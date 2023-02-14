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

sat.lm <- lm(satisfaction ~ tenure, data=d)

summary(sat.lm)




d2 <- d

d2 <- d2 %>% 
  mutate(assigned_sexmale = if_else(assigned_sex=="male",1,0),
         assigned_sexneither = if_else(assigned_sex=="neither",1,0),
         assigned_sexfemale = if_else(assigned_sex == "female",1,0),
         location_Seattle = if_else(location=="Seattle",1,0))


summary(lm(satisfaction ~ assigned_sexmale + assigned_sexneither + 
             assigned_sexfemale,
        data=d2))

lm.1 <- lm(satisfaction ~ assigned_sex + location + tenure,
           data=d)

anova(lm.1)

lm.final <- lm(satisfaction ~ assigned_sex + tenure, 
               data=d)

## Code developed on 20220214

n.sim <- 1000
results <- tibble(adj.r.sq = rep(NA,n.sim),
                  male.coef = rep(NA,n.sim))

for(i in 1:n.sim){
  
  # First, get new data set
  new.d <- d %>% 
    slice_sample(n=nrow(d),replace=T)
  
  lm.new <-  lm(satisfaction ~ assigned_sex + tenure, 
                data=new.d)
  
  results$adj.r.sq[i] <- summary(lm.new)$adj.r.squared
  results$male.coef[i] <- summary(lm.new)$coefficients[2,1]
}

ggplot(results,
       aes(x=adj.r.sq)) + 
  geom_density() + 
  geom_vline(xintercept = summary(lm.final)$adj.r.squared,col="cadet blue")


ggplot(results,
       aes(x=male.coef)) + 
  geom_density() + 
  geom_vline(xintercept = summary(lm.final)$coefficients[2,1],col="cadet blue")


quantile(results$male.coef,probs=c(0.025,0.975))


## Code from 2021 AAM class

# adjusted R squared and tenure coefficient
n.sim <- 1500

results <- tibble(adj_r_sq=rep(NA,n.sim),
                  tenure_coef=rep(NA,n.sim))

for(i in 1:n.sim){
  new.d <- d %>% 
    slice_sample(n=nrow(d),replace=T)
  
  lm.new <- lm(satisfaction ~ assigned_sex + tenure, 
               data=new.d)
  
  results$adj_r_sq[i] <- summary(lm.new)$adj.r.squared
  results$tenure_coef[i] <- coef(lm.new)[4]

}

ggplot(results,
       aes(x=adj_r_sq)) + 
  geom_density() + 
  theme_bw() + 
  labs(x="Adjusted R Squared",
       title="Bootstrap Replicates") +
  geom_vline(xintercept = summary(lm.final)$adj.r.squared,color="pink") + 
  geom_vline(xintercept=0)

ggplot(results,
       aes(x=tenure_coef)) + 
  geom_density() + 
  theme_bw() + 
  labs(x="Coefficient of Tenure",
       title="Bootstrap Replicates") +
  geom_vline(xintercept = coef(lm.final)[4],color="pink") + 
  geom_vline(xintercept=0)


results %>% 
  summarize(r2 = mean(adj_r_sq),
            r2_se = sd(adj_r_sq),
            tenure = mean(tenure_coef),
            tenure_se=sd(tenure_coef))

n.sim <- 500

replicates <- bootstraps(d,times=n.sim)
results.2 <- tibble(adj_r_sq=rep(NA,n.sim),
                    tenure_coef=rep(NA,n.sim))

for(i in 1:n.sim){
  
  lm.new <- lm(satisfaction ~ assigned_sex + tenure, 
               data=as.data.frame(replicates$splits[[i]]))
  
  results.2$adj_r_sq[i] <- summary(lm.new)$adj.r.squared
  results.2$tenure_coef[i] <- coef(lm.new)[4]
  
}




