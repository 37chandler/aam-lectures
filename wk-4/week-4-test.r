library(tidyverse)
library(rsample)

d <- read_tsv("satisfaction_survey.txt")

d$tenure[1] <- 20
d$satisfaction[1] <- 10

lm.1 <- lm(satisfaction ~ assigned_sex + location + tenure,
           data=d)

lm.2 <- lm(satisfaction ~ assigned_sex + tenure,
           data=d)

anova(lm.2,lm.1)

summary(lm.2)

summary(lm.2)$adj.r.squared


# let's do bootstrap estimate of R^2 and tenure coef
n.sim <- 1000
results <- tibble(adj_r_squared=rep(0.0,n.sim),
                  tenure_coef=0.0)

for(i in 2:n.sim){
  new.d <- d %>% 
    slice_sample(n=nrow(d),replace=T)
  
  this.lm <- lm(satisfaction ~ assigned_sex + tenure,
                data=new.d)
  
  results$adj_r_squared[i] <- summary(this.lm)$adj.r.squared
  results$tenure_coef[i] <- coef(this.lm)[4]
  
}

apply(results,2,mean)
apply(results,2,sd)

ggplot(results,
       aes(x=adj_r_squared)) + 
  geom_density() + 
  theme_bw() + 
  labs(x="Adjusted R Squared") + 
  geom_vline(xintercept=summary(lm.2)$adj.r.squared,col="red")

ggplot(results,
       aes(x=tenure_coef)) + 
  geom_density() + 
  theme_bw() + 
  labs(x="Tenure Coef") + 
  geom_vline(xintercept=coef(lm.2)[4],col="red")

# Let's try it using rsample
results.2 <- tibble(statistic=rep(0.0,n.sim))

summary(lm.2)$adj.r.squared

replicates <- bootstraps(d,times=n.sim)

for(i in 1:length(replicates$splits)){
  new.d <- as.data.frame(replicates$splits[[i]])
  
  this.lm <- lm(satisfaction ~ assigned_sex + tenure,
                data=new.d)
  
  results.2$statistic[i] <- summary(this.lm)$adj.r.squared
}

ggplot(results.2,
       aes(x=statistic)) + 
  geom_density() + 
  theme_bw() + 
  labs(x="Adjusted R Squared") + 
  geom_vline(xintercept=results$statistic[1],col="red")
