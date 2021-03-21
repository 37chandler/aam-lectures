library(tidyverse)
library(rsample)

d <- read_tsv("satisfaction_survey.txt")

lm.1 <- lm(satisfaction ~ assigned_sex + location + tenure,
           data=d)

lm.final <- lm(satisfaction ~ assigned_sex + tenure, 
               data=d)

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




