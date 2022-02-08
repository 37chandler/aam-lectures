library(tidyverse)

population <- rt(10000,df=10)

plot(density(population))
s1 <- sample(population,size=8,replace=F)

actual.value <- mean(s1)
results <- tibble(statistic = c(actual.value,rep(NA,999)))

for(i in 2:1000){
  results$statistic[i] <- mean(sample(s1,size=8,replace=T))
}

quantile(results$statistic,c(0.01,0.05,0.5,0.95,0.99))

# > quantile(results$statistic,c(0.01,0.05,0.5,0.95,0.99))
# 1%         5%        50%        95%        99% 
# -0.3847698 -0.2031504  0.2788156  0.8000065  0.9562476 


results.bad <- tibble(statistic = c(actual.value,rep(NA,999)))

for(i in 2:1000){
  results.bad$statistic[i] <- mean(sample(s1,size=300,replace=T))
}

quantile(results$statistic,c(0.01,0.05,0.5,0.95,0.99))
quantile(results.bad$statistic,c(0.01,0.05,0.5,0.95,0.99))

mu <- mean(s1)
sigma <- sd(s1)

theoretical <- tibble(x.val=seq(from=-2,to=2,length.out=200))

theoretical <- theoretical %>% 
  mutate(y.val = dnorm(x.val,mean=mu,sd=sigma))

plot(density(results$statistic),ylim=c(0,10),xlim=c(-2,2))
lines(density(results.bad$statistic),col="red")
lines(x=theoretical$x.val,y=theoretical$y.val,col="blue")
