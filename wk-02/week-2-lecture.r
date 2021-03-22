
library(bigrquery)
library(tidyverse)
library(skimr)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "noaa_gsod",
  billing = "umt-msba"
)


wd <- con %>% 
  tbl("gsod*") %>% 
  filter(stn=="726580") %>% 
  select(year,mo,da,temp,max,min,wdsp,prcp,rain_drizzle,snow_ice_pellets) %>% 
  collect()

write_tsv(wd,"msp_weather_data.txt")

# How often do we have 90 degree maxes, by year
wd %>% 
  filter(year < 2021) %>% 
  group_by(year) %>% 
  summarize(hit_90s = sum(max >= 90)) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year)) %>%  
  ggplot(aes(x=year,y=hit_90s)) + 
  geom_point() + 
  stat_smooth() + 
  labs(x="Year",y="Number of Days in Year Hitting 90") + 
  theme_minimal() 

# Are the number of 90 degree days on the rise? 

d <- read_tsv("survey_data.txt")

set.seed(20210125)

d.s <- d %>% slice_sample(n=6) %>% 
  select(id, age, gender, 
         region, engagement, progressivism,
         main.focal.value)

sqrt(1/6*5/6/6)

5/6 + 2 * sqrt(1/6*5/6/6)
5/6 - 2 * sqrt(1/6*5/6/6)



sample.size <- 6
n.sim <- 1000
results <- tibble(portion=rep(NA,n.sim))
gender.col <- d.s$gender

for(i in 1:n.sim){
  this.sample <- sample(gender.col,size=sample.size,replace=TRUE)
  results$portion[i] <- mean(this.sample=="female")
}

ggplot(results,
       aes(x=portion)) + 
  geom_density() + 
  theme_bw() + 
  labs(x="Portion of Females") + 
  geom_vline(xintercept=5/6,col="red")

quantile(results$portion,probs=c(0.025,0.975))
  

results <- tibble(statistic=rep(NA,n.sim))
sample.col <- d.s$age

for(i in 1:n.sim){
  sample <- sample(sample.col,size=sample.size,replace=TRUE)
  results$statistic[i] <- mean(sample)
}

ggplot(results,
       aes(x=statistic)) + 
  geom_histogram(bins=60) + 
  theme_bw() + 
  labs(x="Mean Age of Six Person Sample",y="Count of Samples")

quantile(results$portion,probs=c(0.025,0.975))

results <- results %>% 
  mutate(shifted_stat = statistic + 37 - mean(d.s$age))

ggplot(results,
       aes(x=shifted_stat)) + 
  geom_histogram(bins=60) + 
  theme_bw() + 
  labs(x="Mean Age of Six Person Sample",y="Count of Samples") + 
  geom_vline(xintercept=mean(sample.col),col="red")

mean(results$statistic + (37 - mean(sample.col)) > mean(sample.col))

