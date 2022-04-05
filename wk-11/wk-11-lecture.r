library(tidyverse)
library(tidymodels)
library(here)
library(DataExplorer)
library(lubridate)


# Needed the below to figure out some issues with the subaru data
# d.full <- read_tsv(paste(here(),"wk-11","full_subaru.csv",sep="/"))
# 
# d.full %>% 
#   filter(year(processed_time) == 2018,
#          month(processed_time) %in% c(11,12)) %>% 
#   mutate(y = year(processed_time),
#          m = month(processed_time),
#          d = day(processed_time)) %>% 
#   group_by(y,m,d) %>% 
#   summarize(n=n(),
#             missing = sum(is.na(make))) %>% 
#   ungroup() %>% 
#   mutate(missing_frac = missing/n,
#          row = 1:n()) %>%
#   ggplot(aes(x=row,y=missing_frac)) + 
#   geom_line()

d <- read_tsv(paste(here("data","subaru_data.txt")))

d <- d %>% 
  mutate(age = if_else(year < 2020, 2019 - year, NA_real_))

skimr::skim(d)

introduce(d)

p <- plot_missing(d)
ggsave(p,file=paste("wk-11","missing_rows.png",sep="/"),dpi=320)

plot_histogram(d)

plot_histogram(d %>% filter(year < 2021))

plot_correlation(d)

p <- 
  ggplot(d,
         aes(x=age)) + 
    geom_density() + 
    theme_minimal() + 
    labs(x="Car Age")

ggsave(p,file=paste("wk-11","age_dist.png",sep="/"),dpi=320)


d <- d %>% 
  mutate(age_norm = age - mean(age,na.rm=T))

d %>% 
  select(age,age_norm)

subaru.recipe <- d %>%  
  recipe() %>% 
  step_center(age) %>% 
  prep()

d %>% 
  select(age,age_norm) %>% 
  bind_cols(juice(subaru.recipe) %>% 
              select(age) %>% 
              rename(prepped_age = age))


subaru.recipe <- d %>%  
  recipe() %>% 
  step_range(age) %>%  
  prep()

d %>% 
  select(age) %>% 
  mutate(name="age") %>% 
  rename(value=age) %>% 
  bind_rows(juice(subaru.recipe) %>% 
              select(age) %>% 
              mutate(name="prepped_age") %>% 
              rename(value = age)) %>% 
  slice_sample(prop=0.05) %>% 
  ggplot(aes(x=value)) + 
  facet_wrap(~name,scales="free",ncol=1) + 
  geom_density()



