# Use groundhog data and weather info to see if one groundhog outperforms
# another. 

library(bigrquery)
library(tidyverse)
library(lubridate)
library(here)

phil <- read_csv(here("wk-03","phil_data.txt"))
chuck <- read_csv(here("wk-03","chuck_data.txt"))

phil_latlong <- c(40.9437, -78.9709)
chuck_latlong <- c(40.5890, -74.1481)

# I spent WAY too much time trying to get accurate
# weather data. Going to fake it further down.

con <- dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  billing = "umt-msba"
)

tbl_name <- "bigquery-public-data.ghcn_d.ghcnd_stations"
stations <- tbl(con,tbl_name)

# Let's get the stations closest to the varmints
station_d <- stations %>% 
  mutate(id = as.character(id)) %>% 
  collect()

station_d %>% 
  mutate(dist = abs(latitude - phil_latlong[1]) + 
           abs(longitude - phil_latlong[2])) %>% 
  arrange(dist) %>% 
  head() %>% 
  data.frame() %>% pull(id) -> x

station_d %>% 
  mutate(dist = abs(latitude - chuck_latlong[1]) + 
           abs(longitude - chuck_latlong[2])) %>% 
  arrange(dist) %>% 
  head() %>% 
  data.frame() 


# Punx: US1PAJF0002
# Chuck: US1NYRC0016

stations <- c("US1PAJF0002","US1NYRC0016")
years <- 2023:1995

if(exists("weather_d")){
  rm(weather_d)
}

for(yr in years) {
  tbl_name <- paste0("bigquery-public-data.ghcn_d.ghcnd_",yr)
  weather <- tbl(con,tbl_name)

  if(!exists("weather_d")){
    weather_d <- weather %>%
      filter(id %in% stations) %>% 
      collect()
  } else {
    weather_d <- weather_d %>% 
      bind_rows(weather %>%
                  filter(id %in% stations) %>% 
                  collect())
  }
  
    
}

weather_d %>% 
  mutate(yr = year(date)) %>% 
  count(yr,id)

# Phil has an 60% success rate, Chuck's is 82 allegedly. There are typically
# between 22 and 36 days between equinox and easter
set.seed(20230205)

phil <- phil %>% 
  mutate(correct = rbinom(nrow(phil),size=1,p=0.6)) 

chuck <- chuck %>% 
  mutate(correct = rbinom(nrow(chuck),size=1,p=0.82))

d <- phil %>% 
  mutate(groundhog = "phil") %>% 
  bind_rows(
    chuck %>% 
      mutate(groundhog = "chuck")
  )

d %>% 
  group_by(groundhog) %>% 
  summarize(m = mean(correct))

