library(tidyverse)
library(skimr)


# Generate survey data
set.seed(20210106)
n.employees <- 300
male.frac <- 0.6
not.mf.frac <- 0.03
sat.sd <- 0.1
sat.vec <- c(0.45,0.5,0.55,NA)
tenure.sat.fact <- 0.02 # 0.1 per 5 years

d <- tibble(assigned_sex=c(rep("male",rpois(1,n.employees*male.frac)),
                     rep("neither",rpois(1,n.employees*not.mf.frac))))

d <- d %>% 
  bind_rows(tibble(assigned_sex=rep("female",n.employees-nrow(d))))

d <- d %>% 
  mutate(assigned_sex_sat = case_when(
    assigned_sex == "neither" ~ sat.vec[1],
    assigned_sex == "female" ~ sat.vec[2],
    assigned_sex == "male" ~ sat.vec[3],
    TRUE ~ sat.vec[4],
  ))


d <- d %>% 
  rowwise() %>% 
  mutate(tenure = round(rgamma(1,2,1),1),
         tenure = if_else(tenure > 6.4,6.4,tenure),
         raw_satisfaction = rnorm(1,assigned_sex_sat + tenure.sat.fact*tenure,sat.sd)) %>% 
  ungroup() %>% 
  mutate(satisfaction = round(6*(raw_satisfaction - min(raw_satisfaction))/(max(raw_satisfaction)-min(raw_satisfaction))+1))

d <- d %>% 
  mutate(tenure_group = case_when(
    tenure < 1 ~ "<1 yr",
    between(tenure,1,2.5) ~ "1 to 2.5 yrs",
    2.5 < tenure ~ ">2.5 yrs",
    TRUE ~ "Oops"
  ))

d$employee_id <- paste0("AQ",sample(1000000:2000000,n.employees,replace=F))

d$location <- sample(c("Seattle","New York"),size=n.employees,replace=T)



skim(d)

write_tsv(d %>% 
            select(employee_id,
                   assigned_sex,
                   location,
                   tenure,
                   satisfaction),
          "satisfaction_survey.txt")

d %>% 
#  group_by(assigned_sex) %>% 
  group_by(location) %>% 
  summarize(m_sat = mean(satisfaction),
            med_sat = median(satisfaction),
            sd_sat = sd(satisfaction),
            n=n())

# Not the right approach, but likely place to start
t.test(d$satisfaction[d$assigned_sex=="male"],
       d$satisfaction[d$assigned_sex=="female"])


get.assigned.sex.diff <- function(x){
  male.score <- mean(x$satisfaction[x$assigned_sex=="male"])
  female.score <- mean(x$satisfaction[x$assigned_sex=="female"])
  return(male.score - female.score)
}

actual.value <- get.assigned.sex.diff(d) 

n.sim <- 9999

results <- tibble(statistic = c(actual.value,
                                rep(NA,n.sim)))
new.d <- d %>% 
  select(assigned_sex,satisfaction)

for(i in 2:nrow(results)){
  new.d$assigned_sex <- sample(new.d$assigned_sex)
  results$statistic[i] <- get.assigned.sex.diff(new.d)
}

ggplot(results,
       aes(x=statistic)) + 
  geom_density() + 
  geom_vline(xintercept=actual.value,color="red") + 
  theme_minimal() + 
  labs(x="Male Mean Minus Female Mean",
       y="")


mean(abs(results$statistic) >= abs(actual.value))

