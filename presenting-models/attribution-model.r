library(arm)
library(tidyverse)
library(here)
library(janitor)

d <- read_tsv(here("presenting-models","modeling-data.txt")) %>% 
  clean_names()

d <- d %>% 
  mutate(
    dow = fct_relevel(dow,
                      c("Monday","Tuesday","Wednesday",
                        "Thursday","Friday","Saturday",
                        "Sunday"))
  )

# v is new site visits in that hour on that day;
# h is hour of the day;
# d is day of the week;
# DD is the number of daily deals sold/claimed in that hour on that day;
# 𝐼_𝑆𝑒𝑎𝑟𝑐ℎ is the non-brand search impressions in that hour on that day;
# 𝐼_𝑇𝑉 is the TV impressions on that day, ignoring cover-up. 


lm_1 <- lm(visits ~ hour * dow + deals + nonbranded_imps + raw_imps,
             data=d)

summary(lm_1)

# good day of predictions: 2014-09-04
for_plot <- d %>% 
  filter(date=="2014-09-04") %>% 
  select(hour,visits) %>% 
  mutate(type="Actuals")

new_data <- d %>% 
  filter(date=="2014-09-04")

for_plot <- for_plot %>% 
  bind_rows(tibble(
    hour = new_data$hour,
    visits = predict(lm_1,newdata=new_data),
    type="Prediction"
  ))

ggplot(for_plot, 
       aes(x=hour,y=visits,group=type,color=type)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Hour of the Day",
       y="Visits",
       title="September 4, a good day of predictions",
       color="Visit Source")

# and a bad one
for_plot <- d %>% 
  filter(date=="2014-07-06") %>% 
  select(hour,visits) %>% 
  mutate(type="Actuals")

new_data <- d %>% 
  filter(date=="2014-07-06")

for_plot <- for_plot %>% 
  bind_rows(tibble(
    hour = new_data$hour,
    visits = predict(lm_1,newdata=new_data),
    type="Prediction"
  ))

ggplot(for_plot, 
       aes(x=hour,y=visits,group=type,color=type)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Hour of the Day",
       y="Visits",
       title="July 6, a bad day of predictions")

# Sunday of July 4 weekend

d <- d %>% 
  mutate(
    hours_from_noon = abs(hour-12)
  )

lm_2 <- lm(visits ~ hours_from_noon * dow + deals + I(nonbranded_imps/100) + I(raw_imps/1000),
           data=d)

# does Dow matter with deals?
lm_3 <- lm(visits ~ hours_from_noon * dow + dow*deals + I(nonbranded_imps/100) + I(raw_imps/1000),
           data=d)

Hmisc::describe(d %>% select(hours_from_noon,dow,deals,nonbranded_imps,raw_imps))

anova(lm_2,lm_3)

display(lm_3)

summary(lm_2)
display(lm_2)

# good day of predictions: 2014-09-04
for_plot <- d %>% 
  filter(date=="2014-09-04") %>% 
  select(hour,visits) %>% 
  mutate(type="Actuals")

new_data <- d %>% 
  filter(date=="2014-09-04")

for_plot <- for_plot %>% 
  bind_rows(tibble(
    hour = new_data$hour,
    visits = predict(lm_3,newdata=new_data),
    type="Prediction"
  ))

ggplot(for_plot, 
       aes(x=hour,y=visits,group=type,color=type)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Hour of the Day",
       y="Visits",
       title="September 4, a good day of predictions",
       color="Visit Source")

# and a bad one
for_plot <- d %>% 
  filter(date=="2014-07-06") %>% 
  select(hour,visits) %>% 
  mutate(type="Actuals")

new_data <- d %>% 
  filter(date=="2014-07-06")

for_plot <- for_plot %>% 
  bind_rows(tibble(
    hour = new_data$hour,
    visits = predict(lm_3,newdata=new_data),
    type="Prediction"
  ))

ggplot(for_plot, 
       aes(x=hour,y=visits,group=type,color=type)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Hour of the Day",
       y="Visits",
       title="July 6, a bad day of predictions",
       color="Visit Source")


new_data <- d %>% 
  select(dow,hour,hours_from_noon) %>% 
  unique() %>% 
  mutate(
    deals=0,
    nonbranded_imps = mean(d$nonbranded_imps,na.rm=T),
    raw_imps = mean(d$raw_imps,na.rm=T)
  )

new_data <- new_data %>% 
  mutate(
    expected_visits = predict(lm_3,new_data)
  )

ggplot(new_data,
       aes(x=hour,y=expected_visits)) + 
  facet_wrap(~dow) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Hour of the Day",
       y="Expected Visits",
       title="Oh crap, I forgot to book a dog sitter!",
       subtitle="Visits Peak on Wednesday")


new_data <- expand_grid(
  dow = unique(d$dow),
  deals=0:4) %>% 
  mutate(
    hours_from_noon = 0,
    nonbranded_imps = mean(d$nonbranded_imps,na.rm=T),
    raw_imps = mean(d$raw_imps,na.rm=T)
  )

new_data <- new_data %>% 
  mutate(
    expected_visits = predict(lm_3,new_data),
    exp_visit_se = predict(lm_3,new_data,se.fit=T)$se.fit
  )

ggplot(new_data,
       aes(x=deals,y=expected_visits)) + 
  facet_wrap(~dow) + 
  geom_line() + 
  theme_minimal() + 
  labs(x="Number of Redeemed Deals per Hour",
       y="Expected Visits",
       title="Friday and Saturday are the sweet spot for Deals")

new_data %>% 
  filter(deals %in% c(0,4)) %>% 
  select(dow,deals,expected_visits) %>% 
  pivot_wider(names_from=deals,
              values_from=expected_visits) %>% 
  mutate(diff = `4` - `0`) %>% 
  arrange(diff)

# percentiles

average_data <- data.frame(
  hours_from_noon = 3,
  dow = 'Monday',  
  deals = median(d$deals, na.rm = TRUE),
  nonbranded_imps = mean(d$nonbranded_imps, na.rm = TRUE),
  raw_imps = mean(d$raw_imps, na.rm = TRUE)
)

percentile_25_data <- data.frame(
  hours_from_noon = 9,
  dow = 'Friday',
  deals = quantile(d$deals, 0.25, na.rm = TRUE),
  nonbranded_imps = quantile(d$nonbranded_imps, 0.25, na.rm = TRUE),
  raw_imps = quantile(d$raw_imps, 0.25, na.rm = TRUE)
)

percentile_75_data <- data.frame(
  hours_from_noon = 0,
  dow = 'Wednesday',
  deals = quantile(d$deals, 0.75, na.rm = TRUE),
  nonbranded_imps = quantile(d$nonbranded_imps, 0.75, na.rm = TRUE),
  raw_imps = quantile(d$raw_imps, 0.75, na.rm = TRUE)
)

# Predict visits for the average case
update_results <- function(results,avg_data, var_name, new_value, percentile_label) {
  avg_data[,var_name] <- new_value
  predicted_visits <- predict(lm_3, newdata = avg_data)
  results <- results %>% 
    bind_rows(data.frame(Variable = var_name, Percentile = percentile_label, Visits = predicted_visits))
  
  return(results)
}

average_visits <- predict(lm_3, newdata = average_data)

results_table <- tibble(Variable = "Average Values", Percentile = "50%", Visits = average_visits)

# Update for dow at 25th percentile (Monday to Wednesday)
results_table <- update_results(results_table,
                                average_data,
                                "dow", 
                                "Wednesday", 
                                "75%")

# Update for dow at 25th percentile (Monday to Wednesday)
results_table <- update_results(results_table,
                                average_data,
                                "dow", 
                                "Friday", 
                                "25%")

results_table <- update_results(results_table, average_data, "hours_from_noon", percentile_25_data$hours_from_noon, "25%")
results_table <- update_results(results_table, average_data, "hours_from_noon", percentile_75_data$hours_from_noon, "75%")

results_table <- update_results(results_table, average_data, "deals", percentile_25_data$deals, "25%")
results_table <- update_results(results_table, average_data, "deals", percentile_75_data$deals, "75%")

results_table <- update_results(results_table, average_data, "nonbranded_imps", percentile_25_data$nonbranded_imps, "25%")
results_table <- update_results(results_table, average_data, "nonbranded_imps", percentile_75_data$nonbranded_imps, "75%")

results_table <- update_results(results_table, average_data, "raw_imps",  percentile_25_data$raw_imps, "25%")
results_table <- update_results(results_table, average_data, "raw_imps", percentile_75_data$raw_imps, "75%")

spread_data <- results_table %>%
  filter(Variable != "Average Values") %>% 
  group_by(Variable) %>%
  summarise(Spread = abs(diff(Visits)),
            xmin=min(Visits),
            xmax=max(Visits)) %>%
  arrange(desc(Spread))

results_table <- results_table %>%
  mutate(Variable = factor(Variable, levels = rev(c("Average Values", spread_data$Variable))))


ggplot(results_table, aes(x = Visits, y = Variable)) +
  geom_segment(data=spread_data,
               aes(x=xmin,y=Variable,xend=xmax,yend=Variable),
               color="gray") + 
  geom_point(size=2,color="black") +
  labs(title = "Impact of Variable Changes on Predicted Visits",
       x = "Predicted Visits",
       y = "") +
  theme_minimal() +
  geom_label(x=250, y=4.7, label="9 hours from noon") +
  geom_label(x=470, y=4.7, label="Noon") +
  geom_label(x=330, y=3.7, label="Friday") +
  geom_label(x=410, y=3.7, label="Wednesday") + 
  geom_label(x=353, y=2.7, label="25%") +
  geom_label(x=417, y=2.7, label="75%")

