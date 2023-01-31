# Two groups, of 10K each with 500 sales in group 1 (control) and 550 in group
# 2 (test). Three tiers of spending at 5, 10 and 25 monthly?/yearly?

library(tidyverse)

num_per_group <- 10000
sales_in_1 <- 500
lift <- 0.1
sales_tiers <- c(5,10,25)
sales_probs <- c(0.5,0.35,0.15)

sales_in_2 <- round((1+lift)*sales_in_1)

d <- tibble(
  group = c(rep("control",num_per_group),
            rep("test",num_per_group))
) %>% 
  mutate(id = 1:n())

sales_1 <- sample(d$id[d$group=="control"],size=sales_in_1)
sales_2 <- sample(d$id[d$group=="test"],size=sales_in_2)

d <- d %>% 
  mutate(
    is_sale = if_else(id %in% sales_1 | id %in% sales_2,
                      1,
                      0)
  )

d <- d %>% 
  mutate(
    sale_amount = is_sale * 
      sample(sales_tiers,size=nrow(d),replace=TRUE,prob=sales_probs)
  )

# permutation test
n_sim <- 10000

get_diff_sales <- function(data) {
  #test_sales <- sum(data$is_sale[data$group=="test"])
  #control_sales <- sum(data$is_sale[data$group=="control"])

  test_sales <- sum(data$sale_amount[data$group=="test"])
  control_sales <- sum(data$sale_amount[data$group=="control"])
  
  return(test_sales - control_sales)
}

results <- tibble(
  statistic = c(get_diff_sales(d),rep(NA_real_,n_sim))
)

new_d <- d 

for(i in 2:nrow(results)){
  new_d$group <- sample(new_d$group)
  results$statistic[i] <- get_diff_sales(new_d)
}

ggplot(results,
       aes(x=statistic)) + 
  geom_density() + 
  geom_vline(xintercept=get_diff_sales(d),col="cadetblue") + 
  theme_minimal() + 
  labs(y="",x="Test vs Control")

mean(abs(results$statistic) >= abs(get_diff_sales(d)))

# Diff in sales at 10% lift = 0.1214879
# Diff in $$ at 10% lift = 0.112488
