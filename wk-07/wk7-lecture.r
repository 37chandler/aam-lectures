library(tidyverse,warn.conflicts = F)
library(here)
library(alr4) # for heights data

d.bank <- read_tsv(paste(here(),"data","bank_survey_data.txt",sep="/"))


arm::display(lm(progressivism ~ 1,
                data=d.bank))


arm::display(lm(progressivism ~ as.numeric(main.focal.value=="Environment"),
                data=d.bank))


data(Heights)

ggplot(Heights,
       aes(x=mheight,y=dheight)) + 
  geom_point(position=position_jitter(),alpha=0.5) + 
  stat_smooth(method="lm",se=F) + 
  theme_bw() + 
  labs(x="Mother's Height (in)",
       y="Daughter's Height (in)",
       title="Relationship between Mother and Daughter Heights",
       caption=paste("Correlation is ", 
                     round(cor(Heights$mheight,Heights$dheight),3),".",sep=""))

arm::display(lm(dheight ~ mheight, data=Heights))


ggplot(Heights,
       aes(x=mheight,y=dheight)) + 
  geom_point(position=position_jitter(),alpha=0.5) + 
  geom_abline(slope=0.54,intercept=30,color="blue") + 
  theme_bw() + 
  scale_x_continuous(limits=c(0,75)) + 
  scale_y_continuous(limits=c(0,75)) + 
  labs(x="Mother's Height (in)",
       y="Daughter's Height (in)",
       title="Relationship between Mother and Daughter Heights",
       caption=paste("Correlation is ", 
                     round(cor(Heights$mheight,Heights$dheight),3),".",sep=""))


Heights <- Heights %>% 
  mutate(mheight_norm = mheight - mean(mheight))

arm::display(lm(dheight ~ mheight_norm, data=Heights))

# Fighter pilot: RaOS pg 89
set.seed(20200221)
n <- 1000
true.ability <- rnorm(n,50,10)
noise.1 <- rnorm(n,0,10)
noise.2 <- rnorm(n,0,10)

test.1 <- true.ability + noise.1
test.2 <- true.ability + noise.2

d <- tibble(test_1=test.1,test_2=test.2)

arm::display(lm(test_2 ~ test_1,data=d))

ggplot(d,
       aes(x=test_1,y=test_2)) + 
  geom_point(position=position_jitter(),alpha=0.5) + 
  geom_abline(slope=0.49,intercept=26,color="blue") + 
  theme_bw() + 
  labs(x="Test 1",
       y="Test 2",
       title="Relationship between subsequent tests",
       caption=paste("Correlation is ", 
                     round(cor(d$test_1,d$test_2),3),".",sep=""))


