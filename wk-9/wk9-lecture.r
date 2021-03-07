library(tidyverse,warn.conflicts = F)
library(here)

d.bank <- read_tsv(paste(here(),"data","bank_survey_data.txt",sep="/"))

all.idx <- 1:nrow(d.bank)
assess.idx <- sample(all.idx,size=round(0.2*nrow(d.bank),0))
train.idx <- all.idx[!(all.idx %in% assess.idx)]

d.assess <- d.bank[assess.idx,]
d.train <- d.bank[train.idx,]

lm.prog <- lm(progressivism ~ age + gender + 
                region + public.sector + 
                pub.greater.priv + 
                engagement + 
                localism + 
                teachers.underpaid,
                data=d.train)

anova(lm.prog,test="Chisq")
arm::display(lm.prog)

d.assess <- d.assess %>% 
  mutate(prog_est = predict(lm.prog,newdata=d.assess,type = "response"))

median(abs(d.assess$progressivism - d.assess$prog_est))
mean(abs(d.assess$progressivism - d.assess$prog_est))

ggplot(d.assess,
       aes(x=prog_est,y=progressivism)) + 
  geom_point(alpha=0.33) + 
  theme_minimal() + 
  labs(x="Estimated Progressivism",y="Actual Progressivism") + 
  geom_abline(slope=1,intercept=0,col="gray50")
