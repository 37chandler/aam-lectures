# Build a v0 attribution model for Cesari\Rover.
# 
# 2014-11-11
# 
# The input data is a result of merging visit data with the following
# media data: Daily Deals (summarized as deals per hour, which are spread
# across the day on which they occurred); SEM data (summarized by branded
# and non-branded search impressions and clicks; the hourly data is 
# spread across the hours from the daily data using the overall distribution
# of search impressions and clicks); and TV Data. TV data is summarized at the hourly 
# level by raw_imps, estimated uncovered impressions which give discounted weight
# to local spots, the daypart (chosen to just be the first one that occurred at that
# hour (since some buys have different dayparts, e.g. "MTWTFSS 3A-6A" vs "SS 3A-6A"),
# the number of channels, and the not-trustworthy Kre8 visitor estimates. 
# 
# Goal: build a model that includes TS vars plus marketing vars. Derive estimates
# from that model to inform overall media mix. Then look at residuals to see
# which buys are looking good/bad.


library(Hmisc)
library(data.table)
library(car)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(reshape)
library(arm)

working_dir <- "C:\\Users\\John\\Dropbox\\Consulting\\CesariRover\\Attribution\\V0\\"
input_file <- "20141111_modeling_data_20140615_to_20141106.txt"

md <- data.table(read.delim(paste0(working_dir,input_file)))
md[,date := as.IDate(date)]

md[,hour := factor(hour)]
md[,dow := factor(dow)]
md[,month := factor(month)]

md[,raw_imps_m := raw_imps/1000]
md[,est_unc_imps_m := est_unc_imps/1000]

md$dow <- factor(md$dow,
                 levels=c("Monday",
                          "Tuesday",
                          "Wednesday",
                          "Thursday",
                          "Friday",
                          "Saturday",
                          "Sunday")) # change DOW levels


md$hour <- relevel(md$hour,ref="0")
md$dow <- relevel(md$dow,ref="Monday")

# Zero model: just TS
glm.0 <- glm(visits ~ hour + dow,
             family = poisson,
             data=md)

glm.00 <- glm(visits ~ hour * dow,
              family = poisson,
              data=md)

(glm.0$aic - glm.00$aic)
anova(glm.0,glm.00) # 6872 deviance on 138 DF

# Pretty clear TS effect...

# Adding marketing one at a time.
# GLM DD
glm.dd <- glm(visits ~ hour * dow + deals,
              family = poisson,
              data=md)
anova(glm.00,glm.dd)
# Again super significant: 52759 on a whopping 1 DF

glm.dds <- glm(visits ~ hour * dow + deals + nonbranded_imps,
               family = poisson,
               data=md)
anova(glm.dd,glm.dds)
# Again good, 1032 on 1 DF

glm.1 <- glm(visits ~ hour * dow + deals + nonbranded_imps + raw_imps_m,
             family = poisson,
             data=md)

glm.2 <- glm(visits ~ hour * dow + deals + nonbranded_imps + est_unc_imps_m,
             family = poisson,
             data=md)
# GLM 2 is slightly worse....

anova(glm.dds,glm.1)

# all significant, 767 on 1 DF
anova(glm.1)
summary(glm.1)
exp(coef(glm.1))

exp(coef(glm.1)[31:33])

xx <- coefficients(summary(glm.1))[31:33,]


# up next: 
# Look at residuals some different ways
# Summarize by hour, dow, channel, pre-10/1 and post-10/1 for TV
# look at overall residuals for DD by peak
# SEM: hour, dow, month

# propse attribution for TV


# I've got 5 minutes. Is there a relationship between deals and TV and branded search?
glm.S <- glm(nonbranded_imps ~ hour * dow + deals + est_unc_imps_m,
             family = poisson,
             data=md)
glm.SC <- glm(nonbranded_clicks ~ hour * dow + deals + est_unc_imps_m,
              family = poisson,
              data=md)

# TV shows a negative effect on branded search, a positive effect on unbranded search....





# At this point we need to summarize our findings...

# 1. plot of visits assuming no marketing
# 2. Add TV impressions
# 3. Compare to actuals.
# 4. Add deals...
# 5. Worth testing imps in the previous hour?


md.3 <- merge(md,
              new_grid,
              by=c("hour","dow"),
              all.x=T)


xx <- md[,list(visits = mean(visits)),by=list(month,dow,hour)]
xx$hour <- as.numeric(xx$hour)

ggplot(xx,
       aes(x=hour,y=visits,color=month)) +
  geom_line() +
  facet_wrap(~dow)
