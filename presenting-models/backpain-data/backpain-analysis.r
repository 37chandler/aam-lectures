library(tidyverse)
library(scales)
library(here)


# questionnaire within 24 hours of delivery, looking
# at severity of backpain. Look for association between
# severity of backpain and other data. Note that the 

d <- read_tsv(here("presenting-models","backpain.txt"))

# pivot to factor for relieve tablet:walking
# aggrevate is fatigue:walking

Hmisc::describe(d)

lm_1 <- lm(back_pain_severity ~ age_of_patient_years + 
             height_of_patient_metres + 
             weight_of_patient_end_kg + 
             number_of_children_previous_pregnancies +
             factor_tablets,
           data=d)

arm::display(lm_1)
