###########################################
###########################################
########28APR2020 by Melanie K. Taylor#####
###########################################

#install and load packages
#install.packages("rFIA")
#install.packages("dplyr")
#install.packages("parallel")

library(rFIA, dplyr)
library(parallel)
detectCores(logical=FALSE)#This command will detect available cores on machine.

#get FIA data from all states east of Mississippi river
#This took about 4 hours at my home. 
getFIA(state=c('FL', 'GA', 'AL', 'MS','TN', 'NC',
               'SC', 'VA','WV','KY','IL','IN','WI',
              'MI','MD','OH', 'PA', 'DE', 'NJ','NY',
              'RI', 'CT','MA', 'VT', 'NH', 'ME'),
              dir= 'C:/share/data',
              common=TRUE, nCores=1 )

##The readFIA command exhausted my memory availability, and kept
#returning errors. 
# Detect and alter R's memory allocation with memory.limit,
#with value dependent on machine.
memory.limit()
#memory.limit(size=500000)
sessionInfo()

#readFIA to load data
eastUSA<-readFIA(dir= 'C:/share/data', common=TRUE, nCores=1)


## Let's use the treatment code in the COND
## table here though
eastUSA$COND <- eastUSA$COND %>%
  ## Here we add a new column to the COND table
  ## The column takes a value of 1 if harvesting
  ## occurred during the remeasurement interval
  ## and 0 otherwise
  mutate(harvest = case_when(TRTCD1 == 10 ~ 1,
                             TRTCD2 == 10 ~ 1,
                             TRTCD3 == 10 ~ 1,
                             TRUE ~ 0))


#In eastern US, 55,159 plots where harvesting was recorded
#compared to 803,166 where it was not. Roughly 7% of plots
#had harvesting recorded.

summary(as.factor(eastUSA$COND$harvest))

55159/803166

## Still, we can use these data to test for
## differences among populations exposed to harvesting
## and those not - let's try mortality and recruitment
## rates in terms of TPA

## Returns plot-level estimates of mortality, recruitment
## and harvest rates for all forestland in RI
plteast <- growMort(eastUSA, grpBy = harvest, byPlot = TRUE)

## Any significant differences -- assuming independence
## Assumption may not hold at species-level, I can help
## with that though
summary(aov(plt$MORT_TPA ~ plt$harvest))
summary(aov(plt$RECR_TPA ~ plt$harvest))

