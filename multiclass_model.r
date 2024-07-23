library(dplyr)
library(tidyr)
library(nnet)
library(corrr)
library(survival) 

## load data
training <- read.csv('cirrhosis_train.csv')
test <- read.csv('cirrhosis_test.csv')


# explore data set
# checking for any NAs
has_na_rows <- any(!complete.cases(training))

if (has_na_rows) {
  cat("The data frame has NA rows.\n")
} else {
  cat("The data frame does not have any NA rows.\n")
}

summary(training)
glimpse(training)

hist(training$N_Days)
hist(training$Stage)

# checking occurence of 'Placebo' value in Drug column 
value_checked <- "Placebo"
occurrence <- 100 * prop.table(table(training$Drug))[value_checked]
print(occurrence)

# change string values to be represented numerically
training <- training %>%
  mutate(Drug = ifelse(Drug == "D-penicillamine", 1, ifelse(Drug == "Placebo", 2, Drug)))
training$Drug <- as.integer(training$Drug)


training <- select(training,c('Age','N_Days',
                              'Drug','Stage','Status'))

# ------------------------------------------------------------------------------
# change column Drug to fit numeric & select columns
test <- test %>%
  mutate(Drug = ifelse(Drug == "D-penicillamine", 1, ifelse(Drug == "Placebo", 2, Drug)))
test$Drug <- as.integer(test$Drug)

test <- select(test,c('Age','N_Days',
                      'Drug','Stage'))

# ------------------------------------------------------------------------------

# cox proportional hazards regression model approach - using 'survival' package
# explore model

# should this be done prior to field selection? 
mod.cir <- coxph(Surv(N_Days,Stage) ~Age + Drug + Status, data = training)

# shows that the null hypothesis was not rejected, need to re-evaluate
summary(mod.cir)


# refrences 
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://socialsciences.mcmaster.ca/jfox/Books/Companion/appendices/Appendix-Cox-Regression.pdf

