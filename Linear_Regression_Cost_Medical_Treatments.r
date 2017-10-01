##### Regression Methods -------------------

# Instructor: Ebson David Allende Quintana
# david.allende@outlook.com
# Versión: 1.0

################################################################################

## Example: Predicting Medical Expenses ----

# age: This is an integer indicating the age of the primary beneficiary (excluding
#those above 64 years, since they are generally covered by the government).
# sex: This is the policy holder's gender, either male or female.
# bmi: This is the body mass index (BMI), which provides a sense of how over
#or under-weight a person is relative to their height. BMI is equal to weight (in
#kilograms) divided by height (in meters) squared. An ideal BMI is within the
#range of 18.5 to 24.9.
# children: This is an integer indicating the number of children / dependents
#covered by the insurance plan.
# smoker: This is yes or no depending on whether the insured regularly
#smokes tobacco.
# region: This is the beneficiary's place of residence in the U.S., divided into
#four geographic regions: northeast, southeast, southwest, or northwest.
#charges: Medical expenses for the treatment

## Step 2: Exploring and preparing the data ----
getwd()
setwd('D:/Cursos/18.PEA_ML/0.Data')
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)

# summarize the charges variable
summary(insurance$charges)

#Outliers detection
outlier_values_charges <- 
  boxplot.stats(insurance$charges)$out  # outlier values.

head(outlier_values_charges)

summary(outlier_values_charges)

dim(insurance)
nrow(as.data.frame(outlier_values_charges))

# histogram of insurance charges
hist(insurance$charges)

#Data excluyendo valores atípicos
insurance2<-insurance[insurance$charges<min(outlier_values_charges),]

# histogram of insurance charges
hist(insurance2$charges)

# table of region
table(insurance$region)

# exploring relationships among features: correlation matrix
cor(insurance[c("age", "bmi", "children", "charges")])

# visualing relationships among features: scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "charges")])


# more informative scatterplot matrix
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

library(rattle)
rattle()

## Step 3: Training a model on the data ----
ins_model <- lm(charges ~ age + children + bmi + sex + smoker 
                + region,
                data = insurance)

ins_model <- lm(charges ~ ., data = insurance) # this is equivalent to above

# see the estimated beta coefficients
ins_model

## Step 4: Evaluating model performance ----
# see more detail about the estimated beta coefficients
summary(ins_model)


ins_model <- lm(charges ~ age + children + bmi + smoker 
                + region,
                data = insurance)
summary(ins_model)

ins_model <- lm(charges ~ age + children + bmi + smoker,
                data = insurance)
summary(ins_model)

library(MASS)
step <- stepAIC(ins_model, direction="both")
summary(step)
## Step 5: Improving model performance ----

# add a higher-order "age" term
insurance$age2 <- insurance$age^2

# add an indicator for BMI >= 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# create final model
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)

summary(ins_model2)

score_charges <- predict(ins_model2, insurance)
head(score_charges)

score_charges2 <- predict(ins_model, insurance)


## Step 4: Evaluate model performance ----

# generate predictions for the testing dataset
insurance_score <- predict(ins_model2, insurance)
head(insurance_score)

# compare the distribution of predicted values vs. actual values
summary(insurance_score)
summary(insurance$charges)

# compare the correlation
cor(insurance_score, insurance$charges)

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
MAE(insurance$charges,score_charges)

MAE(insurance$charges,score_charges2)

