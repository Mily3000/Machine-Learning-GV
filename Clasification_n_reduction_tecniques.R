
## Curso : Analytics - Clustering

################################################################################

# Unidad 2: Técnicas de clasificación y reducción de variables
# Capacitador : Ebson David Allende Quintana
# email: david.allende@outlook.com
# Script Version : 1.0

################################################################################

##### Clustering with k-means -------------------

## Example: Finding Teen Market Segments ----
## Step 2: Exploring and preparing the data ----
getwd()
setwd("D:/Cursos/18.PEA_ML/0.Data")
teens <- read.csv("Clustering_R/snsdata.csv")
str(teens)
head(teens)

# look at missing data for female variable
table(teens$gender)
dim(teens)
table(teens$gender, useNA = "ifany")

# look at missing data for age variable
summary(teens$age)

# eliminate age outliers
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                     teens$age, NA)

summary(teens$age)

# reassign missing gender values to "unknown"
head(teens$gender)
head(!is.na(teens$gender))
colnames(teens)
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

#head(!is.na(teens$gender))
#head(is.na(teens$gender))

# check our recoding work
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# finding the mean age by cohort
mean(teens$age) # doesn't work
mean(teens$age, na.rm = TRUE) 

# age by cohort
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# calculating the expected age for each person
ave_age <- ave(teens$age, teens$gradyear,
                 FUN = function(x) mean(x, na.rm = TRUE))


teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

# check the summary results to ensure missing values are eliminated
summary(teens$age)

## Step 3: Training a model on the data ----
colnames(teens)
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))
head(interests_z)
teen_clusters <- kmeans(interests_z, 5)

## Step 4: Evaluating model performance ----
# look at the size of the clusters
teen_clusters$size

# look at the cluster centers
teen_clusters$centers

## Step 5: Improving model performance ----
# apply the cluster IDs to the original data frame
colnames(teens)
teens$cluster <- teen_clusters$cluster

# look at the first five records
teens[1:5, c("cluster", "gender", "age", "friends")]

# mean age by cluster
aggregate(data = teens, age ~ cluster, mean)

# proportion of females by cluster
aggregate(data = teens, female ~ cluster, mean)

# mean number of friends by cluster
aggregate(data = teens, friends ~ cluster, mean)