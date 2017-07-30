library(dplyr)
library(tidyr)
library(ggplot2)

titanic <- titanic3
summary(titanic)

# Convert empty string embarked rows into NA
titanic$embarked[titanic$embarked == ""] <- NA

# Identify rows where embarked is NA
embarked_missingvalues <- which(is.na(titanic$embarked))

# Convert missing rows into S
titanic$embarked[embarked_missingvalues] <- "S"

# Identify missing values in Age column
age_missingvalues <- which(is.na(titanic$age))

# Find mean of age column`s non-missing values
age_mean <- mean(titanic$age,na.rm = TRUE)

# Use mean of age to populate missing values
titanic$age[age_missingvalues] <- age_mean

# Plot a histogram of original dataset`s age column to think about the other ways to populate missing values
hist(titanic3$age)



