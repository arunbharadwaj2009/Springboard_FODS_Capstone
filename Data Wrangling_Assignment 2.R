library(dplyr)
library(tidyr)
library(ggplot2)

titanic3 <- read.csv("titanic3.csv")
titanic <- titanic3
summary(titanic)

# Convert empty string embarked rows into NA
titanic$embarked[titanic$embarked == ""] <- NA

# Identify rows where embarked is NA
embarked_missingvalues <- which(is.na(titanic$embarked))

# Convert missing rows into S
titanic$embarked[embarked_missingvalues] <- "S"

# Identify missing values in Age column
age_missingvalues <- sum(is.na(titanic$age))


# Find mean of age column`s non-missing values
age_mean <- mean(titanic$age,na.rm = TRUE)

# Use mean of age to populate missing values
titanic$age[age_missingvalues] <- age_mean

# { Following lines of code try to find other ways to populate age column (OWTPAC) with missing values

# Plot a histogram of original dataset`s age column to think about the other ways to populate missing values
Hist_OWTPAC <- hist(titanic3$age)

# Summarise the mean of original dataset`s age column based on sex to see if there are major differences between the means of male and female passengers
Summarise_sex_OWTPAC <- titanic3 %>% group_by(sex) %>% summarise(a = mean(age,na.rm=TRUE))
# Grouping dataset based on sex and finding means shows females have mean age 28.68 and males have mean age 30.585. Since this is not a big difference, we can discard this line of thought

# Summarise the mean of original dataset`s age column based on pclass to see if there are major differences between the means of 1,2 and 3 pclass
Summarise_pclass_OWTPAC <- titanic3 %>% group_by(ï..pclass) %>% summarise(b = mean(age,na.rm=TRUE))

# Grouping dataset based on pclass and finding means shows that pclass = 1 have mean of 39.15, pclass =2 have mean 29.5 and pclass = 3 have mean 24.8

countNA_pclass <- titanic3  %>% group_by(ï..pclass) %>% summarise((c=sum(is.na(age)))) 

countNA_pclass

# Since the original dataset is already grouped by pclass, we try to find the number of NA`s in age column for each pclass (1,2,3)

countNA_pclass1 <- sum(is.na(titanic3$age[1:323]))

countNA_pclass2 <- sum(is.na(titanic3$age[324:600]))

countNA_pclass3 <- sum(is.na(titanic3$age[601:1309]))

# After calculating number of NA`s in respective pclass, we can find the sum product of number of NA`s in each pclass and mean age of each pclass. If this sum product is further divided by the total number of NA`s, the result turns out to be 27.22, slightly lower than the mean of the whole dataset. This difference is due to the larger proportion of NA`s in pclass =3, which has a lower mean than the whole dataset.
# } This ends the discussion about other ways to populate age columns with NA values. While the above method of grouping based on pclass and assigning 3 different means for the 3 types of NA`s maybe more accurate, the overall difference is not very significant.

# Convert empty string boat rows into NA
titanic$boat[titanic$boat == ""] <- NA

# Duplicate cabin column to new column named has_cabin_number
titanic$has_cabin_number <- titanic$cabin

# Identify rows where has_cabin_number column does not have NA values
cabin_number_notNA <- (!is.na(titanic$has_cabin_number))

# Convert these not NA values in has_cabin_number to 1
titanic$has_cabin_number[cabin_number_notNA] <- 1

# Identify rows where has_cabin_number column has NA values
cabin_number_NA <- (is.na(titanic$has_cabin_number))

# Convert these NA values in has_cabin_number to 0
titanic$has_cabin_number[cabin_number_NA] <- 0

# Save new file as titanic_clean
titanic_clean <- titanic

# Write final csv output file to working directory
write.csv(titanic_clean,file="titanic_clean.csv")
