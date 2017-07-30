library(dplyr)
library(tidyr)
titanic <- titanic3
summary(titanic)

# Convert empty string embarked rows into NA
titanic$embarked[titanic$embarked == ""] <- NA

# Identify rows where embarked is NA
embarked_missingrows <- which(is.na(titanic$embarked))

# Convert missing rows into S
titanic$embarked[embarked_missingrows] <- "S"

