library(dplyr)
library(tidyr)
titanic <- titanic3
summary(titanic)
which(is.na(titanic$embarked))
