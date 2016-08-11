### Read data frame ###
titanic_original <- data.frame(titanic_original)
head(titanic_original)

### Fill in embarked missing values ###
titanic_original$embarked[is.na(titanic_original$embarked)]<- "S"
titanic_original$embarked <- sub("^$", "S", titanic_original$embarked)
head(titanic_original)

### Fill in missing values in Age columns with the mean ###
titanic_original$age[is.na(titanic_original$age)]<- mean(titanic_original$age, na.rm=TRUE)
head(titanic_original)

### Fill in missing value in boat column with 'NA' ###
titanic_original$boat <- sub("^$", "NA", titanic_original$boat)
head(titanic_original)

### Create indicator variable for cabin number ###
titanic_original$has_cabin_number <- sub("^$", "0", titanic_original$cabin)
titanic_original$has_cabin_number[titanic_original$has_cabin_number != 0] <- 1
head(titanic_original)
