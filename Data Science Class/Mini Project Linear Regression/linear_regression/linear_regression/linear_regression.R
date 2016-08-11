#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
setwd("C:/Users/William/Desktop/linear_regression (2)/linear_regression")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
states.info
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

# Let's first examine the data.
# summary of energy consumed per capita (energy) and percentage of residents living in metropolitan areas (metro) columns, all rows
sts.ex.sat <- subset(states.data, select = c("energy", "metro"))
summary(sts.ex.sat)
# correlation between energy and metro
cor(sts.ex.sat)
# Based off initial descriptive statistics, there seems to be a wide range of observations for both energy consumed per capita and percentage of residents living in metropolitan areas

# Let's now plot the data
# scatter plot of energy vs metro
plot(sts.ex.sat)
# Looking at the scatterplot, we can see there seems to be clustering of a lot of values between 200-400
# energy consumed per capita. This might suggest a right skewed distribution for empty. Meanwhile for percentage
# of residents living in metropolitan area, it looks to be more normally distributed. There also appears to be a few
# outliers towards the outer right edge of the scatterplot where energy has more extreme values. 

# After looking at these initial descriptive plots and statistics, let's fit the model. 

# Fit our regression model
sat.mod <- lm(energy ~ metro, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table
# Looking at the result, for every unit increase in percentage of residents living in metropolitan areas, there is
# a -2.29 unit decrease in energy consumed per capita. At the alpha level of 0.01, we can say that we have sufficient evidence 
# to suggest a significant relationship between percentage of residents living in metropolitan areas and the energy consumed
# per capita.

## Linear Regression Assumptions
## ─────────────────────────────────

# Let's now plot the resdiuals plot and the normal QQ plot
# As we can see, some of the assumptions appear to be violated. Even though we can assume the errors are indepedent,
# the residuals appear to be skewed and there seems like a prescence of hetereoscedasticity.
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

# In order to see if we can fix the assumptions issue at the end, let's use people per square mile (density) as a factor
# Let's first examine the data.
# summary of energy consumed per capita (energy) and percentage of residents living in metropolitan areas (metro) columns, all rows
sts.ex.sat2 <- subset(states.data, select = c("energy", "metro", "density"))
summary(sts.ex.sat2)
# correlation between energy and metro
cor(sts.ex.sat2)
# Based off initial descriptive statistics, there seems to be a wide range of observations for people per square mile, energy consumed per capita and percentage of residents living in metropolitan areas

# Let's now plot the data
# scatter plot of energy vs metro vs density
plot(sts.ex.sat2)
# Looking at the scatterplot, we can see there seems to be clustering of a lot of values between 200-400
# energy consumed per capita. This might suggest a right skewed distribution for empty. Meanwhile for percentage
# of residents living in metropolitan area, it looks to be more normally distributed. There also appears to be a few
# outliers towards the outer right edge of the scatterplot where energy has more extreme values. Between metro and density,
# there appears to be some sort of exponential relationship. This also seems true between energy and density. While this is
# not a great sign to improving the model, we still must conduct a full analysis.

# After looking at these initial descriptive plots and statistics, let's fit the model. 

# Fit our regression model
sat.mod2 <- lm(energy ~ metro+density, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod2) # show regression coefficients table
# Looking at the results, we can see that both metro and density are insiginificant at the highest alpha level of 0.1 for each predictor. Therefore,
# there is no sufficient evidence to suggest a relationship between the predictors of metro and density and the outcome, energy.
# The reason the model is significant but the predictors are not is because of multicollineaity (as seen by the initial scatterplots
# showing some sort of non-linear relationship). This means that metro and density are having an effect on eachother. Since
# multicollineaity is detected, there is no reason to go into the regression assumptions. 

# While we can assume the second model will be worse than the first model, let's check using an anova test.
anova(sat.mod, sat.mod2)
# As we can see, the p-value is insiginificant at the 0.1 level. Therefore, there is sufficient evidence to suggest that
# the second model is no better than the first model.

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

energy.by.metro_density <- lm(energy ~ density*metro,
                             data=states.data) 
summary(energy.by.metro_density)

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
energy.by.metro_density.region <- lm(energy ~ density*metro+region,
                              data=states.data) 
summary(energy.by.metro_density.region)

# According to our model, there appears to be significant differences across the four regions. Individually as predictors,
# they don't appear to be significant. Again, this might suggest some multicollinearity. 