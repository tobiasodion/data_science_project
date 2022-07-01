require(dplyr)
library(devtools)
install_github("vqv/ggbiplot", force = TRUE)
library(ggbiplot)

###################### PRELIMINARY ANALYSIS (DESCRIPTIVE STATISTICS)   ######################################
###


### QUESTION 1
### How many observations are there? How many variables?
garment = garments_worker_productivity
View(garment)
nrow(garment)
ncol(garment)
## Number of observations/rows is 1197
## Number of columns/variables is 15

## QUESTION 2
## Are there any missing values in the dataset? 
is.na(garment)
## If so, how many observations contain missing values 
sum(is.na(garment)) 
## There are missing values in the dataset
## 506 observations contain missing values

## Which variables are concerned with this missing data ?
colSums(is.na(garment)) ## The only variable/column concerned with missing data is 'wip'

## QUESTION 3
### Theoretical question : Without performing any calculation, suggest at least two methods to deal with missing data.
### Now, you will create a new dataset by deleting all the observations containing missing values. Hereafter, you will use this dataset.

## First method is deleting the records with missing observations from the dataset 
## Second method is imputation i.e systematically guessing what value the missing data should take using some techniques

## New dataset without missing variables
# name of new dataset is 'newgarment'
newgarment <- na.omit(garment)
View(newgarment)

### QUESTION 4
## Calculate descriptive statistics for the variable target productivity. Interpret the results.
summary(newgarment$actual_productivity)

# Create the function to calculate mode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode using the user function.
v<-c(newgarment$actual_productivity)
result <- getmode(v)
print(result) #The mode is



var(newgarment$actual_productivity)
sd(newgarment$actual_productivity) 

## Mean is 0.7220 ## On average, the actual productivity of each worker is 0.7220
## Median is 0.7506  ## 
## Mode is 0.800402

## Range ## 
## Variance is 0.0239 
## Standard deviation is 0.1547           

### QUESTION 5
## Perform an initial analysis of the variable based on the others by 
## calculating the correlation coefficient between productivity and each of the other variables. 
## Which ones are the most correlated with the productivity ?

## Null hypothesis: there is no relationship between the two variable.names
## Alternate hypo. there is a relationship between the two

### CORRELATION BETWEEN VARIABLES
## correlation between productivity and number of workers in each team
cor.test(newgarment$actual_productivity, newgarment$no_of_workers)
# correlation is 0.00337   

## correlation between productivity and number of changes in the style of a particular product 
cor.test(newgarment$actual_productivity, newgarment$no_of_style_change)
# correlation is -0.28168

## correlation between productivity and targeted productivity
cor.test(newgarment$actual_productivity, newgarment$targeted_productivity)
# correlation is 0.69796

## correlation between productivity and standard minute value
cor.test(newgarment$actual_productivity, newgarment$smv)
# correlation is -0.155

## correlation between productivity and work in progress
cor.test(newgarment$actual_productivity, newgarment$wip)
# correlation is 0.1311

## correlation between productivity and over-time 
cor.test(newgarment$actual_productivity, newgarment$over_time)
# correlation is -0.0168

## correlation between productivity and financial incentive 
cor.test(newgarment$actual_productivity, newgarment$incentive)
# correlation is 0.8041

## correlation between productivity and the amount of time production was interrupted
cor.test(newgarment$actual_productivity, newgarment$idle_time)
# correlation is -0.113

## correlation between productivity and the number of workers who were idle due to production interruption
cor.test(newgarment$actual_productivity, newgarment$idle_men)
# correlation is -0.258

## Which ones are the most correlated with the productivity?
## Productivity and targeted productivity, productivity and financial incentive are most correlated. 



#################################### PRINCIPAL COMPONENT ANALYSIS ########################################################## 

##1. [graded question] Calculate the variance of each variable and interpret the results.
apply(newgarment, 2, var)
summarise(newgarment, across(where(is.numeric), var))

  
# Do you think it is necessary to standardize the variables before performing PCA for this dataset ? Why ?
## There are two reasons we need to standardize the variables before performing the PCA

## First, as we know, the PCA is sensitive to variances of variables. Large differences between the variances of the variances will bias our result.
## As we can see with the variances calculated, different variables in this database have varying variances. Without standardization, we will get 
## wrong result for our PCA.

## Second, Principal component analysis is sensitive to differences in scale we use in measuring our variables 
## Given that the scales of measurement for the variables are not the same, 
## If we do not standardize them, our analysis will yield influence our result (pca) i.e some variables will appear more important than other 
## simply due to varying measurement scale 
## Standardizing our variables removes this constraint as measurement scale of variables no longer matter


## 2.[graded question] Perform PCA using the following variables : targeted_productivity, smv, over_time, incentive, no_of_workers and actual productivity. 
# You will use the appropriate function with the appropriate arguments and options considering the previous question. 
# Analyze the output of the function. Interpret the values of the two first principal component loading vectors ?

## Principal Component Analysis

pca_newgarment = newgarment[sapply(newgarment, is.numeric)]
pca_newgarment = subset(pca_newgarment, select = -c(wip,idle_time,idle_men, no_of_style_change, team))
pr.out=prcomp(pca_newgarment, scale=TRUE)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x

pca = pr.out

biplot = ggbiplot(pcobj = pca,
                  choices = c(1,2),
                  obs.scale = 1, var.scale = 1,  # Scaling of axis
                  labels = row.names(data),     # Add labels as rownames
                  labels.size = 4,
                  varname.size = 5,
                  varname.abbrev = FALSE,  # Abbreviate variable names (TRUE)
                  var.axes = TRUE,      # Remove variable vectors (TRUE)
                  circle = TRUE,        # Add unit variance circle (TRUE)
                  ellipse = TRUE) # Adding ellipses
print(biplot)


##biplot(pr.out, scale=0,cex=0.7)

##In our analysis, the first two principal components have eigenvalues greater than 1, and the third componet has that close to one, the fourth at 0.7
## These four components explain 92% of the variation in our data 
## 


## 3.[graded question] Use a biplot with a correlation circle to display both the principal component scores and the loading vectors in a single plot. 
# Interpret the results.
#biplot(pr.out, scale = 0) 

## interpretation

## 4. [graded question] Calculate the percentage of variance explained (PVE) by each component?
summary(pca)
# 39% of the variance in the data is explained by Principal Component 1 
# 29% of the variance in the data is explained by Principal Component 2 
# 13% of the variance in the data is explained by Principal Component 3 
# 0.9% of the variance in the data is explained by Principal Component 4 
# 0.5% of the variance in the data is explained by Principal Component 5 
# 0.2% of the variance in the data is explained by Principal Component 6 


# Plot the PVE explained by each component, as well as the cumulative PVE. How many com- ponents would you keep ? Why ?
plot(pca)
plot(pca, type = "line", main = "Scree plot")
#plot(pca, type = "l") 
## We will keep the first three components as these already provide a almost all the explanations of the variation in our dataset

## Geometrically speaking, principal components represent the directions of the data that explain a maximal amount of variance, 
## that is to say, the lines that capture most information of the data.
## As there are as many principal components as there are variables in the data, 
## principal components are constructed in such a manner that the first principal component accounts for the largest possible variance in the data set.








################################## LINEAR REGRESSION ##################################################
### THEORETICAL APPLICATION
### QUESTION 1
## Let us suppose that we fit a simple linear regression model to explain Y as a linear function of X. 
## What is the relationship between, the correlation coefficient between these two variables r(X, Y ) 
## and the coefficient of determination R2 obtained by fitting the model? 

##The correlation coefficient r tells us the relationship between both variables. 
## This coefficient is used to compute the coefficient of determination R2,
## which is used to describes the amount of variation in Y explained by X. 
## The R2 tells us to what extent our independent variable (X in this case)
## can explain the dependent variable (Y)


### QUESTION 2
### What is the range of values that can be taken by R2 ?How about r?

## R2 ranges from 0 to 1 (0% to 100%)
## r ranges from -1 to 1


### PRACTICAL APPLICATION
## QUESTION 1
## What are the coefficient estimates? Interpret coefficient estimate β1?

incentive <- lm(actual_productivity~incentive, data = newgarment)
summary(incentive)

## β0 is 0.521379
## β1 is 0.004510
## The B1 coefficient means that a unit change in financial incentive improves 
## the average actual productivity of workers by 0.004510


## QUESTION 2
##Give the general expression of a 1 − α confidence interval for the parameter
## β1. Calculate the 95% confidence interval for this coefficient. Interpret the results.

confint(incentive)


## QUESTION 3
## Elaborate the zero slope hypothesis test for coefficient β1 and conclude if there is a relationship between 
## the productivity and the financial incentive. Is β1 significantly non zero ?

## Given that the p-value of the slope (B1) is lesser than 0.05, we conclude that there is a relationship 
## between actual productivity and financial incentive. 
## This relationship is statistically significant as B1 is significantly non zero. 

## QUESTION 4
## What is the value of the coefficient of determination R2 ? Interpret this result. 
## Is this model suitable to predict the productivity ?

## R2 is 0.6467. This means that the variable "financial incentive" can explain up to 64.6% of the 
## variations in the variable "actual productivity". The remaining 35.4% of variation can be explained by 
## other factors/reasons which we have not included in this model. 








######################################### MULTIPLE REGRESSION ###########################################
install.packages("olsrr")
install.packages("leaps")
library(leaps)
require(olsrr)

#### QUESTION 1
## Use Best Subset Selection to select the best model for any possible number of features ranging from 1 to 6. Plot the curve R2 versus the number of features. 
## Then, select the best model. That is, the model for which the adjusted coefficient of determination R ̄2 is the highest.

## creating a new dataset that contains only the required variables
newgarment2 <- select(newgarment, actual_productivity, targeted_productivity, smv, wip, over_time, incentive, no_of_workers)
View(newgarment2)

## finding the best model
bestmodel <- regsubsets(actual_productivity ~ ., data = newgarment2, method = "exhaustive") 
b <- summary(bestmodel)
b
 

## finding adjusted r-squares of our models
tableADJR <- data.frame(AdjustedR2 = b$adjr2)
tableADJR
      ######### AND ############
tableADJR <- data.frame(AdjustedR2 = b$adjr2, 
                        cp =b$cp,   ## lower cp (process capability) means the model is more precise (with lower error) ## a small value means the model is relatively precise
                        rss =b$rss,  ## residual sum of squares  ## the lower value is better for the model
                        rsq= b$rsq)
tableADJR
  
## The best model 
## Both model 5 and 6 (with five and six variables respectively) have adjusted R-square of 0.780. However, model 5 has a lower cp
## This means it is more suitable and therefore our best model 


## plotting the R2 versus the number of features
plot(b$rsq, xlab="Number of Variables", ylab = "R2", type ="l")


## QUESTION 2
## How many features did you keep? Which ones? 

## Five features are kept
## The features kept are target_productivity, smv, over_time, incentive, number of workers

### QUESTION 3
## Why is it more appropriate to use the adjusted coefficient of determination R2 
## instead of the coefficient of determination R2 when comparing two models with different numbers of predictors ?

## Typically, R-square increases as we add more and more predictors to our model, even if it is just due to chance 
## Thus, a model with more predictors will appear to have better explanatory power than a model with lesser number of predictors if we interprete using the R-square
## The Adjusted R-square on the other hand helps us deal with this problem. It only increases if the predictor actually improves the model 
## rather than just increase because there are more predictors.

### QUESTION 4
## For the selected model, what are the values of the coefficient estimates? 
## Interpret them. What is the value of the coefficient of determination R2?

coef (bestmodel, 5)

## interpretation
## average actual productivitity is 0.138
## one unit change in targeted productivity increases actual productivity by 1.621 given that other variables are constant
## one unit change in smv increases actual productivity by 0.622 given that other variables are constant
## one unit change in over_time decreases actual productivity by -0.000002 given that other variables are constant
## one unit change in incentive increases actual productivity by 0.003 given that other variables are constant
## one unit change in number of workers increases actual productivity by 0.0012 given that other variables are constant
  

### QUESTION 5
## For the selected model, perform the zero slope hypothesis test for all the coefficients except β0 and conclude.
selectedmodel <- lm(actual_productivity~targeted_productivity + smv + over_time + incentive + no_of_workers, data = newgarment2)
summary(selectedmodel)

 ## interpretation
## The effect of all predictors on actual_productivity included is statistically significant as the P-values of all predictors are less than 0.005








