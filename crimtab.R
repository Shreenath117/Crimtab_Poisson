# Case Study Solutions : Crimtab.csv file
#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 1:
# Reading the CSV file and dropping the first column
data=read.csv('crimtab.csv')
# View the data loaded
data
# Dropping the first column which is nothing but the Serial number
data=data[2:4]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)
# There are 924 rows and 3 columns

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 2:

# The key objective of the case study is to model/predict the freq variable 

# The response variable freq we see is a "count" variable 
data$Freq


#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 3:

q3<- c(mean(data$Freq), var(data$Freq))
c(mean=q3[1], var=q3[2], ratio=q3[2]/q3[1])

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 4:

# The mean Freq is 3.25 and the variance is 68.75, 
# ~21% of the mean. 
# The data is clearly over-dispersed

#-------------------------------------------------------------------------------------------------
# Soln. to Question 4:

#Summarising the dataset : 
summary(data)

# Observations :
# Here : var 1 denotes the prisoner's finger length
# Var1 has max value of 13.5 and median of 11.45

# var 2 denotes for the prisoner's height
# Var2 has max value of 195.6 and median of 168.9

# Freq has a max value of 58 with mean of 3.247

#-------------------------------------------------------------------------------------------------
# Soln. to Question 6:
#Plotting a scatterplot matrix

pairs(~Freq+Var1+Var2, data=data)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 7:

correlation<-corr.test(data)
correlation

# Looking at the plot : it seems var1 and var2 are highly correlated  with each other
# Also : Freq the response/target variable is more correlated with var 1 (finger length) than var 2 (height)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 8:

# Plotting a histogram : 

hist(data$Freq, col="red", xlim=c(0,60),
     xlab="Frequency")

# The distribution here clearly looks like a Poisson Distribution
# It looks clumped at 0 (total zeroes : 623 out of 924 overall)
# Histogram indicates that this dataset has abundance of zeroes

#-------------------------------------------------------------------------------------------------
# Soln. to Question 9:

# We have seen that the response variable "Freq" is clearly a count variable.
# The distribution as we clearly see isn't normal which the implementation of linear regression model assumptions require.
# The count data does not follow a normal distribution, because it simply can not and 
# hence, simple linear regression is not the way to go. 
# Hence, we go for Poisson , Negative Binomial models for count variables.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 10:

# Check the datatypes of the variables 
str(data)

#-------------------------------------------------------------------------------------------------

# Soln. to Question 11:

data.pois <- glm(Freq ~ ., data, family = poisson)
summary(data.pois)

# Here : as we observe as per significance : all predictor variables are significant

#-------------------------------------------------------------------------------------------------
# Soln. to Question 12:

# AIC stands for : Akaike Information Criterion or 
# AIC is an alternative criterion for model selection and is based on log of maximum likelihood 
# function under the assumed model when the model dimension is also unknown

# AIC does not provide a test for model fit but makes a statement about the relative quality of 
# models and the model with smallest AIC is prefered.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 13:

# Influence plot
# install.packages("car")
library(car)
influencePlot(data.pois)

# So data points : 41, 42, 357, 399 and 400 are at the extremes. Let's check in the dataset

data[c(41,42,357,399:400),]

# Also, or the Poisson model, 10 observations are nominated as large + outliers:
outlierTest(data.pois, cutoff=0.001)


#-------------------------------------------------------------------------------------------------
# Soln. to Question 14:

# The output from summary(data.pois) above showed that the Poisson model fits quite badly. 
# The residual deviance is 10209 with 921 degrees of freedom

# The residual deviance is way too higher than the residual degrees of freedom
# If the asssumed model (Poisson) fits the data well, the residual deviance are expected to be 
# approximately equal to the residual degrees of freedom.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 15:

# As, we clearly saw in answer 2:  The mean Freq is 3.25 and the variance is 68.75, 
# ~21% of the mean. 
# Thus the data are highly over dispersed

# Variance > Mean : Overdispersion

# If the variance is equal to the mean, the dispersion statistic would equal one.
# When the dispersion statistic is close to one, a Poisson model fits. 
# If it is larger than one, a negative binomial model fits better.

# The existence of overdispersion leads to the requirement of alternative models that fit the data better.
# Overdispersion is a common phenomenon with count data which is due to either excess of zeros or heterogeneity of population.

# install.packages("AER")
library(AER)
dispersiontest(data.pois,trafo = 1)

# Here : we observe the alternative hypothesis is true , indicating overdispersion

#-------------------------------------------------------------------------------------------------
# Soln. to Question 16:

# Alternative model which you would use to take care of over-dispersion is : 
# "Negative Binomial" model

# The negative-binomial model is a different generalization of the Poisson that allows for over-dispersion

#-------------------------------------------------------------------------------------------------
# Soln. to Question 17:

# This is implemented in glm.nb() in the MASS package.

library(MASS)

data.nbin <- glm.nb(Freq ~ Var1, data)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 18:

summary(data.nbin)
# Observations: (Negative Binomial Model)
# Residual deviance: 608.87  on 922  degrees of freedom
# AIC = 3045.4
#-------------------------------------------------------------------------------------------------
# Soln. to Question 19:

# Comparison of Poisson Model with the above fit NB Model

summary(data.pois)

# Observations: (Poisson Model)
# Residual deviance: 10209  on 921  degrees of freedom
# AIC: 11259

# Observations: (Negative Binomial Model)
# Residual deviance: 608.87  on 922  degrees of freedom
# AIC = 3045.4

# Here : it's clearly observed : NB model performs better than Poisson due to over dispersion
# and is a good fit (Residual deviance is closer to the residual degrees of freedom and lower AIC)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 20:

# Zero Inflated & Hurdle Models

# Hurdle models assume that there is only one process by which a zero can be produced, 
# while zero-inflated models assume that there are 2 different processes that can produce a zero.

# Hurdle models assume 2 types of subjects: 
# (1) those who never experience the outcome and 
# (2) those who always experience the outcome at least once. 

# Zero-inflated models conceptualize subjects: 
# (1) those who never experience the outcome and 
# (2) those who can experience the outcome but don't always.

# There is package pscl containing hurdle() and zeroinfl() functions to implement in R

#-------------------------------------------------------------------------------------------------
# Soln. to Question 21:

# Fitting Zero Inflated and Hurdle Models

# install.packages("pscl")
library(pscl)

# Zero Inflated Models ( Poisson and Negative Binomial)
data.zip <- zeroinfl(Freq ~ ., data, dist="poisson")
data.znb <- zeroinfl(Freq ~ ., data, dist="negbin")

summary(data.zip)
summary(data.znb)


# Hurdle Models ( Poisson and Negative Binomial)
data.hp  <- hurdle(Freq ~ ., data, dist="poisson")
data.hnb <- hurdle(Freq ~., data, dist="negbin")


summary(data.hp)
summary(data.hnb)


#-------------------------------------------------------------------------------------------------
# Soln. to Question 22:

summary(data.znb)

# Mentor is the only variable affecting the zero counts for the best fit model !

# ZINB has the lowest AIC amongst all the models proving it to be the best fit for the given data.


#-------------------------------------------------------------------------------------------------
# Soln. to Question 23:

# Comparing Models : 
LRstats(data.pois, data.nbin, data.zip, data.znb, data.hp, data.hnb, 
        sortby="AIC")

# From the final output of comparison between Poisson, Negative Binomial, Zero Inflated and Hurdle Models

# The final recommendation is that the Zero Inflated Negative Binomial model is the best fit !
