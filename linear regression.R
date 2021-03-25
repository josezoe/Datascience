# Introduction
# In this example of a bivariate relationship 
# we have two continuous numerical variables X and Y, 
# where Y is the response (or dependent) variable and X is
# the explanatory (or independent/predictor) variable 
# (something you might think is related to the response).
# The goal of simple linear regression is to predict the
# dependent variable Y based on the independent variable X. 
# Let's load our packages and data to see what we are working with.


library(tidyverse)


#load the dataset

train=read.csv(choose.files())
test=read.csv(choose.files())


# summary of both dataset
summary(train) # 1 na value in the dataset

summary (test)  # no na value 


#let remove the na value as we don't know much about the data set'

trainnew<-na.omit(train) # 699 rows now compared to 700 rows


# let see the summary
summary(trainnew)



plot(trainnew)
abline(trainnew)

#lm([target variable] ~ [predictor variables], data = [data source])

train1=lm(y~x,data=trainnew)
summary(train1)

plot(train1$residuals)

# Call:
#   lm(formula = x ~ y, data = trainnew)
# 
# Coefficients:
#   (Intercept)            y  
# 0.5713       0.9901  
# 
# > train1=lm(x~y,data=trainnew)
# > summary(train1)
# 
# Call:
#   lm(formula = x ~ y, data = trainnew)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.3598 -1.8873  0.0081  2.0166  9.5167 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.571255   0.209970   2.721  0.00668 ** 
#   y           0.990052   0.003633 272.510  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.794 on 697 degrees of freedom
# Multiple R-squared:  0.9907,	Adjusted R-squared:  0.9907 
# F-statistic: 7.426e+04 on 1 and 697 DF,  p-value: < 2.2e-16

#y=-0.107265 +1.000656*x is relationship with 99% r2 which a very good fit

abline(train1,col="red")

# 
# From the summary we can see that the model is a great fit 
# for the data (R squared of 0.99). The residual standard error 
# is very small which is good, too (only 2.8 on 697 degrees of freedom). 
# Our model tells us that, for every increase of 1 in X, 
# we see an increase of very close to 1, too (1.000656 to be exact). 
# In a real setting we might want to try other modelling approaches 
# now and compare the models to the linear model, 
# however, here we will just proceed with the linear model and 
# evaluate its preformance on the test data.


# evaluate the model on newdata
library(broom)

testfitted=augment(train1,newdata = test)


summary(testfitted)



ggplot(testfitted, aes(x = x, y = y)) +
  geom_jitter() +
  geom_point(aes(y = .fitted), size = 3, color = "dodgerblue") +
  labs(title = "Test data: Fitted vs. Actual values") 


# Calculate mean square error
MSE <- testfitted %>%
  summarize(mean((y - .fitted)^2)) %>% pull()
print(paste("The mean square error of the training model on the test data is", round(MSE, 3)))

# Calculate root mean square error
RMSE <- sqrt(MSE)
print(paste("The  root mean square error of the training model on the test data is", round(RMSE, 3)))