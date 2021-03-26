library(tidyverse)
# let read the file  the file name is sales.csv file

revenue=read.csv(choose.files())


dim(revenue)
summary(revenue)

#Calculate revenues 

TotalRev=sum(revenue$Sales)
TotalRev

#let model and linear model using variable where sales id dependant variable
model=lm(Sales~Distance,data=revenue)
model

#Test data set
library(caTools)
split=sample.split(revenue,SplitRatio = 0.8) 
test=subset(revenue,split==T)
train=subset(revenue,split=F)

#Let check the summary
summary(model)



#plotting a regression line on the graphs 
abline(lm(Sales~Distance,data=revenue))

# Residual standard error: 390.9 on 18 degrees of freedom
# Multiple R-squared:  0.01269,	Adjusted R-squared:  -0.04216 
# F-statistic: 0.2313 on 1 and 18 DF,  p-value: 0.6363

# show model is very bad with r^2 in negative

#create a new model
model2=predict(model,newdata = train)
model2
model3=cbind(model,model2)
summary(model2)
plot(model2)
model3
