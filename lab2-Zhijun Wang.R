rm(list=ls())
multivariate <- read.csv("/Users/zhijun/Desktop/multivariate.csv")
attach(multivariate)
mm<-lm(Homeowners~Immigrant)
mm

#Multivariate Regression
multivariate<- read.csv("/Users/zhijun/Desktop/multivariate.csv")
head(multivariate)
attach(multivariate)
help(lm)
mm<-lm(Homeowners~Immigrant)
mm # mm here is a R object
summary(mm)$coef # The output above shows the estimate of the regression beta coefficients (column Estimate) 
                 # and their significance levels (column Pr(>|t|).
                 # The intercept is 107494.898 and the coefficient of Immigrant variable is -6656.839.
                 # The estimated regression equation can be written as follow:
                 # Homeowners = 107494.898 + (-6656.839)*Immigrant 
                 # We can rewrite it as: Homeowners = 107494.898 - 6656.839*Immigrant.

plot(Homeowners~Immigrant)
help("abline")
abline(mm)
abline(mm,col=2,lwd=3) # Using this formula, for each new value in Immigrant, you can predict the value for Homeowners.
# As an examle:
# For Immigrant value = 0, we will get: Homeowners = 107494.898 - 6656.839*0 = 107494.898
# for Immigrant value = 20, we will get: Homeowners = 107494.898 - 6656.839*20 = -25641.88
# Predictions can be easily made using the R function predict().
# In the following example, we predict Homeowners for two Immigrant values: 0 and 20.
# you can pass the 0 and 20 values as a concatenated list for Immigrants as follows:
newImmigrantdata <- data.frame(Immigrant = c(0,  20))
library(dplyr)
mm%>%predict(newImmigrantdata)

abline(mm)
library(nycflights13)
abline(mm,col=3,lwd=3)#line color=green, line width=3
attributes(mm)
mm$coefficients

#Creating Plots
#Chapter 2 -- R Graphics Cookbook
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data = mtcars)
ggplot(mtcars,aes(x=wt,y=mpg)) + geom_point()
plot(pressure$temperature,pressure$pressure,type = "l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2, col="red")
points(pressure$temperature,pressure$pressure/2, col="blue")
library(ggplot2)
qplot(pressure$temperature, pressure$pressure,geom = "line")
qplot(temperature,pressure,data = pressure,geom = "line")
ggplot(pressure, aes(x=temperature,y=pressure))+geom_line()+geom_point()

#Creating Bar graghs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) #generate a table of counts
qplot(mtcars$cyl) #cyl is continous here 
qplot(factor(cyl),data = mtcars)
ggplot(mtcars,aes(x=factor(cyl))) + geom_bar()

#creating histogram
#view the distribution of one-dimentional data with a histgram.
hist(mtcars$mpg)
hist(mtcars$mpg,breaks=10) # specify approximate number of bins with breaks.
hist(mtcars$mpg,breaks=5)
hist(mtcars$mpg,breaks=12)
qplot(mpg,data = mtcars, binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 5)

#Creating Box-plot
plot(ToothGrowth$supp,ToothGrowth$len) 
boxplot(len~supp,data = ToothGrowth)
boxplot(len~supp+dose,data=ToothGrowth)
library(ggplot2)
qplot(ToothGrowth$supp,ToothGrowth$len,geom = "boxplot")
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom = "boxplot")
qplot(interaction(supp,dose),len,data=ToothGrowth,geom = "boxplot")
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()


