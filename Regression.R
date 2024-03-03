# Linear regression
library(tidyverse)
library(stats)
library(ggplot2)

#Let's load the data and build a data frame
x <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
y <- c(38, 45, 43, 48, 47, 56, 61, 72, 75, 75, 58, 81, 66, 86, 72, 69, 83, 83, 79, 85)
df <- data.frame(x, y)

#Let's start by preparing various columns that will ease our operations
df <- cbind(df, xSquared = df$x ^ 2)
df <- cbind(df, xy = df$x * df$y)
view(df)

#Lets find the coefficient of correlation
#We will need to find x-mean(x) and y-mean(y)
mean(df$x)
mean(df$y)

df<- cbind(df, XminusXbar=(df$x - mean(df$x)))
df<- cbind(df, YminusYbar=(df$y - mean(df$y)))
#Question 1
#Draw the scatter plot
ggplot(df, aes(x = x, y = y))+
  geom_point(color = "darkgreen", size = 4)+
  labs(title = "Scatter Plot of the Population")


#Question 2
#Find the coefficient of Correlation and the coeficient of determination
num<- sum(df$XminusXbar * df$YminusYbar)
den<- sqrt((sum(df$XminusXbar ^ 2) * sum(df$YminusYbar ^ 2)))

r<- num/den
r
print("The coefficient ofdetermination is given by r squared")

rsquared<- r * r
cat("The coefficient of correlation is ",r ,"While the coefficient of determination is ",rsquared)
print("Since our r is 0.8547834 we can say that their relationship is is good such that a shift in one likely to cause the other one to shift. But this is likely to affect approximately 73% of the data since our R squared is 0.7306547")

#Question 3
#Draw a simple regression line of Y=a + bx + error term
#model<- lm(y ~ x, data=df)
model<- lm(y ~ x, data=df)
ggplot(df,aes(x=x,y=y))+
  geom_point(color="darkblue")+
  geom_smooth(method= "lm" , color="red")

#Question 4
# Find the residual
#To find the residual, first we need to find the values of a and b
#a = mean of y - b*mean of x

#lets start by finding b
Bnum1<- sum(df$xy)
Bnum2<- 20 * mean(df$x) * mean(df$y)

Bden1 <- sum(df$xSquared)
Bden2 <- 20 * ((mean(df$x))^2)
num <- Bnum1 - Bnum2
den <- Bden1 - Bden2

b <- num/den

cat("The numerator is: ",num ,"The denominator is: ",den, "The value of B becomes",b)

#Now to find the value of a
a <- (mean(df$y))-(b * (mean(df$x)))
a

#residual = y - y Hat
#Lets find yHat
#yiHat = a + bxi for i = 1,2,...,20

df<- cbind(df, yHat = a + (b* df$x))
df

#Now to find the residual or the standard error
df <- cbind(df, Residual = df$y - df$yHat)
df

#Question 4
#Test the hypothesis on correlation coefficient of the population
cor.test(df$x,df$y,alternative = "greater")

print("Since the p_value is extremely smaller than 0.05, that is 7.952e-07. We can conclude and reject the null hypothesis and conclude that there is a positive correlation between x and y")

# Test the hypothesis of the regression line regression line coefficient
model<- lm(y ~ x, data=df)
summary(model)

print("For the intercept, that is for testing the hypothesis of a. We have obtained the p_value as 5.72e-09 and the test statistic as 10.293, since the p value is extremelly small as compared to the level of significance that is usually 0.05. We can conclude that the intercept b is statistically significant and we reject the null hypothesis")
print("For the slope, that is b. We have obtained the test statistic t as 6.988 and the p_value as 1.59e-06 which is signifficantly smaller as compared to the normal level of significance. we can safely reject the null hypothesis and conclude that the slope is statistically significant")
print("For the model, the F-statistic is 48.83 and the p-value is 1.59e-06.Since the P value is smaller as compared to the level of significance,we can conclude that the model is statistically significant")