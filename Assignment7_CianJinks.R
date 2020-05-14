#Question 1
cor(faithful$waiting, faithful$eruptions) # Check correlation between eruptions and waiting

#Question 2
plot(faithful$waiting, faithful$eruptions) # Scatterplot
scatter.smooth(x=faithful$waiting, y=faithful$eruptions, main="Eruptions ~ Waiting")  # Smooth line on scatterplot

#Question 3
par(mfrow=c(1, 2))  # Split graph area into 2
boxplot(faithful$eruptions, main="Eruptions", sub=paste("Outlier rows: ", boxplot.stats(faithful$eruptions)$out))  # Box plot for eruptions
boxplot(faithful$waiting, main="Waiting", sub=paste("Outlier rows: ", boxplot.stats(faithful$waiting)$out))  # Box plot for waiting

#Question 4
par(mfrow=c(1, 2))  # Split graph area into 2
plot(density(faithful$eruptions), main="Density Plot: Eruptions", ylab="Frequency")  # Density plot for eruptions
plot(density(faithful$waiting), main="Density Plot: Waiting", ylab="Frequency") # Density plot for waiting

#Question 5
faithful.lm <- lm(eruptions ~ waiting, data=faithful)  # Build a linear regression model

#Question 6
plot(faithful$waiting, faithful$eruptions) # Scatterplot
abline(faithful.lm) # Add regression line

#Question 7
print(faithful.lm) # Used to get the coefficents for the equation
# y = -1.87402 + 0.07563x

#Question 8
summary(faithful.lm) # A summary of the results
# Based on the resulting pvalues from the summary command above we can see that the independent
# variable "waiting" has a pvalue very much <0.05 and therefore we have significant reason to reject the null hypothesis
# and conclude there is a relationship between eruptions and waiting.

#Question 9
# The F statistic is based on the number of variables and there is only one variable therefore they are very similar.



