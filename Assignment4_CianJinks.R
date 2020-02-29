install.packages('Rlab')
library(stats)
library(Rlab)

OneSampleTest <-function(type=NULL, tails=NULL, alpha, mu, n, x_bar, sd)
{
  #calculate the test statistic
  se = (sd/sqrt(n))
  test_stat <- (x_bar-mu)/(sd/sqrt(n))
  
  if (type=="z") {  
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    #define df
    df <- n-1  
    #get the p-value for this test statistic
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  #check if significant
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  
  ret <- list(type=paste("One Sample", type, "test.", tails, "tailed"), mu=mu, n=n, x_bar=x_bar, se=se, p = p_val, alpha = alpha, significance = sig , test_statistic = test_stat)
  #return the list
  return( ret )
}

#Question 1
n <- 100 #Sample size
knownsd <- 5 #Population standard deviation
x0 <- rnorm(n, 4, knownsd) #Create a a vector of length 100 drawn from a normal distribution with mean 4 and sd 5
mean1h0 <- 0 #Mean of the population under the null hypothesis
alpha <- 0.05 #Significance level
x_bar <- mean(x0) #Sample mean
OneSampleTest("z", "two", alpha, mean1h0, n, x_bar, knownsd) #Run a two tailed hypothesis z test, using the variables above, where H0 means the mean = 0 and HA means the mean does not equal 0

#Results:
# Test Statistic: 8.138071
# P-Value: 4.016254e-16
# Conclusion: The test is significant and therefore we reject the null hypothesis and conclude that the mean of the population is not equal to 0 with a significance level of 0.05

#Question 2
mean2h0 <- 4.2
alpha2 <- 0.025
unknownsd <- sd(x0)
OneSampleTest("t", "left", alpha2, mean2h0, n, x_bar, unknownsd) #Run a one tailed hypothesis t test, using the variables above, where H0 means the mean >= 4.2 and HA means the mean < 4.2
#Results:
# Test Statistic: 0.147821
# P-Value: 0.5586076
# Conclusion: The test is insignificant and therefore we fail to reject the null hypothesis and conclude that the mean of the population is greater than or equal to 4.2.

#Question 3
t.test(x0, mu=mean2h0, alternative="less") #Run a one tailed hypothesis t test using the stats library, with the variables above, where H0 means the mean >= 4.2 and HA means the mean < 4.2
#Results:
# Test Statistic: 0.14782 (same)
# P-Value: 0.5586 (same)
# Conclusion: The test is insignificant and therefore we fail to reject the null hypothesis and conclude that the mean of the population is greater than or equal to 4.2.


#Question 4
prop <- 0.3 #Chance of success
x1 <- rbern(n, prop) #Run 100 bernoulli trials with a 0.3 chance of success
propsd <- sqrt((prop) * (1-prop)) # Calculate sd for a ztest of proportions
prop3h0 <- 0.28 #The null hypothesis for the proportion
OneSampleTest("z", "two", alpha, prop3h0, n, prop, propsd) #Run a two tailed hypothesis z test, using the variables above, where H0 means the proportion = 0.28 and HA means the proportion does not equal 0.28
#Results:
# Test Statistic: 0.4364358
# P-Value: 0.6625206
# Conclusion: The test is insignificant and therefore we fail to reject the null hypothesis and conclude that the population proportion is equal to 0.28


#Question 5
prop4h0 <- 0.35 #The null hypothesis for the proportion
OneSampleTest("z", "right", alpha, prop4h0, n, prop, propsd) #Run a one tailed hypothesis z test, using the variables above, where H0 means the proportion <= 0.35 and HA means the proportion > 0.35
#Results:
# Test Statistic: -1.091089
# P-Value: 0.8623832
# Conclusion: The test is insignificant and therefore we fail to reject the null hypothesis and conclude that the population proportion is less than or equal to 0.35







