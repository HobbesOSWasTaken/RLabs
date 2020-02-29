install.packages('Rlab')
library(stats)
library(Rlab)

TwoSampleTest <- function(type=NULL, proportion=NULL, alpha, mean1, mean2, n1, n2, sd1, sd2, h0) {
  
  # Difference in means
  difference <- mean1 - mean2
  # Check if the data are proportions
  if(proportion==FALSE) {
    # Calculate the test statistic:
    esd = sqrt((((n1 - 1) * (sd1 * sd1)) + ((n2 - 1) * (sd2 * sd2)))/(n1 + n2 - 2)) # Estimate of SD using pooled sD's
    se = esd * sqrt((1/n1) + (1/n2)) # Calculate Standard Error
    test_statistic = (mean1 - mean2 - h0)/se # Calculate the test statistic 
  } 
  else if(proportion==TRUE) {
    # Calculate the test statistic for a proportion:
    pp = ((mean1 * n1) + (mean2 * n2))/(n1 + n2) # Calculate the pooled proportion using the proportions
    se = sqrt(pp * (1- pp) * ((1/n1) + (1/n2))) # Calculate Standard Error
    test_statistic = (mean1 - mean2 - h0)/(se) # Calculate the test statistic
  } 
  else {stop("Please specify if the data are proportions or not")}
  
  # Check test type
  if(type == "z") {
    
    p_val <-  2*pnorm(abs(test_statistic), lower.tail=FALSE) # Calculate p value for z test
    
  } 
  else if(type == "t") {
    
    df = n1 + n2 - 2
    p_val <- pt(abs(test_statistic), df, lower.tail=FALSE) # Calculate p value for t test
    
  }
  else {stop("Please choose z or t")}
  
  # Check if significant
  if(p_val < alpha) {
    sig <- "significant"
  } else {
    sig <- "not significant"
  }
  
  # Generate output list
  ret <- list(type=paste("Two Sample", type, "test. two tailed"), n1=n1, n2=n2, Pop_D=h0, diff=difference, se_est=se, test_stat=test_statistic, p=p_val, alpha=alpha, significance=sig)
  return(ret)
}

n1 <- 100
n2 <- 80
knownsd1 <- 5
knownsd2 <- 2
mean1 <- 4
mean2 <- 3.5 
x1 <- rnorm(n1, mean1, knownsd1) # Simulate a vector of size 100 drawn from a Normal(4,5) distribution
x2 <- rnorm(n2, mean2, knownsd2) # Simulate a vector of size 80 drawn from a Normal(3.5,2) distribution
samplemean1 <- mean(x1) # First sample mean
samplemean2 <- mean(x2) # Second sample mean
samplesd1 <- sd(x1) # First sample SD
samplesd2 <- sd(x2) # Second sample SD
nullhypothesis1 <- 1 # Null hypothesis for if the difference of the means is 1
alpha <- 0.05 # Significance level

#Question 1
TwoSampleTest("z", FALSE, alpha, samplemean1, samplemean2, n1, n2, knownsd1, knownsd2, nullhypothesis1) # Run a two sample z test on the generated sample means using the known standard devation with h0 = 1
#Results:
# Test Statistic: -1.382433
# P-Value: 0.1668388
# Conclusion: The test is not significant and therefore we fail to reject the null hypothesis and conclude that the difference between the means is equal to 1 with a significance level of 0.05

#Question 2
nullhypothesis2 <- 0 # Creating a nullhypothesis for if the means are equal
TwoSampleTest("z", FALSE, alpha, samplemean1, samplemean2, n1, n2, samplesd1, samplesd2, nullhypothesis2) # Run a two sample z test on the generated sample means using the sample standard devations with h0 = 0
#Results:
# Test Statistic: 0.3107245
# P-Value: 0.7560101
# Conclusion: The test is not significant and therefore we fail to reject the null hypothesis and conclude that the means are equal with a significance level of 0.05


#Question 3
TwoSampleTest("t", FALSE, alpha, samplemean1, samplemean2, n1, n2, samplesd1, samplesd2, nullhypothesis2) # Run a two sample t test on the generated sample means using the sample standard devations with h0 = 0
#Results:
# Test Statistic: 0.3107245
# P-Value: 0.3781868
# Conclusion: The test is not significant and therefore we fail to reject the null hypothesis and conclude that the means are equal with a significance level of 0.05

#Question 4
t.test(x1, x2, mu=nullhypothesis2, var.equal=TRUE) # Run a two sample t test using t.test function assuming equal variance with h0 = 0
#Results:
# Test Statistic: 0.31072
# P-Value: 0.7564
# Conclusion: The test is not significant and therefore we fail to reject the null hypothesis and conclude that the means are equal with a significance level of 0.05

#Question 5 
t.test(x1, x2, mu=nullhypothesis2, var.equal=FALSE) # Run a two sample t test using t.test function not assuming equal variance with h0 = 0
#Results:
# Test Statistic: 0.33293
# P-Value: 0.7397
# Conclusion: The test is not significant and therefore we fail to reject the null hypothesis and conclude that the means are equal with a significance level of 0.05

#Question 6 - Bernoulli Trials
nbern1 <- 100 
pbern1 <- 0.3
nbern2 <- 85
pbern2 <- 0.7
x3 <- rbern(nbern1, pbern1) # Simulate of vector drawn from 100 independent bernoulli trials each with probablity of success 0.3
x4 <- rbern(nbern2, pbern2) # Simulate of vector drawn from 85 independent bernoulli trials each with probablity of success 0.7
proportion1 <- mean(x3) # Calculate proportion one
proportion2 <- mean(x4) # Calculate proportion two
nullhypothesisprop <- 0 # Null hypothesis is are the proportion equivalent
TwoSampleTest("z", TRUE, alpha, proportion1, proportion2, nbern1, nbern2, 0, 0, nullhypothesisprop) # Run a two sample z test on the generated sample proportions with h0 = 0 and significance level 0.05 (SD are 0 as they dont matter for proportions)
#Results:
# Test Statistic: -6.792734
# P-Value: 1.100278e-11
# Conclusion: The test is significant and therefore we reject the null hypothesis and conclude that the proportions are equal with a significance level of 0.05

#Question 7 - Confidence Interval
test_statistic <- -6.792734
recalculateSE <- sqrt(  ( (proportion1 * (1 - proportion1)) / (nbern1) ) + ( (proportion2 * (1 - proportion2)) / (nbern2) )) # Recalculate the standard error for the difference in proportions because we rejected H0
leftsideCI <- (proportion1 - proportion2) - 1.96 * test_statistic # Calculate the left side of the confidence interval
rightsideCI <- (proportion1 - proportion2) + 1.96 * test_statistic # Calculate ther right side of the confidence interval

# Make sure the confidence interval prints correct sides on the correct side
if(leftsideCI > rightsideCI) {
  print(paste(rightsideCI, "< pi <", leftsideCI)) # Print out the confidence interval
  
} else {
  print(paste(leftsideCI, "< pi <", rightsideCI)) # Print out the confidence interval
}



