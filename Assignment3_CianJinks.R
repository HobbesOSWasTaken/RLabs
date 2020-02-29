normal10000 <- rnorm(10000, 4, 5) #Create a a vector of length 10000 drawn from a normal distribution with mean 4 and sd 5
exponen10000 <- rexp(1000, 1) #Create a a vector of length 10000 drawn from an exponential distribution with mean 4 and sd 5

normalityplot <- function(distribution, title, barwidth) {
  qqnorm(distribution) #Create a normal qq plot
  qqline(distribution) #Adds a line that passes through the quantiles
  hist(distribution, freq = FALSE, main = title, xlab = title) #Create a histogram of the distribution
  xfit <- seq(min(distribution), max(distribution), length = barwidth) #Setting x for the line
  yfit <- dnorm(xfit, mean = mean(distribution), sd = sd(distribution)) #Setting y for the line
  lines(xfit, yfit) #Draw the line
}

sample_normal500 <- sample(normal10000, 500) #Take a random sample of size 500 from the normal distribution
sample_normal50 <- sample(normal10000, 50) #Take a random sample of size 50 from the normal distribution
sample_normal10 <- sample(normal10000, 10) #Take a random sample of size 10 from the normal distribution

sample_exponen500 <- sample(exponen10000, 500) #Take a random sample of size 500 from the exponential distribution
sample_exponen50 <- sample(exponen10000, 50) #Take a random sample of size 50 from the exponential distribution
sample_exponen10 <- sample(exponen10000, 10) #Take a random sample of size 10 from the exponential distribution

normalityplot(normal10000, "Normality of Normal 10000", 40) #Generate the normality plot of the 10000 pop Normal(4,5)
normalityplot(exponen10000, "Normality of Exponential 10000", 40) #Generate the normality plot of the 10000 pop Exponential(1)
normalityplot(sample_normal500, "Normality of Normal 500", 40) #Generate the normality plot of the 500 pop normal sample
normalityplot(sample_normal50, "Normality of Normal 50", 40) #Generate the normality plot of the 50 pop normal sample
normalityplot(sample_normal10, "Normality of Normal 10", 40) #Generate the normality plot of the 10 pop normal sample
normalityplot(sample_exponen500, "Normality of Exponential 500", 40) #Generate the normality plot of the 500 pop exponential sample
normalityplot(sample_exponen50, "Normality of Exponential 50", 40) #Generate the normality plot of the 50 pop exponential sample
normalityplot(sample_exponen10, "Normality of Exponential 10", 40) #Generate the normality plot of the 10 pop exponential sample

#Confidence Intervals (z-score + known sd) (t-distribution + sample sd)
zscore <- qnorm(0.975) #Calculate Z-score for 95% CI
tscoreNorm <- qt(0.975, length(sample_normal50)-1) #Calculate t-value for 95% CI and sample_normal50
tscoreExp <- qt(0.975, length(sample_exponen50)-1) #Calculate t-value for 95% CI and sample_exponen50

#Standard Errors
se_known_norm <- sd(normal10000)/sqrt(length(sample_normal50)) #Standard error of the 50 population normal sample using the known SD
se_known_exp <- sd(exponen10000)/sqrt(length(sample_exponen50)) #Standard error of the 50 population exponential sample using the known SD
se_unknown_norm <- sd(sample_normal50)/sqrt(length(sample_normal50)) #Standard error of the 50 population normal sample using the sample SD
se_unknown_exp <- sd(sample_exponen50)/sqrt(length(sample_exponen50)) #Standard error of the 50 population exponential sample using the sample SD

#Confidence interval using zscore and known population SD
#a)
left_z_known_norm <- mean(sample_normal50)-zscore*se_known_norm #Lower bound of the confidence interval for the normal distribution using the zscore and known SD
right_z_known_norm <- mean(sample_normal50)+zscore*se_known_norm #Upper bound of the confidence interval for the normal distribution using the zscore and known SD
#b)
left_z_known_exp <- mean(sample_normal50)-zscore*se_known_exp #Lower bound of the confidence interval for the exponential distribution using the zscore and known SD
right_z_known_exp <- mean(sample_normal50)+zscore*se_known_exp #Upper bound of the confidence interval for the exponential distribution using the zscore and known SD

#Confidence interval using tscore and sample SD
#a)
left_t_unknown_norm <- mean(sample_normal50)-tscoreNorm*se_known_norm #Lower bound of the confidence interval for the normal distribution using the tscore and unknown SD
right_t_unknown_norm <- mean(sample_normal50)+tscoreNorm*se_known_norm #Upper bound of the confidence interval for the normal distribution using the tscore and unknown SD
#b)
left_t_unknown_exp <- mean(sample_normal50)-tscoreExp*se_known_exp #Lower bound of the confidence interval for the exponential distribution using the tscore and unknown SD
right_t_unknown_exp <- mean(sample_normal50)+tscoreExp*se_known_exp #Upper bound of the confidence interval for the exponential distribution using the tscore and unknown SD

#Print out the confidence intervals
paste("Confidence Interval of Normal Distribution with zscore and known SD:", left_z_known_norm, right_z_known_norm)
paste("Confidence Interval of Exponential Distribution with zscore and known SD:", left_z_known_exp, right_z_known_exp)
paste("Confidence Interval of Normal Distribution with tscore and unknown SD:", left_t_unknown_norm, right_t_unknown_norm)
paste("Confidence Interval of Normal Distribution with tscore and unknown SD:", left_t_unknown_exp, right_t_unknown_exp)



