survey <- read.csv(file="survey.csv", header=TRUE)
tbl <- table(survey$Smoke, survey$Exer)
chisq.test(tbl)

#Results:
#X-squared = 5.4885, df = 6, p-value = 0.4828

#Questions:
#1) The degrees of freedom are (numRows - 1) * (numCols - 1)
#
#2) Assumptions:
#   For this test it is assumed all expected values are greater than 1 and no more than 20% are less than 5.
#   As per usual a simple random sample is presumed.
#   Each observation is assumed to be independent of one another.
#
#3) It gave a warning that the approxiamtion of the test may be incorrect because the expected values in the test were
#   quite small and so the p-value is likely to be inaccurate.
#
#4) H0: There is no relationship between the data
#   HA: There is an relationship between the data
#
#5) With alpha = 0.05 since p < alpha we fail to reject the null hypothesis and conclude from the test that there is
#   no relationship between the two data sets. However due to the warning message given this result may not necessarily
#   be correct as the resulting p-value and test-statistic can be incorrect.