#Load lab1.csv
Lab1 <- read.csv(file="Lab1.csv", header=TRUE)
#Display summary statistics for earn
summary(Lab1$EARN)
#Display frequencies of the variable job.class
table(Lab1$Job.class)
#Display a three-way cross-tabulation of the proportions of variables Educational Level, Gender and Job.class
table.proportions = table(Lab1$EDUC, Lab1$Gender, Lab1$Job.class)
prop.table(table.proportions)

#Basic histogram of the variable EARN
hist(Lab1$EARN)
#Boxplot of EARN by Job Class
boxplot(Lab1$EARN~Lab1$Gender)
#New variable Lab1$EARNx10000 = Lab1$EARN/10000
Lab1$EARNx10000 = Lab1$EARN/10000
#Scatterplot of EARNx1000 on x and AGE on y
plot(Lab1$EARNx10000, Lab1$AGE)
