faithful.lm <- lm(eruptions ~ waiting, data=faithful)  # Build a linear regression model of faithful data

# Question 1
faithful.residuals = resid(faithful.lm) # Obtain the residuals of the faithful linear regression model
plot(density(faithful.residuals), main="Density Plot: Residuals", ylab="Frequency") # Plot the density of them

# Question 2
par(mfrow=c(2,2)) # Divide graph space into 4
plot(faithful.lm) # Plot 4 graphs

# Question 3
# In relation to the many plots produced by the data some potential conclusions can be drawn.
# Residuals vs Leverage:
#   The residuals vs leverage plot will show us points outside a red dashed line if any residuals are seen to be 
#   unimportant. As these red dashed lines don't even appear on the plot this indicates all observations are important 
#   and is what we want to see.
# Normal Q-Q:
#   From the normal Q-Q plot we can determine wether or not the residuals are normally distributed. We can see from the
#   shape of the dots and how they make a line that it would indeed appear to be normally distributed and has little to 
#   no skew in any direction.
# Density Plots:
#   When viewing the density plots for eruptions and waiting from last week it they are seen to have very similar shape.
#   This tells us that there is potentially some link between the two variables when one is influenced by the other.
#   In terms of the density plot of residuals it can also be seem to have a somewhat similar shape to the previously
#   mentioned. However it does vary greatly in that there is no dip in the centre in terms of frequency, unlike the
#   other density plots. This however would not be sufficient evidence to say our model is unappropriate and so all
#   seems in order.
# One final check that could be done would be to take a look at our adjusted R-squared value:
#   The value for our linear model is 0.8108 . This tells us that a large amount of the variance in our dependent
#   variable can be explained by our independent variable and a high value like this is what we want to see assuming we
#   want our model to be appropriate. Also the difference between adjusted R-squared and Multiple R-square is very small
#   indicating we have not overfit our model.
