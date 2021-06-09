##################################################################
# 
# 
#   Mean Centering by Shawn Janzen
#   13.10 Multiple Linear Regression: Mean-Center & Standardization 
#   YouTube: https://www.youtube.com/watch?v=FYjuu9_5oMQ
#   
################################################################## 


# 1. Take all of OBS and subtract the mean -> And the new average is zero.
# 
# 2. Standardization does the same but obs/sd 
#    will transform the mean centered scores into z scored.
# 
# 3.  instead of being spread in terms of tne measured variable, 
#     it will be spread in terms of the standard deviation, 
#     i.e. "normalized distances from the mean"

# Why do this?
# 4. Both have a mean of zero
# 5. The mean-centered can interpret a y-intercept 
#    (standardized, not so much)
# 6. mean centered, retains the unit of measure
# 7. mean centered Scales stays the same = same slope coefficients
# 8. Standardized, Rescales the variable to SD
# 9. Standardized Can help with multicollinearity
# 10. Standadrized can help When you have interactions, or polynomial terms
. 
mtcars$hp

## Mean-Center----
mtcars$hp.mc <- mtcars$hp - mean(mtcars$hp)

## Standardize to z-scores----
# scale is a base R function
mtcars$hp.st <- scale(mtcars$hp)

## Look at Regression coefficients----

summary(lm(qsec ~ hp, data=mtcars))

summary(lm(qsec ~ hp.mc, data=mtcars))

summary(lm(qsec ~ hp.st, data=mtcars))
