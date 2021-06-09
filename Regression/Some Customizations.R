########################################################################
#
#         Simulation-based power analysis for regression w/ Andrew Hales
#         Resources osf.io.e42mg
#         Organized by PsyPag Simulation Summer School
#         Video:  https://www.youtube.com/watch?v=x2hhc5KYw-c
#
########################################################################

# Q: What if I expect different group sizes for categorical predictors?----
# A: Use "prob = " argument in sample()

# Simulating independent variables

simulated_int_data <- data.frame(
  x = rnorm(n = 200, mean = 5, sd = 1),
  w = sample(c(0, 1), 
             prob = c(.1, .9),   # Use "prob = " to change group sizes
             size = 200, 
             replace = TRUE
             )) %>%  
  mutate(x_centered = x - mean(x))                      

# Simulating dependent variable
simulated_int_data <- simulated_int_data %>% 
  mutate(
    y = 2.80 + (.21 * x_centered) + (.6 * w) + (.3 * x_centered * w) + 
      rnorm(n = 200, mean = 0, sd = .74))

# Testing it out
ggplot(simulated_int_data, aes(x_centered, y, color = as.factor(w))) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(-3, 3, 1), limits = c(-3, 3)) +
  theme(text = element_text(size = 20)) 


# Q: What if I want to make sure simulated values are within scale-range? ----
# A: Recode outliers: 
simulated_data <- data.frame(
  x = rnorm(n = 1000, mean = 5, sd = 1))

# Creating Dependent variable
simulated_data <- simulated_data %>% 
  mutate(
    y = 1.80 + .21*x + rnorm(n = 1000, mean = 0, sd = .74),
    x.trimmed = case_when(
      x < 1 ~ 1,
      x > 7 ~ 7,
      TRUE ~ x),
    y.trimmed = case_when(
      y < 1 ~ 1,
      y > 7 ~ 7,
      TRUE ~ y
    )
    ) 
summary(simulated_data$x)
summary(simulated_data$x.trimmed)
summary(simulated_data$y)
summary(simulated_data$y.trimmed)

# Q: What if my independent variables are correlated? ----
# A: Use rnorm_multi() from the faux package:
library(faux)
simulated_data <- rnorm_multi(
  n = 1000,
  vars = 2,
  mu = 5,
  sd = 1,
  r = .3,
  varnames = c("x", "w")
)

# Creating DV
simulated_data <- simulated_data %>% 
  mutate(
    y = 1.80 + .21*x + .53*w + rnorm(n = 1000, mean = 0, sd = .74))
ggplot(simulated_data, aes(x, y)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(text = element_text(size = 20))
ggplot(simulated_data, aes(x, w)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(text = element_text(size = 20))


# Q: What if I want to simulate skewed variables?----
# A: Play with norm2beta() from faux.
simulated_data <- data.frame(
  x = rnorm(n = 1000, mean = 5, sd = 1))
hist(simulated_data$x)
simulated_data <- simulated_data %>% 
  mutate(x_skewed = norm2beta(x, 5, 2))
hist(simulated_data$x_skewed)

