########################################################################
#
#         Simulation-based power analysis for regression w/ Andrew Hales
#         Resources osf.io.e42mg
#         Organized by PsyPag Simulation Summer School
#         Video:  https://www.youtube.com/watch?v=x2hhc5KYw-c
#
########################################################################

# note: Mean Centering is:  x - mean(x)  This script has been revised to correct mc
# Setup ----

# Loading package
library(tidyverse)
library(broom)
library(faux)

# Simulating Single Simple Dataset ----

# Simulating a single dataset based on what we observed in prior study

# Creating Independent Variable:
simulated_data <- data.frame(
  x = rnorm(n = 100, mean = 5, sd = 1)
  )

# Creating Dependent variable
simulated_data <- simulated_data %>% 
  mutate(
    y = 1.80 + .21*x + rnorm(n = 100, mean = 0, sd = .74)
    ) 
# Taking a look at our simulated dataset:
ggplot(simulated_data, aes(x, y)) + 
  geom_point() +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(1, 8, 1), limits = c(1, 8)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Self-Esteem", y = "Happiness") +
  theme(text = element_text(size = 20))

# Analyzing our single dataset
summary(
  lm(y ~ x, data = simulated_data)
)

# Wrapping into function ----

# Function to simulate a single datset:
sim.dataset <- function (n, b0, b1, residual_se) {
  
  # Simulate X variable:
  simulated_data <- data.frame(
    x = rnorm(n = n, mean = 5, sd = 1)
    )
  
  # Simulate Y variable:
  simulated_data <- simulated_data %>% 
    mutate(
      y = b0 + (b1*x) + rnorm(n = n, mean = 0, sd = residual_se)
      )
  
  # Return dataset
  return(simulated_data)
  
}

# Trying out our new tool.
test_dataset <- sim.dataset(n = 150, b0 = 2.80, b1 = .21, residual_se = .74)
ggplot(test_dataset, aes(x, y)) + 
  geom_point() +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(1, 8, 1), limits = c(1, 8)) +
  theme(text = element_text(size = 20)) + 
  geom_smooth(method = "lm") 
summary(lm(y ~ x, data = test_dataset))

# Looping many simulations ----

# Dataframe to hold results:
coef.power <- data.frame(
  sig.b0 = rep(NA, 1000),
  sig.b1 = rep(NA, 1000)
  )

# Simulating many tests:
for (dataset.i in 1:1000) {
  simulated_df <- sim.dataset(n = 100, b0 = 2.80, b1 = .21, residual_se = .76)
  model <- tidy(lm(y ~ x, data = simulated_df))
  coef.power$sig.b0[dataset.i] <- model$p.value[1] < .05
  coef.power$sig.b1[dataset.i] <- model$p.value[2] < .05
}


# Now wrapping this into a "calculate_power" function that simulates and
# aggregates many datasets and returns power
simulate.power <- function(n.sims, n, b0, b1, residual_se) {
  
  # Dataframe to hold results:
  coef.power <- data.frame(
    sig.b0 = rep(NA, n.sims),
    sig.b1 = rep(NA, n.sims))
  
  # Simulating many tests:
  for (dataset.i in 1:n.sims) {
    simulated_df <- sim.dataset(n = n, b0 = b0, b1 = b1, residual_se = residual_se)
    model <- tidy(lm(y ~ x, data = simulated_df))
    coef.power$sig.b0[dataset.i] <- model$p.value[1] < .05
    coef.power$sig.b1[dataset.i] <- model$p.value[2] < .05
  }
  
  # Aggregating into summary 
  coef.power <- summarise(coef.power,
                          b0.power = mean(sig.b0),
                          b1.power = mean(sig.b1))
  
  # Returning a summary dataframe
  return(coef.power)
}

# Calculating some power (finally!)
simulate.power(n.sims = 500, n = 100, 
               b0 = 2.80, b1 = .21, residual_se = .74)


# Simulating Multiple Regression, Interaction ----

# Simulating independent variables
simulated_int_data <- data.frame(
  x = rnorm(n = 200, mean = 5, sd = 1),
  w = sample(c(0, 1), size = 200, replace = TRUE)) %>%  # Use "sample" to generate categorical IV
  mutate(x_centered = x-mean(x))                      # Center continuous IV for easier interpretation
###*** x - mean(x) to center***----  
# Simulating dependent variable
simulated_int_data <- simulated_int_data %>% 
  mutate(
    y = 2.80 + (.21 * x_centered) + (.6 * w) + (.3 * x_centered * w) + 
      rnorm(n = 200, mean = 0, sd = .74))

# Testing it out
ggplot(simulated_int_data, aes(x_centered, y, color = as.factor(w))) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(-3, 3, 1), limits = c(-3, 3)) +
  theme(text = element_text(size = 20)) 


# Function to generate a single dataset for an interaction design
sim.dataset_inctn <- function (n, b0, b1, b2, b3, residual_se) {
  
  # Simulating Independent Variables
  simulated_int_data <- data.frame(
    x = rnorm(n = n, mean = 0, sd = 1),
    w = sample(c(0, 1), size = n, replace = TRUE)) %>% 
    mutate(x_centered = x-mean(x))
  
  # Simulating Dependent Variables
  simulated_int_data <- simulated_int_data %>% 
    mutate(
      y = b0 + (b1*x_centered) + (b2*w) + (b3*x_centered*w) + 
        rnorm(n = n, mean = 0, sd = residual_se))
  return(simulated_int_data)
}

# Testing it out
test.data <- sim.dataset_inctn(n = 100, b0 = 2.8, b1 = .21, b2 = .6, b3 = .3,
                 residual_se = .74)
ggplot(test.data, aes(x_centered, y, color = as.factor(w))) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(-3, 3, 1), limits = c(-3, 3)) +
  theme(text = element_text(size = 20)) 

# Creating function to simulate power based on input parameters
# Creating function to vary parameters over simulations:
simulate.power_intcn <- function(n.sims, n, b0, b1, b2, b3, residual_se) {
  
  # To hold results
  coef.power_inctn <- data.frame(
    sig.b0 = rep(NA, n.sims),
    sig.b1 = rep(NA, n.sims),
    sig.b2 = rep(NA, n.sims),
    sig.b3 = rep(NA, n.sims)
  ) 
  
  # Looping and saving
  for (dataset.i in 1:1000) {
    simulated_df <- sim.dataset_inctn(n = n, b0 = b0, b1 = b1, b2 = b2, 
                                      b3 = b3, residual_se = residual_se)
    model <- tidy(lm(y ~ x_centered*w, data = simulated_df))
    coef.power_inctn$sig.b0[dataset.i] <- model$p.value[1] < .05
    coef.power_inctn$sig.b1[dataset.i] <- model$p.value[2] < .05
    coef.power_inctn$sig.b2[dataset.i] <- model$p.value[3] < .05
    coef.power_inctn$sig.b3[dataset.i] <- model$p.value[4] < .05
  }
  
  # Aggregating
  coef.power_inctn <- summarise(coef.power_inctn,
                                b0.power = mean(sig.b0),
                                b1.power = mean(sig.b1),
                                b2.power = mean(sig.b2),
                                b3.power = mean(sig.b3)
  )
  
  # Returning summary
  return(coef.power_inctn)
}

# Calculating power for all coefficients across a range of sample sizes
simulate.power_intcn(n.sims = 1000, 
                     n = 100, 
                     b0 = 2.8, b1 = .21, b2 = .6, b3 = .3,
                     residual_se = .74)
simulate.power_intcn(n.sims = 1000, 
                     n = 200, 
                     b0 = 2.8, b1 = .21, b2 = .6, b3 = .3,
                     residual_se = .74)
simulate.power_intcn(n.sims = 1000, 
                     n = 300, 
                     b0 = 2.8, b1 = .21, b2 = .6, b3 = .3,
                     residual_se = .74) 



