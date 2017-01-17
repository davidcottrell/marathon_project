library(MASS)
library(tidyverse)
library(lubridate)
df <- read.csv("data/times.csv", stringsAsFactors = FALSE)

# Calculate the percent of runners who did not finish.
# (includes woman who did not start)
dnf_rate <- sum(is.na(df$FINAL))/length(df$FINAL)

# Estimate linear relationship between personal best and marathon result, excluding twins.
m <- lm(FINAL ~ PB, data = df[is.na(df$TWINS),])

# Store the total number of runners, N
N <- length(df$PB)

# Save the vector of personal bests
x <- df$PB

# Create null vectors for simulation
hahner_sims <- vector()
kim_sims <- vector()
luik_sims <- vector()

# Set seed
set.seed(14252345)

# Set total number of sims
total_sims <- 10000

# Begin simulation
for (i in 1:total_sims){
  
  # Determine whether each runner finishes given the dnf rate
  dnf <- sample(x = c(F,T), size = N, replace = T, prob = c(1-dnf_rate, dnf_rate) )
  
  # Draw each runner's result from a multivariate normal distribution with 
  # mean Beta and sigma equal to the variance-covariance of Beta
  # Draw an error term from a normal distibution with standard deviation equal 
  # to the standard deviation of the model residuals.
  B <- mvrnorm(n = N, mu = coef(m), Sigma =  vcov(m))
  e <- rnorm(n = N, mean = 0, sd =sd(m$residuals))
  
  # Remove the results of those that did not finish
  y_hat <- ifelse(dnf, NA, B[,1] + x*B[,2] + e )
  

  # Calculate the ranking of each of the twins
  hahner_pos <- min_rank(y_hat)[df$TWINS == "Hahner Twins" & !is.na(df$TWINS)]
  kim_pos <- min_rank(y_hat)[df$TWINS == "Kim Twins" & !is.na(df$TWINS)]
  luik_pos <- min_rank(y_hat)[df$TWINS == "Luik Triplets" & !is.na(df$TWINS)]
  
  hahner_sims[i] <- abs(hahner_pos[1] - hahner_pos[2]) <= 1 
  kim_sims[i] <- abs(kim_pos[1] - kim_pos[2]) <= 1 
  luik_sims[i] <- abs(luik_pos[1] - luik_pos[2]) <= 1 | abs(luik_pos[2] - luik_pos[3]) <= 1 | abs(luik_pos[1] - luik_pos[3]) <= 1
}

# Calculate the percent of the races where the twins placed in consecutive order.
hahner_sims <- ifelse(is.na(hahner_sims), FALSE, hahner_sims)
pct_consecutive_hahner <- sum(hahner_sims)/length(hahner_sims)

kim_sims <- ifelse(is.na(kim_sims), FALSE, kim_sims)
pct_consecutive_kim <- sum(kim_sims)/length(kim_sims)

luik_sims <- ifelse(is.na(luik_sims), FALSE, luik_sims)
pct_consecutive_luik <- sum(luik_sims)/length(luik_sims)

hahner_and_kim_sims <- hahner_sims & kim_sims
hahner_and_kim_sims <- ifelse(is.na(hahner_and_kim_sims), FALSE, hahner_and_kim_sims)
pct_consecutive_hahner_and_kim <- sum(hahner_and_kim_sims)/length(hahner_and_kim_sims)

cat("The race was simulated", total_sims, "times.")
cat("The Hahner twins place consecutively in ", round(pct_consecutive_hahner*100, 2), "% of the simulations.", sep = "")
cat("The Kim twins place consecutively in ", round(pct_consecutive_kim*100, 2), "% of the simulations.", sep = "")
cat("At least one of the Luik twins place consecutively in ", round(pct_consecutive_luik*100, 2), "% of the simulations.", sep = "")
cat("Both the Hahner and Kim twins place consecutively in ", round(pct_consecutive_hahner_and_kim*100, 2), "% of the simulations.", sep = "")

