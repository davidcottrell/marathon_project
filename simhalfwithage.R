# Calculate the percent of runners who did not finish.
# (includes women who did not start)
df_half <- df[!is.na(df$SPLIT_HALF),] 
dnf_rate <- sum(is.na(df_half$FINAL))/length(df_half$FINAL)

# Estimate linear relationship between personal best and marathon result, excluding twins.
m <- lm(FINAL ~ SPLIT_HALF + I(SPLIT_HALF^2) + years2marathon, data = df_half[is.na(df_half$TWINS),])

# Store the total number of runners, N
N <- length(df_half$SPLIT_HALF)

# Save the vector of personal bests
x <- df_half$SPLIT_HALF
age <- df_half$years2marathon

# Create data frame to capture simulation output
sim_df <- data_frame(ATHLETE = df_half$ATHLETE)

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
  B <- mvrnorm(n = 1, mu = coef(m), Sigma =  vcov(m))
  e <- rnorm(n = N, mean = 0, sd = summary(m)$sigma)
  
  # Remove the results of those that did not finish
  y_hat <- ifelse(dnf, NA, B[1] + x*B[2] + x^2*B[3] + age*B[4] + e )
  
  # Collect the results
  sim_df[[paste0(i)]] <- y_hat
  
}

sim_df <- sim_df %>% gather(SIM, YHAT, -ATHLETE, convert = T) %>% group_by(SIM) %>% mutate(RANK = min_rank(YHAT))


# Compare Anna to Lisa
#
anna <- df %>% filter(ATHLETE == "Anna Hahner")
anna_sim <- sim_df %>% filter(ATHLETE == "Anna Hahner")

lisa <- df %>% filter(ATHLETE == "Lisa Hahner")
lisa_sim <- sim_df %>% filter(ATHLETE == "Lisa Hahner")

hahner_dist <- abs(anna_sim$YHAT - lisa_sim$YHAT) <= abs(anna$FINAL - lisa$FINAL)
hahner_dist <- ifelse(is.na(hahner_dist), FALSE, hahner_dist)

hahner_rank <- abs(anna_sim$RANK - lisa_sim$RANK) <= 1
hahner_rank <- ifelse(is.na(hahner_rank), FALSE, hahner_rank)

pct_less_than_hahner <- sum(hahner_dist)/length(hahner_dist)
pct_consecutive_hahner <- sum(hahner_rank)/length(hahner_rank)

hahner_df <- data_frame(time_diff = abs(anna_sim$YHAT - lisa_sim$YHAT), rank_diff =  abs(anna_sim$RANK - lisa_sim$RANK))

pdf("plots/simulated_time_half_with_age.pdf", height = 5, width = 5)
p <- ggplot(hahner_df, aes(time_diff)) + 
  geom_histogram(binwidth = 30, colour = "black", fill = NA) +
  xlab("\nFinishing time difference, in seconds") +
  ylab("Count\n") +
  xlim(0, max_t) + 
  #  ggtitle("\nSimulated final time") +
  theme_bw()
print(p)
dev.off()

pdf("plots/simulated_rank_half_with_age.pdf", height = 5, width = 5)
p2 <- ggplot(hahner_df, aes(rank_diff)) + 
  geom_histogram(binwidth = 1, colour = "black", fill = NA) +
  xlab("\nDifference in rank") +
  ylab("Count\n") +
  xlim(0, max_r) + 
  #  ggtitle("\nSimulated final rank") +
  theme_bw()
print(p2)
dev.off()
