# Calculate the percent of runners who did not finish.
# (includes women who did not start)
df_half <- df[!is.na(df$SPLIT_HALF),] 
dnf_rate <- sum(is.na(df_half$FINAL))/length(df_half$FINAL)

# Estimate linear relationship between personal best and marathon result, excluding twins.
# m <- lm(FINAL ~ SPLIT_HALF + I(SPLIT_HALF^2), data = df_half[is.na(df_half$TWINS),])
m <- lm(FINAL ~ SPLIT_HALF, data = df_half[is.na(df_half$TWINS),])

# Store the total number of runners, N
N <- length(df_half$SPLIT_HALF)

# Save the vector of personal bests
x <- df_half$SPLIT_HALF

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
  # y_hat <- ifelse(dnf, NA, B[1] + x*B[2] + x^2*B[3] +  e )
  y_hat <- ifelse(dnf, NA, B[1] + x*B[2] +  e )
  
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

hahner_df <- data_frame(time_diff = abs(anna_sim$YHAT - lisa_sim$YHAT), rank_diff =  abs(anna_sim$RANK - lisa_sim$RANK))

pct_less_than_hahner <- sum((hahner_df$time_diff) <= 1, na.rm = T ) / length(na.omit(hahner_df$time_diff))
pct_less_than_min <- sum((hahner_df$time_diff) < 60, na.rm = T ) / length(na.omit(hahner_df$time_diff))
pct_consecutive <- sum((hahner_df$rank_diff) <= 1, na.rm = T ) / length(na.omit(hahner_df$time_diff))

if (exists("p")) {rm(p)}
if (exists("p2")) {rm(p2)}

pdf("plots/simulated_time_half.pdf", height = 5, width = 5)
p <- ggplot(filter(hahner_df, !is.na(time_diff)), aes(time_diff, fill = time_diff < 60 )) + 
  geom_histogram(breaks = seq(0,max_x_t, 30), colour = "black") +
  xlab("\nTime difference, in seconds") +
  ylab("Count\n") +
  ylim(0, max_y_t) + 
  scale_fill_manual(values = c(NA, "red"), guide = F) +
  annotate("text", y = 400, x = 1500, label = paste0("< 1 min:  ", round(pct_less_than_min*100, 1), "%"), size = 5, col = "red") +
  theme_bw()
print(p)
dev.off()

pdf("plots/simulated_rank_half.pdf", height = 5, width = 5)
p2 <- ggplot(filter(hahner_df, !is.na(rank_diff)), aes(rank_diff, fill = rank_diff <= 1 )) + 
  geom_histogram(binwidth=1, center=0, colour = "black") +
  xlab("\nRank difference") +
  ylab("Count\n") +
  ylim(0, max_y_r) + 
  xlim(0, max_x_r) + 
  scale_fill_manual(values = c(NA, "red"), guide = F) +
  annotate("text", y = 225, x = 80, label = paste0("Consecutive finishes:  ", round(pct_consecutive*100, 1), "%"), size = 5, col = "red") +
  theme_bw()
print(p2)
dev.off()

cat("The race was simulated ", total_sims," times.\n", sep = "")
cat("The Hahner twins place consecutively in ", round(pct_consecutive*100, 2), "% of simulated finishes\n", sep = "")
cat("The average simulated time between them was ", round(mean(hahner_df$time_diff/60, na.rm = T), 2), " minutes\n", sep = "")
cat("95% simulated interval for Anna Hahner finishing time in seconds:",round(quantile(x = anna_sim$YHAT, probs = c(0.025, 0.975), na.rm = TRUE), 0), "\n", sep = " ")
cat("95% simulated interval for Lisa Hahner finishing time in seconds:",round(quantile(x = lisa_sim$YHAT, probs = c(0.025, 0.975), na.rm = TRUE), 0), "\n", sep = " ")
