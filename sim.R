# Calculate the percent of runners who did not finish.
# (includes women who did not start)
dnf_rate <- sum(is.na(df$FINAL))/length(df$FINAL)

# Estimate linear relationship between personal best and marathon result, excluding twins.
#m <- lm(FINAL ~ PB + I(PB^2), data = df[is.na(df$TWINS),])
m <- lm(FINAL ~ PB, data = df[is.na(df$TWINS),])

# Store the total number of runners, N
N <- length(df$PB)

# Save the vector of personal bests
x <- df$PB

# Create data frame to capture simulation output
sim_df <- data_frame(ATHLETE = df$ATHLETE)

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
  #y_hat <- ifelse(dnf, NA, B[1] + x*B[2] + x^2*B[3] +  e )
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

hahner_df_sim <- data_frame(anna_final = anna_sim$YHAT,
                            lisa_final = lisa_sim$YHAT,
                            anna_rank = anna_sim$RANK,
                            lisa_rank = lisa_sim$RANK,
                            time_diff = abs(anna_sim$YHAT - lisa_sim$YHAT), 
                            rank_diff =  abs(anna_sim$RANK - lisa_sim$RANK))

less_than_hahner <- sum((hahner_df_sim$time_diff) <= 1, na.rm = T ) 
less_than_min <- sum((hahner_df_sim$time_diff) < 60, na.rm = T )
consecutive <- sum((hahner_df_sim$rank_diff) <= 1, na.rm = T )
pct_less_than_hahner <-less_than_hahner / total_sims
pct_less_than_min <- less_than_min / total_sims
pct_consecutive <- consecutive / total_sims

if (exists("p")) {rm(p)}
if (exists("p2")) {rm(p2)}


pdf("plots/simulated_time.pdf", height = 5, width = 5)
p<-ggplot(filter(hahner_df_sim, !is.na(time_diff)), aes(time_diff, fill = time_diff < 60 )) + 
  geom_histogram(breaks = seq(0,max_x_t, 30), colour = "black") +
  xlab("\nTime difference, in seconds") +
  ylab("Count\n") +
  ylim(0, max_y_t) + 
  scale_fill_manual(values = c(NA, "red"), guide = F) +
  annotate("text", y = text_y*max_y_t, x = text_x*max_x_t, label = paste0("< 1 min:  ", less_than_min), size = 5, col = "red") +
  theme_bw()
print(p)
dev.off()

pdf("plots/simulated_rank.pdf", height = 5, width = 5)
p2<-ggplot(filter(hahner_df_sim, !is.na(rank_diff)), aes(rank_diff, fill = rank_diff <= 1 )) + 
  geom_histogram(binwidth=1, center=0, colour = "black") +
  xlab("\nRank difference") +
  ylab("Count\n") +
  ylim(0, max_y_r) + 
  xlim(0, max_x_r) + 
  scale_fill_manual(values = c(NA, "red"), guide = F) +
  annotate("text", y = text_y*max_y_r, x = text_x*max_x_r, label = paste0("Consecutive finishes:  ", consecutive), size = 5, col = "red") +
  theme_bw()
print(p2)
dev.off()

cat("The race was simulated ", total_sims," times.\n", sep = "")
cat("The Hahner twins place consecutively in ", round(pct_consecutive*100, 2), "% of simulations\n", sep = "")
cat("The average simulated time between them was ", round(mean(hahner_df_sim$time_diff/60, na.rm = T), 2), " minutes\n", sep = "")
cat("95% simulated interval for Anna Hahner finishing time in seconds:",round(quantile(x = anna_sim$YHAT, probs = c(0.025, 0.975), na.rm = TRUE), 0), "\n", sep = " ")
cat("95% simulated interval for Lisa Hahner finishing time in seconds:",round(quantile(x = lisa_sim$YHAT, probs = c(0.025, 0.975), na.rm = TRUE), 0), "\n", sep = " ")
